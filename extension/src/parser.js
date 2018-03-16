import debounce from "lodash/debounce";

const TIMELINE_SELECTOR = ".userContentWrapper";
const SIDEBAR_SELECTOR = ".ego_unit";
const DEBUG =
  (process.env.NODE_ENV === "dev" || process.env.NODE_ENV) === "development"
    ? "development"
    : "production";

// This function cleans all the elements that could leak user data
// before sending to the server. It also removes any attributes that
// could have personal data so we end up with a clean dom tree.
const selectors = [
  "video",
  "input",
  "button",
  "iframe",
  'a[href=""]',
  ".accessible_elem",
  ".uiLikePagebutton",
  ".uiPopOver",
  ".uiCloseButton",
  ".uiChevronSelectorButton",
  "h5._1qbu",
  ".commentable_item"
].join(", ");

const cleanAd = html => {
  let node = document.createElement("div");
  node.innerHTML = html;

  // We're not saving video ads for now, we don't need buttons, hidden forms,
  // or like links
  Array.from(node.querySelectorAll(selectors)).forEach(i => i.remove());

  // remove attributes
  const killAttrs = node => {
    Array.from(node.attributes).forEach(attr => {
      if (
        attr.name !== "id" &&
        attr.name !== "class" &&
        attr.name !== "src" &&
        attr.name !== "href"
      )
        node.removeAttribute(attr.name);
      // remove tracking get variables from links and l.facebook.com
      if (attr.name === "href") {
        try {
          let url = new URL(attr.value);
          if (url.host === "l.facebook.com")
            url = new URL(new URLSearchParams(url.search).get("u"));
          if (url.origin && url.pathname) {
            node.setAttribute(attr.name, url.origin + url.pathname);
          } else {
            node.removeAttribute(attr.name);
          }
        } catch (e) {
          node.removeAttribute(attr.name);
        }
      }
    });
    // walk through the tree.
    Array.from(node.children).forEach(killAttrs);
  };
  Array.from(node.children).forEach(killAttrs);
  return node.innerHTML.replace(/&amp;/g, "&");
};

const checkSponsor = node => {
  return Array.from(node.querySelectorAll(".clearfix a, .ego_section a")).some(
    a => {
      a = a.cloneNode(true);
      const canary = Array.from(a.querySelectorAll("._2lgs"));
      Array.from(canary).forEach(canary => canary.remove());
      const text = a.textContent;
      const style = window
        .getComputedStyle(a, ":after")
        .getPropertyValue("content");
      return [
        "Gesponsord",
        "Sponsored",
        "Gesponsert",
        "Sponsrad",
        "Sponsorlu",
        "Sponsoroitu",
        "إعلان مُموَّل",
        "Sponsoreret",
        "Sponsorizzata",
        "Chartered"
      ].some(sponsor => {
        if (text === sponsor || style === `"${sponsor}"`) return true;
        return false;
      });
    }
  );
};

// We have to do this because content scripts can't read window variables
const grabVariable = (fn, args) => {
  let script = document.createElement("script");
  script.textContent =
    'localStorage.setItem("pageVariable", (' +
    fn +
    ").apply(this, " +
    JSON.stringify(args) +
    "));";
  script.setAttribute("id", "pageVariable");
  (document.head || document.documentElement).appendChild(script);
  script.remove();
  return localStorage.getItem("pageVariable");
};

// Getting the targeting information. We're careful to cache any results so as to avoid being
// blocked by facebook.
let targetingBlocked = false;
let targetingCache = new Map();
const getTargeting = ad => {
  if (ad.targeting) {
    if (targetingCache.has(ad.targeting))
      return Promise.resolve({
        ...ad,
        targeting: targetingCache.get(ad.targeting)
      });
    const url = ad.targeting;
    delete ad.targeting;
    if (targetingBlocked) return ad;
    return new Promise(resolve => {
      let req = new XMLHttpRequest();
      req.onreadystatechange = function() {
        if (req.readyState === 4) {
          try {
            const targeting = cleanAd(
              JSON.parse(req.response.replace("for (;;);", ""))["jsmods"][
                "markup"
              ][0][1]["__html"]
            );
            if (!targeting) {
              return resolve(ad);
            }
            targetingCache.set(url, targeting);
            resolve({
              ...ad,
              targeting
            });
          } catch (e) {
            targetingBlocked = true;
            setTimeout(() => (targetingBlocked = false), 15 * 60 * 100);
            ad.targeting = null;
            resolve(ad);
          }
        }
      };

      // This is all built out from a close reading of facebook's code.
      let built = grabVariable(
        url => {
          let parsed = new (window.require("URI"))(url);
          localStorage.setItem("url", url);
          let req = new window.AsyncRequest()
            .setURI(url)
            .setData(parsed)
            .setMethod("GET")
            .setRelativeTo(document.body)
            .setNectarModuleDataSafe(document.body)
            .setReadOnly(true);
          Object.assign(req.data, { __asyncDialog: 1 });
          Object.assign(req.data, window.require("getAsyncParams")(req.method));
          req._setUserActionID();
          req.setNewSerial();
          return req.uri.addQueryData(req.data).toString();
        },
        [url]
      );
      // AsyncRequest builds out the correct targeting url
      req.open("GET", "https://www.facebook.com" + built, true);
      req.send();
    });
  } else {
    ad.targeting = null;
    return Promise.resolve(ad);
  }
};

// We want to minimize the impact of a user's experience using facebook, so this function tries to
// restore the state of the page when the extension clicks around.
const refocus = cb => {
  const focus = document.activeElement;
  const ranges = [];
  for (var i = 0; i < window.getSelection().rangeCount; i++) {
    let range = window.getSelection().getRangeAt(i);
    ranges.push([
      range.startContainer,
      range.startOffset,
      range.endContainer,
      range.endOffset
    ]);
  }
  cb();
  if (focus) focus.focus();
  if (ranges.length > 0) {
    const newSelection = window.getSelection();
    newSelection.removeAllRanges();
    ranges.forEach(range_attrs => {
      const range = document.createRange();
      range.setStart(range_attrs[0], range_attrs[1]);
      range.setEnd(range_attrs[2], range_attrs[3]);
      newSelection.addRange(range);
    });
  }
};

// All of the menus on Facebook are asynchronously opened so we have to use a observer here to make
// sure we can grab the targeting urls. There's a ton of state here, so we should probably try and
// collapse it at some point.
let adCache = new Map();
const parseMenu = (ad, selector, toggle, toggleId, menuFilter, filter) => (
  resolve,
  reject
) => {
  let time = Date.now();
  let cb = (record, self) => {
    // give up if we haven't got anything after a second
    if (Date.now() - time > 1000) {
      self.disconnect();
      // in debug, mark the button green if we failed to get the menu for this ad.
      if (DEBUG) toggle.style.backgroundColor = "#630000";
      return reject("no menu");
    }
    const menu = menuFilter();
    if (!menu) return null;
    const li = Array.from(menu.querySelectorAll("li")).filter(filter)[0];
    if (!li) return null;
    const endpoint = li.querySelector("a");
    if (!endpoint) return null;
    const url = endpoint.getAttribute("ajaxify");
    refocus(() => toggle.click());
    if (DEBUG) toggle.style.backgroundColor = "unset";
    self.disconnect();
    try {
      const resolved = {
        ...ad,
        id: new URL("https://facebook.com" + url).searchParams.get("id"),
        targeting: url
      };

      if (resolved.id) {
        adCache.set(toggleId, resolved);
        resolve(resolved);
      } else {
        reject("No ad id");
      }
    } catch (e) {
      reject(e);
    }
  };

  new MutationObserver(debounce(cb, 250)).observe(
    document.querySelector("#globalContainer"),
    {
      childList: true,
      subtree: true
    }
  );
  refocus(() => toggle.click());
};

// Grab an id from a timeline ad which is hidden in a async popup.
const getTimelineId = (parent, ad) => {
  const control = parent.querySelector(".uiPopover");
  if (!control && control.id === "") return Promise.resolve(ad);

  const toggle = control.querySelector("a");
  if (!toggle) return Promise.resolve(ad);

  if (adCache.has(toggle.id)) return Promise.resolve(adCache.get(toggle.id));

  // this is async, we have to wait until our popup shows up.
  let promise = new Promise(
    parseMenu(
      ad,
      ".uiLayer",
      toggle,
      toggle.id,
      () =>
        Array.from(document.querySelectorAll(".uiLayer")).filter(
          a => a.getAttribute("data-ownerid") === toggle.id
        )[0],
      it =>
        it.getAttribute("data-feed-option-name") === "FeedAdSeenReasonOption"
    )
  ).then(getTargeting);
  return promise;
};

// Similar to the above -- while we could just use the data-ego-fbid from before, it makes sense to
// use the one in the encoded url in case that the dom one goes away.
const getSidebarId = (parent, ad) => {
  const control = parent.querySelector(".uiSelector");
  if (!control) return Promise.resolve(ad);
  //Replicating getTimelineId.
  // Since the sidebar DOM structure is slightly different we need to pull out
  // the toggle Id from the data-gt attribute.
  const toggle = control.querySelector("a");
  if (!toggle) return Promise.resolve(ad);

  const toggleData = JSON.parse(toggle.getAttribute("data-gt"));
  if (!toggleData["data_to_log"]) return Promise.resolve(ad);

  const toggleId = toggleData["data_to_log"]["ad_id"];
  if (!toggleId) return Promise.resolve(ad);

  if (adCache.has(toggleId)) return Promise.resolve(adCache.get(toggleId));

  let promise = new Promise(
    parseMenu(
      ad,
      ".uiMenu",
      toggle,
      toggleId,
      () => control,
      it => it.getAttribute("data-label") === "Why am I seeing this?"
    )
  ).then(getTargeting);

  return promise;
};

const timeline = node => {
  const sponsor = checkSponsor(node);
  // First we check if it is actually a sponsored post
  if (!sponsor) return Promise.resolve(false);

  // And then we try to grab the parent container that has a hyperfeed id
  let parent = node;
  while (parent) {
    let id = parent.getAttribute("id");
    if (id && id.startsWith("hyperfeed")) break;
    parent = parent.parentElement;
  }

  // If we've walked off the top of the parent heirarchy that's an error
  if (!parent) return Promise.resolve(false);

  // Also if there's nothing to save that's an error
  if (node.children.length === 0) return Promise.resolve(false);

  // Check to see that we have the innermost fbUserContent, this cuts out like's
  // and shares.
  if (node.querySelector(TIMELINE_SELECTOR))
    node = node.querySelector(TIMELINE_SELECTOR);

  // in debug, mark an ad green once we've selected it to be submitted (to help find ads that we
  // don't recognize or posts we mistakenly believe are ads)
  if (DEBUG) parent.style.color = "#006304";

  // Finally we have something to save.
  return getTimelineId(parent, {
    html: cleanAd(node.children[0].outerHTML),
    created_at: new Date().toString()
  });
};

// Sidebar ads are a bit more complicated.
const sidebar = node => {
  // Although first we still need to make sure we are in a sponsored box;
  let parent = node;
  while (parent) {
    if (checkSponsor(parent)) break;
    parent = parent.parentElement;
  }

  // As before it is an error if we haven't found a sponsor node.
  if (!(parent && parent.classList && parent.classList.contains("ego_section")))
    return Promise.resolve(false);

  // Sanity check to make sure we have a salvageable id
  if (!node.hasAttribute("data-ego-fbid")) return Promise.resolve(false);

  // and we have childnodes
  if (!node.children.length === 0) return Promise.resolve(false);
  // Then we just need to send the cleaned ad
  return getSidebarId(parent, {
    html: cleanAd(node.outerHTML),
    created_at: new Date().toString()
  });
};

// We are careful here to only accept a valid timeline ad or sidebar ad
const parser = function(node) {
  const list = node.classList;
  if (list.contains("userContentWrapper") || list.contains("_5pcr")) {
    return timeline(node);
  } else if (list.contains("ego_unit")) {
    return sidebar(node);
  } else {
    return Promise.resolve(false);
  }
};

module.exports = {
  parser,
  TIMELINE_SELECTOR,
  SIDEBAR_SELECTOR
};
