// This function cleans all the elements that could leak user data
// before sending to the server. It also removes any attributes that
// could have personal data so we end up with a clean dom tree.
const selectors = [
  'video',
  'input',
  'button',
  'iframe',
  'a[href=""]',
  '.accessible_elem',
  '.uiLikePagebutton',
  '.uiPopOver',
  '.uiCloseButton',
  '.uiChevronSelectorButton'
].join(', ');

const TIMELINE_SELECTOR = '.fbUserContent, .fbUserPost, ._5pcr';
const SIDEBAR_SELECTOR = '.ego_unit';

const cleanAd = (html) => {
  let node = document.createElement("div");
  node.innerHTML = html;

  // We're not saving video ads for now, we don't need buttons, hidden forms,
  // or like links
  Array.from(node.querySelectorAll(selectors))
    .forEach((i) => i.parentElement.removeChild(i));

  // remove attributes
  const killAttrs = (node) => {
    Array.from(node.attributes).forEach(attr => {
      if(attr.name !== "id" &&
         attr.name !== "class" &&
         attr.name !== "src" &&
         attr.name !== "href")
        node.removeAttribute(attr.name);
      // remove tracking get variables from links and l.facebook.com
      if(attr.name === "href") {
        try {
          let url = new URL(attr.value);
          if(url.host === 'l.facebook.com')
            url = new URLSearchParams(url.search).get("u");
          if(url.origin && url.pathname) {
            node.setAttribute(attr.name, url.origin + url.pathname);
          } else {
            node.removeAttribute(attr.name);
          }
        } catch(e) {
          node.removeAttribute(attr.name);
        }
      }

    });
    // walk through the tree.
    Array.from(node.children).forEach(killAttrs);
  };
  Array.from(node.children).forEach(killAttrs);
  return node.innerHTML.replace(/&amp;/g, '&');
};

const checkSponsor = (node) => {
  return Array.from(node.querySelectorAll("a")).some((a) => {
    return ['Sponsored','Gesponsert'].some((sponsor) => a.textContent === sponsor);
  });
};

// We have to do this because content scripts can't read window variables
const grabVariable = (fn, args)  => {
  let script = document.createElement("script");
  script.textContent = 'localStorage.setItem("pageVariable", (' + fn + ').apply(this, ' + JSON.stringify(args) + '));';
  script.setAttribute('id', 'pageVariable');
  (document.head||document.documentElement).appendChild(script);
  //script.remove();
  return localStorage.getItem("pageVariable");
};

// Getting the targeting routine. We use XMLHttpRequest here because facebook
// bans fetch.
let targetingBlocked = false;
let targetingCache = new Map();
const getTargeting = (ad) => {
  if(ad.targeting) {
    if(targetingCache.has(ad.targeting)) return {
      ...ad,
      targeting: targetingCache.get(ad.targeting)
    };
    if(targetingBlocked) return ad;
    const url = ad.targeting;
    delete ad.targeting;
    return new Promise((resolve, reject) => {
      let req = new XMLHttpRequest();

      req.onreadystatechange = function() {
        if(req.readyState === 4) {
          try {
            const targeting = JSON.parse(req.response.replace('for (;;);', ''))["jsmods"]["markup"][0][1]["__html"];
            if(!targeting) {
              targetingCache.set(url, true);
              return resolve(ad);
            }

            targetingCache.set(url, targeting);
            resolve({
              ...ad,
              targeting
            });
          } catch(e) {
            targetingCache.set(url, true);
            targetingBlocked = true;
            setTimeout(() => targetingBlocked = false, 15 * 60 * 100);
            reject(ad);
          }
        }
      };

      let built = grabVariable((url) => {
        let parsed = new (window.require('URI'))(url);
        localStorage.setItem('url', url);
        let req = new window.AsyncRequest()
          .setURI(url)
          .setData(parsed)
          .setMethod('GET')
          .setRelativeTo(document.body)
          .setNectarModuleDataSafe(document.body)
          .setReadOnly(true);
        Object.assign(req.data, {__asyncDialog: 1});
        Object.assign(req.data, window.require('getAsyncParams')(req.method));
        req._setUserActionID();
        req.setNewSerial();
        return req.uri.addQueryData(req.data).toString();
      }, [url]);
      // AsyncRequest builds out the correct targeting url
      req.open('GET', "https://www.facebook.com" + built, true);
      req.send();
    });
  } else {
    return Promise.resolve(ad);
  }
};

const refocus = (cb) => {
  const focus = document.activeElement;
  cb();
  if(focus) focus.focus();
};

let timelineCache = new Map();
const getTimelineId = (parent, ad) => {
  const control = parent.querySelector(".uiPopover");
  if(!control && control.id === "")
    return null;

  const toggle = control.querySelector("a");
  if(!toggle)
    return null;

  if(timelineCache.has(toggle.id))
    return Promise.resolve(timelineCache.get(toggle.id));

  // this is async, we have to wait until our popup shows up.
  let promise = new Promise((resolve, reject) => {
    let cb = (record, self) => {
      const layer = Array.from(document.querySelectorAll(".uiLayer"))
        .filter((a) => {
          return a.getAttribute("data-ownerid") === toggle.id;
        })[0];
      if(!layer) return null;
      const li = Array.from(layer.querySelectorAll("li"))
        .filter((it) => it.getAttribute("data-feed-option-name") === "FeedAdSeenReasonOption")[0];
      if(!li) return null;
      const endpoint = li.querySelector("a");
      if(!endpoint) return null;
      const url =  endpoint.getAttribute("ajaxify");
      refocus(() => toggle.click());
      self.disconnect();
      try {
        const resolved = {
          ...ad,
          id: new URL("https://facebook.com" + url).searchParams.get("id"),
          targeting: url
        };

        if(resolved.id) {
          timelineCache.set(toggle.id, resolved);
          resolve(resolved);
        } else {
          reject("No ad id");
        }
      } catch(e) {
        reject(e);
      }
    };

    new MutationObserver(cb).observe(document, {childList: true, subtree:true});
  }).then(getTargeting);
  refocus(() => toggle.click());
  return promise;
};

const timeline = (node) => {
  // First we check if it is actually a sponsored post
  if(!checkSponsor(node)) return Promise.resolve(false);

  // And then we try to grab the parent container that has a hyperfeed id
  let parent = node;
  while(parent) {
    let id = parent.getAttribute("id");
    if(id && id.startsWith("hyperfeed")) break;
    parent = parent.parentElement;
  }

  // If we've walked off the top of the parent heirarchy that's an error
  if(!parent) return Promise.resolve(false);

  // Also if there's nothing to save that's an error
  if(node.children.length === 0) return Promise.resolve(false);

  // Check to see that we have the innermost fbUserContent, this cuts out like's
  // and shares.
  if(node.querySelector(TIMELINE_SELECTOR))
    node = node.querySelector(TIMELINE_SELECTOR);

  // Finally we have something to save.
  return getTimelineId(parent, {
    html: cleanAd(node.children[0].outerHTML)
  });
};

// Sidebar ads are a bit more complicated.
const sidebar = (node) => {
  // Although first we still need to make sure we are in a sponsored box;
  let parent = node;
  while(parent) {
    if(checkSponsor(parent)) break;
    parent = parent.parentElement;
  }

  // As before it is an error if we haven't found a sponsor node.
  if(!(parent &&
       parent.classList &&
       parent.classList.contains('ego_section'))) return Promise.resolve(false);

  // Sanity check to make sure we have a salvageable id
  if(!node.hasAttribute("data-ego-fbid")) return Promise.resolve(false);

  // and we have childnodes
  if(!node.children.length === 0) return Promise.resolve(false);

  // Then we just need to sent the cleaned ad and the ego-fbid
  return Promise.resolve({
    id: node.getAttribute("data-ego-fbid"),
    html: cleanAd(node.outerHTML)
  });
};

// We are careful here to only accept a valid timeline ad or sidebar ad
const parser = function(node) {
  if(node.classList.contains("fbUserContent") ||
     node.classList.contains("fbUserPost") ||
     node.classList.contains("_5pcr")) {
    return timeline(node);
  } else if(node.classList.contains('ego_unit')) {
    return sidebar(node);
  } else {
    return Promise.resolve(false);
  }
};

module.exports = {
  parser,
  TIMELINE_SELECTOR,
  SIDEBAR_SELECTOR,
};
