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

let getTimelineId = (parent) => {
  const control = parent.querySelector(".uiPopover");
  if(!control && control.id === "") return null;
  const toggle = control.querySelector("a");
  if(!toggle) return null;
  // this is async
  let promise = new Promise((resolve, reject) => {
    new MutationObserver((record, self) => {
      const endpoint = document.querySelector("li[data-feed-option-name=FeedAdSeenReasonOption] a");
      if(!endpoint) return null;
      const url = "https://facebook.com" + endpoint.getAttribute("ajaxify");
      toggle.click();
      self.disconnect();
      try {
        resolve(new URL(url).searchParams.get("ad_id"));
      } catch(e) {
        reject(e);
      }
    }).observe(document.body, {childList: true, subtree:true});
  });
  toggle.click();
  return promise;
};

const timeline = (node) => {
  // First we check if it is actually a sponsored post
  if(!checkSponsor(node)) return false;

  // And then we try to grab the parent container that has a hyperfeed id
  let parent = node;
  while(parent) {
    let id = parent.getAttribute("id");
    if(id && id.startsWith("hyperfeed")) break;
    parent = parent.parentElement;
  }

  // If we've walked off the top of the parent heirarchy that's an error
  if(!parent) return false;

  // Also if there's nothing to save that's an error
  if(node.children.length === 0) return false;

  // Check to see that we have the innermost fbUserContent, this cuts out like's
  // and shares.
  if(node.querySelector(TIMELINE_SELECTOR))
    node = node.querySelector(TIMELINE_SELECTOR);

  // Finally we have something to save.
  return {
    id: parent.getAttribute("id"),
    html: cleanAd(node.children[0].outerHTML)
  };
};

// Sidebar ads are a bit more complicated.
const sidebar = (node, sponsor) => {
  // Although first we still need to make sure we are in a sponsored box;
  let parent = node;
  while(parent) {
    if(checkSponsor(parent, sponsor)) break;
    parent = parent.parentElement;
  }

  // As before it is an error if we haven't found a sponsor node.
  if(!(parent &&
       parent.classList &&
       parent.classList.contains('ego_section'))) return false;

  // Sanity check to make sure we have a salvageable id
  if(!node.hasAttribute("data-ego-fbid")) return false;

  // and we have childnodes
  if(!node.children.length === 0) return false;

  // Then we just need to sent the cleaned ad and the ego-fbid
  return {
    id: node.getAttribute("data-ego-fbid"),
    html: cleanAd(node.outerHTML)
  };
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
    return false;
  }
};

module.exports = {
  parser,
  TIMELINE_SELECTOR,
  SIDEBAR_SELECTOR,
};
