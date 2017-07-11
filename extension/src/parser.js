// This function cleans all the elements that could leak user data
// before sending to the server. It also removes any attributes that
// could have personal data so we end up with a clean dom tree.
const selectors = [
  'video',
  'input',
  'button',
  'a[href=""]',
  '.accessible_elem',
  '.uiLikePagebutton',
  '.uiPopOver',
  '.uiCloseButton',
  '.uiChevronSelectorButton'
].join(', ');

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
      if(attr.name != "id" &&
         attr.name != "class" &&
         attr.name != "src" &&
         attr.name != "href")
        node.removeAttribute(attr.name);
      // remove tracking get variables from links and l.facebook.com
      if(attr.name == "href") {
        try {
          let url = new URL(attr.value);
          if(url.host == 'l.facebook.com') {
            node.removeAttribute(attr.name);
          } else {
            node.setAttribute(attr.name, url.origin + url.pathname);
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
  return node.innerHTML.replace('&amp;', '&');
};

const checkSponsor = (node, sponsor) => {
  return Array.from(node.querySelectorAll("a")).some((a) => {
    return a.textContent == sponsor;
  });
};

const timeline = (node, sponsor) => {
  // First we check if it is actually a sponsored post
  if(!checkSponsor(node, sponsor)) return false;

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
  if(node.querySelector('.fbUserContent'))
    node = node.querySelector('.fbUserContent');

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
       parent.classList.contains("ego_section"))) return false;

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
module.exports = function(node, sponsor) {
  if(node.classList.contains("fbUserContent")) {
    return timeline(node, sponsor);
  } else if(node.classList.contains('ego_unit')) {
    return sidebar(node, sponsor);
  } else {
    return false;
  }

};
