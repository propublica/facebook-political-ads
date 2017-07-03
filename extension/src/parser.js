// This function cleans all the elements that could leak user data
// before sending to the server. It also removes any attributes that
// could have personal data so we end up with a clean dom tree.
let cleanAd = (html) => {
  let node = document.createElement("div");
  node.innerHTML = html;
  // remove likes and shares
  Array.from(node.querySelectorAll(".commentable_item"))
    .forEach(i => i.parentElement.removeChild(i));
  // remove like buttons
  Array.from(node.querySelectorAll("button"))
    .forEach(i => i.parentElement.removeChild(i));
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

// We are careful here to only accept a valid timeline ad
module.exports = function(node, sponsor){
  // First we check that it does have a valid class
  if(!node.classList.value.startsWith("fbUserContent")) return false;

  // Then we see if it is actually a sponsored post
  if(!Array.from(node.querySelectorAll("a")).some((it) => {
    return it.textContent == sponsor;
  })) return false;

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
  if(!node.children) return false;

  // Finally we have something to save.
  return {
    id: parent.getAttribute("id"),
    ad: cleanAd(node.children[0].outerHTML)
  };
};
