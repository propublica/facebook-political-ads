// We are careful here to only accept a valid timeline ad
module.exports = function(node){
  // First we check that it does have a valid class
  if(!node.classList.value.startsWith("fbUserContent")) return false;

  // Then we see if it is actually a sponsored post
  if(!Array.from(node.querySelectorAll("a")).some((it) => {
    return it.innerText == "Sponsored";
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
    ad: node.children[0].outerHTML
  };
};
