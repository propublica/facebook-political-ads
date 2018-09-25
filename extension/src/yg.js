// it's guaranteed that we're on a yougov.com page.
// check for the right "flag"/canary
// send ID
const DEBUG =
  process.env.NODE_ENV === "dev" || process.env.NODE_ENV === "development";

let ygid = null;

const isThisTheRightPage = () => {
  return (
    document.getElementById("ppfbpac-yg-canary") ||
    document.getElementById("”ppfbpac-yg-canary”")
  );
};
const setYgid = () => {
  if (ygid) return;
  console.log("checking on", window.location.pathname);
  let is_right = isThisTheRightPage();
  console.log("is this right?", is_right);
  if (is_right) {
    ygid = window.location.pathname.substring(1);
    chrome.runtime.sendMessage({ type: "ygid", ygid: ygid });
    if (DEBUG) console.log("correct yougov page; ygid is", ygid);
  } else {
    if (DEBUG) console.log("wrong yougov page");
  }
};

if (!ygid) {
  if (DEBUG) console.log("yg.js has been injected.");
  setYgid();
  setTimeout(setYgid, 10 * 1000);
  setTimeout(setYgid, 30 * 1000);
}

document.getElementById("back_button").onclick = function() {
  console.log("setTimeouted");
  setTimeout(setYgid, 1 * 1000);
};
document.getElementById("next_button").onclick = function() {
  console.log("setTimeouted");
  setTimeout(setYgid, 1 * 1000);
};
