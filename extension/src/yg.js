// it's guaranteed that we're on a yougov.com page.
// check for the right "flag"/canary
// send ID
const DEBUG =
  process.env.NODE_ENV === "dev" || process.env.NODE_ENV === "development";

let ygid = null;

const isThisTheRightPage = () => {
  return (
    document.getElementById("main_cont") &&
    Array.from(document.getElementById("main_cont").getElementsByTagName("a"))
      .length > 0 &&
    Array.from(
      document.getElementById("main_cont").getElementsByTagName("a")
    )[0]["href"] ===
      "https://chrome.google.com/webstore/detail/facebook-political-ad-col/enliecaalhkhhihcmnbjfmmjkljlcinl"
  );
};
const setYgid = () => {
  if (ygid) return;
  console.log("checking");
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
  setTimeout(setYgid, 5 * 1000);
  setTimeout(setYgid, 10 * 1000);
  setTimeout(setYgid, 30 * 1000);
}
