import React from "react";

class YougovHelp extends React.Component {
  componentDidMount() {
    // gets params from the URL and dispatches the relevant actions, which'll cause a DidUpdate, and
    // then a getAds
    document.getElementById("app-guff").style.display = "none";
  }

  render() {
    return (
      <div className="yougovhelp">
        <h1>Help for YouGov Panel Participants</h1>
        <p>
          <b>Thank you for participating in this study.</b> Here’s some
          additional information about the extension.
        </p>
        <p>
          The Political Ad Collector is a tool you add to your Web browser. It
          copies the ads you see on Facebook, so anyone, on any part of the
          political spectrum, can see the political ads in our public database.
          The ads, along with the unique YouGov ID associated with your survey
          answers and demographic info, will also be shared with academic
          researchers and certain journalists; your ID, answers and demographic
          info will NOT be shared publicly.
        </p>
        <p>
          The Political Ad Collector is a tool you add to your Web browser. It
          copies the ads you see on Facebook, so anyone, on any part of the
          political spectrum, can see them in our public database.
        </p>
        <p>
          If you’d like to stop participating in the study, you can simply
          uninstall the extension at any time. Here’s how:
        </p>
        <p>
          <b>For Mozilla Firefox</b>
        </p>
        <ol>
          <li>
            Click Tools → Addons (alternatively, you type “about:addons” into
            the address bar and hit enter)
          </li>
          <li>
            On the next screen find the add-on called “Facebook Political Ad
            Collector for YouGov Panel Participants” and click Remove.{" "}
          </li>
        </ol>
        <p>
          <b>For Google Chrome</b>
        </p>
        <ol>
          <li>
            Click Window → Extensions (or type “chrome://extensions” into the
            address bar and press enter)
          </li>
          <li>
            On the next screen find “Facebook Political Ad Collector for YouGov
            Panel Participants” and click Remove.
          </li>
        </ol>
        <p>
          If you have technical difficulties with the extension, contact{" "}
          <a href="mailto:adcollector@propublica.org">
            adcollector@propublica.org
          </a>. If you have questions about the study, contact YouGov at{" "}
          <a href="mailto:help.us@yougov.com">help.us@yougov.com</a>.
        </p>
        <hr />
      </div>
    );
  }
}

export default YougovHelp;
