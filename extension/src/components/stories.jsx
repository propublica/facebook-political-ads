import React from "react";

export const Stories = () => (
  <div id="stories">
    <p className="precis">
      Thank you for participating in ProPublica’s Facebook Political Ad
      Collector project. We’re still going strong, but wanted to highlight some
      of the articles we’ve written so far using the data contributed by
      participants like you.
    </p>
    <ul>
      <li>
        <a href="https://www.propublica.org/article/facebook-is-letting-job-advertisers-target-only-men">
          Facebook Is Letting Job Advertisers Target Only Men{" "}
        </a>
      </li>

      <li>
        <a href="https://www.propublica.org/article/why-am-i-seeing-this-interesting-facebook-ads-from-our-political-ad-collector">
          Why Am I Seeing This? Interesting Ads We've Found So Far
        </a>
      </li>
      <li>
        <a href="https://www.propublica.org/article/facebook-new-screening-system-flags-the-wrong-ads-as-political">
          Facebook’s Screening for Political Ads Nabs News Sites Instead of
          Politicians
        </a>
      </li>
      <li>
        <a href="https://www.propublica.org/article/facebook-political-ads-malware-scams-misleading">
          Facebook Allowed Political Ads That Were Actually Scams and Malware
        </a>
      </li>
    </ul>
    <p className="postcis">
      If you’d like to stay in touch with ProPublica,{" "}
      <a href="https://go.propublica.org/FBPAC">
        sign up for our Big Story newsletter
      </a>{" "}
      — we’ll also let you know about future updates to this project. And please
      share with your friends.
    </p>
    <p className="postcis">
      You can look through all the ads collected in this project{" "}
      <a href="https://projects.propublica.org/facebook-ads">here</a>. And
      please share with your friends.
    </p>
  </div>
);
