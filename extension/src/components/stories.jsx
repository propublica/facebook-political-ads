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
    </ul>
    <p className="postcis">
      If you’d like to keep in touch with this project, please sign up for our
      occasional newsletter here LINKTK. And please share with your friends.
    </p>
    <p className="postcis">
      You can look through all the ads collected in this project{" "}
      <a href="https://projects.propublica.org/facebook-ads">here</a>.
    </p>
  </div>
);
