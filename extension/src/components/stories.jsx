import React from "react";

export const Stories = () => (
  <div id="stories">
    <div className="precis">
      <h2 id="intro-thankyou">
        Thank you for participating in ProPublica’s Facebook Political Ad
        Collector project!
      </h2>
      <p>
        We’re still going strong, but wanted to highlight some of the articles
        we’ve written so far using the data contributed by participants like
        you.
      </p>
    </div>
    <ul>
      <li>
        <a
          target="_blank"
          href="https://www.propublica.org/article/how-big-oil-dodges-facebooks-new-ad-transparency-rules"
        >
          How Big Oil Dodges Facebook’s New Ad Transparency Rules&nbsp;→
        </a>
      </li>
      <li>
        <a
          target="_blank"
          href="https://www.propublica.org/article/groups-mask-partisan-attacks-behind-neutral-sounding-names-in-facebook-ads"
        >
          Groups Mask Partisan Attacks Behind Neutral-Sounding Names in Facebook
          Ads&nbsp;→
        </a>
      </li>
      <li>
        <a
          target="_blank"
          href="https://www.propublica.org/article/facebook-is-letting-job-advertisers-target-only-men"
        >
          Facebook Is Letting Job Advertisers Target Only Men&nbsp;→{" "}
        </a>
      </li>

      <li>
        <a
          target="_blank"
          href="https://www.propublica.org/article/why-am-i-seeing-this-interesting-facebook-ads-from-our-political-ad-collector"
        >
          Why Am I Seeing This? Interesting Ads We've Found So Far&nbsp;→
        </a>
      </li>
    </ul>
    <div className="postcis">
      If you’d like to stay in touch with ProPublica,{" "}
      <a target="_blank" href="https://go.propublica.org/FBPAC">
        sign up for our Big Story newsletter
      </a>{" "}
      — we’ll also let you know about new stories about ads submitted through
      the ad collector. And please share with your friends.
    </div>
    <div className="postcis">
      You can look through all the ads collected in this project{" "}
      <a target="_blank" href="https://projects.propublica.org/facebook-ads">
        here
      </a>.
    </div>
  </div>
);
