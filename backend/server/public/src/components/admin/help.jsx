import React from "react";
import { Link } from "react-router-dom";

const AdminHelp = () => (
  <div id="help">
    <p>
      {" "}
      Here’s how to use this admin dashboard. We want you to be able to
      find&nbsp;
      <strong>dishonest ads</strong> or <strong>ads from shadowy groups</strong>{" "}
      or ads that demonstrate each{" "}
      <strong>candidates’ targeting strategies</strong>.
    </p>
    <h2>Seeing the wrong country’s ads?</h2>
    <p>
      If you are outside the USA, you should append your country’s language code
      to the end instead of en-US, e.g. “en-CA” or “pt-BR”, e.g.
      <a href="https://projects.propublica.org/facebook-ads/admin?lang=en-US">
        https://projects.propublica.org/facebook-ads/admin?lang=en-US
      </a>.
    </p>

    <h2>How to search</h2>
    <p>
      Once you visit that page, you can search, with the search bar at the top,
      for whatever’s most interesting to you: Trump, Trudeau, housing, etc.
    </p>

    <p>
      By default, we only show ads that our “classifier” algorithm believes are
      at least 70% likely to be political ads. The slider underneath the search
      bar lets you lower that threshold if you want to search ads that might’ve
      been missed by the classifier. (E.g. a lot of ads from Canada’s Liberal
      Party inviting people to their convention are being seen as non-political,
      since they don’t talk about issues -- just the convention. If you wanted
      to see all of those ads, you’d want to lower the slider’s threshold.)
    </p>

    <h2>What each item of metadata means</h2>
    <p>
      Each result includes a picture of the ad and some details about when it
      was <strong>first seen</strong>. “<strong>Impressions</strong>” is the
      number of times that that ad has been seen by participants in the project.
      (We have no idea how many times the ad has been seen overall.)
    </p>

    <p>
      “<strong>Targeting</strong>” is info from Facebook about why that one
      participant saw the ad; it’s based on (micro-)targeting decisions made by
      the advertiser – and it often offers a fascinating look into strategy.
      (E.g. why are the Democrats running ads with retiring House speaker Paul
      Ryan’s face on them in Arizona, hypothetically?)
    </p>

    <p>
      “<strong>Parsed Targets</strong>”: shows what the database has parsed out
      of the “Targeting” info. If there’s something missing from here (and the
      Targeting info is in English), it’s a bug. Please let us know at
      <a href="mailto:adcollector@propublica.org">
        adcollector@propublica.org
      </a>.
    </p>

    <p>
      Be very careful with the “Suppress” button and only click it for ads that
      are clearly not political – like cars or vacations or jeans. That button
      is irreversible and affects everyone.
    </p>

    <p>
      The <strong>Political/Not Political</strong> count is the number of votes
      from users (who vote in the extension, in the popup under the ProPublica
      logo in the top left of your browser) as to whether the ad is political or
      not. This probably is not of interest, since you can judge if an ad is
      interesting yourself.
    </p>
    <h2>Finding ads by advertiser or by targeting strategy</h2>

    <p>
      You can also visit the{" "}
      <strong>
        <Link to="/facebook-ads/admin/tools">Tools</Link>
      </strong>{" "}
      page to slice-and-dice the ads to see, overall, in the past week and in
      the past month: the number of ads by a given advertiser, by a given
      targeting <i>method</i> (e.g. ads shown only to people in a certain
      location) or by a given targeting <i>segment</i> (e.g. ads shown only to
      people in Ottawa). If you click any of the links there, you can see all
      the ads that are, for instance, shown only to people in Ottawa.
    </p>
  </div>
);

export default AdminHelp;
