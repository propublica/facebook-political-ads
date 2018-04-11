import React from "react";
import { connect } from "react-redux";
import { withRouter, Route, Link } from "react-router-dom";
import Ads from "components/admin/ads.jsx";
import AdDetail from "components/admin/addetail.jsx";
import GroupedAttrs from "components/admin/groupedattrs.jsx";
import AdminTools from "components/admin/tools.jsx";
import { URL_ROOT } from "actions.js";

export const LoggedInApp = () => {
  fetch(`${URL_ROOT}/fbpac-api/loggedin`, {
    method: "POST",
    credentials: "include",
    redirect: "follow" // in case we get redirected to the login page.
  }).then(resp => {
    console.log("resp", resp);
    if (resp.redirected === true || !resp.ok) {
      window.location.href = `${URL_ROOT}/fbpac-api/partners/sign_in`;
      return <div>An error has occurred</div>;
    }
  });

  return (
    <div>
      <Route exact path="/facebook-ads/admin" component={Ads} />
      {/* confusingly, despite being `exact`, this matches /facebook-ads/admin, without the trailing slash */}
      <Route exact path="/facebook-ads/admin/ads" component={Ads} />
      <Route
        exact
        path="/facebook-ads/admin/grouped/:groupingType/:timing"
        component={GroupedAttrs}
      />
      <Route
        exact
        path="/facebook-ads/admin/grouped/:groupingType/"
        component={GroupedAttrs}
      />

      <Route path="/facebook-ads/admin/ads/:ad_id" component={AdDetail} />
      <Route path="/facebook-ads/admin/tools" component={AdminTools} />
    </div>
  );
};

export const AdminUnconnected = () => {
  return (
    <div id="app">
      <nav>
        <ul>
          <li>
            <Link to="/facebook-ads/admin">
              <h1>FBPAC Admin</h1>
            </Link>
          </li>
          <li>
            <Link to="/facebook-ads/admin/tools">Tools</Link>
          </li>
        </ul>
      </nav>
      <LoggedInApp />
    </div>
  );
};
export default withRouter(connect(() => ({}))(AdminUnconnected));
