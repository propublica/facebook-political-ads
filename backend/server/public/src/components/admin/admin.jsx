import React from "react";
import { connect } from "react-redux";
import { withRouter, Route, Link } from "react-router-dom";
import Ads from "components/admin/ads.jsx";
import AdDetail from "components/admin/addetail.jsx";
import Login from "components/admin/login.jsx";

export const LoggedInApp = () => {
  return (
    <div>
      <Route exact path="/facebook-ads/admin" render={() => <Ads />} />
      {/* confusingly, despite being `exact`, this matches /facebook-ads/admin, without the trailing slash */}
      <Route
        exact
        path="/facebook-ads/admin/ads"
        render={() => (
          <Ads />
        )} /* should be component={Ad} as soon as we figure out how to factor out this thing with store and subscribe */
      />
      <Route path="/facebook-ads/admin/ads/:ad_id" component={AdDetail} />
    </div>
  );
};

export const AdminUnconnected = ({ credentials }) => {
  return (
    <div id="app">
      <h1>
        <Link to="/facebook-ads/admin">FBPAC Admin</Link>
      </h1>
      {credentials && credentials.token ? <LoggedInApp /> : <Login />}
    </div>
  );
};
export default withRouter(
  connect(({ credentials }) => ({ credentials }))(AdminUnconnected)
);
