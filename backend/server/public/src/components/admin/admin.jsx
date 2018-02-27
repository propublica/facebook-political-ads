import React from "react";
import { connect } from "react-redux";
import { withRouter, Route, Link } from "react-router-dom";
import Ads from "components/admin/ads.jsx";
import AdDetail from "components/admin/addetail.jsx";
import Login from "components/admin/login.jsx";
import GroupedAttrs from "components/admin/groupedattrs.jsx";
import AdminTools from "components/admin/tools.jsx";

export const LoggedInApp = () => {
  return (
    <div>
      <Route exact path="/facebook-ads/admin" component={Ads} />
      {/* confusingly, despite being `exact`, this matches /facebook-ads/admin, without the trailing slash */}
      <Route exact path="/facebook-ads/admin/ads" component={Ads} />
      <Route
        exact
        path="/facebook-ads/admin/grouped/:groupingType"
        component={GroupedAttrs}
      />
      <Route path="/facebook-ads/admin/ads/:ad_id" component={AdDetail} />
      <Route path="/facebook-ads/admin/tools" component={AdminTools} />
    </div>
  );
};

export const AdminUnconnected = ({ credentials }) => {
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
      {credentials && credentials.token ? <LoggedInApp /> : <Login />}
    </div>
  );
};
export default withRouter(
  connect(({ credentials }) => ({ credentials }))(AdminUnconnected)
);
