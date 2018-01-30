import React from "react";
import { connect } from "react-redux";
import Ads from "components/admin/ads.jsx";
import Login from "components/admin/login.jsx";

const Admin = ({ credentials }) => {
  return (
    <div id="app">
      <h1>
        <a href="/facebook-ads/admin?">FBPAC Admin</a>
      </h1>
      {credentials && credentials.token ? <Ads /> : <Login />}
    </div>
  );
};
export default connect(({ credentials }) => ({ credentials }))(Admin);
