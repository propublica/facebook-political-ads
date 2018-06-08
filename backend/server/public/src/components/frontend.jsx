import React from "react";
import { Route, Switch } from "react-router-dom";
import AdDetail from "components/addetail.jsx";
import AdList from "components/adlist.jsx";
import { t } from "i18n.js";

export const FrontEnd = () => (
  <div id="app">
    <div dangerouslySetInnerHTML={{ __html: t("guff") }} />
    <Switch>
      <Route exact path="/facebook-ads" component={AdList}>
        {/* confusingly, despite being `exact`, this also matches /facebook-ads/, with the trailing slash */}
      </Route>
      <Route path="/facebook-ads/ad/:ad_id" component={AdDetail} />
    </Switch>
    <div id="which-ads" dangerouslySetInnerHTML={{ __html: t("whichads") }} />
  </div>
);

export default FrontEnd;
