import React from "react";
import { Route, Switch } from "react-router-dom";
import AdDetail from "components/addetail.jsx";
import AdList from "components/adlist.jsx";
import YougovHelp from "components/yougov-help.jsx";

import { t } from "i18n.js";
import BottomGuffItem from "./bottom-guff-item";

export const FrontEnd = () => (
  <div id="app">
    <div id="app-guff" dangerouslySetInnerHTML={{ __html: t("guff") }} />
    <Switch>
      <Route exact path="/facebook-ads" component={AdList}>
        {/* confusingly, despite being `exact`, this also matches /facebook-ads/, with the trailing slash */}
      </Route>
      <Route path="/facebook-ads/ad/:ad_id" component={AdDetail} />
      <Route exact path="/facebook-ads/yghelp" component={YougovHelp} />
    </Switch>

    <div id="bottom-guff">
      <BottomGuffItem
        titleHtml={t("whichads-title")}
        shown={false}
        bodyHtml={t("whichads")}
      />
      <BottomGuffItem
        titleHtml={t("buckets-title")}
        shown={false}
        bodyHtml={t("buckets")}
      />
      <BottomGuffItem
        titleHtml={t("sorting-title")}
        shown={false}
        bodyHtml={t("sorting")}
      />
    </div>
    <div id="contrib" dangerouslySetInnerHTML={{ __html: t("contrib") }} />
  </div>
);

export default FrontEnd;
