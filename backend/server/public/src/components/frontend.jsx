import React from "react";
import { connect } from "react-redux";
import { Route, Switch, withRouter } from "react-router-dom";
import AdDetail from "components/addetail.jsx";
import AdList from "components/adlist.jsx";
import { newSearch } from "actions.js";
import { debounce } from "lodash";
import { t } from "i18n.js";

export const FrontendUnconnected = () => (
  <div id="app">
    <div dangerouslySetInnerHTML={{ __html: t("guff") }} />
    <Switch>
      <Route exact path="/facebook-ads" component={AdList}>
        {/* confusingly, despite being `exact`, this also matches /facebook-ads/, with the trailing slash */}
      </Route>
      <Route path="/facebook-ads/ad/:ad_id" component={AdDetail} />
    </Switch>
  </div>
);

const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);

const Frontend = withRouter(
  connect(
    ({ ads, search, ad }) => ({ ads, search, ad }),
    dispatch => ({
      onKeyUp: e => {
        e.preventDefault();
        throttledDispatch(
          dispatch,
          e.target.value.length ? e.target.value : null
        );
      }
    })
  )(FrontendUnconnected)
);
export default Frontend;
