import React from "react";
import { withI18n } from "i18n.js";
import { Language } from "components/language.jsx";

export const Onboarding = withI18n(({ getMessage, onAcceptClick }) => (
  <div id="tos">
    <div id="main">
      <Language />
      <div
        id="terms"
        dangerouslySetInnerHTML={{ __html: getMessage("terms_of_service") }}
      />
      <div id="accept-box">
        <button id="accept" onClick={onAcceptClick}>
          {getMessage("terms_of_service_accept")}
        </button>
      </div>
    </div>
    <div id="accept-box">
      <button id="accept" onClick={onAcceptClick}>
        {getMessage("terms_of_service_accept")}
      </button>
    </div>
  </div>
));
