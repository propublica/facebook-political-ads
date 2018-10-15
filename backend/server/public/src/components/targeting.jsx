import React from "react";
import DOMPurify from "dompurify";

const Targeting = ({ targeting }) => (
  <div className="targeting_info">
    <div
      className="targeting"
      dangerouslySetInnerHTML={{
        __html: "<h3>Targeting Information</h3>" + DOMPurify.sanitize(targeting)
      }}
    />
  </div>
);
export default Targeting;
