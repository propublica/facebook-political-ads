import React from "react";
const Targeting = ({ targeting }) => (
  <div className="targeting_info">
    <div
      className="targeting"
      dangerouslySetInnerHTML={{
        __html: "<h3>Targeting Information</h3>" + targeting
      }}
    />
  </div>
);
export default Targeting;
