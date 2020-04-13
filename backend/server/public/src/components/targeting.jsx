import React from "react";
import DOMPurify from "dompurify";

const Targeting = ({ targeting, targets }) => {
  if (targeting[0] == "{") {
    // console.log(targets)
    let rearranged = targets.filter(({target, segment}) => ["MinAge", "MaxAge", "Location Granularity", "Location Type"].indexOf(target) == -1).reduce((memo, {target, segment}) => { if( target == "Interest") { memo["Interest"] = memo["Interest"] || []; memo["Interest"].push(segment) }else{ memo[target] = segment}; return memo }, {});
    if(rearranged["Interest"])  
      rearranged["Interest"] = rearranged["Interest"].join(", ");
    return (
      <div className="targeting_info">
        <div
          className="targeting"
        >
        <ul>
          { Object.entries(rearranged).map((k, v) => (<li>{k}: {v}</li>)) }
        </ul>
        </div>
      </div>
    );
  
  } else {
    return (
      <div className="targeting_info">
        <div
          className="targeting"
          dangerouslySetInnerHTML={{
            __html: "" 
            // __html: "<h3>Targeting Information</h3>" + DOMPurify.sanitize(targeting)
          }}
        />
      </div>
    );
  }
};
export default Targeting;