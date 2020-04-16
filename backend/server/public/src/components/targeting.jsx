import React from "react";
import DOMPurify from "dompurify";

const TARGETS = ["MinAge", "MaxAge", "Audience Owner", "Location Granularity", "Location Type"];

const LABELS = {
  List: "You’re on a list",
  "Activity on the Facebook Family" : "Activity on Facebook",
  Retargeting: "You're similar to another group",
  Like:"Liking a Facebook Page"

};

const Targeting = ({ targeting, targets }) => {
  if (targeting[0] == "{") {
    // console.log(targets)
    let rearranged = targets
      .filter(({ target, segment }) => TARGETS.indexOf(target) == -1)
      .reduce((memo, { target, segment }) => {
        if (target == "Interest") {
          memo["Interest"] = memo["Interest"] || [];
          memo["Interest"].push(segment);
        } else {
          memo[target] = segment;
        }
        return memo;
      }, {});
    if (rearranged["Interest"])
      rearranged["Interest"] = rearranged["Interest"].join(", ");
    return (
      <div className="targeting_info">
        <div className="targeting">
          <ul>
            {Object.entries(rearranged).map((k, v) => (
              <li>
                <span>{LABELS[k[0]] || k[0]}</span>

                { k[1] && k[0]!= "Retargeting" ? (
                <span>{k[1]}</span>
                ) : (
                ""
                )}



                
              </li>
            ))}
          </ul>
        </div>
      </div>
    );
  } else {
    return (
      <div className="targeting_info">
        <div
          className="targeting"
          dangerouslySetInnerHTML= {{
            __html: ""
            // __html: "<h3>Targeting Information</h3>" + DOMPurify.sanitize(targeting)
          }}
        />
      </div>
    );
  }
};
export default Targeting;
