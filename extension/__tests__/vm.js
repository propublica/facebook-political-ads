import { readFileSync } from "fs";

export class FacebookVM {
  constructor() {
    document.body.innerHTML = `
    <html>
      <body>
       ${readFileSync(__dirname + "/fixtures/timeline-ad.html")}
       ${readFileSync(__dirname + "/fixtures/timeline.html")}
      </body>
    </html>
    `;

    this.click = () => this._click();
    this.req = () => this._req();
    this.timelineAd = document.querySelector(
      "#hyperfeed_story_id_5ac2bbb055fd90225091584 .toggle"
    );

    this.timelineAd.addEventListener("click", this.click);
  }

  _click() {}

  _req() {}

  detach() {
    this.timelineAd.removeEventListener("click", this.click);
  }
}
