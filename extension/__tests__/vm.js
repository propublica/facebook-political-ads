import { readFileSync } from "fs";

class AsyncRequest {
  constructor() {
    this.data = {};
    this.uri = { addQueryData: () => "" };
    this.calls = [];
  }

  setURI() {
    this.calls.push("setURI");
    return this;
  }

  setData() {
    this.calls.push;
    return this;
  }

  setMethod() {
    return this;
  }

  setRelativeTo() {
    return this;
  }

  setNectarModuleDataSafe() {
    return this;
  }

  setReadOnly() {
    return this;
  }

  _setUserActionID() {}

  setNewSerial() {}
}

globals.AsyncRequest = AsyncRequest;
Object.assign(globals.window, { require: () => () => {} });

class XMLHttpRequest {
  open() {}
  send() {
    this.readyState = 4;
    this.onreadystatechange();
  }
}
globals.XMLHttpRequest = XMLHttpRequest;

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
    this.timelineAd = document.querySelector(
      "#hyperfeed_story_id_5ac2bbb055fd90225091584 .toggle"
    );

    this.timelineAd.addEventListener("click", this.click);
  }

  _click() {
    const node = document.createElement("div");
    div.innerHTML = readFileSync(__dirname + "/fixtures/toggle.html");
    document.body.appendChild(node);
  }

  detach() {
    this.timelineAd.removeEventListener("click", this.click);
  }
}
