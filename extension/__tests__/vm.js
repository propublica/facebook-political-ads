import { readFileSync } from "fs";

class AsyncRequest {
  constructor() {
    this.data = {};
    this.uri = { addQueryData: () => "" };
  }

  setURI() {
    return this;
  }

  setData() {
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
global.AsyncRequest = AsyncRequest;
Object.assign(global.window, { require: () => () => {} });

class XMLHttpRequest {
  constructor() {
    this.response =
      'for (;;);{"jsmods":{"markup":[["",{"__html":"targeting"}]]}}';
  }
  open() {}
  send() {
    this.readyState = 4;
    this.onreadystatechange();
  }
}
global.XMLHttpRequest = XMLHttpRequest;
global.getSelection = () => ({ rangeCount: 0 });
global.localStorage = { setItem: a => a, getItem: () => "targeting" };

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
      "#hyperfeed_story_id_5ac2bbb055fd90225091584 .uiPopover"
    );

    this.timelineAd.addEventListener("click", this.click);
  }

  _click() {
    const node = (this.popup = document.createElement("div"));
    node.innerHTML = readFileSync(__dirname + "/fixtures/timeline-popup.html");
    document.body.appendChild(node);
  }

  detach() {
    if (this.popup) this.popup.remove();
    this.timelineAd.removeEventListener("click", this.click);
  }
}
