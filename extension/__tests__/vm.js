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
       ${readFileSync(__dirname + "/fixtures/sisi-timeline-ad.html")}
       ${readFileSync(__dirname + "/fixtures/timeline.html")}
       ${readFileSync(__dirname + "/fixtures/paid-for-timeline-ad.html")}
       ${readFileSync(__dirname + "/fixtures/lkw-timeline-ad.html")}
       ${readFileSync(__dirname + "/fixtures/sidebar-ad.html")}
      </body>
    </html>
    `;

    this.click = () => this._click();

    const selectors = [
      ["#hyperfeed_story_id_5ac2bbb055fd90225091584", "u_jsonp_2_1d"],
      ["#hyperfeed_story_id_5b073958c2f8d1e41312928", "u_fetchstream_4_v"],
      ["#hyperfeed_story_id_5b880d5373d714746163772", "u_fetchstream_3_7"],
      ["#hyperfeed_story_id_5b85b34ce73376c62775420", "u_fetchstream_2_t"]
    ];
    selectors.forEach(([sel, owner_id]) => {
      this.timelineAd = document.querySelector(sel + " .uiPopover");
      this.timelineAd.addEventListener("click", () => this._click(owner_id));
    });
  }

  _click(owner_id_should_be) {
    const node = (this.popup = document.createElement("div"));
    node.innerHTML = readFileSync(__dirname + "/fixtures/timeline-popup.html");
    node.innerHTML = node.innerHTML.replace("u_jsonp_2_1d", owner_id_should_be);
    document.body.appendChild(node);
  }

  detach() {
    if (this.popup) this.popup.remove();
    this.timelineAd.removeEventListener("click", this.click);
  }
}
