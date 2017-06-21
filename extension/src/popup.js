const e = function(/* tag, attrs, text, children...*/ ) {
  let args = Array.from(arguments);
  let tag  = args.shift() || 'div';
  let atts = args.shift() || {};
  let text = args.shift() || '';
  let kids = args;
  let el = document.createElement(tag);
  Object.keys(atts).forEach(function(k) {
    el.setAttribute(k, atts[k]);
  });

  el.appendChild(document.createTextNode(text));

  kids.forEach(function(e) {
    el.appendChild(e);
  });

  return el;
};

let state = {
  _values: {},
  _callbacks: [],
  set: (key, value) => {
    this._values[key] = value;
    this._callbacks.forEach((cb) => cb(this, key, value));
    return this;
  },
  get: (key) => this._values[key],
  observe: function(cb){
    this._callbacks.push(cb);
  }
};
