(function(w, d, s, l, i) {
  w[l] = w[l] || [];
  w[l].push({
    "gtm.start": new Date().getTime(),
    event: "gtm.js"
  });
  var f = d.getElementsByTagName(s)[0],
    j = d.createElement(s),
    dl = l !== "dataLayer" ? "&l=" + l : "";
  j.async = true;
  j.src = "//www.googletagmanager.com/gtm.js?id=" + i + dl;
  f.parentNode.insertBefore(j, f);
})(window, document, "script", "dataLayer", "GTM-M4BNWZ");

(function() {
  function loadChartbeat() {
    window._sf_endpt = new Date().getTime();
    var e = document.createElement("script");
    e.setAttribute("language", "javascript");
    e.setAttribute("type", "text/javascript");
    e.setAttribute("src", "//static.chartbeat.com/js/chartbeat.js");
    document.body.appendChild(e);
  }
  var oldonload = window.onload;
  window.onload =
    typeof window.onload !== "function"
      ? loadChartbeat
      : function() {
          oldonload();
          loadChartbeat();
        };
})();
