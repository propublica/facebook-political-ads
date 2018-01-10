

propublica.views.socialSection = propublica.View.extend({
  cssClass : "ss-social",
  tag      : "span",

  bindings : {
    click : 'socialRef'
  },

  socialRef : function(e) {
    e.preventDefault();
    var cur = $(e.currentTarget);
    if (cur.hasClass("ss-fb")) {
      this.fb(cur);
    }
    if (cur.hasClass("ss-tw")) {
      this.tw(cur)
    }
  },

  getTitle : function(cur) {
    return cur.attr("data-title");
  },

  getImg : function(cur) {
    return cur.attr("data-img");
  },

  getText : function(cur) {
    return cur.attr("data-txt");
  },

  getDesc : function(cur) {
    return cur.attr("data-desc");
  },

  getLink : function(cur) {
    return cur.attr("data-url");
  },

  getHashtag : function(cur) {
    return cur.attr("data-hashtag");
  },

  fb : function(cur) {
    window.dataLayer = (window.dataLayer || []);
    var socialObj = { link : cur.attr("data-url"), name : cur.attr("data-title"), classes : cur.attr("class"), id : cur.attr("id"), network : "facebook", action : "share" };

    FB.ui({
      method: 'feed',
      link: this.getLink(cur),
      picture: this.getImg(cur),
      name: this.getTitle(cur),
      caption: this.getText(cur),
      description : this.getDesc(cur),
    }, function(response){
      if(response["post_id"] !== undefined) {
        window.dataLayer.push({
        'socialShare': socialObj,
        'event':'socialShare'
        });
      }
    });


  },

  tw : function(cur) {
    window.dataLayer = (window.dataLayer || []);
    window.dataLayer.push({event : 'socialShare', socialShare : { link : cur.attr("data-url"), name : cur.attr("data-title"), classes : cur.attr("class"), id : cur.attr("id"), network : "twitter", action : "tweet-intent" }});

    var baseUrl = "https://twitter.com/intent/tweet";
    baseUrl += "?text=" + encodeURIComponent(this.getText(cur));
    baseUrl += "&url="  + encodeURIComponent(this.getLink(cur));
    baseUrl += "&hashtags="  + encodeURIComponent(this.getHashtag(cur));
    window.open(baseUrl,'','toolbar=0, status=0, width=550, height=420');
  }
});



propublica.views.tweetTakeaways = propublica.View.extend({
  cssClass : "anno-tweet",
  tag      : "span",

  bindings : {
    click : 'socialTweet'
  },

  socialTweet : function(e) {
    e.preventDefault();
    var cur = $(e.currentTarget);
      this.tw(cur)
  },

  getLink : function(cur) {
    return cur.attr("data-url");
  },

  getHashtag : function(cur) {
    return cur.attr("data-hashtag");
  },

  getText : function(cur) {
    return cur.attr("data-txt");
  },

  tw : function(cur) {
    window.dataLayer = (window.dataLayer || []);
    window.dataLayer.push({event : 'socialShare', socialShare : { link : cur.attr("data-url"), name : cur.attr("data-title"), classes : cur.attr("class"), id : cur.attr("id"), network : "twitter", action : "tweet-takeaways" }});

    var baseUrl = "https://twitter.com/intent/tweet";
    baseUrl += "?text=" + encodeURIComponent(this.getText(cur));
    baseUrl += "&url="  + encodeURIComponent(this.getLink(cur));
    baseUrl += "&hashtags="  + encodeURIComponent(this.getHashtag(cur));
    window.open(baseUrl,'','toolbar=0, status=0, width=550, height=420');
  }
});