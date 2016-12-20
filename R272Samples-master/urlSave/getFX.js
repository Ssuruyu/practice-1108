require("webduino-js");
require("webduino-blockly");

var request = require("request");
var fs = require("fs");
var cheerio = require("cheerio");

var timer;
var m = 2;

var pm = function() {
  clearTimeout(timer);
  request({
    url: "http://rate.bot.com.tw/Pages/Static/UIP003.zh-TW.htm",
    method: "GET"
  }, function(e, r, b) {
    if (e || !b) {
      return;
    }
    var $ = cheerio.load(b);
    var varTime = new Date();
    var result = [];
    var title = $(".titleLeft");
    var decimal = $(".decimal");
    for (var i = 0; i < title.length; i++) {
      result.push('{"'+title[i].children[1].data+'":['+decimal[4*i].children[0].data + ','+decimal[4*i+1].children[0].data+']}');
    }
    fs.writeFile("result.json", result);
  });
  timer = setTimeout(pm,m*60*1000);
};

pm();