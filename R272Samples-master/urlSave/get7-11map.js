//npm install webduino-blockly
require("webduino-js");
require("webduino-blockly");
var firebase = require("firebase");

var request = require("request");
var cheerio = require("cheerio");
var fs = require("fs");

var XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;
var xhr = new XMLHttpRequest();

//https://emap.pcsc.com.tw/lib/emap.js
//https://emap.pcsc.com.tw/lib/areacode.js?random=2016/8/21%20%E4%B8%8B%E5%8D%88%2004:20:11

// Configure the request

var options = {
  url: 'https://emap.pcsc.com.tw/EMapSDK.aspx',
  method: 'POST',
  form: {
    //'commandid': 'GetTown', 'cityid': '01'
    'commandid': 'Search0007', 'x1': '121517166', 'y1': '25048055', 'x2': '121768104', 'y2': '25151627'
    //'commandid': 'UpdatePosition', 'storeid': '131593', 'x': '121548322.390798', 'y': '25052738.9557778'
  }
}

// Start the request

request(options, function(error, response, body) {
  if (!error && response.statusCode == 200) {
    // Print out the response body
 
    var fs = require("fs");
    var path = "./address.html";//file output path
    var options = {
      encoding: "utf8"
    };

    fs.writeFile(path, "\ufeff" + body, options, function(error) {
      if (error) {
        console.error("write error:  " + error.message);
      } else {
        console.log("Successful Write to " + path);
      }
    });

  }else{
   			console.log("error:"+ error +" response.statusCode: " + response.statusCode);
  }

})