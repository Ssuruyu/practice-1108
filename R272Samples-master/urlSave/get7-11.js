//npm install webduino-blockly
require("webduino-js");
require("webduino-blockly");
var firebase = require("firebase");

var request = require("request");
var cheerio = require("cheerio");
var fs = require("fs");

//http://www.ibon.com.tw/js/commonutil.js

// Configure the request
var options = {
  url: 'http://www.ibon.com.tw/retail_inquiry_ajax.aspx',
  method: 'POST',
  form: {
    'strTargetField': 'COUNTY',
    'strKeyWords': ''
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
