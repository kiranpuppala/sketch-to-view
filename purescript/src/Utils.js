"use strict";

exports._parseInt = function (hex) {
    return function (radInt) {
        console.log(parseInt(hex, radInt));
        return parseInt(hex, radInt);
    }
}

exports.logit = function (a) {
    return function(){
        console.log("Logging:",JSON.stringify(a));
    }
}