/* global exports, require */
"use strict";

var msgpack = require('msgpack-lite');

exports.encode = function(x) {
    return msgpack.encode(x);
};

exports.decodeImpl = function(left, right, buffer) {
    try {
        return right(msgpack.decode(buffer)) 
    } catch (e) {
        return left(String(JSON.stringify(e))); 
    }
};
