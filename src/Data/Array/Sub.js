'use strict';

exports.runSubArray = function(sub) {
  return function(array) {
    return sub(array.slice());
  };
};

exports.reverse = function(array) {
  return array.reverse();
};
