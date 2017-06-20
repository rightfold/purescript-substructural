'use strict';

exports.runSubArray = function(sub) {
  return function(array) {
    return sub(array.slice());
  };
};

exports.borrowArray = function(scope) {
  return function(array) {
    scope(array)();
    return array;
  };
};

exports.reverse = function(array) {
  return array.reverse();
};
