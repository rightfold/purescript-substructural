'use strict';

exports.runSubArray = function(sub) {
  return function(array) {
    return sub(array.slice());
  };
};

exports.borrowArray = function(scope) {
  return function(array) {
    return scope(array);
  };
};
