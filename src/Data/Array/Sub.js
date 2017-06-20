'use strict';

exports.borrowArray = function(scope) {
  return function(array) {
    return scope(array);
  };
};
