'use strict';

exports.cloneArrayFFI = function(Tuple) {
  return function(array) {
    return Tuple(array, array.slice());
  };
};
