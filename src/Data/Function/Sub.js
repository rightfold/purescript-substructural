'use strict';

exports.unsafeCloneFFI = function(Tuple) {
  return function(a) {
    return Tuple(a, a);
  };
};

exports.unsafeDrop = function(a) {
  return null;
};

exports.cloneArrayFFI = function(Tuple) {
  return function(array) {
    return Tuple(array, array.slice());
  };
};

exports.composeFFI = function(f) {
  return function(g) {
    return function(x) {
      return f(g(x));
    };
  };
};

exports.idFFI = function(a) {
  return a;
};
