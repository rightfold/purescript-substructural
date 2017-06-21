'use strict';

exports.unsafeCloneFFI = function(Tuple) {
  return function(a) {
    return Tuple(a, a);
  };
};

exports.unsafeDrop = function(a) {
  return null;
};

exports['fst\'FFI'] = function(drop) {
  return function(fst) {
    return function(snd) {
      return function(tuple) {
        drop(snd(tuple));
        return fst(tuple);
      };
    };
  };
};

exports['snd\'FFI'] = function(drop) {
  return function(fst) {
    return function(snd) {
      return function(tuple) {
        drop(fst(tuple));
        return snd(tuple);
      };
    };
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
