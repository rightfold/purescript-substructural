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

exports.cloneArrayFFI = function(clone) {
  return function(Tuple) {
    return function(fst) {
      return function(snd) {
        return function(array) {
          var length = array.length;
          var left = Array(length);
          var right = Array(length);
          for (var i = 0; i < length; ++i) {
            var clones = clone(array[i]);
            left[i] = fst(clones);
            right[i] = snd(clones);
          }
          return Tuple(left, right);
        };
      };
    };
  };
};

exports.dropArrayFFI = function(drop) {
  return function(array) {
    var length = array.length;
    for (var i = 0; i < length; ++i) {
      drop(array[i]);
    }
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
