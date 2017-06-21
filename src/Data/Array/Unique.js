'use strict';

/* -------------------------------------------------------------------------- */

exports.cloneUniqueArrayFFI = function(clone) {
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

exports.dropUniqueArrayFFI = function(drop) {
  return function(array) {
    var length = array.length;
    for (var i = 0; i < length; ++i) {
      drop(array[i]);
    }
  };
};

/* -------------------------------------------------------------------------- */

exports.empty = function(unit) {
  return [];
};

exports.singleton = function(element) {
  return [element];
};

exports.fromSharedFFI = function(array) {
  return array.slice();
};

exports.toSharedFFI = function(array) {
  return array;
};

/* -------------------------------------------------------------------------- */

exports.snocFFI = function(fst) {
  return function(snd) {
    return function(tuple) {
      var array = fst(tuple);
      var element = snd(tuple);
      array.push(element);
      return array;
    };
  };
};

/* -------------------------------------------------------------------------- */

exports.length = function(array) {
  return array.length;
};

/* -------------------------------------------------------------------------- */

exports.reverse = function(array) {
  return array.reverse();
};
