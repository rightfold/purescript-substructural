'use strict';

exports.dropSndFFI = function(drop) {
  return function(fst) {
    return function(snd) {
      return function(tuple) {
        drop(snd(tuple));
        return fst(tuple);
      };
    };
  };
};

exports.dropFstFFI = function(drop) {
  return function(fst) {
    return function(snd) {
      return function(tuple) {
        drop(fst(tuple));
        return snd(tuple);
      };
    };
  };
};

