'use strict';

/* -------------------------------------------------------------------------- */

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

exports.runSharedFFI = function(func) {
  return function(value) {
    return func(value);
  };
};

exports.liftSharedFFI = function(func) {
  return function(value) {
    return func(value);
  };
};

/* -------------------------------------------------------------------------- */

exports.unsafeCloneFFI = function(Tuple) {
  return function(a) {
    return Tuple(a)(a);
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

/* -------------------------------------------------------------------------- */

exports.cloneTupleFFI = function(Tuple) {
  return function(fst) {
    return function(snd) {
      return function(cloneA) {
        return function(cloneB) {
          return function(tuple) {
            var aClones = cloneA(fst(tuple));
            var bClones = cloneB(snd(tuple));
            return Tuple(
              Tuple(fst(aClones))(fst(bClones))
            )(
              Tuple(snd(aClones))(snd(bClones))
            );
          };
        };
      };
    };
  };
};

exports.dropTupleFFI = function(fst) {
  return function(snd) {
    return function(dropA) {
      return function(dropB) {
        return function(tuple) {
          dropA(fst(tuple));
          dropB(snd(tuple));
        };
      };
    };
  };
};

/* -------------------------------------------------------------------------- */

exports.borrowFFI = function(Tuple) {
  return function(func) {
    return function(value) {
      var result = func(value);
      return Tuple(value)(result);
    };
  };
};
