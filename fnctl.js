function compose(a, rest) {
        if (rest == null) return a;
        rest = compose.apply(null, [].slice.call(arguments, 1));
        return function() { return a(rest.apply(this, arguments)) };
};

function range(start, stop) {
        var a = [];
        for (var i = start, j = 0; i <= stop; ++i, ++j)
                a[j] = i;
        return a;
};

var amb = (function(){
        function fail() { throw fail }
        function amb(values, program, fail_value) {
                if (!values) fail();
                var n = values.length;
                if (n == 0) fail();
                for (var i = 0; i < n; ++i) try {
                        return program(values[i]);
                } catch(ex) {
                        if (ex !== fail) throw ex;
                }
                if (fail_value === fail) fail();
                return fail_value;
        }
        amb.fail = fail;
        return amb;
})();

function repeat_string(str, i) {
        if (i <= 0) return "";
        if (i == 1) return str;
        var d = repeat_string(str, i >> 1);
        d += d;
        if (i & 1) d += str;
        return d;
};

function defaults(args, defs) {
        var ret = {};
        if (args === true)
                args = {};
        for (var i in defs) if (HOP(defs, i)) {
                ret[i] = (args && HOP(args, i)) ? args[i] : defs[i];
        }
        return ret;
};

function HOP(obj, prop) {
        return Object.prototype.hasOwnProperty.call(obj, prop);
};

// exports

exports.amb = amb;
exports.compose = compose;
exports.defaults = defaults;
exports.repeat_string = repeat_string;
exports.HOP = HOP;
exports.range = range;
