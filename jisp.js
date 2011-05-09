// JCLS shall be the official name of this software.
// An implementation of 0.001% of Common Lisp in JavaScript.
// Any help to increase this shy percent is greatly appreciated!
//
// (c) 2011 Mihai Bazon <mihai.bazon@gmail.com>
// Distributed under the BSD license (header to be updated)

/* -----[ utils ]----- */

function compose(a, rest) {
    if (rest == null) return a;
    rest = compose.apply(null, [].slice.call(arguments, 1));
    return function() { return a(rest.apply(this, arguments)) };
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

/* -----[ basics ]----- */

function car(pair) { return nullp(pair) ? NIL : pair.first };
function set_car(pair, val) { return pair.first = val };

function cdr(pair) { return nullp(pair) ? NIL : pair.second };
function set_cdr(pair, val) { return pair.second = val };

function cons(first, second) { return new Pair(first, second) };

function last(list) {
    if (nullp(list)) return NIL;
    while (!nullp(cdr(list))) list = cdr(list);
    return list;
};

function copy_list(list) {
    var ret = NIL, q;
    while (!nullp(list)) {
        var cell = cons(car(list), NIL);
        if (q) set_cdr(q, cell);
        else ret = cell;
        q = cell;
        list = cdr(list);
    }
    return ret;
}

function list_to_array(list) {
    var a = [];
    while (list !== NIL) {
        a.push(car(list));
        list = cdr(list);
    }
    return a;
};

function array_to_list(a) {
    if (a.length == 0) return NIL;
    return cons(a[0], array_to_list([].slice.call(a, 1)));
};

// too bad we can't afford to use recursion for some list manipulation
// functions. :-\  without TCO we wouldn't get far.

function eachlist(list, func) {
    while (!nullp(list)) {
        func(car(list));
        list = cdr(list);
    }
    return NIL;
};

function maplist(list, func) {
    var ret = NIL, p = NIL;
    while (!nullp(list)) {
        var val = func(car(list));
        list = cdr(list);
        var tmp = cons(val, NIL);
        if (!nullp(p)) set_cdr(p, tmp);
        else ret = tmp;
        p = tmp;
    }
    return ret;
};

/* -----[ Symbols and packages ]----- */

var _PACKAGE_;
var _ALL_PACKAGES_ = {};

function pushnew(a, el) {
    if (a.indexOf(el) < 0)
        a.push(el);
    return a;
};

function Pair(first, second) {
    this.first = first;
    this.second = second;
};

function Symbol(pack, name) {
    this._package = pack;
    this._name = name;
    this._fullname = this._package._name + "::" + this._name;
    this._plist = {};
};

Symbol.prototype = {
    toString: function() {
        return this._fullname;
    },
    fullname: function() {
        return this._fullname;
    },
    name: function() {
        return this._name;
    },
    set: function(name, val) {
        this._plist[name] = val;
        return this;
    },
    get: function(name) {
        return HOP(this._plist, name) ? this._plist[name] : NIL;
    },
    special: function(func) {
        return this.set("&SPECIAL", func);
    }
};

function Package(name, options) {
    var self = this;
    options = defaults(options, {
        use: null
    });
    _ALL_PACKAGES_[name.toUpperCase()] = self;
    self._name = name;
    self._symbols = {};
    self._use_list = [];
    if (options.use) options.use.map(function(p){
        self.use_package(p);
    });
};

Package.get = function(name) {
    return _ALL_PACKAGES_[name.toUpperCase()];
};

Package.prototype = {
    find_symbol: function(name) {
        if (HOP(this._symbols, name))
            return this._symbols[name];
        for (var i = 0; i < this._use_list.length; ++i) {
            var sym = this._use_list[i].find_symbol(name);
            if (sym != null) return sym;
        }
    },
    intern: function(name) {
        return this.find_symbol(name) ||
            (this._symbols[name] = new Symbol(this, name));
    },
    discard_symbol: function(name) {
        if (name instanceof Symbol) name = name._name;
        delete this._symbols[name];
    },
    use_package: function(name) {
        pushnew(this._use_list, Package.get(name));
    },
    uses: function(name) {
        return this._use_list.indexOf(name) >= 0;
    },
    defun: function(name, func) {
        return _GLOBAL_SCOPE_.defun(this.intern(name), func);
    },
    special: function(name, func) {
        return this.intern(name).special(func);
    }
};

var CL = new Package("CL");
var JCLS = new Package("JCLS");
var KEYWORD = new Package("KEYWORD");
var CL_USER = new Package("CL-USER", {
    use: [ "CL" ]
});

_PACKAGE_ = CL_USER;

/* -----[ the reader ]----- */

function make_string_stream($SOURCE) {
    var $line = 0, $col = 0, $pos = 0;
    function rest() {
        return $SOURCE.substr($pos);
    };
    function peek(type) {
        return $SOURCE.charAt($pos);
    };
    function next() {
        var ch = $SOURCE.charAt($pos++);
        if (ch == "\n") $col = 0, ++$line;
        else ++$col;
        return ch;
    };
    function skip_ws() {
        read_while(is_whitespace);
    };
    function read_while(test) {
        var str = "";
        while (test(peek()))
            str += next();
        return str;
    };
    var backquote = 0, commas = 0;
    function with_backquote(cont) {
        var prev_commas = commas;
        commas = 0;
        ++backquote;
        try { return cont(); }
        finally { --backquote; commas = prev_commas; }
    };
    var list = 0;
    function with_list(cont) {
        ++list;
        try { return cont(); }
        finally { --list; }
    };
    return {
        get line() { return $line },
        get col() { return $col },
        get pos() { return $pos },
        get in_backquote() { return backquote > 0 },
        get rest() { return rest() },
        get in_list() { return list },

        error: function(text) {
            throw new Error(text + "\nLine: " + $line
                            + ", Col: " + $col
                            + ", Pos: " + $pos);
        },

        next: next,
        peek: peek,
        skip_ws: skip_ws,
        read_while: read_while,
        with_backquote: with_backquote,
        with_list: with_list,
        add_comma: function() {
            ++commas;
        }
    };
};

var WHITESPACE_CHARS = {
    " ": true,
    "\t": true,
    "\n": true,
    "\x0C" : true,
    "\u2028": true,
    "\u2029": true
};

function is_whitespace(ch) {
    return HOP(WHITESPACE_CHARS, ch);
};

var _READTABLE_ = {
    "(": read_standard_list,
    '"': read_string,
    "'": read_quote,
    "`": read_backquote,
    ",": read_comma,
    "|": read_pipe,
    "#": read_sharp,

    ":": read_keyword,
    ";": ignore_comment,

    ")": function(stream) {
        stream.error("unmatched close parenthesis");
    },

    // whitespace is ignored
    " ": ignore,
    "\t": ignore,
    "\n": ignore,
    "\x0C" : ignore,
    "\u2028": ignore,
    "\u2029": ignore
};

function ignore(stream) {
    stream.next();
};

function ignore_comment(stream) {
    stream.read_while(function(ch){ return ch != "\n" });
};

function read_delimited_list(stream, endchar) {
    return stream.with_list(function(){
        var list = NIL, ret = NIL;
        while (true) {
            stream.skip_ws();
            if (stream.peek() == endchar) {
                stream.next();
                return ret;
            } else {
                var el = read(stream);
                if (el != null) {
                    var tmp = cons(el, NIL);
                    if (!nullp(list)) {
                        set_cdr(list, tmp);
                    } else
                        ret = tmp;
                    list = tmp;
                }
            }
        }
    });
};

function read_standard_list(stream) {
    return read_delimited_list(stream, ")");
};

function read_escaped(stream, endchar) {
    var esc = false, str = "";
    while (true) {
        var ch = stream.next();
        if (!ch) stream.error("Unterminated literal");
        if (esc) { str += ch; esc = false; }
        else if (ch == "\\") esc = true;
        else if (ch == endchar) return str;
        else str += ch;
    }
};

function read_string(stream) {
    return read_escaped(stream, '"');
};

function read_quote(stream) {
    return cons(CL.intern("QUOTE"), cons(read(stream), NIL));
};

function read_backquote(stream) {
    return stream.with_backquote(function(){
        return cons(JCLS.intern("QUASIQUOTE"), cons(read(stream), NIL));
    });
};

function read_comma(stream) {
    if (!stream.in_backquote)
        stream.error("Comma not inside backquote");
    stream.add_comma();
    var sym = "UNQUOTE";
    if (stream.peek() == "@") {
        stream.next();
        sym = "UNQUOTE-SPLICE";
    }
    return cons(JCLS.intern(sym), cons(read(stream), NIL));
};

function read_pipe(stream) {
    // TBD
    // return [ "SYMBOL", read_escaped(stream, "|") ];
};

function read_sharp(stream) {
    // TBD
};

function read_symbol(stream, pack) {
    var esc = false, colon = null, str = "";
    while (true) {
        var ch = stream.peek();
        if (esc) { str += stream.next(); esc = false; }
        else if (ch == ":") {
            if (colon != null || pack === KEYWORD)
                stream.error("too many colons");
            colon = str.length; str += stream.next();
        }
        else if (ch == "\\") { esc = true; stream.next(); }
        else if (HOP(_READTABLE_, ch)) break;
        str += stream.next();
    }
    if (/^[0-9]*\.?[0-9]+$/.test(str)) {
        return parseFloat(str);
    }
    str = str.toUpperCase();
    if (pack == null) pack = _PACKAGE_;
    var n = str;
    if (colon) {
        var p = str.substr(0, colon);
        n = str.substr(colon + 1);
        pack = Package.get(p);
        if (!pack) stream.error("no package " + p);
    }
    return pack.intern(n);
};

function read_keyword(stream) {
    return read_symbol(stream, KEYWORD);
};

function read(stream, eof_error, eof_value) {
    if (arguments.length == 1) eof_error = true;
    stream.skip_ws();
    var ch = stream.peek();
    if (!ch) {
        if (eof_error) stream.error("end of input");
        return eof_value;
    }
    var reader = _READTABLE_[ch], ret;
    if (reader) {
        stream.next();
        ret = reader(stream, ch);
        if (ret == null && !stream.in_list)
            // toplevel comment, advance
            ret = read(stream, eof_error, eof_value);
        return ret;
    }
    else return read_symbol(stream);
};

/* -----[ print ast ]----- */

// that's really gross for the time being, we could make it nicer.
function write_ast_to_string(node) {
    var ret = "";
    if (consp(node)) {
        if (symbolp(car(node))) {
            switch (car(node)) {
              case JCLS.intern("QUASIQUOTE"):
                return "`" + write_ast_to_string(cadr(node));
              case JCLS.intern("UNQUOTE"):
                return "," + list_to_array(cdr(node)).map(write_ast_to_string).join(" ,");
              case JCLS.intern("UNQUOTE-SPLICE"):
                return ",@" + write_ast_to_string(cadr(node));
              case CL.intern("QUOTE"):
                return "'" + write_ast_to_string(cadr(node));
            }
        }
        while (!nullp(node)) {
            if (ret) ret += " ";
            ret += write_ast_to_string(car(node));
            node = cdr(node);
            if (!consp(node)) {
                ret += " . " + write_ast_to_string(node);
                break;
            }
        }
        ret = "(" + ret + ")";
    }
    else if (symbolp(node)) {
        if (_PACKAGE_.find_symbol(node.name()))
            ret = node.name();
        else
            ret = node.fullname();
    }
    else {
        ret = node;
    }
    return ret;
};

/* -----[ evaluator stuff ]----- */

var NIL = CL.intern("NIL"); NIL.toString = function() { return "NIL" };
var T = CL.intern("T"); T.toString = function() { return "T" };

function nullp(arg) { return arg === NIL };
function consp(arg) { return arg instanceof Pair || nullp(arg) };
function symbolp(arg) { return arg instanceof Symbol };
function numberp(arg) { return typeof arg == "number" };
function stringp(arg) { return typeof arg == "string" };
function atom(x) { return symbolp(x) || numberp(x) || stringp(x) };

function quote(x) { return x };

function eq(x, y) {
    return (atom(x) && atom(y) && x === y) ? T : NIL;
};

/* -----[ Env ]----- */

function Scope(parent) {
    function a(){};
    if (parent) a.prototype = parent.data;
    this.data = new a();
    this.parent = parent;
};

Scope.prototype = {
    full: function(ns, name) {
        if (!(name instanceof Symbol)) throw new Error("Expecting a Symbol"); // XXX: should be able to drop this in production code
        return ns + "___" + name;
    },
    get: function(ns, name) {
        return this.data[this.full(ns, name)];
    },
    get_origin: function(name) {
        var s = this;
        while (s) {
            if (HOP(s.data, name)) return s;
            s = s.parent;
        }
    },
    set: function(ns, name, value) {
        var full = this.full(ns, name);
        var org = this.get_origin(full);
        if (!org) throw new Error("Undeclared variable " + name);
        return org.data[full] = value;
    },
    force: function(ns, name, value) {
        return this.data[this.full(ns, name)] = value;
    },
    defun: function(name, value) {
        return this.force("funcs", name, value);
    },
    fork: function() {
        return new Scope(this);
    }
};

// auto-generate c[ad]+r combinations
var _GLOBAL_SCOPE_ = new Scope();
var LST = {};
(function(n){
    var base = [ car, cdr ];
    while (n < 4) {
        for (var i = 0; i < (2 << n); ++i) {
            var a = i.toString(2);
            while (a.length < n + 1) a = "0" + a;
            var name = "C" + a.replace(/0/g, "A").replace(/1/g, "D") + "R";
            var func = compose.apply(null, a.split("").map(function(ch){ return base[ch] }));
            CL.defun(name, func);
            LST[name.toLowerCase()] = func;
        }
        n++;
    }
})(0);

var caar = LST.caar
, cadr = LST.cadr
, cdar = LST.cdar
, cddr = LST.cddr
, caaar = LST.caaar
, caadr = LST.caadr
, cadar = LST.cadar
, caddr = LST.caddr
, cdaar = LST.cdaar
, cdadr = LST.cdadr
, cddar = LST.cddar
, cdddr = LST.cdddr
, caaaar = LST.caaaar
, caaadr = LST.caaadr
, caadar = LST.caadar
, caaddr = LST.caaddr
, cadaar = LST.cadaar
, cadadr = LST.cadadr
, caddar = LST.caddar
, cadddr = LST.cadddr
, cdaaar = LST.cdaaar
, cdaadr = LST.cdaadr
, cdadar = LST.cdadar
, cdaddr = LST.cdaddr
, cddaar = LST.cddaar
, cddadr = LST.cddadr
, cdddar = LST.cdddar
, cddddr = LST.cddddr;

CL.defun("ATOM", atom);
CL.defun("EQ", eq);
CL.defun("CONS", cons);
CL.defun("LIST", function(){ return array_to_list(arguments) });

var analyze = (function(){
    var LAMBDA  = CL.intern("LAMBDA");

    CL.defun("FUNCALL", function(){
        var list = array_to_list(arguments);
        var func = car(list), args = cdr(list);
        return apply(func, args);
    });

    CL.defun("APPLY", function(func) {
        var list = NIL, tmp = NIL;
        for (var i = 1; i < arguments.length - 1; ++i) {
            var cell = cons(arguments[i], NIL);
            if (!nullp(tmp)) set_cdr(tmp, cell);
            if (nullp(list)) list = cell;
            tmp = cell;
        }
        if (tmp) set_cdr(tmp, arguments[arguments.length - 1]);
        else list = arguments[arguments.length - 1];
        return apply(func, list);
    });

    {
        var UNQUOTE = JCLS.intern("UNQUOTE");
        var SPLICE = JCLS.intern("UNQUOTE-SPLICE");
        var QUASIQUOTE = JCLS.intern("QUASIQUOTE");
        JCLS.special("QUASIQUOTE", function(stuff){
            stuff = car(stuff);
            if (!consp(stuff)) return itself(stuff);
            var hot = [], nest = 0;
            (function walk(node){
                var prev = NIL;
                while (!nullp(node)) {
                    if (consp(car(node))) {
                        var tag = caar(node);
                        switch (tag) {
                          case UNQUOTE:
                          case SPLICE:
                            if (nest == 0) {
                                hot.push([ tag, analyze(cadar(node)), node, prev ]);
                            } else {
                                --nest; walk(car(node)); ++nest;
                            }
                            break;
                          case QUASIQUOTE:
                            ++nest; walk(car(node)); --nest;
                            break;
                          default:
                            walk(car(node));
                        }
                    }
                    prev = node;
                    node = cdr(node);
                }
            }(stuff));
            return function(env) {
                for (var i = 0; i < hot.length; ++i) {
                    var spot = hot[i];
                    var type = spot[0], value = spot[1](env), node = spot[2], prev = spot[3];
                    if (type == UNQUOTE) {
                        set_car(node, value);
                    } else if (type == SPLICE) {
                        if (!consp(value))
                            throw "UNQUOTE-SPLICE (,@) wants a list!";
                        if (nullp(value)) {
                            set_cdr(prev, cdr(node));
                        } else {
                            value = copy_list(value);
                            set_cdr(last(value), cdr(node));
                            set_cdr(prev, value);
                        }
                    }
                }
                return stuff;
            };
        });
    }

    CL.special("QUOTE", function(ast){ return itself(car(ast)) });
    CL.special("IF", function(ast){ return do_if(car(ast), cadr(ast), caddr(ast)) });
    CL.special("LAMBDA", function(ast){ return do_lambda(car(ast), cdr(ast)) });
    CL.special("PROGN", function(ast){ return do_sequence(ast) });
    CL.special("LET", function(ast){
        var names = [], values = [];
        eachlist(car(ast), function(def){
            names.push(car(def));
            values.push(analyze(cadr(def)));
        });
        var body = do_sequence(cdr(ast));
        return function(env) {
            env = new Scope(env);
            var val = values.map(function(proc){ return proc(env) });
            for (var i = 0; i < names.length; ++i) {
                env.force("vars", names[i], val[i]);
            }
            return body(env);
        };
    });
    CL.special("LET*", function(ast){
        var names = [], values = [];
        eachlist(car(ast), function(def){
            names.push(car(def));
            values.push(analyze(cadr(def)));
        });
        var body = do_sequence(cdr(ast));
        return function(env) {
            env = new Scope(env);
            for (var i = 0; i < names.length; ++i) {
                env.force("vars", names[i], values[i](env));
            }
            return body(env);
        };
    });
    // these are so similar to LET/FLET it's almost boring.
    CL.special("FLET", function(ast){
        var names = [], values = [];
        eachlist(car(ast), function(def){
            names.push(car(def));
            values.push(do_lambda(cadr(def), cddr(def)));
        });
        var body = do_sequence(cdr(ast));
        return function(env) {
            env = new Scope(env);
            var val = values.map(function(proc){ return proc(env) });
            for (var i = 0; i < names.length; ++i) {
                env.force("funcs", names[i], val[i]);
            }
            return body(env);
        };
    });
    CL.special("LABELS", function(ast){
        var names = [], values = [];
        eachlist(car(ast), function(def){
            names.push(car(def));
            values.push(do_lambda(cadr(def), cddr(def)));
        });
        var body = do_sequence(cdr(ast));
        return function(env) {
            env = new Scope(env);
            for (var i = 0; i < names.length; ++i) {
                env.force("funcs", names[i], values[i](env));
            }
            return body(env);
        };
    });
    CL.special("SETQ", function(defs){
        var names = [], values = [];
        while (!nullp(defs)) {
            names.push(car(defs));
            values.push(analyze(cadr(defs)));
            defs = cddr(defs);
        }
        return function(env) {
            for (var ret = NIL, i = 0; i < names.length; ++i)
                env.set("vars", names[i], ret = values[i](env));
            return ret;
        };
    });
    CL.special("DEFUN", function(ast){
        var name = car(ast), func = do_lambda(cadr(ast), cddr(ast));
        return function(env) {
            return _GLOBAL_SCOPE_.force("funcs", name, func(env));
        };
    });
    CL.special("FUNCTION", function(ast){
        var name = car(ast);
        return function(env) {
            return env.get("funcs", name);
        };
    });

    CL.special("DEFMACRO", function(ast){
        var name = car(ast), func = do_lambda(cadr(ast), cddr(ast));
        name.special(function(values){
            // "analyzing" a macro call means in fact calling the
            // macro function and analyze its return value instead.
            return analyze(apply(func(_GLOBAL_SCOPE_), values));
        });
        return function(env) {
            return NIL;
        };
    });

    function analyze(expr) {
        var tmp;
        if (symbolp(expr)) switch (expr) {
          case NIL:
          case T:
            return itself(expr);
          default:
            if (expr._package === KEYWORD) return itself(expr);
            else return get_var(expr);
        }
        else if (numberp(expr) || stringp(expr)) return itself(expr);
        else if (atom(tmp = car(expr))) {
            var spec;
            if (symbolp(tmp) && !nullp(spec = tmp.get("&SPECIAL")))
                return spec(cdr(expr));
            return do_application(tmp, cdr(expr));
        }
        else if (caar(expr) === LAMBDA) {
            return do_inline_call(cadar(expr), cddar(expr), cdr(expr));
        }
    };

    function itself(el) {
        return function(){ return el };
    };

    function get_var(symbol) {
        return function(env) {
            return env.get("vars", symbol);
        };
    };

    function do_if(condition, consequent, alternative) {
        condition = analyze(condition);
        consequent = analyze(consequent);
        alternative = analyze(alternative);
        return function(env) {
            return nullp(condition(env))
                ? alternative(env)
                : consequent(env);
        };
    };

    function do_lambda(args, body) {
        body = do_sequence(body);
        return function(env) {
            return [ args, body, env ];
        };
    };

    function do_sequence(list) {
        list = maplist(list, analyze);
        return function(env) {
            var val = NIL;
            eachlist(list, function(proc){ val = proc(env) });
            return val;
        };
    };

    function do_application(operator, args) {
        args = maplist(args, analyze);
        return function(env) {
            var func = env.get("funcs", operator);
            if (!func)
                throw new Error("Undefined function: " + write_ast_to_string(operator));
            return apply(func, maplist(args, function(proc){
                return proc(env);
            }));
        };
    };

    function do_inline_call(args, body, values) {
        values = maplist(values, analyze);
        body = do_sequence(body);
        return function(env) {
            return apply([ args, body, env ], maplist(values, function(proc){
                return proc(env);
            }));
        };
    };

    function apply(func, values) {
        if (func instanceof Function) {
            return func.apply(null, list_to_array(values));
        }
        else if (func instanceof Array) {
            var names = func[0], body = func[1], env = func[2];
            if (!nullp(names)) {
                env = env.fork();
                while (!nullp(names)) {
                    env.force("vars", car(names), car(values));
                    names = cdr(names);
                    values = cdr(values);
                }
            }
            return body(env);
        }
    };

    return analyze;

}());

function eval(ast, env) {
    return analyze(ast)(env || _GLOBAL_SCOPE_);
};

CL.defun("EVAL", eval);

exports.write_ast_to_string = write_ast_to_string;
exports.read = read;
exports.eval = eval;
exports.make_string_stream = make_string_stream;
exports.analyze = analyze;

/* -----[ Arithmetic ]----- */

CL.defun("+", function(a, b){
    return [].slice.call(arguments).reduce(function(a, b){ return a + b }, 0);
});

CL.defun("-", function(){
    return [].slice.call(arguments, 1).reduce(function(a, b){ return a - b }, arguments[0]);
});

CL.defun("*", function(){
    return [].slice.call(arguments).reduce(function(a, b){ return a * b }, 1);
});

CL.defun("/", function(){
    return [].slice.call(arguments, 1).reduce(function(a, b){ return a / b }, arguments[0]);
});

CL.defun("=", function(val){
    for (var i = 1; i < arguments.length; ++i)
        if (arguments[i] !== val) return NIL;
    return T;
});

CL.defun("<", function(last){
    for (var i = 1; i < arguments.length; ++i) {
        if (arguments[i] <= last) return NIL;
        last = arguments[i];
    }
    return T;
});

CL.defun("<=", function(last){
    for (var i = 1; i < arguments.length; ++i) {
        if (arguments[i] < last) return NIL;
        last = arguments[i];
    }
    return T;
});

CL.defun(">", function(last){
    for (var i = 1; i < arguments.length; ++i) {
        if (arguments[i] >= last) return NIL;
        last = arguments[i];
    }
    return T;
});

CL.defun(">=", function(last){
    for (var i = 1; i < arguments.length; ++i) {
        if (arguments[i] > last) return NIL;
        last = arguments[i];
    }
    return T;
});

/* -----[ temporary stuff ]----- */

(function(IO){
    IO.defun("LOG", function(){
        //console.log([].slice.call(arguments).join(", "));
        console.log(write_ast_to_string(array_to_list(arguments)));
    });
})(new Package("IO"));

// until I figure out proper SETF..
CL.defun("SET-CAR", set_car);
CL.defun("SET-CDR", set_cdr);

// Local Variables:
// js-indent-level:4
// End:
