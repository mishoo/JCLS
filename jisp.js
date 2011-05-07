/* -----[ functional utilities ]----- */

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
        full_name: function() {
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
                return this._plist[name];
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
        defun: function(name, func) {
                _GLOBAL_SCOPE_.defun(this.intern(name), func);
        }
};

var CL = new Package("CL");
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
                try { return cont() }
                finally { --backquote; commas = prev_commas; }
        };
        return {
                get line() { return $line },
                get col() { return $col },
                get pos() { return $pos },
                get in_backquote() { return backquote > 0 },
                get rest() { return rest() },

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
        stream.read_while(function(ch){ return ch == "\n" });
};

function read_delimited_list(stream, endchar) {
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
                return cons(CL.intern("BACKQUOTE"), cons(read(stream), NIL));
        });
};

function read_comma(stream) {
        if (!stream.in_backquote)
                stream.error("Comma not inside backquote");
        stream.add_comma();
        return cons(CL.intern("UNQUOTE"), cons(read(stream), NIL));
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
        var ch = stream.peek();
        if (!ch) {
                if (eof_error) stream.error("end of input");
                return eof_value;
        }
        var reader = _READTABLE_[ch];
        if (reader) {
                stream.next();
                while (true) {
                        var ret = reader(stream, ch);
                        if (ret == null) ret = read(stream, eof_error, eof_value);
                        if (ret != null) return ret;
                }
        }
        return read_symbol(stream);
};

/* -----[ print ast ]----- */

// that's really gross for the time being, we could make it nicer.
function write_ast_to_string(node) {
        var ret = "";
        if (consp(node)) {
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
                ret = node.name();
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
        this.parent = parent;
        this.ns_vars = {};
        this.ns_funcs = {};
};

Scope.prototype = {
        ns: function(ns) {
                return this["ns_" + ns];
        },
        get: function(ns, name) {
                if (!(name instanceof Symbol)) throw new Error("Expecting a Symbol");
                var tmp = this.ns(ns);
                return HOP(tmp, name) ? tmp[name] : (this.parent && this.parent.get(ns, name));
        },
        set: function(ns, name, value) {
                if (!(name instanceof Symbol)) throw new Error("Expecting a Symbol");
                this.ns(ns)[name] = value;
        },
        defun: function(name, value) {
                return this.set("funcs", name, value);
        },
        extend: function(ns, names, values) {
                while (names !== NIL) {
                        var name = car(names);
                        this.set(ns, name, car(values));
                        names = cdr(names);
                        values = cdr(values);
                }
                return this;
        },
        fork: function(ns, names, values) {
                if (nullp(names)) return this;
                var s = new Scope(this);
                return s.extend(ns, names, values);
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
                        //console.log("var " + name.toLowerCase() + " = LST." + name.toLowerCase() + ";");
                        var func = compose.apply(null, a.split("").map(function(ch){ return base[ch] }));
                        _GLOBAL_SCOPE_.set("funcs", CL.intern(name), func);
                        LST[name.toLowerCase()] = func;
                }
                n++;
        }
})(1);

var caar = LST.caar;
var cadr = LST.cadr;
var cdar = LST.cdar;
var cddr = LST.cddr;
var caaar = LST.caaar;
var caadr = LST.caadr;
var cadar = LST.cadar;
var caddr = LST.caddr;
var cdaar = LST.cdaar;
var cdadr = LST.cdadr;
var cddar = LST.cddar;
var cdddr = LST.cdddr;
var caaaar = LST.caaaar;
var caaadr = LST.caaadr;
var caadar = LST.caadar;
var caaddr = LST.caaddr;
var cadaar = LST.cadaar;
var cadadr = LST.cadadr;
var caddar = LST.caddar;
var cadddr = LST.cadddr;
var cdaaar = LST.cdaaar;
var cdaadr = LST.cdaadr;
var cdadar = LST.cdadar;
var cdaddr = LST.cdaddr;
var cddaar = LST.cddaar;
var cddadr = LST.cddadr;
var cdddar = LST.cdddar;
var cddddr = LST.cddddr;

var eval = (function(){
        var QUOTE = CL.intern("QUOTE")
        , ATOM    = CL.intern("ATOM")
        , EQ      = CL.intern("EQ")
        , CAR     = CL.intern("CAR")
        , CDR     = CL.intern("CDR")
        , CONS    = CL.intern("CONS")
        , IF      = CL.intern("IF")
        , LET     = CL.intern("LET")
        , LET_    = CL.intern("LET*")
        , FLET    = CL.intern("FLET")
        , LABELS  = CL.intern("LABELS")
        , SETQ    = CL.intern("SETQ")
        , DEFUN   = CL.intern("DEFUN")
        , PROGN   = CL.intern("PROGN")
        , LAMBDA  = CL.intern("LAMBDA");

        CL.defun("FUNCALL", function() {
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

        return function eval(expr, env) {
                if (env == null) env = _GLOBAL_SCOPE_;
                if (symbolp(expr)) switch (expr) {
                    case NIL:
                        return NIL;
                    case T:
                        return T;
                    default:
                        if (expr._package === KEYWORD) return expr;
                        else return env.get("vars", expr);
                }
                else if (numberp(expr) || stringp(expr)) return expr;
                else if (atom(car(expr))) switch (car(expr)) {
                    case QUOTE:
                        return cadr(expr);
                    case ATOM:
                        return atom(eval(cadr(expr), env));
                    case EQ:
                        return eq(eval(cadr(expr), env),
                                  eval(caddr(expr), env));
                    case CAR:
                        return car(eval(cadr(expr), env));
                    case CDR:
                        return cdr(eval(cadr(expr), env));
                    case CONS:
                        return cons(eval(cadr(expr), env), eval(caddr(expr), env));
                    case IF:
                        return eval_if(cadr(expr), caddr(expr), cadddr(expr), env);
                    case LAMBDA:
                        return make_lambda(cadr(expr), cddr(expr), env);
                    case LET:
                        return eval_sequence(cddr(expr), (function(defs, env){
                                var names = [], values = [];
                                while (!nullp(defs)) {
                                        var f = car(defs);
                                        names.push(car(f));
                                        values.push(eval(cadr(f), env));
                                        defs = cdr(defs);
                                }
                                env = new Scope(env);
                                for (var i = 0; i < names.length; ++i)
                                        env.set("vars", names[i], values[i]);
                                return env;
                        })(cadr(expr), env));
                    case LET_:
                        return eval_sequence(cddr(expr), (function(defs, env){
                                while (!nullp(defs)) {
                                        var f = car(defs);
                                        env = new Scope(env);
                                        env.set("vars", car(f), eval(cadr(f), env));
                                        defs = cdr(defs);
                                }
                                return env;
                        })(cadr(expr), env));
                    case FLET:
                        return eval_sequence(cddr(expr), (function(defs, env){
                                var names = [], values = [];
                                while (!nullp(defs)) {
                                        var f = car(defs);
                                        names.push(car(f));
                                        values.push(make_lambda(cadr(f), cddr(f), env));
                                        defs = cdr(defs);
                                }
                                env = new Scope(env);
                                for (var i = 0; i < names.length; ++i)
                                        env.set("funcs", names[i], values[i]);
                                return env;
                        })(cadr(expr), env));
                    case LABELS:
                        return eval_sequence(cddr(expr), (function(defs, env){
                                while (!nullp(defs)) {
                                        var f = car(defs);
                                        env = new Scope(env);
                                        env.set("funcs", car(f), make_lambda(cadr(f), cddr(f), env));
                                        defs = cdr(defs);
                                }
                                return env;
                        })(cadr(expr), env));
                    case SETQ:
                        return (function(defs){
                                var val = NIL;
                                while (!nullp(defs)) {
                                        val = eval(cadr(defs), env);
                                        env.set("vars", car(defs), val);
                                        defs = cddr(defs);
                                }
                                return val;
                        })(cdr(expr));
                    case PROGN:
                        return eval_sequence(cdr(expr), env);
                    case DEFUN:
                        return (function(name, args, body){
                                return _GLOBAL_SCOPE_.set("funcs", name, make_lambda(args, body, env));
                        })(cadr(expr), caddr(expr), cdddr(expr));
                    default:
                        var func = env.get("funcs", car(expr));
                        if (!func)
                                throw new Error("Undefined function: " + write_ast_to_string(car(expr)));
                        return apply(func, eval_list(cdr(expr), env));
                }
                else if (caar(expr) === LAMBDA) {
                        return eval_sequence(cddar(expr),
                                             env.fork("vars",
                                                      cadar(expr),
                                                      eval_list(cdr(expr), env)));
                }
        };
        function eval_if(condition, consequent, alternative, env) {
                if (eval(condition, env) !== NIL)
                        return eval(consequent, env);
                else return eval(alternative, env);
        };
        function eval_list(list, env) {
                if (nullp(list)) return NIL;
                return cons(eval(car(list), env),
                            eval_list(cdr(list), env));
        };
        function eval_sequence(list, env) {
                var ret = NIL;
                while (!nullp(list)) {
                        ret = eval(car(list), env);
                        list = cdr(list);
                }
                return ret;
        };
        function make_lambda(args, body, env) {
                return [ args, body, env ];
        };
        function apply(func, values) {
                if (func instanceof Function) {
                        return func.apply(null, list_to_array(values));
                }
                else if (func instanceof Array) {
                        var names = func[0], body = func[1], env = func[2];
                        return eval_sequence(body, env.fork("vars", names, values));
                }
        };
}());

exports.write_ast_to_string = write_ast_to_string;
exports.read = read;
exports.eval = eval;
exports.make_string_stream = make_string_stream;

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

/* -----[ temporary I/O ]----- */

(function(IO){
        IO.defun("LOG", function(){
                console.log([].slice.call(arguments).join(", "));
        });
})(new Package("IO"));
