var fnctl = require("./fnctl");
var HOP = fnctl.HOP;

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

/* -----[ Symbols and packages ]----- */

var _READTABLE_;
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
                return this._package + ":" + this._name;
        },
        full_name: function() {
                return this._fullname;
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
        options = fnctl.defaults(options, {
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
                var sym = this._symbols[name];
                if (sym) return sym;
                for (var i = 0; i < this._use_list; ++i) {
                        var p = this._use_list[i];
                        sym = p._symbols[name];
                        if (sym) return sym;
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
        }
};

var CL = new Package("CL");
_PACKAGE_ = CL;
// XXX: we have a problem with symbol inheritance
// _PACKAGE_ = new Package("CL-USER", {
//         use: [ "CL" ]
// });

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
                read_while(function(ch){
                        return is_whitespace(ch);
                });
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

var SPECIAL_CHARS = {
        "(": read_standard_list,
        '"': read_string,
        "'": read_quote,
        "`": read_backquote,
        ",": read_comma,
        "|": read_pipe,
        "#": read_sharp,

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
        read_while(function(ch){ return ch == "\n" });
};

function read_delimited_list(stream, endchar) {
        var list = NIL, ret;
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

function read_symbol(stream) {
        var esc = false, colon = null, str = "";
        while (true) {
                var ch = stream.peek();
                if (esc) { str += stream.next(); esc = false; }
                else if (ch == ":") {
                        if (colon != null) stream.error("too many colons");
                        colon = str.length; str += stream.next();
                }
                else if (ch == "\\") { esc = true; stream.next(); }
                else if (HOP(SPECIAL_CHARS, ch)) break;
                str += stream.next();
        }
        if (/^[0-9]*\.?[0-9]+$/.test(str)) {
                return parseFloat(str);
        }
        str = str.toUpperCase();
        var pack = _PACKAGE_, n = str;
        if (colon) {
                var p = str.substr(0, colon);
                n = str.substr(colon + 1);
                pack = Package.get(p);
                if (!pack) stream.error("no package " + p);
        }
        return pack.intern(n);
};

function read(stream, eof_error, eof_value) {
        if (arguments.length == 1) eof_error = true;
        var ch = stream.peek();
        if (!ch) {
                if (eof_error) stream.error("end of input");
                return eof_value;
        }
        var reader = SPECIAL_CHARS[ch];
        if (reader) {
                stream.next();
                return reader(stream);
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
                ret = node.full_name();
        }
        else {
                ret = node;
        }
        return ret;
};

/* -----[ evaluator stuff ]----- */

var NIL = CL.intern("NIL");
CL.intern("T");
CL.intern("IF");
CL.intern("LAMBDA");
CL.intern("QUOTE");
CL.intern("CONS");
CL.intern("ATOM");
CL.intern("EQ");
CL.intern("CAR");
CL.intern("CDR");

function T() { return CL.find_symbol("T") };
function nullp(arg) { return arg === NIL };
function consp(arg) { return arg instanceof Pair || nullp(arg) };
function symbolp(arg) { return arg instanceof Symbol };
function numberp(arg) { return typeof arg == "number" };
function stringp(arg) { return typeof arg == "string" };
function atom(x) { return symbolp(x) || numberp(x) || stringp(x) };

function quote(x) { return x };

function eq(x, y) {
        if (atom(x) && atom(y) && x === y) return true;
        if (x === null && y === null) return true;
        return false;
};

// auto-generate c[ad]+r combinations
var LST = {};
(function(n){
        var base = [ car, cdr ];
        while (n < 4) {
                for (var i = 0; i < (2 << n); ++i) {
                        var a = i.toString(2);
                        while (a.length < n + 1) a = "0" + a;
                        var name = "C" + a.replace(/0/g, "A").replace(/1/g, "D") + "R";
                        var func = fnctl.compose.apply(null, a.split("").map(function(ch){ return base[ch] }));
                        CL.intern(name).set("FUNCTION", func).set("COMPILED", true);
                        LST[name.toLowerCase()] = func;
                }
                n++;
        }
})(1);

/* -----[ Env ]----- */

function Scope(parent) {
        this.parent = parent;
        this.vars = {};
};

Scope.prototype = {
        get: function(symbol) {
                if (symbol instanceof Symbol)
                        symbol = symbol.full_name();
                return this.vars[symbol] || this.parent && this.parent.get(symbol);
        },
        extend: function(names, values) {
                var s = names !== NIL ? new Scope(this) : this;
                while (names !== NIL) {
                        var name = car(names);
                        if (name instanceof Symbol)
                                name = name.full_name();
                        s.vars[name] = car(values);
                        names = cdr(names);
                        values = cdr(values);
                }
                return s;
        }
};

var eval = (function(){
        var QUOTE = CL.find_symbol("QUOTE"),
            ATOM = CL.find_symbol("ATOM"),
            EQ = CL.find_symbol("EQ"),
            CAR = CL.find_symbol("CAR"),
            CDR = CL.find_symbol("CDR"),
            CONS = CL.find_symbol("CONS"),
            IF = CL.find_symbol("IF"),
            LAMBDA = CL.find_symbol("LAMBDA");
        return function eval(expr, env) {
                //console.log(write_ast_to_string(expr));
                if (env == null) env = new Scope();
                if (symbolp(expr)) return env.get(expr);
                else if (numberp(expr) || stringp(expr)) return expr;
                else if (atom(car(expr))) switch (car(expr)) {
                    case QUOTE:
                        return LST.cadr(expr);
                    case ATOM:
                        return atom(eval(LST.cadr(expr), env));
                    case EQ:
                        return eq(eval(LST.cadr(expr), env), eval(LST.caddr(expr), env));
                    case CAR:
                        return car(eval(LST.cadr(expr), env));
                    case CDR:
                        return cdr(eval(LST.cadr(expr), env));
                    case CONS:
                        return cons(eval(LST.cadr(expr), env), eval(LST.caddr(expr), env));
                    case IF:
                        return eval_if(LST.cadr(expr), LST.caddr(expr), LST.cadddr(expr), env);
                    default:
                        var func = car(expr).get("FUNCTION");
                        if (!func) throw "Undefined function: " + expr.full_name();
                        if (car(expr).get("COMPILED"))
                                return func.apply(null, list_to_array(eval_list(cdr(expr), env)));
                        throw new Error("No support for this yet");
                }
                else if (LST.caar(expr) === LAMBDA) {
                        return eval_sequence(LST.cddar(expr),
                                             env.extend(LST.cadar(expr),
                                                        eval_list(cdr(expr), env)));
                }
        };
        function eval_if(condition, consequent, alternative, env) {
                if (eval(condition, env) !== NIL)
                        return eval(consequent, env);
                else return eval(alternative, env);
        };
        function eval_list(list, env) {
                if (list === NIL) return NIL;
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
}());

// // console.log(
// //         LST.cadr(
// //                 cons(1, cons(2, cons(3, cons(4, NIL))))
// //         )
// // )

// // return;

// console.log(
//         write_ast_to_string(
//                 eval(
//                         read(
//                                 make_string_stream(
//                                         "((lambda (x) (cadr x) (cons (caddr x) (cadddr x))) '(1 2 3 4 5))"
//                                         //"((lambda (x) (caddr x)) (cons 1 (cons 2 (cons 3 NIL))))"
//                                         //"'(1 2 3 4 5)"
//                                 )
//                         )
//                 )
//         )
// )

// // console.log(
// //         write_ast_to_string(
// //                 read(
// //                         make_string_stream(
// //                                 "'(1 2 3 4 5)"
// //                                 //"(cons 1 (cons 2 (cons 3 nil)))"
// //                         )
// //                 )
// //         )
// // )










// // var sys = require("sys");
// // var str = "(defun f\\(oo(a b c) (cl:format nil \"foo ~a ~a ~a\" \n\
// // a b (+ c '0.5 0.3.4)))";
// // console.log( sys.inspect( read(make_string_stream(str)), null, null ) );
// // //console.log( read(make_string_stream(str)) );
