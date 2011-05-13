// JCLS shall be the official name of this software.
// An implementation of 0.001% of Common Lisp in JavaScript.
// Any help to increase this shy percent is greatly appreciated!
//
// (c) 2011 Mihai Bazon <mihai.bazon@gmail.com>
// Distributed under the BSD license (header to be updated)

/* -----[ utils ]----- */

function curry(f) {
    var args = [].slice.call(arguments, 1);
    return function() { return f.apply(this, args.concat([].slice.call(arguments))); };
};

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

/* -----[ Symbols and packages ]----- */

var _PACKAGE_;
var _ALL_PACKAGES_ = {};

function pushnew(a, el) {
    if (a.indexOf(el) < 0)
        a.push(el);
    return a;
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
        return _GLOBAL_ENV_.defun(this.intern(name), func);
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

/* -----[ basics ]----- */

var NIL = CL.intern("NIL"); NIL.toString = function() { return "NIL" };
var T = CL.intern("T"); T.toString = function() { return "T" };
function nullp(arg) { return arg === NIL };

function Pair(first, second) {
    this.first = first;
    this.second = second;
};

function car(pair) { return nullp(pair) ? NIL : pair.first };
function set_car(pair, val) { return pair.first = val };

function cdr(pair) { return nullp(pair) ? NIL : pair.second };
function set_cdr(pair, val) { return pair.second = val };

function cons(first, second) { return new Pair(first, second) };
function consp(arg) { return arg instanceof Pair || nullp(arg) };

// function car(pair) { return nullp(pair) ? NIL : pair[0] };
// function set_car(pair, val) { return pair[0] = val };

// function cdr(pair) { return nullp(pair) ? NIL : pair[1] };
// function set_cdr(pair, val) { return pair[1] = val };

// function cons(first, second) { return [ first, second ] };
// function consp(arg) { return arg instanceof Array || nullp(arg) };

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
    var ret = NIL, p;
    while (!nullp(list)) {
        var tmp = cons(func(car(list)), NIL);
        list = cdr(list);
        if (p) set_cdr(p, tmp);
        else ret = tmp;
        p = tmp;
    }
    return ret;
};

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
        var ret = cont();
        --backquote;
        commas = prev_commas;
        return ret;
    };
    var list = 0;
    function with_list(cont) {
        ++list;
        var ret = cont();
        --list;
        return ret;
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

var DOT = CL.intern(".");
function read_delimited_list(stream, endchar) {
    return stream.with_list(function(){
        var list, ret = NIL;
        while (true) {
            stream.skip_ws();
            if (stream.peek() == endchar) {
                stream.next();
                return ret;
            } else {
                var el = read(stream);
                if (el != null) {
                    var tmp;
                    tmp = el === DOT
                        ? read(stream)
                        : cons(el, NIL);
                    if (list) set_cdr(list, tmp);
                    else ret = tmp;
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
    return _PACKAGE_.intern(read_escaped(stream, "|"));
};

function read_sharp(stream) {
    throw "TBD";
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
        if (typeof node == "string") ret = JSON.stringify(node);
        else ret = node;
    }
    return ret;
};

/* -----[ evaluator stuff ]----- */

function symbolp(arg) { return arg instanceof Symbol };
function numberp(arg) { return typeof arg == "number" };
function stringp(arg) { return typeof arg == "string" };
function atom(x) { return symbolp(x) || numberp(x) || stringp(x) };
function quote(x) { return x };
function eq(x, y) { return (atom(x) && atom(y) && x === y) ? T : NIL };

/* -----[ Env ]----- */

function Environment(parent) {
    function a(){};
    if (parent) a.prototype = parent.data;
    this.data = new a();
    this.parent = parent;
};

Environment.prototype = {
    full: function(ns, name) {
        if (!(name instanceof Symbol)) {
            // XXX: should drop this in production code.  There's no
            // way to get here for anything other than a Symbol.
            throw new Error("Expecting a Symbol");
        }
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
        return new Environment(this);
    }
};

// auto-generate c[ad]+r combinations
var _GLOBAL_ENV_ = new Environment();
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

CL.defun("ATOM", function(arg) { return atom(arg) ? T : NIL });
CL.defun("SYMBOLP", function(arg) { return symbolp(arg) ? T : NIL });
CL.defun("EQ", eq);
CL.defun("CONS", cons);
CL.defun("LIST", function(){ return array_to_list(arguments) });

var analyze = (function(){
    var LAMBDA  = CL.intern("LAMBDA")
    , PROGN = CL.intern("PROGN");

    CL.defun("FUNCALL", function(){
        var list = array_to_list(arguments);
        var func = car(list), args = cdr(list);
        return apply(func, args);
    });

    CL.defun("APPLY", function(func) {
        var list = NIL, p, len = arguments.length - 1, last = arguments[len];
        if (!consp(last))
            throw new Error("Last argument to apply must be a list");
        for (var i = 1; i < len; ++i) {
            var cell = cons(arguments[i], NIL);
            if (p) set_cdr(p, cell);
            else list = cell;
            p = cell;
        }
        if (p) set_cdr(p, last);
        else list = last;
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
            env = env.fork();
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
            env = env.fork();
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
            env = env.fork();
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
            env = env.fork();
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
            return _GLOBAL_ENV_.force("funcs", name, func(env));
        };
    });
    CL.special("FUNCTION", function(ast){
        var name = car(ast);
        return function(env) {
            return env.get("funcs", name);
        };
    });

    CL.special("DEFMACRO", function(ast){
        var name = car(ast), func = do_lambda(cadr(ast), cddr(ast), true);
        _GLOBAL_ENV_.force("macs", name, func);
        return itself(NIL);
    });

    function analyze(expr) {
        var tmp;
        if (symbolp(expr)) switch (expr) {
          case NIL:
          case T:
            return itself(expr);
          default:
            if (expr._package === KEYWORD) return itself(expr);
            else return function(env) {
                return env.get("vars", expr);
            };
        }
        else if (numberp(expr) || stringp(expr)) return itself(expr);
        else if (atom(tmp = car(expr))) {

            if (tmp !== PROGN) ++$level;
            var run = do_application(tmp, cdr(expr));
            if (tmp !== PROGN) --$level;

            // XXX: I don't like this.

            // evaluating all is a bad idea
            // if ($level == 0 && tmp !== PROGN) {
            //     run($environment);
            // }

            // Are we safe if we only evaluate DEFUN, DEFMACRO and LET?
            // And return NIL?

            // HELL.p!

            if ($level == 0 && (tmp === CL.find_symbol("DEFUN")
                                || tmp === CL.find_symbol("DEFMACRO")
                                || tmp === CL.find_symbol("LET")
                                || tmp === CL.find_symbol("LET*")
                                || tmp === CL.find_symbol("FLET")
                                || tmp === CL.find_symbol("LABELS")))
            {
                // calling run here evaluates the stuff at "compile-time".
                // because it happens that $environment == _GLOBAL_ENV_,
                // after compiling we'll get the defuns and defmacros right.
                run($environment);

                // by returning NIL we effectively discard these from
                // the tree, such that at evaluation time these forms
                // won't run a second time.  that's probably wrong in
                // the general case. (we need eval-when)
                return itself(NIL);
            }

            return run;
        }
        else if (caar(expr) === LAMBDA) {
            return do_inline_call(cadar(expr), cddar(expr), cdr(expr));
        }
    };

    function itself(el) {
        return function(){ return el };
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

    // LAMBDA-LIST parser
    var do_lambda_list = (function(){
        var $REST = CL.intern("&REST");
        var $BODY = CL.intern("&BODY");
        var $KEY = CL.intern("&KEY");
        var $OPTIONAL = CL.intern("&OPTIONAL");

        function find(key, list) {
            key = key._name;
            while (!nullp(list)) {
                if (car(list)._name == key) {
                    return cadr(list);
                }
                list = cddr(list);
            }
            // returns undefined on purpose if the key wasn't found
        };

        function lambda_arg_key_list(arg, env, values) {
            var name = arg[0], def = arg[1], arg_p = arg[2];
            var val = find(name, values);
            env.force("vars", name, val || def(env));
            if (!nullp(arg_p)) env.force("vars", arg_p, val ? T : NIL);
            return values;
        };

        function lambda_arg_key(arg, env, values) {
            env.force("vars", arg, find(arg, values) || NIL);
            return values;
        };

        function lambda_arg_optional_list(arg, env, values) {
            var name = arg[0], def = arg[1], arg_p = arg[2];
            if (nullp(values)) {
                env.force("vars", name, def(env));
                if (!nullp(arg_p)) env.force("vars", arg_p, NIL);
                return NIL;
            } else {
                env.force("vars", name, car(values));
                if (!nullp(arg_p)) env.force("vars", arg_p, T);
                return cdr(values);
            }
        };

        function lambda_arg_itself(name, optional, env, values) {
            if (nullp(values)) {
                if (!optional) throw new Error(name + " is a required argument");
                return env.force("vars", name, NIL);
            } else {
                env.force("vars", name, car(values));
                return cdr(values);
            }
        };

        function lambda_arg_rest(name, env, values) {
            env.force("vars", name, values);
            return NIL;
        };

        function lambda_arg_destruct(args, env, values) {
            if (!consp(car(values)))
                throw new Error("Expecting a list");
            var list = car(values);
            eachlist(args, function(arg) {
                list = arg(env, list);
            });
            return cdr(values);
        };

        return function do_lambda_list(args, destructuring) {
            var ret = NIL, p, optional = false, key = false;
            function add(val) {
                var cell = cons(val, NIL);
                if (p) set_cdr(p, cell);
                else ret = cell;
                p = cell;
            };
            while (!nullp(args)) {
                var arg = car(args);
                args = cdr(args);
                if (symbolp(arg)) switch(arg) {
                  case $REST:
                  case $BODY:
                    add(curry(lambda_arg_rest, car(args)));
                    return ret;
                  case $OPTIONAL:
                    optional = true; key = false;
                    continue;
                  case $KEY:
                    key = true; optional = false;
                    continue;
                  default:
                    if (key) add(curry(lambda_arg_key, arg));
                    else add(curry(lambda_arg_itself, arg, optional));
                }
                else if (optional || key) {
                    add(curry(key ? lambda_arg_key_list : lambda_arg_optional_list, [
                        car(arg),
                        analyze(cadr(arg)), // default value might be an expression
                        caddr(arg)
                    ]));
                }
                else if (destructuring) {
                    // and here we recurse to analyze the sub-list
                    add(curry(lambda_arg_destruct, do_lambda_list(arg, destructuring)));
                }
                else throw new Error("Not a destructuring lambda list");
            }
            return ret;
        };
    })();
    // END LAMBDA-LIST parser

    function do_lambda(args, body, destructuring) {
        args = do_lambda_list(args, destructuring);
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
        var spec = operator.get("&SPECIAL");
        // special operator?
        if (!nullp(spec)) {
            return spec(args);
        }
        // macro?
        var mac = $environment.get("macs", operator);
        if (mac) {
            return analyze(apply(mac($environment), args));
        }
        // otherwise function call
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
        args = do_lambda_list(args);
        body = do_sequence(body);
        values = maplist(values, analyze);
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
            var args = func[0], body = func[1], env = func[2];
            if (!nullp(args)) {
                env = env.fork();
                eachlist(args, function(arg){
                    values = arg(env, values);
                });
            }
            return body(env);
        }
    };

    var $level = 0;
    var $environment = _GLOBAL_ENV_;

    return analyze;

}());

function eval(ast, env) {
    return analyze(ast)(env || _GLOBAL_ENV_);
};

exports.write_ast_to_string = write_ast_to_string;
exports.read = read;
exports.eval = eval;
exports.make_string_stream = make_string_stream;
exports.analyze = analyze;

/* -----[ Other functions ]----- */

CL.defun("EVAL", eval);
CL.defun("RPLACA", set_car);
CL.defun("RPLACD", set_cdr);

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

CL.defun("1+", function(a){
    return a + 1;
});

CL.defun("1-", function(a){
    return a - 1;
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

JCLS.defun("PRINT", function(){
    //console.log([].slice.call(arguments).join(", "));
    console.log(write_ast_to_string(array_to_list(arguments)));
    return NIL;
});

// Local Variables:
// js-indent-level:4
// espresso-indent-level:4
// End:
