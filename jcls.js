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

var _ALL_PACKAGES_ = {};

function pushnew(a, el) {
    if (a.indexOf(el) < 0)
        a.push(el);
    return a;
};

function Symbol(pack, name) {
    this._package = pack;
    this._name = name;
    this._fullname = (this._package ? this._package._name + "::": "#:") + this._name;
    this._plist = {};
    this._special_var = false;
    this._special_op = null;
    this._bindings = [];
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
    special_op: function(func) {
        if (func != null) {
            this._special_op = func;
            return this;
        }
        return this._special_op;
    },
    bind: function(val) {
        this._bindings.unshift(val);
        return this;
    },
    unbind: function() {
        this._bindings.shift();
        return this;
    },
    setq: function(val) {
        return this._bindings[0] = val;
    },
    value: function() {
        return this._bindings[0];
    },
    special_var: function(is) {
        if (is != null) {
            this._special_var = is;
            return this;
        };
        return this._special_var;
    }
};

function as_name(thing) {
    if (thing instanceof Symbol) return thing.name();
    return thing;
}

function as_list(thing) {
    if (listp(thing)) return thing;
    return cons(thing, NIL);
}

function Package(name, options) {
    var self = this;
    options = defaults(options, {
        use: null,
        nicknames: null
    });
    _ALL_PACKAGES_[name.toUpperCase()] = self;
    self._name = name;
    self._symbols = {};
    self._exported = {};
    self._use_list = [];
    if (options.use) {
        options.use.map(function(p){
            self.use_package(p);
        });
        delete options.use;
    }
    if (options.nicknames) {
        options.nicknames.map(function(nick){
            _ALL_PACKAGES_[nick] = self;
        });
    }
};

Package.get = function(name) {
    if (name instanceof Package) return name;
    return _ALL_PACKAGES_[name.toUpperCase()];
};

Package.prototype = {
    find_symbol: function(name, exp) {
        if (exp) {
            if (HOP(this._exported, name))
                return this._exported[name];
        } else {
            if (HOP(this._symbols, name))
                return this._symbols[name];
        }
        for (var i = this._use_list.length; --i >= 0;) {
            var sym = this._use_list[i].find_symbol(name, true);
            if (sym != null) return sym;
        }
    },
    intern: function(name) {
        return this._symbols[name] || (
            this._symbols[name] = new Symbol(this, name)
        );
    },
    find_or_intern: function(name) {
        var sym = this.find_symbol(name);
        return sym || this.intern(name);
    },
    impsym: function(symbol) {
        return this._symbols[symbol._name] = symbol;
    },
    expsym: function(name) {
        return this._exported[name] = this.find_or_intern(name);
    },
    use_package: function(name) {
        pushnew(this._use_list, Package.get(name));
    },
    uses: function(name) {
        return this._use_list.indexOf(name) >= 0;
    },
    defun_: function(name, func) {
        return _GLOBAL_ENV_.defun(this.intern(name), func);
    },
    defun: function(name, func) {
        var ret = this.defun_(name, func);
        this.expsym(name);
        return ret;
    },
    special_: function(name, func) {
        return this.intern(name).special_op(func);
    },
    special: function(name, func) {
        var ret = this.special_(name, func);
        this.expsym(name);
        return ret;
    }
};

var CL = new Package("COMMON-LISP", { nicknames: [ "CL" ] });
var JCLS = new Package("JCLS");
var KEYWORD = new Package("KEYWORD");
var CL_USER = new Package("CL-USER", {
    use: [ "CL" ]
});

var _PACKAGE_ = CL.expsym("*PACKAGE*").special_var(true).bind(CL_USER);

/* -----[ basics ]----- */

var NIL = CL.expsym("NIL"); NIL.toString = itself("NIL")();
var T = CL.expsym("T"); T.toString = itself("T")();
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
function consp(arg) { return arg instanceof Pair };
function listp(arg) { return arg instanceof Pair || nullp(arg) };

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
        var val = this.data[this.full(ns, name)];
        if (val == null) val = name.value();
        return val;
    },
    get_origin: function(name) {
        var s = this;
        while (s) {
            if (HOP(s.data, name)) return s;
            s = s.parent;
        }
    },
    set: function(ns, name, value) {
        if (name.special_var()) return name.setq(value);
        var full = this.full(ns, name);
        var org = this.get_origin(full);
        if (!org) throw new Error("Undeclared variable " + name);
        return org.data[full] = value;
    },
    force: function(ns, name, value) {
        return this.data[this.full(ns, name)] = value;
    },
    defun: function(name, value) {
        return this.force("f", name, value);
    },
    fork: function() {
        return new Environment(this);
    }
};

var _GLOBAL_ENV_ = new Environment();

/* -----[ the reader ]----- */

function lisp_input_stream($SOURCE) {
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

var _READTABLE_ = CL.expsym("*READTABLE*").special_var(true).bind({
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
});

function ignore(stream) {
    stream.next();
};

function ignore_comment(stream) {
    stream.read_while(function(ch){ return ch != "\n" });
};

var DOT = CL.expsym(".");   // XXX: this is a gross hack.
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
    return cons(CL.expsym("QUOTE"), cons(read(stream), NIL));
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
    else if (stream.peek() == ".") {
        stream.next();
        sym = "UNQUOTE-NSPLICE";
    }
    return cons(JCLS.intern(sym), cons(read(stream), NIL));
};

function read_pipe(stream) {
    return _PACKAGE_.value().find_or_intern(read_escaped(stream, "|"));
};

function read_sharp(stream) {
    throw "TBD";
};

function read_symbol(stream, pack) {
    var esc = false, colon = null, internal = false, str = "";
    while (true) {
        var ch = stream.peek();
        if (esc) { str += stream.next(); esc = false; }
        else if (ch == ":") {
            if (colon != null || pack === KEYWORD)
                stream.error("too many colons");
            colon = str.length; str += stream.next();
            if (stream.peek() == ":") {
                internal = true;
                stream.next();
            }
        }
        else if (ch == "\\") { esc = true; stream.next(); }
        else if (HOP(_READTABLE_.value(), ch)) break;
        str += stream.next();
    }
    if (/^[0-9]*\.?[0-9]+$/.test(str)) {
        return parseFloat(str);
    }
    str = str.toUpperCase();
    if (pack == null) pack = _PACKAGE_.value();
    var n = str;
    var sym;
    if (colon) {
        var p = str.substr(0, colon);
        n = str.substr(colon + 1);
        pack = Package.get(p);
        if (!pack) stream.error("No package " + p);
        sym = pack._symbols[n];
        if (!sym) stream.error("Symbol " + n + " not found in package " + pack._name);
        if (!internal && !pack._exported[n]) stream.error("Symbol " + n + " not external in package " + pack._name);
    } else {
        sym = pack.find_or_intern(n);
    }
    return sym;
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
    var reader = _READTABLE_.value()[ch], ret;
    if (reader) {
        stream.next();
        if (reader instanceof Function)
            ret = reader(stream, ch);
        else
            ret = fapply(reader, cons(stream, cons(ch, NIL)));
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
    if (listp(node)) {
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
            if (!listp(node)) {
                ret += " . " + write_ast_to_string(node);
                break;
            }
        }
        ret = "(" + ret + ")";
    }
    else if (symbolp(node)) {
        if (_PACKAGE_.value().find_symbol(node.name()))
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

// The following got quite complicated.  Essentially, it defines a
// function — analyze — which takes an AST and returns a function of
// one argument (an Environment).  This function in turn evaluates the
// expression described by AST in the given environment and returns
// the result.  The technique is described in SICP 4.1.7.
//
var analyze = (function(){
    var LAMBDA  = CL.expsym("LAMBDA")
    , PROGN = CL.expsym("PROGN")
    , DEFVAR = CL.expsym("DEFVAR");

    (function(UNQUOTE, SPLICE, NSPLICE, QUASIQUOTE){
        function Splice(list, destructive) {
            this.list = list;
            this.destructive = destructive;
        };
        function cons_splice(a, b) {
            if (a instanceof Splice) {
                a = a.destructive ? a.list : copy_list(a.list);
                set_cdr(last(a), b);
                return a;
            }
            return cons(a, b);
        };
        JCLS.special("QUASIQUOTE", function(stuff){
            stuff = car(stuff);
            if (!consp(stuff)) return itself(stuff);
            var nest = 0;
            (function walk(node){
                while (!nullp(node)) {
                    if (listp(car(node))) {
                        switch (caar(node)) {
                          case UNQUOTE:
                          case SPLICE:
                          case NSPLICE:
                            if (nest == 0) {
                                set_car(cdr(car(node)), analyze(cadar(node)));
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
                    node = cdr(node);
                }
            }(stuff));
            return function(env) {
                return (function walk(node){
                    if (consp(node)) {
                        switch (car(node)) {
                          case UNQUOTE: return cadr(node)(env);
                          case SPLICE: return new Splice(cadr(node)(env), false);
                          case NSPLICE: return new Splice(cadr(node)(env), true);
                        }
                        return cons_splice(walk(car(node)), walk(cdr(node)));
                    } else {
                        return node;
                    }
                })(stuff);
            };
        });
    }(JCLS.intern("UNQUOTE"),
      JCLS.intern("UNQUOTE-SPLICE"),
      JCLS.intern("UNQUOTE-NSPLICE"),
      JCLS.intern("QUASIQUOTE")));

    CL.special("QUOTE", function(ast){ return itself(car(ast)) });
    CL.special("IF", function(ast){ return do_if(car(ast), cadr(ast), caddr(ast)) });
    CL.special("LAMBDA", function(ast){ return do_lambda(car(ast), cdr(ast)) });
    CL.special("PROGN", function(ast){
        if (nullp(cdr(ast))) return analyze(car(ast));
        else return do_sequence(ast);
    });

    (function(dv){
        CL.special("DEFVAR", dv);
        CL.special("DEFPARAMETER", dv);
    })(function(ast){
        var name = car(ast), value = analyze(cadr(ast));
        return function(env) {
            if (this === DEFVAR && name.special_var())
                return name;
            name.special_var(true);
            return name.bind(value(env));
        };
    });

    CL.special("LET", function(ast){
        var names = [], values = [];
        eachlist(car(ast), function(def){
            names.push(listp(def) ? car(def) : def);
            values.push(analyze(listp(def) ? cadr(def) : NIL));
        });
        var body = do_sequence(cdr(ast));
        return function(env) {
            var val = values.map(function(proc){ return proc(env) });
            env = env.fork();
            var specials = [];
            for (var i = 0; i < names.length; ++i) {
                var name = names[i];
                if (name.special_var()) {
                    name.bind(val[i]);
                    specials.push(name);
                } else {
                    env.force("v", names[i], val[i]);
                }
            }
            try {
                return body(env);
            } finally {
                for (i = specials.length; --i >= 0;)
                    specials[i].unbind();
            }
        };
    });
    CL.special("LET*", function(ast){
        var names = [], values = [];
        eachlist(car(ast), function(def){
            names.push(listp(def) ? car(def) : def);
            values.push(analyze(listp(def) ? cadr(def) : NIL));
        });
        var body = do_sequence(cdr(ast));
        return function(env) {
            env = env.fork();
            var specials = [];
            for (var i = 0; i < names.length; ++i) {
                var name = names[i];
                if (name.special_var()) {
                    name.bind(values[i](env));
                    specials.push(name);
                } else {
                    env.force("v", name, values[i](env));
                }
            }
            try {
                return body(env);
            } finally {
                for (i = specials.length; --i >= 0;)
                    specials[i].unbind();
            }
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
            var val = values.map(function(proc){ return proc(env) });
            env = env.fork();
            for (var i = 0; i < names.length; ++i) {
                env.force("f", names[i], val[i]);
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
                env.force("f", names[i], values[i](env));
            }
            return body(env);
        };
    });
    CL.special("PSETQ", function(defs){
        var names = [], values = [];
        while (!nullp(defs)) {
            names.push(car(defs));
            values.push(analyze(cadr(defs)));
            defs = cddr(defs);
        }
        return function(env) {
            var val = values.map(function(proc){ return proc(env) });
            for (var ret = NIL, i = 0; i < names.length; ++i)
                env.set("v", names[i], ret = val[i]);
            return ret;
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
                env.set("v", names[i], ret = values[i](env));
            return ret;
        };
    });
    CL.special("DEFUN", function(ast){
        var name = car(ast), func = do_lambda(cadr(ast), cddr(ast));
        return function(env) {
            return _GLOBAL_ENV_.force("f", name, func(env));
        };
    });
    CL.special("FUNCTION", function(ast){
        var name = car(ast);
        return function(env) {
            return env.get("f", name);
        };
    });

    CL.special("DEFMACRO", function(ast){
        var name = car(ast), func = do_lambda(cadr(ast), cddr(ast), true);
        _GLOBAL_ENV_.force("m", name, func);
        return itself(NIL);
    });
    CL.special("DESTRUCTURING-BIND", function(ast){
        var names = do_lambda_list(car(ast), true);
        var expr = analyze(cadr(ast));
        var body = do_sequence(cddr(ast));
        return function(env) {
            var values = expr(env);
            env = env.fork();
            eachlist(names, function(arg){
                values = arg(env, values);
            });
            return body(env);
        };
    });

    (function(Catch){
        CL.special("CATCH", function(ast){
            var name = analyze(car(ast));
            var body = do_sequence(cdr(ast));
            return function(env) {
                var symbol = name(env);
                try {
                    return body(env);
                }
                catch(ex) {
                    if (ex instanceof Catch && ex.symbol === symbol)
                        return ex.value;
                    else throw ex;
                }
            };
        });
        CL.special("THROW", function(ast){
            var tag = analyze(car(ast));
            var val = analyze(cadr(ast));
            return function(env) {
                throw new Catch(tag(env), val(env));
            };
        });
        CL.special("IGNORE-ERRORS", function(ast){
            var body = do_sequence(ast);
            return function(env) {
                try {
                    return body(env);
                } catch(ex) {
                    if (!(ex instanceof Catch))
                        return NIL;
                    throw ex;
                }
            };
        });
        CL.special("UNWIND-PROTECT", function(ast){
            var expr = analyze(car(ast));
            var body = do_sequence(cdr(ast));
            return function(env) {
                try {
                    return expr(env);
                } finally {
                    body(env);
                }
            };
        });
    }(function(symbol, value) {
        this.symbol = symbol;
        this.value = value;
    }));

    function analyze(expr) {
        var tmp;
        if (symbolp(expr)) switch (expr) {
          case NIL:
          case T:
            return itself(expr);
          default:
            if (expr._package === KEYWORD) return itself(expr);
            else return function(env) {
                return env.get("v", expr);
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
        var $REST = CL.expsym("&REST");
        var $BODY = CL.expsym("&BODY");
        var $KEY = CL.expsym("&KEY");
        var $OPTIONAL = CL.expsym("&OPTIONAL");

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
            env.force("v", name, val || def(env));
            if (!nullp(arg_p)) env.force("v", arg_p, val ? T : NIL);
            return values;
        };

        function lambda_arg_key(arg, env, values) {
            env.force("v", arg, find(arg, values) || NIL);
            return values;
        };

        function lambda_arg_optional_list(arg, env, values) {
            var name = arg[0], def = arg[1], arg_p = arg[2];
            if (nullp(values)) {
                env.force("v", name, def(env));
                if (!nullp(arg_p)) env.force("v", arg_p, NIL);
                return NIL;
            } else {
                env.force("v", name, car(values));
                if (!nullp(arg_p)) env.force("v", arg_p, T);
                return cdr(values);
            }
        };

        function lambda_arg_itself(name, optional, env, values) {
            if (nullp(values)) {
                if (!optional) throw new Error(name + " is a required argument");
                return env.force("v", name, NIL);
            } else {
                env.force("v", name, car(values));
                return cdr(values);
            }
        };

        function lambda_arg_rest(name, env, values) {
            env.force("v", name, values);
            return NIL;
        };

        function lambda_arg_destruct(args, env, values) {
            if (!listp(car(values)))
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
            if (nullp(env)) env = _GLOBAL_ENV_;
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
        var spec = operator.special_op();
        // special operator?
        if (spec) {
            return spec.call(operator, args);
        }
        // macro?
        var mac = $environment.get("m", operator);
        if (mac) {
            return analyze(fapply(mac($environment), args));
        }
        // otherwise function call
        args = maplist(args, analyze);
        return function(env) {
            var func = env.get("f", operator);
            if (!func)
                throw new Error("Undefined function: " + write_ast_to_string(operator));
            return fapply(func, maplist(args, function(proc){
                return proc(env);
            }));
        };
    };

    function do_inline_call(args, body, values) {
        args = do_lambda_list(args);
        body = do_sequence(body);
        values = maplist(values, analyze);
        return function(env) {
            return fapply([ args, body, env ], maplist(values, function(proc){
                return proc(env);
            }));
        };
    };

    var $level = 0;
    var $environment = _GLOBAL_ENV_;

    return analyze;

}());

function itself(el) {
    return function(){ return el };
};

function fapply(func, values) {
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

function eval(ast, env) {
    return analyze(ast)(env || _GLOBAL_ENV_);
};

function eval_string(input) {
    var ret = NIL, expr, EOF = {};
    input = lisp_input_stream(input);
    while ((expr = read(input, false, EOF)) !== EOF) {
        ret = eval(expr);
    }
    return ret;
};

exports.write_ast_to_string = write_ast_to_string;
exports.read = read;
exports.eval = eval;
exports.eval_string = eval_string;
exports.lisp_input_stream = lisp_input_stream;
exports.analyze = analyze;

/* -----[ Other functions ]----- */

// auto-generate c[ad]+r combinations
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
CL.defun("LISTP", function(expr){
    return listp(expr) ? expr : NIL;
});
CL.defun("LAST", last);
CL.defun("LIST", function(){ return array_to_list(arguments) });
CL.defun("EVAL", eval);
CL.defun("RPLACA", set_car);
CL.defun("RPLACD", set_cdr);
(function(counter){
    CL.defun("GENSYM", function(name){
        return new Symbol(null, (name || "G") + (++counter));
    });
}(0));

CL.defun("FUNCALL", function(){
    var list = array_to_list(arguments);
    var func = car(list), args = cdr(list);
    if (symbolp(func))
        func = _GLOBAL_ENV_.get("f", func);
    return fapply(func, args);
});

CL.defun("APPLY", function(func) {
    if (symbolp(func))
        func = _GLOBAL_ENV_.get("f", func);
    var list = NIL, p, len = arguments.length - 1, last = arguments[len];
    if (!listp(last))
        throw new Error("Last argument to apply must be a list");
    for (var i = 1; i < len; ++i) {
        var cell = cons(arguments[i], NIL);
        if (p) set_cdr(p, cell);
        else list = cell;
        p = cell;
    }
    if (p) set_cdr(p, last);
    else list = last;
    return fapply(func, list);
});

CL.defun("MACRO-FUNCTION", function(name, env){
    return (nullp(env) ? _GLOBAL_ENV_ : env).get("m", name) || NIL;
});

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

/* -----[ utilities on which we'll build further functions in :CL ]----- */

JCLS.defun("IMPORT", function(syms, pack){
    if (pack == null) pack = _PACKAGE_.value();
    maplist(as_list(syms), function(s){
        // XXX-E: errors should be signaled
        pack.impsym(s);
    });
    return T;
});

JCLS.defun("EXPORT", function(syms, pack){
    if (pack == null) pack = _PACKAGE_.value();
    maplist(as_list(syms), function(s){
        // XXX-E: errors should be signaled
        pack.expsym(s.name());
    });
    return T;
});

JCLS.defun("MAKE-PACKAGE", function(name, use, nicknames){
    var opts = {};
    if (use) opts.use = list_to_array(use).map(as_name);
    if (nicknames) opts.nicknames = list_to_array(nicknames).map(as_name);
    return new Package(as_name(name), opts);
});

JCLS.defun("FIND-PACKAGE", function(name){
    return Package.get(as_name(name));
});

JCLS.defun("USE-PACKAGE", function(names, pack){
    if (pack == null) pack = _PACKAGE_.value();
    maplist(as_list(names), function(name){
        pack.use_package(as_name(name));
    });
    return T;
});

JCLS.defun("PRINT", function(){
    //console.log([].slice.call(arguments).join(", "));
    console.log([].slice.call(arguments).map(write_ast_to_string).join(" "));
    return NIL;
});

JCLS.defun("NATIVE", function(){
    var g = global;
    for (var i = 0; i < arguments.length; ++i)
        g = g[arguments[i]];
    return g;
});

JCLS.defun("CALL-NATIVE", function(path, obj, args){
    var g = global;
    var f;
    if (listp(path)) {
        while (!nullp(cdr(path))) {
            g = g[car(path)];
            path = cdr(path);
        }
        f = g[car(path)];
    } else {
        f = obj[path];
    }
    return f.apply(nullp(obj) ? g : obj, list_to_array(args));
});

JCLS.defun("TO-NATIVE", function(func){
    return function() { return fapply(func, array_to_list(arguments)) };
});

//* Reader utils

JCLS.defun("READ-DELIMITED-LIST", function(endchar, stream) {
    return read_delimited_list(stream, endchar);
});

JCLS.defun("GET-MACRO-CHARACTER", function(ch, readtable){
    if (arguments.length == 1) readtable = _READTABLE_.value();
    return readtable[ch];
});

JCLS.defun("SET-MACRO-CHARACTER", function(ch, func, readtable){
    if (arguments.length == 2) readtable = _READTABLE_.value();
    return readtable[ch] = func;
});

JCLS.defun("READ", function(stream, eof_error, eof_value){
    if (arguments.length < 2) eof_error = true;
    if (arguments.length < 3) eof_value = NIL;
    return read(stream, eof_error, eof_value);
});

JCLS.defun("READ-FROM-STRING", function(string){
    return read(lisp_input_stream(string));
});

// Local Variables:
// js-indent-level:4
// espresso-indent-level:4
// End:
