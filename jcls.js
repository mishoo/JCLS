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
    defun2: function(name, func) {
        var ret = this.defun_(name, func);
        this.expsym(name);
        return ret;
    },
    defun: function(name, func) {
        return this.defun2(name, function(){
            return this.succeed(func.apply(this, arguments), this.fail);
        });
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
var JCLS = new Package("JCLS", { use: [ "CL" ]});
var KEYWORD = new Package("KEYWORD");
var CL_USER = new Package("CL-USER", {
    use: [ "CL" ]
});

var _PACKAGE_ = CL.expsym("*PACKAGE*").special_var(true).bind(CL_USER);

/* -----[ basics ]----- */

var NIL = CL.expsym("NIL"); NIL.toString = function(){ return "NIL" };
var T = CL.expsym("T"); T.toString = function(){ return "T" };
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
        var val = func(car(list));
        if (val != null) {
            var tmp = cons(val, NIL);
            if (p) set_cdr(p, tmp);
            else ret = tmp;
            p = tmp;
        }
        list = cdr(list);
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
        else {
            // XXX: HACK, BIG HACK!  The whole reader should be implemented in continuation-passing style :-\
            fapply(reader, cons(stream, cons(ch, NIL)), function(val){ ret = val });
        }
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
function eq(x, y) { return x === y ? T : NIL };

function NextCont(cont) { this.cont = cont };
function NEXT() { return new NextCont(curry.apply(null, arguments)) };
function trampoline_apply(f, args, obj) {
    while (true) {
        f = f.apply(obj, args);
        if (f instanceof NextCont) f = f.cont;
        else return f;
    }
};

// The following got quite complicated.  Essentially, it defines a
// function — analyze — which takes an AST and returns a function of
// one argument (an Environment).  This function in turn evaluates the
// expression described by AST in the given environment and returns
// the result.  The technique is described in SICP 4.1.7.
//
var analyze = (function(){
    var LAMBDA  = CL.expsym("LAMBDA")
    , PROGN = CL.expsym("PROGN");

    // quasiquotation algorithm described by Alan Bawden in his paper "Quasiquotation in Lisp".
    (function(UNQUOTE, SPLICE, NSPLICE, QUASIQUOTE, APPEND, QUOTE, LIST){
        function qq_expand(x) {
            if (consp(x)) switch (car(x)) {
              case UNQUOTE: return cadr(x);
              case NSPLICE:
              case SPLICE: throw new Error("Illegal splice");
              case QUASIQUOTE: return qq_expand(qq_expand(cadr(x)));
              default:
                return cons(APPEND,
                            cons(qq_expand_list(car(x)),
                                 cons(qq_expand(cdr(x)),
                                      NIL)));
            }
            return cons(QUOTE, cons(x, NIL));
        };
        function qq_expand_list(x) {
            if (consp(x)) switch (car(x)) {
              case UNQUOTE: return cons(LIST, cons(cadr(x), NIL));
              case NSPLICE:
              case SPLICE: return cadr(x);
              case QUASIQUOTE: return qq_expand_list(qq_expand(cadr(x)));
              default:
                return cons(LIST,
                            cons(cons(APPEND,
                                      cons(qq_expand_list(car(x)),
                                           cons(qq_expand(cdr(x)),
                                                NIL))),
                                 NIL));
            }
            return cons(LIST, cons(cons(QUOTE, cons(x, NIL)), NIL));
        };
        JCLS.special("QUASIQUOTE", function(ast){
            var expr = car(ast);
            if (atom(expr)) return itself(expr);
            return function(env, succeed, fail) {
                return NEXT(analyze(qq_expand(expr)), env, function(val, fail2){
                    return NEXT(succeed, val, fail2);
                }, fail);
            };
        });
    }(JCLS.intern("UNQUOTE"),
      JCLS.intern("UNQUOTE-SPLICE"),
      JCLS.intern("UNQUOTE-NSPLICE"),
      JCLS.intern("QUASIQUOTE"),
      CL.expsym("APPEND"),
      CL.expsym("QUOTE"),
      CL.expsym("LIST")));

    CL.special("QUOTE", function(ast){ return itself(car(ast)) });
    CL.special("IF", function(ast){ return do_if(car(ast), cadr(ast), caddr(ast)) });
    CL.special("LAMBDA", function(ast){ return do_lambda(car(ast), cdr(ast)) });
    CL.special("PROGN", function(ast){
        if (nullp(cdr(ast))) return analyze(car(ast));
        else return do_sequence(ast);
    });

    CL.special("FUNCTION", function(ast){
        var name = car(ast);
        return function(env, succeed, fail) {
            return NEXT(succeed, env.get("f", name), fail);
        };
    });

    CL.special("DEFMACRO", function(ast){
        var name = car(ast), func = do_lambda(cadr(ast), cddr(ast), true);
        _GLOBAL_ENV_.force("m", name, func);
        return itself(NIL);
    });

    CL.special("MACROEXPAND-1", function(ast){
        var tree = analyze(car(ast)), menv = analyze(cadr(ast));
        return function(env, succeed, fail0) {
            return NEXT(menv, env, function(menv, fail1){
                return NEXT(tree, env, function(tree, fail2){
                    var m = env.get("m", car(tree));
                    if (!m) return succeed(tree);
                    return NEXT(
                        m,
                        nullp(menv) ? _GLOBAL_ENV_ : menv,
                        function(func){
                            return fapply(func, cdr(tree), succeed, fail2);
                        },
                        fail2
                    );
                }, fail1);
            }, fail0);
        };
    });                         // my brain's on fire.

    // to manipulate the environment
    {
        JCLS.special("FORK-ENVIRONMENT", function(body){
            body = do_sequence(body);
            return function(env, succeed, fail) {
                return NEXT(body, env.fork(), succeed, fail);
            };
        });

        JCLS.special("SET!", function(ast){
            var name = car(ast);
            var kind = cadr(ast);
            var value = analyze(caddr(ast));
            return function(env, succeed, fail) {
                return NEXT(value, env, function(value, fail2){
                    env.set(kind, name, value);
                    return NEXT(succeed, value, fail2);
                }, fail);
            };
        });

        JCLS.special("DEF!", function(ast){
            var name = car(ast);
            var kind = cadr(ast);
            var value = analyze(caddr(ast));
            var global = cadddr(ast); // only T or NIL
            if (symbolp(name)) return function(env, succeed, fail) {
                return NEXT(value, env, function(value, fail2){
                    (nullp(global) ? env : _GLOBAL_ENV_).force(kind, name, value);
                    return NEXT(succeed, value, fail2);
                }, fail);
            };
            else return name = analyze(name), function(env, succeed, fail) {
                return NEXT(name, env, function(name, fail2){
                    return NEXT(value, env, function(value, fail3){
                        (nullp(global) ? env : _GLOBAL_ENV_).force(kind, name, value);
                        return NEXT(succeed, value, fail3);
                    }, fail2);
                }, fail);
            };
        });

        JCLS.special("SPECIAL!", function(ast){
            var name = car(ast);
            return function(env, succeed, fail) {
                var spec = !nullp(cadr(ast));
                name.special_var(spec);
                return NEXT(succeed, spec);
            };
        });

        JCLS.special("SPECIAL?", function(ast){
            var name = car(ast);
            return function(env, succeed, fail) {
                NEXT(succeed, name.special_var());
            };
        });
    }

    CL.special("UNWIND-PROTECT", function(ast){
        var expr = analyze(car(ast)), cleanup = do_sequence(cdr(ast));
        return function(env, succeed, fail){
            return NEXT(expr, env, function(expr, fail2){
                return NEXT(cleanup, env, curry(succeed, expr, fail2), fail2);
            }, function(expr){
                return NEXT(cleanup, env, fail2);
            });
        };
    });

    CL.special("TAGBODY", function(list){
        var tags = [];
        var ct = null;
        eachlist(list, function(node){
            if (atom(node)) {
                if (ct) {
                    if (tags.length == 0) tags.push(NIL);
                    tags.push(do_sequence(array_to_list(ct)));
                }
                tags.push(node);
                ct = [];
            } else {
                if (!ct) ct = [];
                ct.push(node);
            }
        });
        if (tags.length == 0) tags[0] = NIL;
        tags.push(do_sequence(array_to_list(ct)));
        var body = (function loop(i){
            if (i == tags.length - 1) return tags[i];
            return tags[i] = seq(tags[i], loop(i + 2));
        })(1);
        return function(env, succeed, fail) {
            env = env.fork();
            for (var i = 0; i < tags.length;) {
                env.force("t", tags[i++], [ tags[i++], succeed ]);
            }
            return NEXT(body, env, succeed, fail);
        };
    });

    CL.special("GO", function(tag){
        tag = car(tag);
        if (!symbolp(tag)) throw new Error("Expecting a symbol for GO");
        return function(env, succeed, fail) {
            var cont = env.get("t", tag);
            if (!cont) throw new Error("GO tag not found " + tag);
            return NEXT(cont[0], env, function(){
                // A "successful" GO means that we reached the end of
                // the tagbody statements.  We can't call succeed
                // here--if we did, evaluation will continue with the
                // line after the current GO statement.  What we
                // should do instead is call the tagbody's success
                // continuation.
                return NEXT(cont[1], NIL, fail);
            }, fail);
        };
    });

    JCLS.special("CALL/CC", function(ast){
        var func = analyze(car(ast));
        return function(env, succeed, fail){
            return NEXT(func, env, function(func, fail2){
                return NEXT(fapply, func, cons(function(val){
                    return NEXT(succeed, val, fail);
                }, NIL), function(){}, fail2);
            }, fail);
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
            else return function(env, succeed, fail) {
                return NEXT(succeed, env.get("v", expr), fail);
            };
        }
        else if (numberp(expr) || stringp(expr)) return itself(expr);
        else if (atom(tmp = car(expr))) {
            return do_application(tmp, cdr(expr));
        }
        else if (caar(expr) === LAMBDA) {
            return do_inline_call(cadar(expr), cddar(expr), cdr(expr));
        }
    };

    function itself(el) {
        return function(env, succeed, fail){ return succeed(el, fail) };
    };

    function do_if(condition, consequent, alternative) {
        condition = analyze(condition);
        consequent = analyze(consequent);
        alternative = analyze(alternative);
        return function(env, succeed, fail) {
            return NEXT(
                condition,
                env,
                function(val, fail2) {
                    if (nullp(val)) {
                        return NEXT(alternative, env, succeed, fail2);
                    } else {
                        return NEXT(consequent, env, succeed, fail2);
                    }
                },
                fail
            );
        };
    };

    // LAMBDA-LIST parser
    var do_lambda_list = (function($REST, $BODY, $KEY, $OPTIONAL){
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

        function lambda_arg_key_list(arg, env, values, succeed, fail) {
            var name = arg[0], def = arg[1], arg_p = arg[2];
            var val = find(name, values);
            if (val) {
                env.force("v", name, val);
                if (!nullp(arg_p)) env.force("v", arg_p, T);
                return NEXT(succeed, values, fail);
            } else {
                return NEXT(def, env, function(val, fail2) {
                    env.force("v", name, val);
                    if (!nullp(arg_p)) env.force("v", arg_p, NIL);
                    return NEXT(succeed, values, fail2);
                }, fail);
            }
        };

        function lambda_arg_key(arg, env, values, succeed, fail) {
            env.force("v", arg, find(arg, values) || NIL);
            return NEXT(succeed, values, fail);
        };

        function lambda_arg_optional_list(arg, env, values, succeed, fail) {
            var name = arg[0], def = arg[1], arg_p = arg[2];
            if (nullp(values)) {
                return NEXT(def, env, function(val, fail2){
                    env.force("v", name, val);
                    if (!nullp(arg_p)) env.force("v", arg_p, NIL);
                    return NEXT(succeed, NIL, fail2);
                }, fail);
            } else {
                env.force("v", name, car(values));
                if (!nullp(arg_p)) env.force("v", arg_p, T);
                return NEXT(succeed, cdr(values), fail);
            }
        };

        function lambda_arg_itself(name, optional, env, values, succeed, fail) {
            if (nullp(values)) {
                if (!optional) throw new Error(name + " is a required argument");
                env.force("v", name, NIL);
                return NEXT(succeed, NIL, fail);
            } else {
                env.force("v", name, car(values));
                return NEXT(succeed, cdr(values), fail);
            }
        };

        function lambda_arg_rest(name, env, values, succeed, fail) {
            env.force("v", name, values);
            return NEXT(succeed, NIL, fail);
        };

        function lambda_arg_destruct(args, env, values, succeed, fail) {
            if (!listp(car(values)))
                throw new Error("Expecting a list");
            return NEXT(inject_lambda_args, args, env, car(values), function(result, fail2){
                return NEXT(succeed, cdr(values), fail2);
            }, fail);
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
    })(CL.expsym("&REST"),
       CL.expsym("&BODY"),
       CL.expsym("&KEY"),
       CL.expsym("&OPTIONAL"));
    // END LAMBDA-LIST parser

    function do_lambda(args, body, destructuring) {
        args = do_lambda_list(args, destructuring);
        body = do_sequence(body);
        return function(env, succeed, fail) {
            if (nullp(env)) env = _GLOBAL_ENV_;
            return NEXT(succeed, [ args, body, env ], fail);
        };
    };

    function seq(a, b) {
        return function(env, succeed, fail) {
            return NEXT(a, env, function(a, fail2){
                return NEXT(b, env, succeed, fail2);
            }, fail);
        };
    };

    function do_sequence(list) {
        list = maplist(list, analyze);
        return (function loop(first, rest) {
            if (nullp(rest)) return first;
            else return loop(seq(first, car(rest)), cdr(rest));
        })(car(list), cdr(list));
    };

    function get_args(aprocs, env, succeed, fail) {
        if (nullp(aprocs)) return NEXT(succeed, NIL, fail);
        return NEXT(car(aprocs), env, function(arg, fail2) {
            return NEXT(get_args, cdr(aprocs), env, function(args, fail3) {
                return NEXT(succeed, cons(arg, args), fail3);
            }, fail2);
        }, fail);
    };

    function do_application(operator, args) {
        var spec = operator.special_op();
        // special operator?
        if (spec) {
            return spec.call(operator, args);
        }
        // macro?
        var mac = _GLOBAL_ENV_.get("m", operator);
        if (mac) {
            // XXX: HACK, BIG HACK!  The whole reader should be implemented in continuation-passing style :-\
            var ret;
            trampoline_apply(mac, [
                _GLOBAL_ENV_,
                function(func){
                    return fapply(func, args, function(val){
                        return ret = analyze(val);
                    });
                }
            ]);
            // XXX: this is horrible.
            return ret;
        }
        // otherwise function call
        args = maplist(args, analyze);
        return function(env, succeed, fail) {
            var func = env.get("f", operator);
            if (!func)
                throw new Error("Undefined function: " + write_ast_to_string(operator));
            return NEXT(get_args, args, env, function(args, fail2){
                return NEXT(fapply, func, args, succeed, fail2);
            }, fail);
        };
    };

    function do_inline_call(args, body, values) {
        args = do_lambda_list(args);
        body = do_sequence(body);
        values = maplist(values, analyze);
        return function(env, succeed, fail){
            return NEXT(get_args, values, env, function(values, fail2){
                return NEXT(fapply, [ args, body, env ], values, succeed, fail);
            }, fail);
        };
    };

    return analyze;

}());

function inject_lambda_args(aprocs, env, values, succeed, fail) {
    if (nullp(aprocs)) return NEXT(succeed, NIL, fail);
    return NEXT(car(aprocs), env, values, function(next_values, fail2){
        return NEXT(inject_lambda_args, cdr(aprocs), env, next_values, function(results, fail3){
            return succeed(results, fail3);
        }, fail2);
    }, fail);
};

function fapply(func, values, succeed, fail) {
    if (func instanceof Function) {
        return func.apply({
            succeed: succeed,
            fail: fail
        }, list_to_array(values));
    }
    else if (func instanceof Array) {
        var args = func[0], body = func[1], env = func[2].fork();
        return inject_lambda_args(
            args,
            env,
            values,
            curry(NEXT, body, env, succeed, fail),
            fail
        );
    }
};

function eval(ast, env, succeed, fail) {
    return trampoline_apply(analyze(ast), [
        env || _GLOBAL_ENV_,
        succeed,
        fail
    ]);
};

function eval_string(input, succeed, fail) {
    var EOF = {};
    input = lisp_input_stream(input);
    trampoline_apply(function goon(ret, fail) {
        var expr = read(input, false, EOF);
        if (expr === EOF) succeed(ret, fail);
        else eval(expr, null, goon, fail);
    }, [ NIL, fail ]);
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
CL.defun("CONSP", function(expr){ return consp(expr) ? T : NIL });
CL.defun("LISTP", function(expr){ return listp(expr) ? T : NIL });
CL.defun("LAST", last);
CL.defun("LIST", function(){ return array_to_list(arguments) });
CL.defun("EVAL", eval);
CL.defun("RPLACA", set_car);
CL.defun("RPLACD", set_cdr);

CL.defun("COPY-LIST", copy_list);

CL.defun("APPEND", function(a, b){
    if (!a) return NIL;
    if (!b) return a;
    var ret = NIL, p, n = arguments.length - 1;
    for (var i = 0; i < n; ++i) {
        var list = arguments[i];
        if (!listp(list)) {
            break;
        }
        else while (!nullp(list)) {
            var cell = cons(car(list), NIL);
            if (p) set_cdr(p, cell);
            else ret = cell;
            p = cell;
            list = cdr(list);
        }
    }
    if (p)
        set_cdr(p, arguments[i]);
    return ret;
});

CL.defun("NCONC", function(){
    var prev = arguments[0], ret = prev || NIL;
    for (var i = 1; i < arguments.length; ++i) {
        var current = arguments[i];
        if (!nullp(current)) {
            set_cdr(last(prev), arguments[i]);
            prev = arguments[i];
        }
    }
    return ret;
});

(function(counter){
    CL.defun("GENSYM", function(name){
        return new Symbol(null, (name || "G") + (++counter));
    });
}(0));

CL.defun2("FUNCALL", function(){
    var list = array_to_list(arguments);
    var func = car(list), args = cdr(list);
    if (symbolp(func))
        func = _GLOBAL_ENV_.get("f", func);
    return NEXT(fapply, func, args, this.succeed, this.fail);
});

CL.defun2("APPLY", function(func) {
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
    return NEXT(fapply, func, list, this.succeed, this.fail);
});

CL.defun("MACRO-FUNCTION", function(name, env){
    return (nullp(env) ? _GLOBAL_ENV_ : env).get("m", name) || NIL;
});

// XXX: temporary
CL.defun("ERROR", function(text){
    throw new Error(text);
});

/* -----[ Arithmetic ]----- */

CL.defun("+", function(){
    return [].slice.call(arguments).reduce(function(a, b){ return a + b }, 0);
});

CL.defun("-", function(a){
    if (arguments.length == 1) return -a;
    return [].slice.call(arguments, 1).reduce(function(a, b){ return a - b }, arguments[0]);
});

CL.defun("*", function(){
    return [].slice.call(arguments).reduce(function(a, b){ return a * b }, 1);
});

CL.defun("/", function(){
    if (arguments.length == 1) return 1/a;
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

CL.defun("/=", function(a, b){
    if (arguments.length == 2) return a !== b ? T : NIL;
    var seen = {}, n;
    for (var i = arguments.length; --i >= 0;) {
        n = arguments[i];
        if (HOP(seen, n)) return NIL;
        seen[n] = 1;
    }
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

JCLS.defun("APPLY-NATIVE", function(func, obj, args){
    return func.apply(nullp(obj) ? global : obj, list_to_array(args));
});

JCLS.defun("MAKE-NATIVE-FUNCTION", function(func){
    return function() { return fapply(func, array_to_list(arguments), this.succeed, this.fail) };
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
