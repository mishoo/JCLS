var sys = require("sys");

var _READTABLE_;
var _PACKAGE_;
var _ALL_PACKAGES_ = {};

function Symbol(pack, name) {
        this._package = pack;
        this._name = name;
        this._plist = {};
};

Symbol.prototype = {
        toString: function() {
                return this._package + ":" + this._name;
        }
};

function Package(name) {
        _ALL_PACKAGES_[name.toUpperCase()] = this;
        this._name = name;
        this._symbols = {};
};

Package.prototype = {
        symbol: function(name) {
                return this._symbols[name] ||
                        (this._symbols[name] = new Symbol(this, name));
        },
        discard_symbol: function(name) {
                if (name instanceof Symbol) name = name._name;
                delete this._symbols[name];
        }
};

new Package("CL");
_PACKAGE_ = new Package("CL-USER");

function get_package(name) {
        return _ALL_PACKAGES_[name.toUpperCase()];
};

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
                        switch (ch) {
                            case " ":
                            case "\t":
                            case "\n":
                            case "\x0C":
                            case "\u2028":
                            case "\u2029":
                                return true;
                        }
                });
        };
        function read_while(test) {
                var str = "";
                while (test(peek()))
                        str += next();
                return str;
        };
        var backquote = 0;
        function with_backquote(cont) {
                ++backquote;
                try { return cont() }
                finally { --backquote }
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
        };
};

function HOP(obj, prop) {
        return Object.prototype.hasOwnProperty.call(obj, prop);
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
        var list = [];
        while (true) {
                stream.skip_ws();
                if (stream.peek() == endchar) {
                        stream.next();
                        return list;
                } else {
                        var el = read(stream);
                        if (el != null) list.push(el);
                }
        }
};

function read_standard_list(stream) {
        return [ "LIST", read_delimited_list(stream, ")") ];
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
        return [ "STRING", read_escaped(stream, '"') ];
};

function read_quote(stream) {
        return [ "QUOTE", read(stream) ];
};

function read_backquote(stream) {
        return stream.with_backquote(function(){
                return [ "BACKQUOTE", read(stream) ];
        });
};

function read_comma(stream) {
        if (!stream.in_backquote)
                stream.error("Comma not inside backquote");
        return [ "UNQUOTE", read(stream) ];
};

function read_pipe(stream) {
        return [ "SYMBOL", read_escaped(stream, "|") ];
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
                return [ "NUMBER", parseFloat(str) ];
        }
        str = str.toUpperCase();
        var pack = _PACKAGE_, n = str;
        if (colon) {
                var p = str.substr(0, colon);
                n = str.substr(colon + 1);
                pack = get_package(p);
                if (!pack) stream.error("no package " + p);
        }
        return [ "SYMBOL", pack.symbol(n) ];
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

var str = "(defun f\\(oo(a b c) (cl:format nil \"foo ~a ~a ~a\" \n\
a b (+ c '0.5 0.3.4)))";
console.log( sys.inspect( read(make_string_stream(str)), null, null ) );
//console.log( read(make_string_stream(str)) );
