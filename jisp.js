function with_input_from_string($SOURCE) {
        var $line = 0, $col = 0, $pos = 0;
        function peek() { return $SOURCE.charAt($pos) };
        function next() {
                var ch = $SOURCE.charAt($pos++);
                if (ch == "\n") $col = 0, ++$line;
                else ++$col;
                return ch;
        };
        return {
                get line() { return $line },
                get col() { return $col },
                get pos() { return $pos },
                get peek() { return peek() },
                get next() { return next() },
                get rest() { return $SOURCE.substr($pos) },

                error: function(text) {
                        throw new Error(text + "\nLine: " + $line
                                        + ", Col: " + $col
                                        + ", Pos: " + $pos);
                },

                read_while: function(test) {
                        var str = "";
                        while (test(peek()))
                                str += next();
                        return str;
                }
        };
};

function make_reader() {

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
                "(": true,
                ")": true,
                '"': true,
                "'": true,
                "`": true,
                ",": true,
                "|": true,
                "\\": true,
                ":": true,
                "#": true
        };

        function is_special(ch) {
                return HOP(SPECIAL_CHARS, ch);
        };

        function tokenizer($SOURCE) {
                var stream = with_input_from_string($SOURCE);
                var token;

                function start_token() {
                        return token = { line: stream.line,
                                         col: stream.col,
                                         pos: stream.pos };
                };

                function as(type, val) {
                        token.type = type;
                        token.val = val;
                        return token;
                };

                function error(text) {
                        stream.error(text);
                };

                function next() { return stream.next };

                function skip(ch) {
                        if (stream.peek != ch) error("Expected '" + ch + "'");
                        next();
                };

                function skip_ws() {
                        stream.read_while(is_whitespace);
                };

                function read_string() {
                        skip('"');
                        var str = "", esc = false;
                        while (true) {
                                var ch = next();
                                if (esc) {
                                        str += ch;
                                        esc = false;
                                } else if (ch == "\\") {
                                        esc = true;
                                } else if (ch == '"') {
                                        return str;
                                } else if (ch == null) {
                                        error("Unterminated string");
                                } else {
                                        str += ch;
                                }
                        }
                };

                function read_comment() {
                        skip(";");
                        return stream.read_while(function(ch){
                                return ch != "\n";
                        });
                };

                function read_token() {
                        var esc = false, tok = "", ch;
                        while (ch = stream.peek) {
                                if (esc) tok += next(), esc = false;
                                else if (ch == "\\") next(), esc = true;
                                else if (is_whitespace(ch) || is_special(ch)) break;
                                else tok += next();
                        }
                        return tok;
                };

                var parens = [];
                return function() {
                        skip_ws();
                        start_token();
                        var ch = stream.peek;
                        if (!ch) return as("eof");
                        switch (ch) {
                            case '"': return as("string", read_string());
                            case ";": return as("comment", read_comment());
                            case "'": return as("quote", next());
                            case "`": return as("backquote", next());
                            case ",": return as("comma", next());
                            case "(": return as("lparen", next());
                            case ")": return as("rparen", next());
                            case ":": return as("colon", next());
                        }
                        var token = read_token();
                        if (/^[0-9]*\.?[0-9]+$/.test(token))
                                return as("number", parseFloat(token));
                        return as("token", token);
                };
        };

        return tokenizer;

};


var str = "(defun f\\(oo(a b c) (cl:format nil \"foo ~a ~a ~a\" \n\
a b (+ c 0.5 0.3.4)))";
var token = make_reader()(str);
while (true) {
        var t = token();
        if (t.type == "eof") break;
        console.log(t);
}
