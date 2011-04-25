var jisp = require("../jisp");

var write_ast_to_string = jisp.write_ast_to_string;
var eval = jisp.eval;
var read = jisp.read;
var make_string_stream = jisp.make_string_stream;

console.log(
        write_ast_to_string(
                eval(
                        read(
                                make_string_stream(
                                        require("fs").readFileSync("test.lisp").toString()
                                )
                        )
                )
        )
);
