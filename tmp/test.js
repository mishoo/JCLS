var jcls = require("../jcls");

var write_ast_to_string = jcls.write_ast_to_string;
var eval = jcls.eval;
var read = jcls.read;
var make_string_stream = jcls.make_string_stream;

var ast = read(
        make_string_stream(
                require("fs").readFileSync("test.lisp").toString()
        )
);

console.log(write_ast_to_string(ast));

console.log("-----")

console.log(
        "==> " +
        write_ast_to_string(
                eval(
                        ast
                )
        )
);
