var fs = require("fs");
var jcls = require("../jcls");

var write_ast_to_string = jcls.write_ast_to_string;
var eval = jcls.eval;
var read = jcls.read;
var lisp_input_stream = jcls.lisp_input_stream;
var eval_string = jcls.eval_string;

//console.log(
        eval_string(
                fs.readFileSync(process.argv[2]).toString(),
                function succeed(val) {
                        console.log("==> " + val);
                },
                function fail() {
                        console.log("ERROR");
                }
        )
//);
