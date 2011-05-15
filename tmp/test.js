var fs = require("fs");
var jcls = require("../jcls");

var write_ast_to_string = jcls.write_ast_to_string;
var eval = jcls.eval;
var read = jcls.read;
var lisp_input_stream = jcls.lisp_input_stream;
var eval_string = jcls.eval_string;

eval_string(fs.readFileSync("test.lisp").toString());
