var fs = require("fs");
var path = require("path");
var JCLS = require("./jcls");
var sys = require("sys");

var file = process.argv[2];
var code = fs.readFileSync(file, "utf8");
var input = JCLS.lisp_input_stream(code);
var EOF = {};
var expressions = [];

JCLS.trampoline_apply(function next(ret, fail){
        var expr = JCLS.read(input, false, EOF);
        if (expr === EOF) return true;
        expressions.push(expr);
        JCLS.eval(expr, null, next, fail);
}, [ false, function(){} ]);

expressions.forEach(function(expr){
        console.log(JCLS.write_ast_to_string(expr));
});
