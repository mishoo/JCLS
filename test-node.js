var fs = require("fs");
var path = require("path");
var JCLS = require("./jcls");

function eval_file(filename) {
        console.log("* Loading", filename);
        var time = new Date().getTime();
        JCLS.eval_string(
                fs.readFileSync(filename).toString(),
                function succeed(ret) {
                        var diff = (new Date().getTime() - time) / 1000
                        console.log("  ==>", JCLS.write_ast_to_string(ret), " in ", diff.toFixed(3), "s.");
                }
        );
};

// load cl/common-lisp.lisp
eval_file(path.join(path.dirname(__filename), "cl/common-lisp.lisp"));

if (process.argv[2])
        eval_file(process.argv[2]);
