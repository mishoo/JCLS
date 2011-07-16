global.require = require;

var fs = require("fs");
var path = require("path");
var JCLS = require("./jcls");
var sys = require("sys");

function eval_file(filename) {
        sys.debug("* Loading " + filename);
        var time = new Date().getTime();
        JCLS.eval_string(
                fs.readFileSync(filename).toString(),
                function succeed(ret) {
                        var diff = (new Date().getTime() - time) / 1000
                        sys.debug("  ==> " + JCLS.write_ast_to_string(ret) + " in " + diff.toFixed(3) + "s.\n");
                }
        );
};

// load cl/common-lisp.lisp
eval_file(path.join(path.dirname(__filename), "cl/common-lisp.lisp"));
eval_file(path.join(path.dirname(__filename), "cl/javascript.lisp"));
JCLS.eval_string("(in-package :cl-user)", function(){
        if (process.argv[2])
                eval_file(process.argv[2]);
}, function(){});
