(function(){

        function load_eval_lisp_seq(files, next) {
                var n = files.length, a = [], i = 0;
                while (i < n) {
                        load(files[i], (function(i){
                                return function(code) {
                                        a[i] = code;
                                        if (--n == 0) {
                                                a.foreach(function(code) {
                                                        JCLS.eval_string(code, function(val){
                                                                // success
                                                        }, function(){
                                                                // failure
                                                        });
                                                });
                                                next();
                                        }
                                };
                        })(i++));
                }
        };

        load("../jcls.js", function(code){
                new Function("exports", "global", code).call(window, window.JCLS = {}, window);
                load_eval_lisp_seq([
                        "../cl/common-lisp.lisp",
                        "./ymacs.lisp"
                ], make_desktop);
        });

})();
