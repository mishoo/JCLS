function load(url, cont) {
        var xhr = new XMLHttpRequest();
        xhr.onreadystatechange = function(){
                if (this.readyState == 4 && this.status == 200)
                        cont(this.responseText);
        };
        xhr.open("GET", url);
        xhr.send();
}

load("../jcls.js", function(code){
        new Function("exports", "global", code).call(window, window.JCLS = {}, window);
        load("../cl/common-lisp.lisp", eval_lisp);
        make_desktop();         // in ide.js
});

function eval_lisp(code) {
        console.time("eval_lisp");
        var ret = null;
        JCLS.eval_string(code, function(val){
                // success
                ret = val;
        }, function(){
                // failure
        });
        console.timeEnd("eval_lisp");
        return ret;
};

function time_it(name, func) {
        return function() {
                var start = new Date().getTime();
                var ret = func.apply(this, arguments);
                console.log(name + ": " + ((new Date().getTime() - start) / 1000).toFixed(3));
                return ret;
        };
}
