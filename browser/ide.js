Ymacs_Keymap_Emacs().defineKeys({
        "C-\\"    : "switch_to_buffer",
        "C-x C-s" : "save_file",
        "C-x C-f" : "load_file",
});

DEFINE_SINGLETON("Ymacs_Keymap_JCLS", Ymacs_Keymap, function(D, P){

        function getPP(p) {
                var pp = p.context.passedParens;
                return pp instanceof Function ? pp() : pp;
        };

        function compareRowCol(p1, p2) {
                return p1.line < p2.line ? -1 : p1.line > p2.line ? 1 : p1.col - p2.col;
        };

        function find_toplevel_sexp(buffer, blink) {
                var rc = buffer._rowcol, parser = buffer.tokenizer.finishParsing();
                if (parser) {
                        var lc = { line: rc.row, col: rc.col };
                        var p = getPP(parser).grep("closed").mergeSort(compareRowCol).grep_first(function(p){
                                return compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) >= 0;
                        });
                        if (p == null) p = getPP(parser).grep("closed").mergeSort(function(a, b){
                                return compareRowCol(a.closed, b.closed);
                        }).grep_last(function(p){
                                return compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) < 0;
                        });
                        if (p == null)
                                throw new Ymacs_Exception("Can't figure out toplevel expression");
                        var s = p, e = p.closed;
                        if (blink) {
                                buffer.setOverlay("flash-code", {
                                        line1: s.line, col1: s.col,
                                        line2: e.line, col2: e.col + 1
                                });
                                (function(){
                                        buffer.deleteOverlay("flash-code");
                                }).delayed(500);
                        }
                        return buffer._bufferSubstring(buffer._rowColToPosition(s.line, s.col),
                                                       buffer._rowColToPosition(e.line, e.col + 1));
                }
        };

        function get_output_buffer() {
                var ed = THE_EDITOR;
                var out = ed.getBuffer("*jcls*");
                if (!out) {
                        var frame = ed.getActiveFrame(), buf = ed.getActiveBuffer();
                        out = ed.createBuffer({ name: "*jcls*" });
                        out.cmd("jcls_mode");
                        buf.cmd("split_frame_vertically", "70%");
                        buf.cmd("other_frame");
                        buf.cmd("switch_to_buffer", "*jcls*");
                        ed.setActiveFrame(frame);
                }
                return out;
        };

        function jcls_log(txt) {
                var output = get_output_buffer();
                output.preventUpdates();
                output.cmd("end_of_buffer");
                output.cmd("insert", txt);
                output.cmd("newline");
                output.forAllFrames(function(frame){
                        frame.ensureCaretVisible();
                        frame.redrawModeline();
                });
                output.resumeUpdates();
        };

        DEFINE_CLASS("Ymacs_JCLS", Ymacs, function(D, P){
                P.jcls_log = function(thing) {
                        if (typeof thing != "string")
                                thing = JCLS.write_ast_to_string(thing);
                        jcls_log(thing);
                        return JCLS.NIL;
                };
        });

        Ymacs_Buffer.newCommands({
                jcls_get_output_buffer: get_output_buffer,
                jcls_log: jcls_log,
                jcls_clear_output: Ymacs_Interactive(function(){
                        var buf = get_output_buffer();
                        buf.setCode("");
                }),
                jcls_macroexpand_1: Ymacs_Interactive("d", function(point){
                        var code = this._bufferSubstring(point);
                        var stream = JCLS.lisp_input_stream(code);
                        var expr = JCLS.read(stream, false, null);
                        if (expr == null)
                                throw new Ymacs_Exception("Couldn't read Lisp expression starting at point");
                        // XXX: this is pretty sucky
                        expr = JCLS.write_ast_to_string(expr);
                        eval(this, "(macroexpand-1 '" + expr + ")");
                })
        });

        function eval(buf, expr) {
                try {
                        var start = new Date().getTime();
                        JCLS.eval_string(expr, function(val){
                                jcls_log("==> " + JCLS.write_ast_to_string(val)
                                         + " (" + ((new Date().getTime() - start) / 1000).toFixed(3) + "s)");
                        }, function(val){
                                jcls_log("**> " + JCLS.write_ast_to_string(val));
                        });
                } catch(ex) {
                        jcls_log("**> " + JCLS.write_ast_to_string(ex));
                }
        };

        D.KEYS = {
                "C-c C-c && C-M-x": function() {
                        var expr = find_toplevel_sexp(this, true);
                        (function(){
                                eval(this, expr);
                        }).delayed(1, this);
                },
                "C-c C-k": function() {
                        eval(this, this.getCode());
                },
                "C-c M-o && C-c DELETE && C-c C-DELETE": "jcls_clear_output",
                "C-c ENTER": "jcls_macroexpand_1"
        };
});

Ymacs_Buffer.newMode("jcls_mode", function(){
        this.cmd("lisp_mode", true);
        this.pushKeymap(Ymacs_Keymap_JCLS());
        return function() {
                this.popKeymap(Ymacs_Keymap_JCLS());
                this.cmd("lisp_mode", false);
        };
});

var THE_EDITOR;

function make_desktop() {
        var desktop = new DlDesktop();
        desktop.fullScreen(true);

        var dlg = new DlDialog({
                title: "JCLS / Ymacs",
                resizable: true
        });

        var ymacs = THE_EDITOR = new Ymacs_JCLS({ parent: dlg, buffers: [], lineNumbers: true });
        ymacs.setColorTheme([ "light", "andreas" ]);
        ymacs.getActiveBuffer().cmd("jcls_mode");

        dlg.setSize({ x: 640, y: 480 });
        dlg.show(true);
        dlg.maximize(true);
};

function load(url, cont) {
        var xhr = new XMLHttpRequest();
        xhr.onreadystatechange = function(){
                if (this.readyState == 4 && this.status == 200)
                        cont(this.responseText);
        };
        xhr.open("GET", url);
        xhr.send();
};
