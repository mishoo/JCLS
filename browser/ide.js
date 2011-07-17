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

        function flash_region(buffer, begin, end) {
                var s = buffer._positionToRowCol(begin);
                var e = buffer._positionToRowCol(end);
                buffer.setOverlay("flash-code", {
                        line1: s.row, col1: s.col,
                        line2: e.row, col2: e.col
                });
                (function(){
                        buffer.deleteOverlay("flash-code");
                }).delayed(500);
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
                        return [ buffer._rowColToPosition(s.line, s.col),
                                 buffer._rowColToPosition(e.line, e.col + 1) ];
                }
        };

        function find_package(buffer, start) {
                return buffer.cmd("save_excursion", function(){
                        this.cmd("goto_char", start);
                        if (this.cmd("search_backward", "\n(in-package")) {
                                this.cmd("forward_char");
                                var a = this.point();
                                if (this.cmd("search_forward", ")")) {
                                        var b = this.point();
                                        return this.cmd("buffer_substring", a, b);
                                }
                        }
                });
        };

        function jcls_log(txt) {
                var output = get_output_buffer();
                output.preventUpdates();
                output.cmd("end_of_buffer");
                var pos = output._positionToRowCol(output.point());
                if (pos.col > 0)
                        output.cmd("newline");
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
                        buf.cmd("jcls_repl_prompt");
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
                }),
                jcls_eval_buffer: Ymacs_Interactive(function(){
                        eval(this, this.getCode());
                }),
                jcls_eval_sexp: Ymacs_Interactive(function(){
                        var points = find_toplevel_sexp(this, true);
                        var expr = this.cmd("buffer_substring", points[0], points[1]);
                        (function(){
                                eval(this, expr, find_package(this, points[0]));
                        }).delayed(1, this);
                }),
                jcls_eval_region: Ymacs_Interactive("r", function(begin, end){
                        eval(this, this.cmd("buffer_substring", begin, end), find_package(this, begin));
                }),
                jcls_repl_prompt: function(){
                        if (this._positionToRowCol(this.point()).col > 0)
                                this.cmd("newline");
                        var m = this.getq("jcls_repl_marker");
                        if (m) m.destroy();
                        JCLS.eval_string(
                                "(jcls:@ *PACKAGE* \"_name\")",
                                function(name){
                                        this.cmd("insert", name + "> ");
                                        this.cmd("end_of_line");
                                        m = this.createMarker(null, true);
                                        this.setq("jcls_repl_marker", m);
                                        this.forAllFrames(function(frame){ // this stinks. :-\
                                                frame.ensureCaretVisible();
                                                frame.redrawModeline();
                                                frame._redrawCaret(true);
                                        });
                                }.$(this)
                        );
                },
                jcls_repl_eval: function() {
                        var m = this.getq("jcls_repl_marker");
                        var code = this.cmd("buffer_substring", m.getPosition());
                        var stream = JCLS.lisp_input_stream(code);
                        try {
                                var expr = JCLS.read(stream, false, null);
                                flash_region(this, m.getPosition(), stream.pos + m.getPosition());
                                if (expr == null)
                                        return this.cmd("newline_and_indent");
                        } catch(ex) {
                                // XXX:
                                return this.cmd("newline_and_indent");
                        }
                        JCLS.eval(expr, null, function(result){
                                jcls_log("==> " + JCLS.write_ast_to_string(result));
                                this.cmd("jcls_repl_prompt");
                        }.$(this), function(val){
                                jcls_log("**> " + JCLS.write_ast_to_string(val));
                                this.cmd("jcls_repl_prompt");
                        }.$(this));
                }
        });

        function eval(buf, expr, preamble) {
                try {
                        if (preamble)
                                expr = preamble + expr;
                        var start = new Date().getTime();
                        JCLS.eval_string(expr, function(val){
                                jcls_log("==> " + JCLS.write_ast_to_string(val)
                                         + " <== in " + ((new Date().getTime() - start) / 1000).toFixed(3) + "s");
                                get_output_buffer().cmd("jcls_repl_prompt");
                        }, function(val){
                                jcls_log("**> " + JCLS.write_ast_to_string(val));
                                get_output_buffer().cmd("jcls_repl_prompt");
                        });
                } catch(ex) {
                        jcls_log("**> " + JCLS.write_ast_to_string(ex));
                }
        };

        D.KEYS = {
                "C-c C-c && C-M-x"                      : "jcls_eval_sexp",
                "C-c C-k"                               : "jcls_eval_buffer",
                "C-c C-r"                               : "jcls_eval_region",
                "C-c M-o && C-c DELETE && C-c C-DELETE" : "jcls_clear_output",
                "C-c ENTER"                             : "jcls_macroexpand_1"
        };

        DEFINE_SINGLETON("Ymacs_Keymap_JCLS_REPL", Ymacs_Keymap, function(D, P){
                D.KEYS = {
                        "ENTER" : Ymacs_Interactive(function(){
                                this.cmd("jcls_repl_eval");
                        })
                };
        });
});

Ymacs_Buffer.newMode("jcls_mode", function(){
        this.cmd("lisp_mode", true);
        this.pushKeymap(Ymacs_Keymap_JCLS());
        return function() {
                this.popKeymap(Ymacs_Keymap_JCLS());
                this.cmd("lisp_mode", false);
        };
});

Ymacs_Buffer.newMode("jcls_repl_mode", function(){
        this.cmd("jcls_mode");
        this.pushKeymap(Ymacs_Keymap_JCLS_REPL());
        return function() {
                this.popKeymap(Ymacs_Keymap_JCLS_REPL());
                this.cmd("jcls_mode", false);
        };
});

var THE_EDITOR;

function get_output_buffer() {
        var ed = THE_EDITOR;
        var out = ed.getBuffer("*jcls*");
        if (!out) {
                var frame = ed.getActiveFrame(), buf = ed.getActiveBuffer();
                out = ed.createBuffer({ name: "*jcls*" });
                out.cmd("jcls_repl_mode");
                buf.cmd("split_frame_vertically", "70%");
                buf.cmd("other_frame");
                buf.cmd("switch_to_buffer", "*jcls*");
                ed.setActiveFrame(frame);
                out.forAllFrames(function(frame){
                        frame.__lineNumbers = false; // :-\
                        frame.delClass("Ymacs-line-numbers");
                });
        }
        return out;
};

function make_desktop() {
        var desktop = new DlDesktop();
        desktop.fullScreen(true);

        var dlg = new DlDialog({
                title: "JCLS / Ymacs",
                resizable: true
        });

        var layout = new DlLayout({ parent: dlg });

        var toolbar = new DlContainer({ className: "DlToolbar" });
        var menu = new DlHbox({ parent: toolbar });

        make_samples_menu(menu);

        menu.addSeparator("wide-separator");
        function btn(label, action) {
                var b = new DlButton({ parent: menu, label: label });
                b.addEventListener("onClick", function(){
                        THE_EDITOR.focus();
                        action();
                });
                return b;
        };

        function buffer(){ return THE_EDITOR.getActiveBuffer() };

        btn("Eval buffer", function(){ buffer().cmd("jcls_eval_buffer") });
        btn("Eval expression", function(){ buffer().cmd("jcls_eval_sexp") });
        btn("Eval selection", function(){ buffer().cmd("jcls_eval_region") });
        btn("Macroexpand", function(){ buffer().cmd("jcls_macroexpand_1") });

        menu.addSeparator("wide-separator");

        btn("Copy to system clipboard", function(){ buffer().cmd("copy_for_operating_system") });
        btn("Paste from system clipboard", function(){ buffer().cmd("yank_from_operating_system") });

        var ymacs = THE_EDITOR = new Ymacs_JCLS({ buffers: [], lineNumbers: true });
        ymacs.setColorTheme([ "light", "standard" ]);
        ymacs.getActiveBuffer().cmd("jcls_mode");

        layout.packWidget(toolbar, { pos: "top" });
        layout.packWidget(ymacs, { pos: "bottom", fill: "*" });

        dlg.setSize({ x: 640, y: 480 });
        dlg.show(true);
        dlg.maximize(true);

        load("./scratch.lisp", function(code){
                ymacs.getBuffer("*scratch*").setCode(code);
        });

        ymacs.focus();
        get_output_buffer().cmd("jcls_repl_prompt");
};

function make_samples_menu(parent) {
        var samples = new DlButtonMenu({ parent: parent, label: "Load sample", connected: true });
        var menu = new DlVMenu();
        menu.addEventListener("onSelect", function(file){
                load(file, function(code){
                        var ymacs = THE_EDITOR;
                        var buf = ymacs.getBuffer(file);
                        if (!buf) {
                                buf = ymacs.createBuffer({ name: file });
                                buf.cmd("jcls_mode");
                        }
                        buf.setCode(code);
                        ymacs.switchToBuffer(buf);
                });
        });
        samples.setMenu(menu);
        [
                "samples/continuations.lisp",
                "samples/amb.lisp",
                "samples/dlcanvas.lisp",
                null,
                "../cl/common-lisp.lisp",
                "../cl/javascript.lisp",
                "./ymacs.lisp"
        ].foreach(function(file){
                if (file == null)
                        menu.addSeparator()
                else
                        new DlMenuItem({ parent: menu, label: file, name: file });
        });
};

function load(url, cont) {
        var xhr = new XMLHttpRequest();
        xhr.onreadystatechange = function(){
                if (this.readyState == 4 && this.status == 200)
                        cont(this.responseText);
        };
        xhr.open("GET", url + "?kc=" + new Date().getTime());
        xhr.send();
};

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
                "../cl/javascript.lisp",
                "./ymacs.lisp"
        ], function(){
                JCLS.eval_string("(in-package :cl-user)", make_desktop);
        });
});
