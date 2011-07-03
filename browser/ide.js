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

        D.KEYS = {
                "C-c C-c && C-M-x": function() {
                        var expr = find_toplevel_sexp(this, true);
                        (function(){
                                try {
                                        JCLS.eval_string(expr, function(val){
                                                // success
                                        }, function(){
                                                // failure
                                        });
                                } catch(ex) {
                                        throw new Ymacs_Exception(ex + "");
                                }
                        }).delayed(1);
                }
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

function make_desktop() {
        var desktop = new DlDesktop();
        desktop.fullScreen(true);

        var dlg = new DlDialog({
                title: "JCLS / Ymacs",
                resizable: true
        });

        var ymacs = new Ymacs({ parent: dlg, buffers: [] });
        ymacs.setColorTheme([ "light", "andreas" ]);
        ymacs.getActiveBuffer().cmd("jcls_mode");

        dlg.setSize({ x: 640, y: 480 });
        dlg.show(true);
        dlg.maximize(true);
};
