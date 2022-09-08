# Things currently broken or missing in Aero

More or less a TODO list

- LSP doesn't work in HTML (particularly Angular)
- Sometimes Apheleia inserts garbage at the start of files
    - Example garbage: `../../../../../../../../../../../var/folders/3q/c7s5symj1ql4d800bv1btml80000gp/T/apheleiaRCDNmD.ts[2K[1G[1A[2K[1G`
- git-gutter doesn't extend to wrapped lines
- `package!` has inconsistent indentation?? Works correctly in some files but not in others
- magit blame always errors with `Debugger entered--Lisp error: (args-out-of-range #<buffer  *temp*-355332> 0 1)`
- jest always rebuilds on startup
- Emacs crashes on Mac M1 a lot. some errors include:
    - Compressor failed a blocking pager_get
