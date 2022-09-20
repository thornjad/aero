# Things currently broken or missing in Aero

More or less a TODO list

- LSP doesn't work in HTML (particularly Angular)
- `package!` has inconsistent indentation?? Works correctly in some files but not in others
- magit blame always errors with `Debugger entered--Lisp error: (args-out-of-range #<buffer  *temp*-355332> 0 1)`
- jest always rebuilds on startup
- Emacs crashes on Mac M1 a lot. some errors include:
    - Compressor failed a blocking pager_get
    - KERN_INVALID_ADDRESS at 0x4091900104914608 -> 0xffff900104914608
    - pmap_enter failed with resource shortage
- In markdown mode, typing `[ ` results in `[ ]]`
