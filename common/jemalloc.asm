extrn 'malloc' as _malloc
malloc = PLT _malloc

extrn 'free' as _free
free = PLT _free

extrn 'realloc' as _realloc
realloc = PLT _realloc
