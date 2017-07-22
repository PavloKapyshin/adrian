from adrian.cgen import libc


CMODULE_NAME = "c"
FREE_FUNC_NAME = "free"
MALLOC_FUNC_NAME = "malloc"
SIZEOF_FUNC_NAME = "sizeof"

CFUNCS = (
    FREE_FUNC_NAME,
    MALLOC_FUNC_NAME
)

MALLOC_FUNC_DESCR = libc.malloc
FREE_FUNC_DESCR = libc.free

# CFUNCS = (
#     "tpr",
# )
# CFUNCS_SIGNATURES = {
#     "puts": cgen.CFuncDescr(
#         "puts",
#         rettype=cgen.CTypes.void,
#         args=(cgen.CTypes.str, ),
#         includes=[stdio])
# }