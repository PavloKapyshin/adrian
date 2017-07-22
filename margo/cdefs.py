# from vendor.adrian import cgen

# stdio = cgen.Include("stdio.h")


CMODULE_NAME = "c"
MALLOC_FUNC_NAME = "malloc"
SIZEOF_FUNC_NAME = "sizeof"

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