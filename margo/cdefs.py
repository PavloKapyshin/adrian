from vendor.adrian import cgen

stdio = cgen.Include("stdio.h")


CMODULE_NAME = "c"

CFUNCS = (
    "tpr",
)
CFUNCS_SIGNATURES = {
    "puts": cgen.CFuncDescr(
        "puts",
        rettype=cgen.CTypes.void,
        args=(cgen.CTypes.str, ),
        includes=[stdio])
}