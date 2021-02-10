#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(conc)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(unocw)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(unou2p)(void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"conc",   (DL_FUNC) &F77_NAME(conc),    8},
    {"unocw",  (DL_FUNC) &F77_NAME(unocw),  17},
    {"unou2p", (DL_FUNC) &F77_NAME(unou2p),  6},
    {NULL, NULL, 0}
};

void R_init_survC1(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
