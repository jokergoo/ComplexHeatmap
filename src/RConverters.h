#ifndef RAUTO_CONVERTERS_H
#define RAUTO_CONVERTERS_H

/* For now! */
#define DEBUG_R_RUNTIME 1

#include <stdlib.h>

#include <Rinternals.h>
#include <Rdefines.h>


#ifdef __cplusplus
extern "C" {
#endif


typedef enum { R_DEEP_COPY, R_DUPLICATE, R_REFERENCE } R_CopyLevel;

#define INTEGER_ELT(x, i)  INTEGER(x)[i]
#define REAL_ELT(x, i)     REAL(x)[i]


#define FAIL(msg)  { PROBLEM msg \
                     ERROR; \
                   }

#define NEW_REAL  NEW_NUMERIC

typedef enum {NATIVE_ROUTINE, R_FUNCTION} CallableType;


int convertFromRToInt(SEXP obj);
SEXP convertDoubleToR(double x);
SEXP convertIntToR(int x);
//SEXP convertIntArrayToR(const int *x, int len, int start, int end);
//SEXP convertUnsignedIntArrayToR(const unsigned int *x, int len);

char ** getRStringArray(SEXP);

/**
   Utility for converting a value from an enumeration to an R object 
   that is an integer scalar (vector of length 1) with a name
 */
SEXP createREnumerationValue(int val, const char * const *names, const int *values, int namesLength, const char *name);

/**
   Finalizer routine that cleans up the memory used when allocating a reference
   to a C-level structure. 
 */
void SimpleAllocFinalizer(SEXP ans);

void *Rfrom_Callbable(SEXP obj, const char * const TypeDefName, CallableType *type) ;


#ifdef __cplusplus
extern "C"
#endif
SEXP convertStringArrayToR(const char * const *x, int len);


void * R_getNativeReference(SEXP arg, const char *type, const char *tag);
SEXP R_createNativeReference(const void * const val, const char *className, const char *tagName);

#define R_GET_REF_TYPE(arg, class) \
    (class *) R_getNativeReference(arg, #class, #class)


#define R_MAKE_REF_TYPE(arg, class) \
     R_createNativeReference(arg, #class, #class)


#define DEREF_REF(x, type) * ((type *) R_getNativeReference((x), #type, #type))

#define DEREF_REF_PTR(x, type) ((type *) R_getNativeReference((x), #type, #type))


SEXP R_makeNames(const char *names[], int len);



SEXP R_createArrayReference(const void  *ref, const char * const className, const char * const type, 
			    int *dimensions, unsigned int numDimension, size_t sizeofElement);

SEXP R_make_var_reference(void *ref, const char * const type);
#define R_MAKE_VAR_REFERENCE(addr, type) R_make_var_reference((addr), type)
void *getVariableReference(SEXP arg, SEXP el, const char *type, const char *tag);

/*XXX Put the proper type on the stack */
void *R_asFunctionPointer(SEXP r_val, void *defaultFun, void *stack);

//XXXX  This is then macro-ized out!!
//SEXP copy_int_array_to_R(const int * const x, int len);
#define copy_int_array_to_R convertIntArrayToR


/* XXX This must be kept in synchronization with C++CastValues in R/classes.R in this package */
typedef enum {
    STATIC = 1,
    DYNAMIC,
    REINTERPRET,
    CONST
} CastType;


Rboolean IS_S4_INSTANCE(SEXP  obj, const char *className);


SEXP R_duplicateArray(SEXP r_ref, SEXP r_size, SEXP r_elementDup);


SEXP R_isNativeNull(SEXP ext);
SEXP R_addressOfPointer(SEXP ext);


SEXP convertDoubleArrayToR(int len, const double *x, int copy, int start, int end);
SEXP convertCharArrayToR(int dim, const char *x, int copy, int start, int end);

void convertRCharacterToCharArray(char *dest, SEXP r_value, int array_len);


SEXP  createRRoutineReference(void *, const char * const routineName,  const char * const returnTypeName, unsigned int numParams, ...);

#ifdef __cplusplus
}
#endif

#endif
