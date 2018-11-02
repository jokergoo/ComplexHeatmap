#include "RConverters.h"
#include "Rdefines.h"

#include <stdio.h>

int
R_isVariableReference(SEXP arg)
{
    SEXP e, ans;
    int val;
    
    PROTECT(e = allocVector(LANGSXP, 3));
    SETCAR(e, Rf_install("is"));
    SETCAR(CDR(e), arg);
    SETCAR(CDR(CDR(e)), mkString("VariableReference"));
    ans = Rf_eval(e, R_GlobalEnv);
    val = INTEGER(ans)[0];
    UNPROTECT(1);
    return(val);
}

void *
getVariableReference(SEXP arg, SEXP el, const char *type, const char *tag)
{
    char tmp[256];
    sprintf(tmp, "%sRef", tag);
    return(R_getNativeReference(el, type, tmp));
}

SEXP
R_make_var_reference(void *ref, const char * const type)
{
    SEXP ans;
    SEXP klass = MAKE_CLASS("VariableReference");
    PROTECT(klass);
    PROTECT(ans = NEW(klass));
#ifdef DEBUG_R_RUNTIME
    fprintf(stderr, "variable reference %p\n", ref);
#endif
    SET_SLOT(ans, Rf_install("ref"), R_createNativeReference(ref, type, type));
    UNPROTECT(2);

    return(ans);
}


SEXP
R_createArrayReference(const void * ref, const char * const className, const char * const alias, 
		       int *dims, unsigned int numDims,   size_t sizeofElement)
{
    SEXP ans, r_dims;
    SEXP klass = MAKE_CLASS(className);
    int i;

    PROTECT(klass);
    PROTECT(ans = NEW(klass));
    SET_SLOT(ans, Rf_install("ref"), R_MakeExternalPtr((void *) ref, Rf_install(alias), R_NilValue));
    PROTECT(r_dims = NEW_INTEGER(numDims));
    for(i = 0; i < numDims; i++) INTEGER(r_dims)[i] = dims[i];
    SET_SLOT(ans, Rf_install("length"), r_dims);
    SET_SLOT(ans, Rf_install("elementSize"), ScalarInteger(sizeofElement));
    UNPROTECT(3);

    return(ans);
}




SEXP
R_createNativeReference(const void *val, const char *className, const char *tagName)
{
 SEXP ans;
 SEXP klass = MAKE_CLASS((char *) className);

 if(klass == R_NilValue) {
	 PROBLEM "Can't find class %s", className
    ERROR;
 }

 /* should check this extends RC++Reference.. */
 PROTECT(klass);
 PROTECT(ans = NEW(klass));

 ans = SET_SLOT(ans, Rf_install("ref"),  R_MakeExternalPtr((void *) val, Rf_install(tagName), R_NilValue));

 UNPROTECT(2);
 return(ans);
}

void *
R_getNativeReference(SEXP arg, const char *type, const char *tag)
{
 SEXP el, elTag; 
 void *ans;

 el = GET_SLOT(arg, Rf_install("ref"));
 if(R_isVariableReference(arg)) {
     void *tmp;
     tmp = getVariableReference(arg, el, type, tag);
     if(!tmp) {
        PROBLEM "Got null value for variable reference %s", type
        ERROR;
     }
     return(tmp);
 }


/* XXX added allow through if the TAG is null on the object. Just for now. */
 if(tag && tag[0] && (elTag = R_ExternalPtrTag(el)) && elTag != Rf_install(tag)) {

        /* So not a direct match. Now see if it is from a derived class
           by comparing the value in the object to the name of each of the
           ancestor classes.
         */
    SEXP ancestors = GET_SLOT(arg, Rf_install("classes"));
    int n, i;
    n = Rf_length(ancestors);
    for(i = 0; i < n  ; i ++) {
        if(strcmp(CHAR(STRING_ELT(ancestors, i)), tag) == 0)
  	   break;
    }
    if(i == n) {
      PROBLEM "Looking for %s, got %s",
	      tag, elTag != R_NilValue ? CHAR(PRINTNAME(elTag)) : "NULL"
      ERROR;
    }
 }

 ans = R_ExternalPtrAddr(el);

 if(!ans) {
   PROBLEM "NULL value passed to R_getNativeReference. This may not be an error, but it could be.\n      Have you loaded an object from a previous R session?"
   ERROR;
 }
 return(ans);
}


int
convertFromRToInt(SEXP obj)
{
    return(INTEGER(obj)[0]);
}


SEXP
convertIntToR(int x)
{
    SEXP ans;
    ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = x;
    return(ans);
}

SEXP
convertDoubleToR(double x)
{
    SEXP ans;
    ans = allocVector(REALSXP, 1);
    REAL(ans)[0] = x;
    return(ans);
}

SEXP
convertDoubleArrayToR(int len, const double *x, int copy, int start, int end)
{
    SEXP ans;
    int i, num;
    
    num = end  - start + 1;
    ans = allocVector(REALSXP, num);
    for(i = 0; i < num ; i++)
	REAL(ans)[i] = x[i + start];
    return(ans);
}

#define MIN(a, b)  ((a) < (b) ? (a) : (b))

void
convertRCharacterToCharArray(char *dest, SEXP r_value, int array_len)
{
    int i;
    int len;

    len = MIN(Rf_length(r_value), array_len);
    for(i = 0; i < len; i++)
	dest[i] = CHAR(STRING_ELT(r_value, i))[0];
}

SEXP
convertCharArrayToR(int dim, const char *x, int copy, int start, int end)
{
    SEXP ans;
    int i, num;
    char buf[2];

    buf[1] = '\0';
    num = end - start + 1;
    PROTECT(ans = allocVector(STRSXP, num));
    for(i = 0; i < num ; i++) {
        buf[0] = x[i + start];
	SET_STRING_ELT(ans, i, mkChar(buf));
    }
    UNPROTECT(1);
    return(ans);
}


#if 0
/* Why are these commented out? Because we generate them programmatically.
  
   convertDoubleArrayToR.
*/
SEXP
convertIntArrayToR(const int *x, int len, int start, int end)
{
    SEXP ans;
    int i;

    
    ans = allocVector(INTSXP, len);
    for(i = 0; i < len ; i++)
	INTEGER(ans)[i] = x[i];
    return(ans);
}


SEXP
convertUnsignedIntArrayToR(const unsigned int *x, int len)
{
    SEXP ans;
    int i;
    
    ans = allocVector(REALSXP, len);
    for(i = 0; i < len ; i++)
	REAL(ans)[i] = x[i];
    return(ans);
}
#endif



SEXP
convertStringArrayToR(const char * const *x, int len)
{
    SEXP ans;
    int i;

    PROTECT(ans = allocVector(STRSXP, len));
    for(i = 0; i < len ; i++)
	SET_STRING_ELT(ans, i, mkChar(x[i] ? x[i] : ""));
    UNPROTECT(1);
    return(ans);
}



SEXP
createREnumerationValue(int val, const char * const *names, const int *values, int namesLength, const char *name)
{
    SEXP ans;
    int i;

    PROTECT(ans =allocVector(INTSXP, 1));
    INTEGER(ans)[0] = val;
    
    for(i = 0; i < namesLength; i++) {
	if(val == values[i]) {
	    SET_NAMES(ans, mkString(names[i]));
	    break;
	}
    }

    if(i == namesLength) {
	PROBLEM "Unrecognized value (%d) in enumeration %s", val, name
        ERROR;
    }
    /* Do we want an enumeration value element here also. */
    SET_CLASS(ans, mkString(name));

    UNPROTECT(1);
    return(ans);
}


/*
  Finalize for deallocating the space we allocate for references to structures
  created in S as part of the automatically generated code.
 */
void
SimpleAllocFinalizer(SEXP ans)
{
    void *ptr = R_ExternalPtrAddr(ans);
    if(ptr) {
#ifdef DEBUG_R_RUNTIME
	fprintf(stderr, "Finalizing %p\n", ptr); fflush(stderr);
#endif
	free(ptr);
	R_ClearExternalPtr(ans);
    }
}

/**
  Convert R object into either a function or the address of a C routine.
  For a C routine, the caller can specify the name of the typedef which is
  checked using the TAG for the external pointer.
*/
void *
Rfrom_Callbable(SEXP obj, const char * const TypeDefName, CallableType *type) 
{

           /* If TypeDefName is NULL, we don't bother checking*/
        if(TYPEOF(obj) == EXTPTRSXP) {
	    if(TypeDefName && R_ExternalPtrTag(obj) != Rf_install(TypeDefName)) {
   	         PROBLEM "[RfromCallbable] incorrect type name for a native routine pointer %s, not %s",
		    CHAR(asChar(R_ExternalPtrTag(obj))), TypeDefName
		 ERROR;
	    }

	    if(type) 
		*type = NATIVE_ROUTINE;

	    return(R_ExternalPtrAddr(obj));
        } else if(TYPEOF(obj) == CLOSXP) {
	    if(type) 
		*type = R_FUNCTION;
	    return(obj);
	}

	PROBLEM  "the Rfrom_Callable routine only handles native routines and "
        ERROR;

	return((void *) NULL);
   }


SEXP
R_makeNames(const char *names[], int len)
{
  SEXP ans;
  int i;
  PROTECT(ans = NEW_CHARACTER(len));
  for(i = 0; i < len; i++) 
    SET_STRING_ELT(ans, i, mkChar(names[i]));
  UNPROTECT(1);

  return(ans);
}

typedef struct {
    void **els;
    unsigned long length;
} RPointerList;

SEXP
R_listToRefArray(SEXP r_els, SEXP r_type)
{
    const char *type;
    SEXP el;
    int i, n;
    void *tmp;
    RPointerList *ans;
    
    n = GET_LENGTH(r_els);
    ans = (RPointerList *) malloc(sizeof(RPointerList));
    ans->els = (void **) malloc(sizeof(void *) * n);

    for(i = 0; i < n; i++) {
	el = VECTOR_ELT(r_els, i);
	tmp = R_getNativeReference(el, type, type);
	ans->els[i] = tmp;
    }
    /*XXX Need finalizer */
    return(R_MAKE_REF_TYPE(ans, RPointerList));
}


SEXP
R_RPointerList_length(SEXP r_ref)
{
    RPointerList *l = R_GET_REF_TYPE(r_ref, RPointerList);
    return(ScalarReal(l->length));
}


char **
getRStringArray(SEXP els)
{
    char **ans;
    int i, len;
    
    len = GET_LENGTH(els);
    if(len == 0)
	return(NULL);
    ans = (char **) malloc(sizeof(char *) * len);
    for(i = 0; i < len ; i++)
	ans[i] = strdup(CHAR(STRING_ELT(els, i)));
    return(ans);
}



/* 
 Determine if obj is an instance of the class given by className
 which should be an S4 class.
*/
Rboolean
IS_S4_INSTANCE(SEXP  obj, const char *className)
{
	SEXP e, ans;
	Rboolean status;

	if(!IS_S4_OBJECT(obj))
	   return(FALSE);

	PROTECT(e = allocVector(LANGSXP, 3));
	SETCAR(e, Rf_install("is"));
	SETCAR(CDR(e), obj);
	SETCAR(CDR(CDR(e)), mkString(className));
	ans = eval(e, R_GlobalEnv);
	status = LOGICAL(ans)[0];
	UNPROTECT(1);
	return(status);
}



#ifdef __cplusplus
extern "C"
#endif
SEXP
R_new_int(SEXP r_value)
{
	int *val = (int *) malloc(sizeof(int));
	if(!val) {
   	    PROBLEM "cannot allocate space for a single integer"
	    ERROR;
	}

	return(R_createNativeReference((void *) val, "intPtr", "intPtr"));
}


#ifdef __cplusplus
extern "C"
#endif
SEXP
R_new_long_int(SEXP r_value)
{
	void *val = malloc(sizeof(long int));
	if(!val) {
   	    PROBLEM "cannot allocate space for a single long int"
	    ERROR;
	}

	return(R_createNativeReference((void *) val, "long_intPtr", "long_intPtr"));
}


void *
R_asFunctionPointer(SEXP r_val, void *defaultFun, void *stack)
{
	if(TYPEOF(r_val) == CLOSXP) {
// put on the relevant stack
 	   return(defaultFun);
	} else if(TYPEOF(r_val) == EXTPTRSXP) {
	   return(R_ExternalPtrAddr(r_val));
	}

	return(NULL);
}


#ifdef __cplusplus
extern "C"
#endif
SEXP
R_duplicateArray(SEXP r_ref, SEXP r_size, SEXP r_elementDup)
{
    void *array, *copy;
    size_t numBytes = (size_t) REAL(r_size)[0];
    SEXP r_ans, tmp;

    array = R_getNativeReference(r_ref, NULL, NULL);
    copy = malloc( numBytes );
    if(!copy) {
	    PROBLEM "Cannot allocate %lf bytes to copy native array", REAL(r_size)[0]
	    ERROR;
    }
    memcpy(copy, array, numBytes);
    tmp = GET_SLOT(r_ref, Rf_install("ref"));
    r_ans = R_MakeExternalPtr(copy, R_ExternalPtrTag(tmp), R_ExternalPtrProtected(tmp));
    return(r_ans);
}



#ifdef __cplusplus
extern "C"
#endif
SEXP
R_isNativeNull(SEXP ext)
{
    return(ScalarLogical(R_ExternalPtrAddr(ext) == NULL));
}


#ifdef __cplusplus
extern "C"
#endif
SEXP
R_addressOfPointer(SEXP ext)
{
/* Need to know the tag and the protected for the external pointer. 
Leave to R code to put in the correct class.
*/
   void *p = R_ExternalPtrAddr(ext);
	
   return(R_NilValue);
}


#include <stdarg.h>

SEXP  
createRRoutineReference(void *fun, const char * const routineName,  const char * const returnTypeName, unsigned int numParams, ...)
{
    SEXP ans, klass, tmp;
    va_list args;

    PROTECT(klass = MAKE_CLASS("CRoutineRef"));
    PROTECT(ans = NEW(klass));
    SET_SLOT(ans, Rf_install("ref"), R_MakeExternalPtr(fun, Rf_install("CRoutine"), R_NilValue));
    if(routineName)
	SET_SLOT(ans, Rf_install("name"), ScalarString(mkChar(routineName)));
    SET_SLOT(ans, Rf_install("returnType"), ScalarString(mkChar(returnTypeName)));
    SET_SLOT(ans, Rf_install("numParameters"), ScalarInteger(numParams));

    if(numParams > 0) {
	PROTECT( tmp = NEW_CHARACTER(numParams));
	va_start(args, numParams);
	for(int i = 0; i < numParams; i++) 
	    SET_STRING_ELT(tmp, i, mkChar(va_arg(args, const char * const)));
	SET_SLOT(ans, Rf_install("parameterTypes"), tmp);
	va_end(args);
	UNPROTECT(1);
    }


    UNPROTECT(2);
    return(ans);
}


#define GET_EXT_PTR_REF(x) \
    (TYPEOF((x)) == EXTPTRSXP ? R_ExternalPtrAddr((x)) :		\
                                R_ExternalPtrAddr(GET_SLOT((x), Rf_install("ref"))))


SEXP
R_isNilPointer(SEXP r_ref)
{
    void *val = GET_EXT_PTR_REF(r_ref);
    return(ScalarLogical( val ? FALSE : TRUE  ));
}
