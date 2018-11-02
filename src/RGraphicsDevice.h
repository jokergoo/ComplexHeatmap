#include <Rdefines.h> 
#include <R_ext/GraphicsEngine.h> 
#include <R_ext/GraphicsDevice.h> 
//#include <RConverters.h> 
#include "RConverters.h"


struct  RDevDescMethods {
    SEXP activate ;
    SEXP circle ;
    SEXP clip ;
    SEXP close ;
    SEXP deactivate ;
    SEXP locator ;
    SEXP line ;
    SEXP metricInfo ;
    SEXP mode ;
    SEXP newPage ;
    SEXP polygon ;
    SEXP polyline ;
    SEXP rect ;
    SEXP size ;
    SEXP strWidth ;
    SEXP text ;
    SEXP onExit ;
    SEXP getEvent ;
    SEXP newFrameConfirm ;
    SEXP textUTF8 ;
    SEXP strWidthUTF8 ; 

    SEXP initDevice;
    SEXP state;
};
typedef  struct  RDevDescMethods RDevDescMethods ;

SEXP R_copyStruct_RDevDescMethods ( const struct  RDevDescMethods *value);


#include "proxyDecls.h"

#include "createExpressions.h"
