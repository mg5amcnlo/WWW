/* qcdloop1 library MAPLE wrappers
   Generated automatically by Maple 14.00.
   edited and put together by VY */

#include "config.h"
#include <qcdloop1.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mplshlib.h>

#include <maplec.h>

typedef void *MaplePointer;
static MKernelVector mapleKernelVec;
static ALGEB *args;

/* MWRAP_qlinit_ Wrapper
   Generated automatically by Maple 14.00.
*/

/* main - MWRAP_qlinit_ */
ALGEB M_DECL MWRAP_qlinit_( MKernelVector kv,
    ALGEB fn_args )

{
    qlinit_();
    return NULL;
}

/* MWRAP_qli1_ Wrapper
   Generated automatically by Maple 14.00.
*/

/* main - MWRAP_qli1_ */
ALGEB M_DECL MWRAP_qli1_( MKernelVector kv,
	ALGEB fn_args )

{
    FLOAT64 a1;
    FLOAT64 a2;
    INTEGER32 a3;
    ql_cmplx_dbl r;
    ALGEB mr;
    int i;
    mapleKernelVec = kv;
    args = (ALGEB*) fn_args;
    
    if( MapleNumArgs(mapleKernelVec,(ALGEB)args) != 3 ) 
        MapleRaiseError(mapleKernelVec,"Incorrect number of arguments");
    
    /* float[8] */
    a1 = MapleToFloat64(mapleKernelVec,args[1]);
    
    /* float[8] */
    a2 = MapleToFloat64(mapleKernelVec,args[2]);
    
    /* integer[4] */
    a3 = MapleToInteger32(mapleKernelVec,args[3]);

#ifdef USE_F2C
    qli1_(&r, &a1, &a2, &a3);
#else
    r = qli1_(&a1, &a2, &a3);
#endif
    
    mr = 
    ToMapleComplex(mapleKernelVec,(double)r.re,(double)r.im);
    return( mr );
}

/* MWRAP_qli2_ Wrapper
   Generated automatically by Maple 14.00.
*/

/* main - MWRAP_qli2_ */
ALGEB M_DECL MWRAP_qli2_( MKernelVector kv,
	ALGEB fn_args )

{
    FLOAT64 a1;
    FLOAT64 a2;
    FLOAT64 a3;
    FLOAT64 a4;
    INTEGER32 a5;
    ql_cmplx_dbl r;
    ALGEB mr;
    int i;
    mapleKernelVec = kv;
    args = (ALGEB*) fn_args;
    
    if( MapleNumArgs(mapleKernelVec,(ALGEB)args) != 5 ) 
        MapleRaiseError(mapleKernelVec,"Incorrect number of arguments");
    
    /* float[8] */
    a1 = MapleToFloat64(mapleKernelVec,args[1]);
    
    /* float[8] */
    a2 = MapleToFloat64(mapleKernelVec,args[2]);
    
    /* float[8] */
    a3 = MapleToFloat64(mapleKernelVec,args[3]);
    
    /* float[8] */
    a4 = MapleToFloat64(mapleKernelVec,args[4]);
    
    /* integer[4] */
    a5 = MapleToInteger32(mapleKernelVec,args[5]);

#ifdef USE_F2C
    qli2_(&r, &a1, &a2, &a3, &a4, &a5);
#else
    r = qli2_(&a1, &a2, &a3, &a4, &a5);
#endif
    
    mr = 
    ToMapleComplex(mapleKernelVec,(double)r.re,(double)r.im);
    return( mr );
}

/* MWRAP_qli3_ Wrapper
   Generated automatically by Maple 14.00.
*/

/* main - MWRAP_qli3_ */
ALGEB M_DECL MWRAP_qli3_( MKernelVector kv,
	ALGEB fn_args )

{
    FLOAT64 a1;
    FLOAT64 a2;
    FLOAT64 a3;
    FLOAT64 a4;
    FLOAT64 a5;
    FLOAT64 a6;
    FLOAT64 a7;
    INTEGER32 a8;
    ql_cmplx_dbl r;
    ALGEB mr;
    int i;
    mapleKernelVec = kv;
    args = (ALGEB*) fn_args;
    
    if( MapleNumArgs(mapleKernelVec,(ALGEB)args) != 8 ) 
        MapleRaiseError(mapleKernelVec,"Incorrect number of arguments");
    
    /* float[8] */
    a1 = MapleToFloat64(mapleKernelVec,args[1]);
    
    /* float[8] */
    a2 = MapleToFloat64(mapleKernelVec,args[2]);
    
    /* float[8] */
    a3 = MapleToFloat64(mapleKernelVec,args[3]);
    
    /* float[8] */
    a4 = MapleToFloat64(mapleKernelVec,args[4]);
    
    /* float[8] */
    a5 = MapleToFloat64(mapleKernelVec,args[5]);
    
    /* float[8] */
    a6 = MapleToFloat64(mapleKernelVec,args[6]);
    
    /* float[8] */
    a7 = MapleToFloat64(mapleKernelVec,args[7]);
    
    /* integer[4] */
    a8 = MapleToInteger32(mapleKernelVec,args[8]);

#ifdef USE_F2C
    qli3_(&r, &a1, &a2, &a3, &a4, &a5, &a6, &a7, &a8);
#else
    r = qli3_(&a1, &a2, &a3, &a4, &a5, &a6, &a7, &a8);
#endif
    
    mr = 
    ToMapleComplex(mapleKernelVec,(double)r.re,(double)r.im);
    return( mr );
}

/* MWRAP_qli4_ Wrapper
   Generated automatically by Maple 14.00.
*/

/* main - MWRAP_qli4_ */
ALGEB M_DECL MWRAP_qli4_( MKernelVector kv,
	ALGEB fn_args )

{
    FLOAT64 a1;
    FLOAT64 a2;
    FLOAT64 a3;
    FLOAT64 a4;
    FLOAT64 a5;
    FLOAT64 a6;
    FLOAT64 a7;
    FLOAT64 a8;
    FLOAT64 a9;
    FLOAT64 a10;
    FLOAT64 a11;
    INTEGER32 a12;
    ql_cmplx_dbl r;
    ALGEB mr;
    int i;
    mapleKernelVec = kv;
    args = (ALGEB*) fn_args;
    
    if( MapleNumArgs(mapleKernelVec,(ALGEB)args) != 12 ) 
        MapleRaiseError(mapleKernelVec,"Incorrect number of arguments");
    
    /* float[8] */
    a1 = MapleToFloat64(mapleKernelVec,args[1]);
    
    /* float[8] */
    a2 = MapleToFloat64(mapleKernelVec,args[2]);
    
    /* float[8] */
    a3 = MapleToFloat64(mapleKernelVec,args[3]);
    
    /* float[8] */
    a4 = MapleToFloat64(mapleKernelVec,args[4]);
    
    /* float[8] */
    a5 = MapleToFloat64(mapleKernelVec,args[5]);
    
    /* float[8] */
    a6 = MapleToFloat64(mapleKernelVec,args[6]);
    
    /* float[8] */
    a7 = MapleToFloat64(mapleKernelVec,args[7]);
    
    /* float[8] */
    a8 = MapleToFloat64(mapleKernelVec,args[8]);
    
    /* float[8] */
    a9 = MapleToFloat64(mapleKernelVec,args[9]);
    
    /* float[8] */
    a10 = MapleToFloat64(mapleKernelVec,args[10]);
    
    /* float[8] */
    a11 = MapleToFloat64(mapleKernelVec,args[11]);
    
    /* integer[4] */
    a12 = MapleToInteger32(mapleKernelVec,args[12]);

#ifdef USE_F2C
    qli4_(&r, &a1, &a2, &a3, &a4, &a5, &a6, &a7, &a8, &a9, &a10, &a11, &a12);
#else
    r = qli4_(&a1, &a2, &a3, &a4, &a5, &a6, &a7, &a8, &a9, &a10, &a11, &a12);
#endif
    
    mr = 
    ToMapleComplex(mapleKernelVec,(double)r.re,(double)r.im);
    return( mr );
}
