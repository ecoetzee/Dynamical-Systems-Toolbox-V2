#if 0

This file is not available for use in any application other than as a
  MATLAB(R) MEX file for use with the Simulink(R) product.
  If you do not have the Real-Time Workshop licensed, this file contains
  encrypted block names, etc. If you purchase the Real-Time Workshop,
  this file will contain full block descriptions and improved variable
  names.
#endif

/*
 * ab_acc.c
 *
 * Real-Time Workshop code generation for Simulink model "ab_acc.mdl".
 *
 * Model Version              : 1.15
 * Real-Time Workshop version : 7.3  (R2009a)  15-Jan-2009
 * C source code generated on : Fri Jul 30 00:38:26 2010
 *
 * Target selection: accel.tlc
 *   Note: GRT includes extra infrastructure and instrumentation for prototyping
 * Embedded hardware selection: 32-bit Generic
 * Emulation hardware selection:
 *   Differs from embedded hardware (MATLAB Host)
 * Code generation objectives: Unspecified
 * Validation result: Not run
 */
#include <math.h>
#include "ab_acc.h"
#include "ab_acc_private.h"
#include <stdio.h>
#include "simstruc.h"
#include "fixedpoint.h"
#define CodeFormat                     S-Function
#define AccDefine1                     Accelerator_S-Function

/* Outputs for root system: '<Root>' */
static void mdlOutputs(SimStruct *S, int_T tid)
{
  {
    real_T B_0_5_0;
    real_T B_0_6_0;

    /* Integrator: '<Root>/U1' */
    ((BlockIO *) _ssGetBlockIO(S))->B_0_0_0 = ((ContinuousStates *)
      ssGetContStates(S))->U1_CSTATE;
    ssCallAccelRunBlock(S, 0, 1, SS_CALL_MDL_OUTPUTS);

    /* Integrator: '<Root>/U2' */
    ((BlockIO *) _ssGetBlockIO(S))->B_0_2_0 = ((ContinuousStates *)
      ssGetContStates(S))->U2_CSTATE;
    ssCallAccelRunBlock(S, 0, 3, SS_CALL_MDL_OUTPUTS);
    B_0_5_0 = muDoubleScalarExp(((BlockIO *) _ssGetBlockIO(S))->B_0_2_0);
    B_0_6_0 = rtC(S)->B_0_4_0 - ((BlockIO *) _ssGetBlockIO(S))->B_0_0_0;
    ((BlockIO *) _ssGetBlockIO(S))->B_0_10_0 = B_0_6_0 * ((ExternalUPtrs *)
      ssGetU(S))->P1 * B_0_5_0 - ((BlockIO *) _ssGetBlockIO(S))->B_0_0_0;
    ((BlockIO *) _ssGetBlockIO(S))->B_0_12_0 = (B_0_5_0 * ((ExternalUPtrs *)
      ssGetU(S))->P1 * B_0_6_0 * ((ExternalUPtrs *) ssGetU(S))->P2 - ((BlockIO *)
      _ssGetBlockIO(S))->B_0_2_0) - ((ExternalUPtrs *) ssGetU(S))->P3 *
      ((BlockIO *) _ssGetBlockIO(S))->B_0_2_0;
  }

  /* tid is required for a uniform function interface.
   * Argument tid is not used in the function. */
  UNUSED_PARAMETER(tid);
}

/* Update for root system: '<Root>' */
#define MDL_UPDATE

static void mdlUpdate(SimStruct *S, int_T tid)
{
  /* tid is required for a uniform function interface.
   * Argument tid is not used in the function. */
  UNUSED_PARAMETER(tid);
}

/* Derivatives for root system: '<Root>' */
#define MDL_DERIVATIVES

static void mdlDerivatives(SimStruct *S)
{
  /* Integrator Block: '<Root>/B_0_0' */
  {
    ((StateDerivatives *) ssGetdX(S))->U1_CSTATE = ((BlockIO *) _ssGetBlockIO(S))
      ->B_0_10_0;
  }

  /* Integrator Block: '<Root>/B_0_2' */
  {
    ((StateDerivatives *) ssGetdX(S))->U2_CSTATE = ((BlockIO *) _ssGetBlockIO(S))
      ->B_0_12_0;
  }
}

/* Function to initialize sizes */
static void mdlInitializeSizes(SimStruct *S)
{
  /* checksum */
  ssSetChecksumVal(S, 0, 3919374162U);
  ssSetChecksumVal(S, 1, 4217032675U);
  ssSetChecksumVal(S, 2, 122131495U);
  ssSetChecksumVal(S, 3, 3419359039U);

  /* options */
  ssSetOptions(S, SS_OPTION_EXCEPTION_FREE_CODE);

  /* Accelerator check memory map size match for BlockIO */
  if (ssGetSizeofGlobalBlockIO(S) != sizeof(BlockIO)) {
    ssSetErrorStatus(S,"Unexpected error: Internal BlockIO sizes do "
                     "not match for accelerator mex file.");
  }

  /* constant block I/O */
  _ssSetConstBlockIO(S, &rtInvariantSignals);
}

/* Empty mdlInitializeSampleTimes function (never called) */
static void mdlInitializeSampleTimes(SimStruct *S)
{
}

/* Empty mdlTerminate function (never called) */
static void mdlTerminate(SimStruct *S)
{
}

/* MATLAB MEX Glue */
#include "simulink.c"
