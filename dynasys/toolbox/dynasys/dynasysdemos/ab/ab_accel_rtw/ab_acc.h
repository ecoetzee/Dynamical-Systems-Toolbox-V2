/*
 * ab_acc.h
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
#ifndef RTW_HEADER_ab_acc_h_
#define RTW_HEADER_ab_acc_h_
#ifndef ab_acc_COMMON_INCLUDES_
# define ab_acc_COMMON_INCLUDES_
#include <stdlib.h>
#include <stddef.h>
#define S_FUNCTION_NAME                simulink_only_sfcn
#define S_FUNCTION_LEVEL               2
#define RTW_GENERATED_S_FUNCTION
#include "rtwtypes.h"
#include "simstruc.h"
#include "fixedpoint.h"
#include "mwmathutil.h"
#endif                                 /* ab_acc_COMMON_INCLUDES_ */

#include "ab_acc_types.h"

/* Block signals (auto storage) */
typedef struct {
  real_T B_0_10_0;                     /* '<Root>/Sum1' */
  real_T B_0_12_0;                     /* '<Root>/Sum3' */
  real_T B_0_0_0;                      /* '<Root>/U1' */
  real_T B_0_2_0;                      /* '<Root>/U2' */
} BlockIO;

/* Continuous states (auto storage) */
typedef struct {
  real_T U1_CSTATE;                    /* '<Root>/U1' */
  real_T U2_CSTATE;                    /* '<Root>/U2' */
} ContinuousStates;

/* State derivatives (auto storage) */
typedef struct {
  real_T U1_CSTATE;                    /* '<Root>/U1' */
  real_T U2_CSTATE;                    /* '<Root>/U2' */
} StateDerivatives;

/* State disabled  */
typedef struct {
  boolean_T U1_CSTATE;                 /* '<Root>/U1' */
  boolean_T U2_CSTATE;                 /* '<Root>/U2' */
} StateDisabled;

/* Invariant block signals (auto storage) */
typedef struct {
  const real_T B_0_4_0;                /* '<Root>/Constant' */
} ConstBlockIO;

/* For easy access from the SimStruct */
#define rtC(S)                         ((ConstBlockIO *) _ssGetConstBlockIO(S))

/* Constant parameters (auto storage) */
typedef struct {
  /* Expression: 0
   * Referenced by blocks:
   * '<Root>/U1'
   * '<Root>/U2'
   */
  real_T pooled1;

  /* Expression: 1
   * '<Root>/Constant'
   */
  real_T Constant_Value;
} ConstParam;

/* External inputs (root inport signals with auto storage) */
typedef struct {
  real_T P1;                           /* '<Root>/B_-1_-1' */
  real_T P2;                           /* '<Root>/B_-1_-1' */
  real_T P3;                           /* '<Root>/B_-1_-1' */
} ExternalUPtrs;

/* External outputs (root outports fed by signals with auto storage) */
typedef struct {
  real_T *B_0_1;                       /* '<Root>/B_0_1' */
  real_T *B_0_3;                       /* '<Root>/B_0_3' */
} ExternalOutputs;

extern ConstBlockIO rtInvariantSignals;/* constant block i/o */

/* Constant parameters (auto storage) */
extern const ConstParam rtConstP;

#endif                                 /* RTW_HEADER_ab_acc_h_ */
