/* Include files */

#include "blascompat32.h"
#include "gustlaw_sfun.h"
#include "c2_gustlaw.h"
#include "mwmathutil.h"
#define CHARTINSTANCE_CHARTNUMBER      (chartInstance.chartNumber)
#define CHARTINSTANCE_INSTANCENUMBER   (chartInstance.instanceNumber)
#include "gustlaw_sfun_debug_macros.h"

/* Type Definitions */

/* Named Constants */
#define c2_IN_NO_ACTIVE_CHILD          (0)

/* Variable Declarations */

/* Variable Definitions */
static SFc2_gustlawInstanceStruct chartInstance;

/* Function Declarations */
static void initialize_c2_gustlaw(void);
static void initialize_params_c2_gustlaw(void);
static void enable_c2_gustlaw(void);
static void disable_c2_gustlaw(void);
static void c2_update_debugger_state_c2_gustlaw(void);
static const mxArray *get_sim_state_c2_gustlaw(void);
static void set_sim_state_c2_gustlaw(const mxArray *c2_st);
static void finalize_c2_gustlaw(void);
static void sf_c2_gustlaw(void);
static void c2_c2_gustlaw(void);
static void init_script_number_translation(uint32_T c2_machineNumber, uint32_T
  c2_chartNumber);
static real_T c2_rdivide(real_T c2_x, real_T c2_y);
static void c2_eml_warning(void);
static real_T c2_mpower(real_T c2_a);
static real_T c2_power(real_T c2_a, real_T c2_b);
static void c2_eml_error(void);
static real_T c2_abs(real_T c2_x);
static void c2_eml_scalar_eg(void);
static real_T c2_ceval_xdot(int32_T c2_n, real_T c2_x[2], int32_T c2_ix0,
  int32_T c2_incx, real_T c2_y[2], int32_T c2_iy0, int32_T c2_incy);
static real_T c2_sign(real_T c2_x);
static real_T c2_b_mpower(real_T c2_a);
static real_T c2_c_mpower(real_T c2_a);
static const mxArray *c2_sf_marshall(void *c2_chartInstance, void *c2_u);
static const mxArray *c2_b_sf_marshall(void *c2_chartInstance, void *c2_u);
static const mxArray *c2_c_sf_marshall(void *c2_chartInstance, void *c2_u);
static void c2_info_helper(c2_ResolvedFunctionInfo c2_info[65]);
static const mxArray *c2_d_sf_marshall(void *c2_chartInstance, void *c2_u);
static void init_dsm_address_info(void);

/* Function Definitions */
static void initialize_c2_gustlaw(void)
{
  _sfTime_ = (real_T)ssGetT(chartInstance.S);
  chartInstance.c2_is_active_c2_gustlaw = 0U;
}

static void initialize_params_c2_gustlaw(void)
{
}

static void enable_c2_gustlaw(void)
{
  _sfTime_ = (real_T)ssGetT(chartInstance.S);
}

static void disable_c2_gustlaw(void)
{
  _sfTime_ = (real_T)ssGetT(chartInstance.S);
}

static void c2_update_debugger_state_c2_gustlaw(void)
{
}

static const mxArray *get_sim_state_c2_gustlaw(void)
{
  const mxArray *c2_st = NULL;
  const mxArray *c2_y = NULL;
  int32_T c2_i0;
  real_T c2_u[4];
  const mxArray *c2_b_y = NULL;
  uint8_T c2_b_u;
  const mxArray *c2_c_y = NULL;
  real_T (*c2_dx)[4];
  c2_dx = (real_T (*)[4])ssGetOutputPortSignal(chartInstance.S, 1);
  c2_st = NULL;
  c2_y = NULL;
  sf_mex_assign(&c2_y, sf_mex_createcellarray(2));
  for (c2_i0 = 0; c2_i0 < 4; c2_i0 = c2_i0 + 1) {
    c2_u[c2_i0] = (*c2_dx)[c2_i0];
  }

  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_u, 0, 0U, 1U, 0U, 1, 4));
  sf_mex_setcell(c2_y, 0, c2_b_y);
  c2_b_u = chartInstance.c2_is_active_c2_gustlaw;
  c2_c_y = NULL;
  sf_mex_assign(&c2_c_y, sf_mex_create("y", &c2_b_u, 3, 0U, 0U, 0U, 0));
  sf_mex_setcell(c2_y, 1, c2_c_y);
  sf_mex_assign(&c2_st, c2_y);
  return c2_st;
}

static void set_sim_state_c2_gustlaw(const mxArray *c2_st)
{
  const mxArray *c2_u;
  const mxArray *c2_dx;
  real_T c2_dv0[4];
  int32_T c2_i1;
  real_T c2_y[4];
  int32_T c2_i2;
  const mxArray *c2_b_is_active_c2_gustlaw;
  uint8_T c2_u0;
  uint8_T c2_b_y;
  real_T (*c2_b_dx)[4];
  c2_b_dx = (real_T (*)[4])ssGetOutputPortSignal(chartInstance.S, 1);
  chartInstance.c2_doneDoubleBufferReInit = true;
  c2_u = sf_mex_dup(c2_st);
  c2_dx = sf_mex_dup(sf_mex_getcell(c2_u, 0));
  sf_mex_import("dx", sf_mex_dup(c2_dx), &c2_dv0, 1, 0, 0U, 1, 0U, 1, 4);
  for (c2_i1 = 0; c2_i1 < 4; c2_i1 = c2_i1 + 1) {
    c2_y[c2_i1] = c2_dv0[c2_i1];
  }

  sf_mex_destroy(&c2_dx);
  for (c2_i2 = 0; c2_i2 < 4; c2_i2 = c2_i2 + 1) {
    (*c2_b_dx)[c2_i2] = c2_y[c2_i2];
  }

  c2_b_is_active_c2_gustlaw = sf_mex_dup(sf_mex_getcell(c2_u, 1));
  sf_mex_import("is_active_c2_gustlaw", sf_mex_dup(c2_b_is_active_c2_gustlaw),
                &c2_u0, 1, 3, 0U, 0, 0U, 0);
  c2_b_y = c2_u0;
  sf_mex_destroy(&c2_b_is_active_c2_gustlaw);
  chartInstance.c2_is_active_c2_gustlaw = c2_b_y;
  sf_mex_destroy(&c2_u);
  c2_update_debugger_state_c2_gustlaw();
  sf_mex_destroy(&c2_st);
}

static void finalize_c2_gustlaw(void)
{
}

static void sf_c2_gustlaw(void)
{
  int32_T c2_i3;
  int32_T c2_i4;
  int32_T c2_previousEvent;
  real_T *c2_V;
  real_T *c2_nlssFlag;
  real_T *c2_nlsdFlag;
  real_T (*c2_dx)[4];
  real_T (*c2_x)[4];
  c2_dx = (real_T (*)[4])ssGetOutputPortSignal(chartInstance.S, 1);
  c2_nlsdFlag = (real_T *)ssGetInputPortSignal(chartInstance.S, 3);
  c2_x = (real_T (*)[4])ssGetInputPortSignal(chartInstance.S, 0);
  c2_nlssFlag = (real_T *)ssGetInputPortSignal(chartInstance.S, 2);
  c2_V = (real_T *)ssGetInputPortSignal(chartInstance.S, 1);
  _sfTime_ = (real_T)ssGetT(chartInstance.S);
  _SFD_CC_CALL(CHART_ENTER_SFUNCTION_TAG,0);
  for (c2_i3 = 0; c2_i3 < 4; c2_i3 = c2_i3 + 1) {
    _SFD_DATA_RANGE_CHECK((*c2_x)[c2_i3], 0U);
  }

  for (c2_i4 = 0; c2_i4 < 4; c2_i4 = c2_i4 + 1) {
    _SFD_DATA_RANGE_CHECK((*c2_dx)[c2_i4], 1U);
  }

  _SFD_DATA_RANGE_CHECK(*c2_V, 2U);
  _SFD_DATA_RANGE_CHECK(*c2_nlssFlag, 3U);
  _SFD_DATA_RANGE_CHECK(*c2_nlsdFlag, 4U);
  c2_previousEvent = _sfEvent_;
  _sfEvent_ = CALL_EVENT;
  c2_c2_gustlaw();
  _sfEvent_ = c2_previousEvent;
  sf_debug_check_for_state_inconsistency(_gustlawMachineNumber_,
    chartInstance.chartNumber, chartInstance.instanceNumber);
}

static void c2_c2_gustlaw(void)
{
  int32_T c2_i5;
  real_T c2_x[4];
  real_T c2_V;
  real_T c2_nlssFlag;
  real_T c2_nlsdFlag;
  real_T c2_nargout = 1.0;
  real_T c2_nargin = 4.0;
  real_T c2_NLSSM[4];
  real_T c2_SSM[4];
  real_T c2_ADM[4];
  real_T c2_ADM22;
  real_T c2_ADM21;
  real_T c2_ADM12;
  real_T c2_ADM11;
  real_T c2_ASM[4];
  real_T c2_ASM22;
  real_T c2_ASM21;
  real_T c2_ASM12;
  real_T c2_ASM11;
  real_T c2_GYRM[4];
  real_T c2_NLSDM[4];
  real_T c2_SDM[4];
  real_T c2_MM[4];
  real_T c2_rho;
  real_T c2_qlag;
  real_T c2_R1PMF;
  real_T c2_CLa;
  real_T c2_ACDir;
  real_T c2_W;
  real_T c2_Ip;
  real_T c2_S;
  real_T c2_R;
  real_T c2_fpz;
  real_T c2_fpy;
  real_T c2_gzNL;
  real_T c2_gyNL;
  real_T c2_KzNL;
  real_T c2_KyNL;
  real_T c2_azNL;
  real_T c2_ayNL;
  real_T c2_DzNL;
  real_T c2_DyNL;
  real_T c2_L;
  real_T c2_Kz;
  real_T c2_Ky;
  real_T c2_zz;
  real_T c2_zy;
  real_T c2_M;
  real_T c2_dx[4];
  int32_T c2_i6;
  int32_T c2_i7;
  static real_T c2_dv1[4] = { 5.8094750193111250E-002, 0.0, 0.0,
    3.1622776601683798E-002 };

  int32_T c2_i8;
  static real_T c2_dv2[4] = { 0.029, 0.0, 0.0, 0.0158 };

  int32_T c2_i9;
  static real_T c2_dv3[4] = { 0.0, 4.1887902047863912E-001,
    -4.1887902047863912E-001, 0.0 };

  real_T c2_b;
  real_T c2_y;
  real_T c2_b_ASM11[4];
  int32_T c2_i10;
  int32_T c2_i11;
  int32_T c2_i12;
  real_T c2_b_b;
  real_T c2_b_y;
  real_T c2_b_ADM11[4];
  int32_T c2_i13;
  int32_T c2_i14;
  int32_T c2_i15;
  int32_T c2_i16;
  static real_T c2_dv4[4] = { 15.0, 0.0, 0.0, 10.0 };

  int32_T c2_i17;
  static real_T c2_dv5[4] = { 7.5, 0.0, 0.0, 5.0 };

  real_T c2_a;
  int32_T c2_i18;
  real_T c2_b_a[2];
  real_T c2_c_a;
  real_T c2_c_b;
  real_T c2_c_y;
  real_T c2_d_b[2];
  int32_T c2_i19;
  real_T c2_b_x[2];
  int32_T c2_i20;
  real_T c2_d_y[2];
  int32_T c2_i21;
  real_T c2_c_x[2];
  int32_T c2_i22;
  real_T c2_e_y[2];
  int32_T c2_i23;
  real_T c2_d_x[2];
  int32_T c2_i24;
  real_T c2_f_y[2];
  int32_T c2_i25;
  real_T c2_e_x[2];
  int32_T c2_i26;
  real_T c2_g_y[2];
  real_T c2_h_y;
  int32_T c2_i27;
  int32_T c2_i28;
  real_T c2_d_a[2];
  real_T c2_e_b[2];
  int32_T c2_i29;
  real_T c2_f_x[2];
  int32_T c2_i30;
  real_T c2_i_y[2];
  int32_T c2_i31;
  real_T c2_g_x[2];
  int32_T c2_i32;
  real_T c2_j_y[2];
  int32_T c2_i33;
  real_T c2_h_x[2];
  int32_T c2_i34;
  real_T c2_k_y[2];
  int32_T c2_i35;
  real_T c2_i_x[2];
  int32_T c2_i36;
  real_T c2_l_y[2];
  real_T c2_m_y;
  real_T c2_e_a;
  int32_T c2_i37;
  real_T c2_f_a[2];
  real_T c2_g_a;
  real_T c2_f_b;
  real_T c2_n_y;
  real_T c2_g_b[2];
  int32_T c2_i38;
  real_T c2_j_x[2];
  int32_T c2_i39;
  real_T c2_o_y[2];
  int32_T c2_i40;
  real_T c2_k_x[2];
  int32_T c2_i41;
  real_T c2_p_y[2];
  int32_T c2_i42;
  real_T c2_l_x[2];
  int32_T c2_i43;
  real_T c2_q_y[2];
  int32_T c2_i44;
  real_T c2_m_x[2];
  int32_T c2_i45;
  real_T c2_r_y[2];
  real_T c2_s_y;
  int32_T c2_i46;
  int32_T c2_i47;
  real_T c2_h_a[2];
  real_T c2_h_b[2];
  int32_T c2_i48;
  real_T c2_n_x[2];
  int32_T c2_i49;
  real_T c2_t_y[2];
  int32_T c2_i50;
  real_T c2_o_x[2];
  int32_T c2_i51;
  real_T c2_u_y[2];
  int32_T c2_i52;
  real_T c2_p_x[2];
  int32_T c2_i53;
  real_T c2_v_y[2];
  int32_T c2_i54;
  real_T c2_q_x[2];
  int32_T c2_i55;
  real_T c2_w_y[2];
  real_T c2_x_y;
  real_T c2_i_a;
  int32_T c2_i56;
  real_T c2_j_a[2];
  real_T c2_k_a;
  real_T c2_i_b;
  real_T c2_y_y;
  real_T c2_j_b[2];
  int32_T c2_i57;
  real_T c2_r_x[2];
  int32_T c2_i58;
  real_T c2_ab_y[2];
  int32_T c2_i59;
  real_T c2_s_x[2];
  int32_T c2_i60;
  real_T c2_bb_y[2];
  int32_T c2_i61;
  real_T c2_t_x[2];
  int32_T c2_i62;
  real_T c2_cb_y[2];
  int32_T c2_i63;
  real_T c2_u_x[2];
  int32_T c2_i64;
  real_T c2_db_y[2];
  real_T c2_eb_y;
  int32_T c2_i65;
  int32_T c2_i66;
  real_T c2_l_a[2];
  real_T c2_k_b[2];
  int32_T c2_i67;
  real_T c2_v_x[2];
  int32_T c2_i68;
  real_T c2_fb_y[2];
  int32_T c2_i69;
  real_T c2_w_x[2];
  int32_T c2_i70;
  real_T c2_gb_y[2];
  int32_T c2_i71;
  real_T c2_x_x[2];
  int32_T c2_i72;
  real_T c2_hb_y[2];
  int32_T c2_i73;
  real_T c2_y_x[2];
  int32_T c2_i74;
  real_T c2_ib_y[2];
  real_T c2_jb_y;
  real_T c2_m_a;
  int32_T c2_i75;
  real_T c2_n_a[2];
  real_T c2_o_a;
  real_T c2_l_b;
  real_T c2_kb_y;
  real_T c2_m_b[2];
  int32_T c2_i76;
  real_T c2_ab_x[2];
  int32_T c2_i77;
  real_T c2_lb_y[2];
  int32_T c2_i78;
  real_T c2_bb_x[2];
  int32_T c2_i79;
  real_T c2_mb_y[2];
  int32_T c2_i80;
  real_T c2_cb_x[2];
  int32_T c2_i81;
  real_T c2_nb_y[2];
  int32_T c2_i82;
  real_T c2_db_x[2];
  int32_T c2_i83;
  real_T c2_ob_y[2];
  real_T c2_pb_y;
  int32_T c2_i84;
  int32_T c2_i85;
  real_T c2_p_a[2];
  real_T c2_n_b[2];
  int32_T c2_i86;
  real_T c2_eb_x[2];
  int32_T c2_i87;
  real_T c2_qb_y[2];
  int32_T c2_i88;
  real_T c2_fb_x[2];
  int32_T c2_i89;
  real_T c2_rb_y[2];
  int32_T c2_i90;
  real_T c2_gb_x[2];
  int32_T c2_i91;
  real_T c2_sb_y[2];
  int32_T c2_i92;
  real_T c2_hb_x[2];
  int32_T c2_i93;
  real_T c2_tb_y[2];
  real_T c2_ub_y;
  real_T c2_ib_x;
  real_T c2_jb_x;
  real_T c2_kb_x;
  real_T c2_z;
  int32_T c2_i94;
  real_T *c2_b_V;
  real_T *c2_b_nlssFlag;
  real_T *c2_b_nlsdFlag;
  real_T (*c2_b_dx)[4];
  real_T (*c2_lb_x)[4];
  c2_b_dx = (real_T (*)[4])ssGetOutputPortSignal(chartInstance.S, 1);
  c2_b_nlsdFlag = (real_T *)ssGetInputPortSignal(chartInstance.S, 3);
  c2_lb_x = (real_T (*)[4])ssGetInputPortSignal(chartInstance.S, 0);
  c2_b_nlssFlag = (real_T *)ssGetInputPortSignal(chartInstance.S, 2);
  c2_b_V = (real_T *)ssGetInputPortSignal(chartInstance.S, 1);
  _SFD_CC_CALL(CHART_ENTER_DURING_FUNCTION_TAG,0);
  for (c2_i5 = 0; c2_i5 < 4; c2_i5 = c2_i5 + 1) {
    c2_x[c2_i5] = (*c2_lb_x)[c2_i5];
  }

  c2_V = *c2_b_V;
  c2_nlssFlag = *c2_b_nlssFlag;
  c2_nlsdFlag = *c2_b_nlsdFlag;
  sf_debug_symbol_scope_push(48U, 0U);
  sf_debug_symbol_scope_add("nargout", &c2_nargout, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("nargin", &c2_nargin, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("NLSSM", &c2_NLSSM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("SSM", &c2_SSM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("ADM", &c2_ADM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("ADM22", &c2_ADM22, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ADM21", &c2_ADM21, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ADM12", &c2_ADM12, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ADM11", &c2_ADM11, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ASM", &c2_ASM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("ASM22", &c2_ASM22, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ASM21", &c2_ASM21, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ASM12", &c2_ASM12, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ASM11", &c2_ASM11, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("GYRM", &c2_GYRM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("NLSDM", &c2_NLSDM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("SDM", &c2_SDM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("MM", &c2_MM, c2_c_sf_marshall);
  sf_debug_symbol_scope_add("rho", &c2_rho, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("qlag", &c2_qlag, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("R1PMF", &c2_R1PMF, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("CLa", &c2_CLa, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ACDir", &c2_ACDir, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("W", &c2_W, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("Ip", &c2_Ip, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("S", &c2_S, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("R", &c2_R, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("fpz", &c2_fpz, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("fpy", &c2_fpy, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("gzNL", &c2_gzNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("gyNL", &c2_gyNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("KzNL", &c2_KzNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("KyNL", &c2_KyNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("azNL", &c2_azNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("ayNL", &c2_ayNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("DzNL", &c2_DzNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("DyNL", &c2_DyNL, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("L", &c2_L, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("Kz", &c2_Kz, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("Ky", &c2_Ky, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("zz", &c2_zz, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("zy", &c2_zy, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("M", &c2_M, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("dx", &c2_dx, c2_sf_marshall);
  sf_debug_symbol_scope_add("nlsdFlag", &c2_nlsdFlag, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("nlssFlag", &c2_nlssFlag, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("V", &c2_V, c2_b_sf_marshall);
  sf_debug_symbol_scope_add("x", &c2_x, c2_sf_marshall);
  CV_EML_FCN(0, 0);
  _SFD_EML_CALL(0,3);
  for (c2_i6 = 0; c2_i6 < 4; c2_i6 = c2_i6 + 1) {
    c2_dx[c2_i6] = 0.0;
  }

  /* 	Structural and Mass Properties		 */
  _SFD_EML_CALL(0,7);
  c2_M = 0.25;

  /* 	Mass, M (Kg)		 */
  _SFD_EML_CALL(0,8);
  c2_zy = 1.5;

  /* 	Lateral Structural Damping, zy (% Critical)		 */
  _SFD_EML_CALL(0,9);
  c2_zz = 1.0;

  /* 	Vertical Structural Damping, zz (% Critical)		 */
  _SFD_EML_CALL(0,10);
  c2_Ky = 15.0;

  /* 	Lateral Structural Stiffness, Ky (N/m)		 */
  _SFD_EML_CALL(0,11);
  c2_Kz = 10.0;

  /* 	Vertical Structural Stiffness, Kz (N/m)		 */
  _SFD_EML_CALL(0,12);
  c2_L = 0.25;

  /* 	Rod Length, L (m)		 */
  /* 	Non-Linear Damping Definition		 */
  _SFD_EML_CALL(0,17);
  c2_DyNL = 0.029;

  /* 	Lateral non-linear damping constant, DyNL		 */
  _SFD_EML_CALL(0,18);
  c2_DzNL = 0.0158;

  /* 	Vertical non-linear damping constant, DzNL		 */
  _SFD_EML_CALL(0,19);
  c2_ayNL = 1.0;

  /* 	Lateral non-linear damping exponent ayNL		 */
  _SFD_EML_CALL(0,20);
  c2_azNL = 1.0;

  /* 	Vertical non-linear damping exponent azNL		 */
  /* 	Non-Linear Stiffness Definition		 */
  _SFD_EML_CALL(0,25);
  c2_KyNL = 7.5;

  /* 	Lateral non-linear stiffness constant, KyNL		 */
  _SFD_EML_CALL(0,26);
  c2_KzNL = 5.0;

  /* 	Vertical non-linear stiffness constant, KzNL		 */
  _SFD_EML_CALL(0,27);
  c2_gyNL = 1.5;

  /* 	Lateral non-linear stiffness exponent gyNL		 */
  _SFD_EML_CALL(0,28);
  c2_gzNL = 1.5;

  /* 	Vertical non-linear stiffness exponent gzNL		 */
  /* 	Free Play Definition		 */
  _SFD_EML_CALL(0,33);
  c2_fpy = 0.0;

  /* 	Free play range in y direction from origin (m)		 */
  _SFD_EML_CALL(0,34);
  c2_fpz = 0.0;

  /* 	Free play range in z direction from origin (m)		 */
  /* 	Propeller Properties		 */
  _SFD_EML_CALL(0,39);
  c2_R = 0.1;

  /* 	Radius, R (m)		 */
  _SFD_EML_CALL(0,40);
  c2_S = 0.0314;

  /* 	Reference Area, S ( = pR2, m2)		 */
  _SFD_EML_CALL(0,41);
  c2_Ip = 0.0025;

  /* 	Polar Moment of Inertia, Ip (Kgm2)		 */
  _SFD_EML_CALL(0,42);
  c2_W = 100.0;

  /* 	Propeller Speed, W (RPM)		 */
  _SFD_EML_CALL(0,43);
  c2_ACDir = 1.0;

  /* 	Direction, CW=1 or ACW=-1; (in error default is CW)		 */
  /*    Propeller Aerodynamic Properties (1P)		 */
  _SFD_EML_CALL(0,48);
  c2_CLa = 0.7;

  /* 	Force per Unit Incidence Coefficient, CLa (1/rad)		 */
  _SFD_EML_CALL(0,49);
  c2_R1PMF = 0.4;

  /* 	1P Moment to 1P Force Ratio per Unit Radius, R1PMF/R		 */
  _SFD_EML_CALL(0,50);
  c2_qlag = 20.0;

  /* 	Aerodynamic Lag Angle, qlag (degs)		 */
  /*  Airflow */
  _SFD_EML_CALL(0,53);
  c2_rho = 1.225;

  /*  Air Density, r (Kg/m3) */
  /*  Mass Matrix */
  _SFD_EML_CALL(0,56);
  c2_MM[0] = c2_M;
  c2_MM[2] = 0.0;
  c2_MM[1] = 0.0;
  c2_MM[3] = c2_M;

  /*  Structural Damping Matrix */
  _SFD_EML_CALL(0,59);
  for (c2_i7 = 0; c2_i7 < 4; c2_i7 = c2_i7 + 1) {
    c2_SDM[c2_i7] = c2_dv1[c2_i7];
  }

  /*  Nonlinear Structural damping Matrix */
  /*  NLSDM=[(DyNL/abs(DyNL))*abs(DyNL)^ayNL,0;0,(DzNL/abs(DzNL))*abs(DzNL)^azNL]; */
  _SFD_EML_CALL(0,63);
  for (c2_i8 = 0; c2_i8 < 4; c2_i8 = c2_i8 + 1) {
    c2_NLSDM[c2_i8] = c2_dv2[c2_i8];
  }

  /*  Gyroscopic Matrix */
  _SFD_EML_CALL(0,66);
  for (c2_i9 = 0; c2_i9 < 4; c2_i9 = c2_i9 + 1) {
    c2_GYRM[c2_i9] = c2_dv3[c2_i9];
  }

  /*  Aerodynamic Stiffness Matrix */
  _SFD_EML_CALL(0,69);
  c2_ASM11 = -1.0547232229321071E+000;
  _SFD_EML_CALL(0,70);
  c2_ASM12 = -5.0202014332566880E-001;
  _SFD_EML_CALL(0,71);
  c2_ASM21 = 5.0202014332566880E-001;
  _SFD_EML_CALL(0,72);
  c2_ASM22 = -1.0547232229321071E+000;
  _SFD_EML_CALL(0,74);
  c2_b = c2_mpower(c2_V);
  c2_y = 5.3850999999999996E-002 * c2_b;
  c2_b_ASM11[0] = c2_ASM11;
  c2_b_ASM11[2] = c2_ASM12;
  c2_b_ASM11[1] = c2_ASM21;
  c2_b_ASM11[3] = c2_ASM22;
  c2_i10 = 0;
  for (c2_i11 = 0; c2_i11 < 2; c2_i11 = c2_i11 + 1) {
    for (c2_i12 = 0; c2_i12 < 2; c2_i12 = c2_i12 + 1) {
      c2_ASM[c2_i12 + c2_i10] = c2_y * c2_b_ASM11[c2_i12 + c2_i10];
    }

    c2_i10 = c2_i10 + 2;
  }

  /*  Aerodynamic Damping Matrix */
  _SFD_EML_CALL(0,77);
  c2_ADM11 = 2.6368080573302677E-001;
  _SFD_EML_CALL(0,78);
  c2_ADM12 = 1.2550503583141720E-001;
  _SFD_EML_CALL(0,79);
  c2_ADM21 = -1.2550503583141720E-001;
  _SFD_EML_CALL(0,80);
  c2_ADM22 = 2.6368080573302677E-001;
  _SFD_EML_CALL(0,82);
  c2_b_b = c2_V;
  c2_b_y = 5.3850999999999996E-002 * c2_b_b;
  c2_b_ADM11[0] = c2_ADM11;
  c2_b_ADM11[2] = c2_ADM12;
  c2_b_ADM11[1] = c2_ADM21;
  c2_b_ADM11[3] = c2_ADM22;
  c2_i13 = 0;
  for (c2_i14 = 0; c2_i14 < 2; c2_i14 = c2_i14 + 1) {
    for (c2_i15 = 0; c2_i15 < 2; c2_i15 = c2_i15 + 1) {
      c2_ADM[c2_i15 + c2_i13] = c2_b_y * c2_b_ADM11[c2_i15 + c2_i13];
    }

    c2_i13 = c2_i13 + 2;
  }

  /*  Structural Stiffnes Matrix */
  _SFD_EML_CALL(0,85);
  for (c2_i16 = 0; c2_i16 < 4; c2_i16 = c2_i16 + 1) {
    c2_SSM[c2_i16] = c2_dv4[c2_i16];
  }

  /*  Nonlinear Structural Stiffness Matrix */
  /*  NLSSM=[(KyNL/abs(KyNL))*abs(KyNL)^gyNL,0;0,(KzNL/abs(KzNL))*abs(KzNL)^gzNL]; */
  _SFD_EML_CALL(0,89);
  for (c2_i17 = 0; c2_i17 < 4; c2_i17 = c2_i17 + 1) {
    c2_NLSSM[c2_i17] = c2_dv5[c2_i17];
  }

  /* -------------------------------------------------------------------------- */
  /*  Equations of Motion */
  /* -------------------------------------------------------------------------- */
  _SFD_EML_CALL(0,95);
  c2_dx[0] = c2_x[1];
  _SFD_EML_CALL(0,96);
  c2_a = c2_nlssFlag;
  for (c2_i18 = 0; c2_i18 < 2; c2_i18 = c2_i18 + 1) {
    c2_b_a[c2_i18] = c2_a * (7.5 + -7.5 * (real_T)c2_i18);
  }

  c2_c_a = c2_sign(c2_x[0]);
  c2_c_b = c2_c_mpower(c2_abs(c2_x[0]));
  c2_c_y = c2_c_a * c2_c_b;
  c2_d_b[0] = c2_c_y;
  c2_d_b[1] = 0.0;
  c2_eml_scalar_eg();
  for (c2_i19 = 0; c2_i19 < 2; c2_i19 = c2_i19 + 1) {
    c2_b_x[c2_i19] = c2_b_a[c2_i19];
  }

  for (c2_i20 = 0; c2_i20 < 2; c2_i20 = c2_i20 + 1) {
    c2_d_y[c2_i20] = c2_d_b[c2_i20];
  }

  for (c2_i21 = 0; c2_i21 < 2; c2_i21 = c2_i21 + 1) {
    c2_c_x[c2_i21] = c2_b_x[c2_i21];
  }

  for (c2_i22 = 0; c2_i22 < 2; c2_i22 = c2_i22 + 1) {
    c2_e_y[c2_i22] = c2_d_y[c2_i22];
  }

  for (c2_i23 = 0; c2_i23 < 2; c2_i23 = c2_i23 + 1) {
    c2_d_x[c2_i23] = c2_c_x[c2_i23];
  }

  for (c2_i24 = 0; c2_i24 < 2; c2_i24 = c2_i24 + 1) {
    c2_f_y[c2_i24] = c2_e_y[c2_i24];
  }

  for (c2_i25 = 0; c2_i25 < 2; c2_i25 = c2_i25 + 1) {
    c2_e_x[c2_i25] = c2_d_x[c2_i25];
  }

  for (c2_i26 = 0; c2_i26 < 2; c2_i26 = c2_i26 + 1) {
    c2_g_y[c2_i26] = c2_f_y[c2_i26];
  }

  c2_h_y = c2_ceval_xdot(2, c2_e_x, 1, 1, c2_g_y, 1, 1);
  c2_i27 = 0;
  for (c2_i28 = 0; c2_i28 < 2; c2_i28 = c2_i28 + 1) {
    c2_d_a[c2_i28] = c2_SSM[c2_i27] + c2_ASM[c2_i27];
    c2_i27 = c2_i27 + 2;
  }

  c2_e_b[0] = c2_x[0];
  c2_e_b[1] = c2_x[2];
  c2_eml_scalar_eg();
  for (c2_i29 = 0; c2_i29 < 2; c2_i29 = c2_i29 + 1) {
    c2_f_x[c2_i29] = c2_d_a[c2_i29];
  }

  for (c2_i30 = 0; c2_i30 < 2; c2_i30 = c2_i30 + 1) {
    c2_i_y[c2_i30] = c2_e_b[c2_i30];
  }

  for (c2_i31 = 0; c2_i31 < 2; c2_i31 = c2_i31 + 1) {
    c2_g_x[c2_i31] = c2_f_x[c2_i31];
  }

  for (c2_i32 = 0; c2_i32 < 2; c2_i32 = c2_i32 + 1) {
    c2_j_y[c2_i32] = c2_i_y[c2_i32];
  }

  for (c2_i33 = 0; c2_i33 < 2; c2_i33 = c2_i33 + 1) {
    c2_h_x[c2_i33] = c2_g_x[c2_i33];
  }

  for (c2_i34 = 0; c2_i34 < 2; c2_i34 = c2_i34 + 1) {
    c2_k_y[c2_i34] = c2_j_y[c2_i34];
  }

  for (c2_i35 = 0; c2_i35 < 2; c2_i35 = c2_i35 + 1) {
    c2_i_x[c2_i35] = c2_h_x[c2_i35];
  }

  for (c2_i36 = 0; c2_i36 < 2; c2_i36 = c2_i36 + 1) {
    c2_l_y[c2_i36] = c2_k_y[c2_i36];
  }

  c2_m_y = c2_ceval_xdot(2, c2_i_x, 1, 1, c2_l_y, 1, 1);
  c2_e_a = c2_nlsdFlag;
  for (c2_i37 = 0; c2_i37 < 2; c2_i37 = c2_i37 + 1) {
    c2_f_a[c2_i37] = c2_e_a * (0.029 + -0.029 * (real_T)c2_i37);
  }

  c2_g_a = c2_sign(c2_x[1]);
  c2_f_b = c2_b_mpower(c2_abs(c2_x[1]));
  c2_n_y = c2_g_a * c2_f_b;
  c2_g_b[0] = c2_n_y;
  c2_g_b[1] = 0.0;
  c2_eml_scalar_eg();
  for (c2_i38 = 0; c2_i38 < 2; c2_i38 = c2_i38 + 1) {
    c2_j_x[c2_i38] = c2_f_a[c2_i38];
  }

  for (c2_i39 = 0; c2_i39 < 2; c2_i39 = c2_i39 + 1) {
    c2_o_y[c2_i39] = c2_g_b[c2_i39];
  }

  for (c2_i40 = 0; c2_i40 < 2; c2_i40 = c2_i40 + 1) {
    c2_k_x[c2_i40] = c2_j_x[c2_i40];
  }

  for (c2_i41 = 0; c2_i41 < 2; c2_i41 = c2_i41 + 1) {
    c2_p_y[c2_i41] = c2_o_y[c2_i41];
  }

  for (c2_i42 = 0; c2_i42 < 2; c2_i42 = c2_i42 + 1) {
    c2_l_x[c2_i42] = c2_k_x[c2_i42];
  }

  for (c2_i43 = 0; c2_i43 < 2; c2_i43 = c2_i43 + 1) {
    c2_q_y[c2_i43] = c2_p_y[c2_i43];
  }

  for (c2_i44 = 0; c2_i44 < 2; c2_i44 = c2_i44 + 1) {
    c2_m_x[c2_i44] = c2_l_x[c2_i44];
  }

  for (c2_i45 = 0; c2_i45 < 2; c2_i45 = c2_i45 + 1) {
    c2_r_y[c2_i45] = c2_q_y[c2_i45];
  }

  c2_s_y = c2_ceval_xdot(2, c2_m_x, 1, 1, c2_r_y, 1, 1);
  c2_i46 = 0;
  for (c2_i47 = 0; c2_i47 < 2; c2_i47 = c2_i47 + 1) {
    c2_h_a[c2_i47] = (c2_SDM[c2_i46] + c2_ADM[c2_i46]) + c2_GYRM[c2_i46];
    c2_i46 = c2_i46 + 2;
  }

  c2_h_b[0] = c2_x[1];
  c2_h_b[1] = c2_x[3];
  c2_eml_scalar_eg();
  for (c2_i48 = 0; c2_i48 < 2; c2_i48 = c2_i48 + 1) {
    c2_n_x[c2_i48] = c2_h_a[c2_i48];
  }

  for (c2_i49 = 0; c2_i49 < 2; c2_i49 = c2_i49 + 1) {
    c2_t_y[c2_i49] = c2_h_b[c2_i49];
  }

  for (c2_i50 = 0; c2_i50 < 2; c2_i50 = c2_i50 + 1) {
    c2_o_x[c2_i50] = c2_n_x[c2_i50];
  }

  for (c2_i51 = 0; c2_i51 < 2; c2_i51 = c2_i51 + 1) {
    c2_u_y[c2_i51] = c2_t_y[c2_i51];
  }

  for (c2_i52 = 0; c2_i52 < 2; c2_i52 = c2_i52 + 1) {
    c2_p_x[c2_i52] = c2_o_x[c2_i52];
  }

  for (c2_i53 = 0; c2_i53 < 2; c2_i53 = c2_i53 + 1) {
    c2_v_y[c2_i53] = c2_u_y[c2_i53];
  }

  for (c2_i54 = 0; c2_i54 < 2; c2_i54 = c2_i54 + 1) {
    c2_q_x[c2_i54] = c2_p_x[c2_i54];
  }

  for (c2_i55 = 0; c2_i55 < 2; c2_i55 = c2_i55 + 1) {
    c2_w_y[c2_i55] = c2_v_y[c2_i55];
  }

  c2_x_y = c2_ceval_xdot(2, c2_q_x, 1, 1, c2_w_y, 1, 1);
  c2_dx[1] = c2_rdivide(-(((c2_x_y + c2_s_y) + c2_m_y) + c2_h_y), 0.25);
  _SFD_EML_CALL(0,100);
  c2_dx[2] = c2_x[3];
  _SFD_EML_CALL(0,101);
  c2_i_a = c2_nlssFlag;
  for (c2_i56 = 0; c2_i56 < 2; c2_i56 = c2_i56 + 1) {
    c2_j_a[c2_i56] = c2_i_a * (5.0 * (real_T)c2_i56);
  }

  c2_k_a = c2_sign(c2_x[2]);
  c2_i_b = c2_c_mpower(c2_abs(c2_x[2]));
  c2_y_y = c2_k_a * c2_i_b;
  c2_j_b[0] = 0.0;
  c2_j_b[1] = c2_y_y;
  c2_eml_scalar_eg();
  for (c2_i57 = 0; c2_i57 < 2; c2_i57 = c2_i57 + 1) {
    c2_r_x[c2_i57] = c2_j_a[c2_i57];
  }

  for (c2_i58 = 0; c2_i58 < 2; c2_i58 = c2_i58 + 1) {
    c2_ab_y[c2_i58] = c2_j_b[c2_i58];
  }

  for (c2_i59 = 0; c2_i59 < 2; c2_i59 = c2_i59 + 1) {
    c2_s_x[c2_i59] = c2_r_x[c2_i59];
  }

  for (c2_i60 = 0; c2_i60 < 2; c2_i60 = c2_i60 + 1) {
    c2_bb_y[c2_i60] = c2_ab_y[c2_i60];
  }

  for (c2_i61 = 0; c2_i61 < 2; c2_i61 = c2_i61 + 1) {
    c2_t_x[c2_i61] = c2_s_x[c2_i61];
  }

  for (c2_i62 = 0; c2_i62 < 2; c2_i62 = c2_i62 + 1) {
    c2_cb_y[c2_i62] = c2_bb_y[c2_i62];
  }

  for (c2_i63 = 0; c2_i63 < 2; c2_i63 = c2_i63 + 1) {
    c2_u_x[c2_i63] = c2_t_x[c2_i63];
  }

  for (c2_i64 = 0; c2_i64 < 2; c2_i64 = c2_i64 + 1) {
    c2_db_y[c2_i64] = c2_cb_y[c2_i64];
  }

  c2_eb_y = c2_ceval_xdot(2, c2_u_x, 1, 1, c2_db_y, 1, 1);
  c2_i65 = 0;
  for (c2_i66 = 0; c2_i66 < 2; c2_i66 = c2_i66 + 1) {
    c2_l_a[c2_i66] = c2_SSM[c2_i65 + 1] + c2_ASM[c2_i65 + 1];
    c2_i65 = c2_i65 + 2;
  }

  c2_k_b[0] = c2_x[0];
  c2_k_b[1] = c2_x[2];
  c2_eml_scalar_eg();
  for (c2_i67 = 0; c2_i67 < 2; c2_i67 = c2_i67 + 1) {
    c2_v_x[c2_i67] = c2_l_a[c2_i67];
  }

  for (c2_i68 = 0; c2_i68 < 2; c2_i68 = c2_i68 + 1) {
    c2_fb_y[c2_i68] = c2_k_b[c2_i68];
  }

  for (c2_i69 = 0; c2_i69 < 2; c2_i69 = c2_i69 + 1) {
    c2_w_x[c2_i69] = c2_v_x[c2_i69];
  }

  for (c2_i70 = 0; c2_i70 < 2; c2_i70 = c2_i70 + 1) {
    c2_gb_y[c2_i70] = c2_fb_y[c2_i70];
  }

  for (c2_i71 = 0; c2_i71 < 2; c2_i71 = c2_i71 + 1) {
    c2_x_x[c2_i71] = c2_w_x[c2_i71];
  }

  for (c2_i72 = 0; c2_i72 < 2; c2_i72 = c2_i72 + 1) {
    c2_hb_y[c2_i72] = c2_gb_y[c2_i72];
  }

  for (c2_i73 = 0; c2_i73 < 2; c2_i73 = c2_i73 + 1) {
    c2_y_x[c2_i73] = c2_x_x[c2_i73];
  }

  for (c2_i74 = 0; c2_i74 < 2; c2_i74 = c2_i74 + 1) {
    c2_ib_y[c2_i74] = c2_hb_y[c2_i74];
  }

  c2_jb_y = c2_ceval_xdot(2, c2_y_x, 1, 1, c2_ib_y, 1, 1);
  c2_m_a = c2_nlsdFlag;
  for (c2_i75 = 0; c2_i75 < 2; c2_i75 = c2_i75 + 1) {
    c2_n_a[c2_i75] = c2_m_a * (0.0158 * (real_T)c2_i75);
  }

  c2_o_a = c2_sign(c2_x[3]);
  c2_l_b = c2_b_mpower(c2_abs(c2_x[3]));
  c2_kb_y = c2_o_a * c2_l_b;
  c2_m_b[0] = 0.0;
  c2_m_b[1] = c2_kb_y;
  c2_eml_scalar_eg();
  for (c2_i76 = 0; c2_i76 < 2; c2_i76 = c2_i76 + 1) {
    c2_ab_x[c2_i76] = c2_n_a[c2_i76];
  }

  for (c2_i77 = 0; c2_i77 < 2; c2_i77 = c2_i77 + 1) {
    c2_lb_y[c2_i77] = c2_m_b[c2_i77];
  }

  for (c2_i78 = 0; c2_i78 < 2; c2_i78 = c2_i78 + 1) {
    c2_bb_x[c2_i78] = c2_ab_x[c2_i78];
  }

  for (c2_i79 = 0; c2_i79 < 2; c2_i79 = c2_i79 + 1) {
    c2_mb_y[c2_i79] = c2_lb_y[c2_i79];
  }

  for (c2_i80 = 0; c2_i80 < 2; c2_i80 = c2_i80 + 1) {
    c2_cb_x[c2_i80] = c2_bb_x[c2_i80];
  }

  for (c2_i81 = 0; c2_i81 < 2; c2_i81 = c2_i81 + 1) {
    c2_nb_y[c2_i81] = c2_mb_y[c2_i81];
  }

  for (c2_i82 = 0; c2_i82 < 2; c2_i82 = c2_i82 + 1) {
    c2_db_x[c2_i82] = c2_cb_x[c2_i82];
  }

  for (c2_i83 = 0; c2_i83 < 2; c2_i83 = c2_i83 + 1) {
    c2_ob_y[c2_i83] = c2_nb_y[c2_i83];
  }

  c2_pb_y = c2_ceval_xdot(2, c2_db_x, 1, 1, c2_ob_y, 1, 1);
  c2_i84 = 0;
  for (c2_i85 = 0; c2_i85 < 2; c2_i85 = c2_i85 + 1) {
    c2_p_a[c2_i85] = (c2_SDM[c2_i84 + 1] + c2_ADM[c2_i84 + 1]) + c2_GYRM[c2_i84
      + 1];
    c2_i84 = c2_i84 + 2;
  }

  c2_n_b[0] = c2_x[1];
  c2_n_b[1] = c2_x[3];
  c2_eml_scalar_eg();
  for (c2_i86 = 0; c2_i86 < 2; c2_i86 = c2_i86 + 1) {
    c2_eb_x[c2_i86] = c2_p_a[c2_i86];
  }

  for (c2_i87 = 0; c2_i87 < 2; c2_i87 = c2_i87 + 1) {
    c2_qb_y[c2_i87] = c2_n_b[c2_i87];
  }

  for (c2_i88 = 0; c2_i88 < 2; c2_i88 = c2_i88 + 1) {
    c2_fb_x[c2_i88] = c2_eb_x[c2_i88];
  }

  for (c2_i89 = 0; c2_i89 < 2; c2_i89 = c2_i89 + 1) {
    c2_rb_y[c2_i89] = c2_qb_y[c2_i89];
  }

  for (c2_i90 = 0; c2_i90 < 2; c2_i90 = c2_i90 + 1) {
    c2_gb_x[c2_i90] = c2_fb_x[c2_i90];
  }

  for (c2_i91 = 0; c2_i91 < 2; c2_i91 = c2_i91 + 1) {
    c2_sb_y[c2_i91] = c2_rb_y[c2_i91];
  }

  for (c2_i92 = 0; c2_i92 < 2; c2_i92 = c2_i92 + 1) {
    c2_hb_x[c2_i92] = c2_gb_x[c2_i92];
  }

  for (c2_i93 = 0; c2_i93 < 2; c2_i93 = c2_i93 + 1) {
    c2_tb_y[c2_i93] = c2_sb_y[c2_i93];
  }

  c2_ub_y = c2_ceval_xdot(2, c2_hb_x, 1, 1, c2_tb_y, 1, 1);
  c2_ib_x = -(((c2_ub_y + c2_pb_y) + c2_jb_y) + c2_eb_y);
  c2_jb_x = c2_ib_x;
  c2_kb_x = c2_jb_x;
  c2_z = c2_kb_x / 0.25;
  c2_dx[3] = c2_z;
  _SFD_EML_CALL(0,-101);
  sf_debug_symbol_scope_pop();
  for (c2_i94 = 0; c2_i94 < 4; c2_i94 = c2_i94 + 1) {
    (*c2_b_dx)[c2_i94] = c2_dx[c2_i94];
  }

  _SFD_CC_CALL(EXIT_OUT_OF_FUNCTION_TAG,0);
}

static void init_script_number_translation(uint32_T c2_machineNumber, uint32_T
  c2_chartNumber)
{
}

static real_T c2_rdivide(real_T c2_x, real_T c2_y)
{
  real_T c2_b_x;
  real_T c2_b_y;
  real_T c2_c_x;
  real_T c2_c_y;
  if (c2_y == 0.0) {
    c2_eml_warning();
  }

  c2_b_x = c2_x;
  c2_b_y = c2_y;
  c2_c_x = c2_b_x;
  c2_c_y = c2_b_y;
  return c2_c_x / c2_c_y;
}

static void c2_eml_warning(void)
{
  int32_T c2_i95;
  static char_T c2_cv0[15] = { 'D', 'i', 'v', 'i', 'd', 'e', ' ', 'b', 'y', ' ',
    'z', 'e', 'r', 'o', '.' };

  char_T c2_u[15];
  const mxArray *c2_y = NULL;
  int32_T c2_i96;
  static char_T c2_cv1[19] = { 'M', 'A', 'T', 'L', 'A', 'B', ':', 'd', 'i', 'v',
    'i', 'd', 'e', 'B', 'y', 'Z', 'e', 'r', 'o' };

  char_T c2_b_u[19];
  const mxArray *c2_b_y = NULL;
  for (c2_i95 = 0; c2_i95 < 15; c2_i95 = c2_i95 + 1) {
    c2_u[c2_i95] = c2_cv0[c2_i95];
  }

  c2_y = NULL;
  sf_mex_assign(&c2_y, sf_mex_create("y", &c2_u, 10, 0U, 1U, 0U, 2, 1, 15));
  for (c2_i96 = 0; c2_i96 < 19; c2_i96 = c2_i96 + 1) {
    c2_b_u[c2_i96] = c2_cv1[c2_i96];
  }

  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_b_u, 10, 0U, 1U, 0U, 2, 1, 19));
  sf_mex_call_debug("warning", 0U, 2U, 14, c2_b_y, 14, c2_y);
}

static real_T c2_mpower(real_T c2_a)
{
  return c2_power(c2_a, 2.0);
}

static real_T c2_power(real_T c2_a, real_T c2_b)
{
  real_T c2_ak;
  real_T c2_bk;
  real_T c2_x;
  real_T c2_b_x;
  c2_ak = c2_a;
  c2_bk = c2_b;
  if (c2_ak < 0.0) {
    c2_x = c2_bk;
    c2_b_x = c2_x;
    c2_b_x = muDoubleScalarFloor(c2_b_x);
    if (c2_b_x != c2_bk) {
      c2_eml_error();
      goto label_1;
    }
  }

 label_1:
  ;
  return muDoubleScalarPower(c2_ak, c2_bk);
}

static void c2_eml_error(void)
{
  int32_T c2_i97;
  static char_T c2_cv2[102] = { 'D', 'o', 'm', 'a', 'i', 'n', ' ', 'e', 'r', 'r',
    'o', 'r', '.', ' ', 'T', 'o', ' ', 'c', 'o', 'm',
    'p', 'u', 't', 'e', ' ', 'c', 'o', 'm', 'p', 'l', 'e', 'x', ' ', 'r', 'e',
    's', 'u', 'l', 't', 's',
    ',', ' ', 'm', 'a', 'k', 'e', ' ', 'a', 't', ' ', 'l', 'e', 'a', 's', 't',
    ' ', 'o', 'n', 'e', ' ',
    'i', 'n', 'p', 'u', 't', ' ', 'c', 'o', 'm', 'p', 'l', 'e', 'x', ',', ' ',
    'e', '.', 'g', '.', ' ',
    '\'', 'p', 'o', 'w', 'e', 'r', '(', 'c', 'o', 'm', 'p', 'l', 'e', 'x', '(',
    'a', ')', ',', 'b', ')',
    '\'', '.' };

  char_T c2_u[102];
  const mxArray *c2_y = NULL;
  int32_T c2_i98;
  static char_T c2_cv3[32] = { 'E', 'm', 'b', 'e', 'd', 'd', 'e', 'd', 'M', 'A',
    'T', 'L', 'A', 'B', ':', 'p', 'o', 'w', 'e', 'r', ':'
    , 'd', 'o', 'm', 'a', 'i', 'n', 'E', 'r', 'r', 'o', 'r' };

  char_T c2_b_u[32];
  const mxArray *c2_b_y = NULL;
  for (c2_i97 = 0; c2_i97 < 102; c2_i97 = c2_i97 + 1) {
    c2_u[c2_i97] = c2_cv2[c2_i97];
  }

  c2_y = NULL;
  sf_mex_assign(&c2_y, sf_mex_create("y", &c2_u, 10, 0U, 1U, 0U, 2, 1, 102));
  for (c2_i98 = 0; c2_i98 < 32; c2_i98 = c2_i98 + 1) {
    c2_b_u[c2_i98] = c2_cv3[c2_i98];
  }

  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_b_u, 10, 0U, 1U, 0U, 2, 1, 32));
  sf_mex_call_debug("error", 0U, 2U, 14, c2_b_y, 14, c2_y);
}

static real_T c2_abs(real_T c2_x)
{
  real_T c2_b_x;
  c2_b_x = c2_x;
  return muDoubleScalarAbs(c2_b_x);
}

static void c2_eml_scalar_eg(void)
{
}

static real_T c2_ceval_xdot(int32_T c2_n, real_T c2_x[2], int32_T c2_ix0,
  int32_T c2_incx, real_T c2_y[2], int32_T c2_iy0, int32_T
  c2_incy)
{
  real_T c2_d;
  c2_d = 0.0;
  if ((real_T)c2_n > 0.0) {
    return ddot32(&c2_n, &c2_x[_SFD_EML_ARRAY_BOUNDS_CHECK("x", (int32_T)
      _SFD_INTEGER_CHECK("ix0", (real_T)c2_ix0), 1, 2, 1, 0) - 1], &
                  c2_incx, &c2_y[_SFD_EML_ARRAY_BOUNDS_CHECK("y", (int32_T)
      _SFD_INTEGER_CHECK("iy0", (real_T)c2_iy0), 1, 2, 1, 0) - 1], &c2_incy);
  }

  return c2_d;
}

static real_T c2_sign(real_T c2_x)
{
  real_T c2_b_x;
  real_T c2_c_x;
  real_T c2_d_x;
  boolean_T c2_b;
  c2_b_x = c2_x;
  c2_c_x = c2_b_x;
  c2_b_x = c2_c_x;
  c2_d_x = c2_b_x;
  c2_b = rtIsNaN(c2_d_x);
  if (c2_b) {
    return rtNaN;
  } else if (c2_b_x > 0.0) {
    return 1.0;
  } else if (c2_b_x < 0.0) {
    return -1.0;
  } else {
    return 0.0;
  }
}

static real_T c2_b_mpower(real_T c2_a)
{
  return c2_power(c2_a, 1.0);
}

static real_T c2_c_mpower(real_T c2_a)
{
  return c2_power(c2_a, 1.5);
}

static const mxArray *c2_sf_marshall(void *c2_chartInstance, void *c2_u)
{
  const mxArray *c2_y = NULL;
  int32_T c2_i99;
  real_T c2_b_u[4];
  const mxArray *c2_b_y = NULL;
  c2_y = NULL;
  for (c2_i99 = 0; c2_i99 < 4; c2_i99 = c2_i99 + 1) {
    c2_b_u[c2_i99] = (*((real_T (*)[4])c2_u))[c2_i99];
  }

  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_b_u, 0, 0U, 1U, 0U, 1, 4));
  sf_mex_assign(&c2_y, c2_b_y);
  return c2_y;
}

static const mxArray *c2_b_sf_marshall(void *c2_chartInstance, void *c2_u)
{
  const mxArray *c2_y = NULL;
  real_T c2_b_u;
  const mxArray *c2_b_y = NULL;
  c2_y = NULL;
  c2_b_u = *((real_T *)c2_u);
  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_b_u, 0, 0U, 0U, 0U, 0));
  sf_mex_assign(&c2_y, c2_b_y);
  return c2_y;
}

static const mxArray *c2_c_sf_marshall(void *c2_chartInstance, void *c2_u)
{
  const mxArray *c2_y = NULL;
  int32_T c2_i100;
  int32_T c2_i101;
  int32_T c2_i102;
  real_T c2_b_u[4];
  const mxArray *c2_b_y = NULL;
  c2_y = NULL;
  c2_i100 = 0;
  for (c2_i101 = 0; c2_i101 < 2; c2_i101 = c2_i101 + 1) {
    for (c2_i102 = 0; c2_i102 < 2; c2_i102 = c2_i102 + 1) {
      c2_b_u[c2_i102 + c2_i100] = (*((real_T (*)[4])c2_u))[c2_i102 + c2_i100];
    }

    c2_i100 = c2_i100 + 2;
  }

  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_b_u, 0, 0U, 1U, 0U, 2, 2, 2));
  sf_mex_assign(&c2_y, c2_b_y);
  return c2_y;
}

const mxArray *sf_c2_gustlaw_get_eml_resolved_functions_info(void)
{
  const mxArray *c2_nameCaptureInfo = NULL;
  c2_ResolvedFunctionInfo c2_info[65];
  c2_ResolvedFunctionInfo (*c2_b_info)[65];
  const mxArray *c2_m0 = NULL;
  int32_T c2_i103;
  c2_ResolvedFunctionInfo *c2_r0;
  c2_nameCaptureInfo = NULL;
  c2_info_helper(c2_info);
  c2_b_info = (c2_ResolvedFunctionInfo (*)[65])c2_info;
  (*c2_b_info)[64].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_guarded_nan.m";
  (*c2_b_info)[64].name = "nan";
  (*c2_b_info)[64].dominantType = "char";
  (*c2_b_info)[64].resolved = "[B]nan";
  (*c2_b_info)[64].fileLength = 0U;
  (*c2_b_info)[64].fileTime1 = 0U;
  (*c2_b_info)[64].fileTime2 = 0U;
  sf_mex_assign(&c2_m0, sf_mex_createstruct("nameCaptureInfo", 1, 65));
  for (c2_i103 = 0; c2_i103 < 65; c2_i103 = c2_i103 + 1) {
    c2_r0 = &c2_info[c2_i103];
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", c2_r0->context, 15,
      0U, 0U, 0U, 2, 1, strlen(c2_r0->context)), "context",
                    "nameCaptureInfo", c2_i103);
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", c2_r0->name, 15, 0U,
      0U, 0U, 2, 1, strlen(c2_r0->name)), "name",
                    "nameCaptureInfo", c2_i103);
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", c2_r0->dominantType,
      15, 0U, 0U, 0U, 2, 1, strlen(c2_r0->dominantType)),
                    "dominantType", "nameCaptureInfo", c2_i103);
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", c2_r0->resolved, 15,
      0U, 0U, 0U, 2, 1, strlen(c2_r0->resolved)), "resolved"
                    , "nameCaptureInfo", c2_i103);
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", &c2_r0->fileLength,
      7, 0U, 0U, 0U, 0), "fileLength", "nameCaptureInfo",
                    c2_i103);
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", &c2_r0->fileTime1, 7,
      0U, 0U, 0U, 0), "fileTime1", "nameCaptureInfo",
                    c2_i103);
    sf_mex_addfield(c2_m0, sf_mex_create("nameCaptureInfo", &c2_r0->fileTime2, 7,
      0U, 0U, 0U, 0), "fileTime2", "nameCaptureInfo",
                    c2_i103);
  }

  sf_mex_assign(&c2_nameCaptureInfo, c2_m0);
  return c2_nameCaptureInfo;
}

static void c2_info_helper(c2_ResolvedFunctionInfo c2_info[65])
{
  c2_info[0].context = "";
  c2_info[0].name = "ctranspose";
  c2_info[0].dominantType = "double";
  c2_info[0].resolved = "[B]ctranspose";
  c2_info[0].fileLength = 0U;
  c2_info[0].fileTime1 = 0U;
  c2_info[0].fileTime2 = 0U;
  c2_info[1].context = "";
  c2_info[1].name = "mtimes";
  c2_info[1].dominantType = "double";
  c2_info[1].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[1].fileLength = 3302U;
  c2_info[1].fileTime1 = 1242754494U;
  c2_info[1].fileTime2 = 0U;
  c2_info[2].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[2].name = "nargin";
  c2_info[2].dominantType = "";
  c2_info[2].resolved = "[B]nargin";
  c2_info[2].fileLength = 0U;
  c2_info[2].fileTime1 = 0U;
  c2_info[2].fileTime2 = 0U;
  c2_info[3].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[3].name = "gt";
  c2_info[3].dominantType = "double";
  c2_info[3].resolved = "[B]gt";
  c2_info[3].fileLength = 0U;
  c2_info[3].fileTime1 = 0U;
  c2_info[3].fileTime2 = 0U;
  c2_info[4].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[4].name = "isa";
  c2_info[4].dominantType = "double";
  c2_info[4].resolved = "[B]isa";
  c2_info[4].fileLength = 0U;
  c2_info[4].fileTime1 = 0U;
  c2_info[4].fileTime2 = 0U;
  c2_info[5].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[5].name = "isinteger";
  c2_info[5].dominantType = "double";
  c2_info[5].resolved = "[B]isinteger";
  c2_info[5].fileLength = 0U;
  c2_info[5].fileTime1 = 0U;
  c2_info[5].fileTime2 = 0U;
  c2_info[6].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[6].name = "isscalar";
  c2_info[6].dominantType = "double";
  c2_info[6].resolved = "[B]isscalar";
  c2_info[6].fileLength = 0U;
  c2_info[6].fileTime1 = 0U;
  c2_info[6].fileTime2 = 0U;
  c2_info[7].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[7].name = "strcmp";
  c2_info[7].dominantType = "char";
  c2_info[7].resolved = "[B]strcmp";
  c2_info[7].fileLength = 0U;
  c2_info[7].fileTime1 = 0U;
  c2_info[7].fileTime2 = 0U;
  c2_info[8].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[8].name = "size";
  c2_info[8].dominantType = "double";
  c2_info[8].resolved = "[B]size";
  c2_info[8].fileLength = 0U;
  c2_info[8].fileTime1 = 0U;
  c2_info[8].fileTime2 = 0U;
  c2_info[9].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[9].name = "eq";
  c2_info[9].dominantType = "double";
  c2_info[9].resolved = "[B]eq";
  c2_info[9].fileLength = 0U;
  c2_info[9].fileTime1 = 0U;
  c2_info[9].fileTime2 = 0U;
  c2_info[10].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[10].name = "class";
  c2_info[10].dominantType = "double";
  c2_info[10].resolved = "[B]class";
  c2_info[10].fileLength = 0U;
  c2_info[10].fileTime1 = 0U;
  c2_info[10].fileTime2 = 0U;
  c2_info[11].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[11].name = "not";
  c2_info[11].dominantType = "logical";
  c2_info[11].resolved = "[B]not";
  c2_info[11].fileLength = 0U;
  c2_info[11].fileTime1 = 0U;
  c2_info[11].fileTime2 = 0U;
  c2_info[12].context = "";
  c2_info[12].name = "mrdivide";
  c2_info[12].dominantType = "double";
  c2_info[12].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mrdivide.m";
  c2_info[12].fileLength = 800U;
  c2_info[12].fileTime1 = 1238437892U;
  c2_info[12].fileTime2 = 0U;
  c2_info[13].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mrdivide.m";
  c2_info[13].name = "ge";
  c2_info[13].dominantType = "double";
  c2_info[13].resolved = "[B]ge";
  c2_info[13].fileLength = 0U;
  c2_info[13].fileTime1 = 0U;
  c2_info[13].fileTime2 = 0U;
  c2_info[14].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mrdivide.m";
  c2_info[14].name = "rdivide";
  c2_info[14].dominantType = "double";
  c2_info[14].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/rdivide.m";
  c2_info[14].fileLength = 620U;
  c2_info[14].fileTime1 = 1213930366U;
  c2_info[14].fileTime2 = 0U;
  c2_info[15].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/rdivide.m";
  c2_info[15].name = "isempty";
  c2_info[15].dominantType = "double";
  c2_info[15].resolved = "[B]isempty";
  c2_info[15].fileLength = 0U;
  c2_info[15].fileTime1 = 0U;
  c2_info[15].fileTime2 = 0U;
  c2_info[16].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/rdivide.m";
  c2_info[16].name = "eml_warning";
  c2_info[16].dominantType = "char";
  c2_info[16].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_warning.m";
  c2_info[16].fileLength = 262U;
  c2_info[16].fileTime1 = 1236260878U;
  c2_info[16].fileTime2 = 0U;
  c2_info[17].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/rdivide.m";
  c2_info[17].name = "eml_div";
  c2_info[17].dominantType = "double";
  c2_info[17].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_div.m";
  c2_info[17].fileLength = 4269U;
  c2_info[17].fileTime1 = 1228097426U;
  c2_info[17].fileTime2 = 0U;
  c2_info[18].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_div.m/eml_fldiv";
  c2_info[18].name = "isreal";
  c2_info[18].dominantType = "double";
  c2_info[18].resolved = "[B]isreal";
  c2_info[18].fileLength = 0U;
  c2_info[18].fileTime1 = 0U;
  c2_info[18].fileTime2 = 0U;
  c2_info[19].context = "";
  c2_info[19].name = "sqrt";
  c2_info[19].dominantType = "double";
  c2_info[19].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sqrt.m";
  c2_info[19].fileLength = 572U;
  c2_info[19].fileTime1 = 1203451646U;
  c2_info[19].fileTime2 = 0U;
  c2_info[20].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sqrt.m";
  c2_info[20].name = "lt";
  c2_info[20].dominantType = "double";
  c2_info[20].resolved = "[B]lt";
  c2_info[20].fileLength = 0U;
  c2_info[20].fileTime1 = 0U;
  c2_info[20].fileTime2 = 0U;
  c2_info[21].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sqrt.m";
  c2_info[21].name = "eml_error";
  c2_info[21].dominantType = "char";
  c2_info[21].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_error.m";
  c2_info[21].fileLength = 315U;
  c2_info[21].fileTime1 = 1213930346U;
  c2_info[21].fileTime2 = 0U;
  c2_info[22].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sqrt.m";
  c2_info[22].name = "eml_scalar_sqrt";
  c2_info[22].dominantType = "double";
  c2_info[22].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_sqrt.m";
  c2_info[22].fileLength = 664U;
  c2_info[22].fileTime1 = 1209334394U;
  c2_info[22].fileTime2 = 0U;
  c2_info[23].context = "";
  c2_info[23].name = "uminus";
  c2_info[23].dominantType = "double";
  c2_info[23].resolved = "[B]uminus";
  c2_info[23].fileLength = 0U;
  c2_info[23].fileTime1 = 0U;
  c2_info[23].fileTime2 = 0U;
  c2_info[24].context = "";
  c2_info[24].name = "pi";
  c2_info[24].dominantType = "";
  c2_info[24].resolved = "[B]pi";
  c2_info[24].fileLength = 0U;
  c2_info[24].fileTime1 = 0U;
  c2_info[24].fileTime2 = 0U;
  c2_info[25].context = "";
  c2_info[25].name = "mpower";
  c2_info[25].dominantType = "double";
  c2_info[25].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mpower.m";
  c2_info[25].fileLength = 3710U;
  c2_info[25].fileTime1 = 1238437890U;
  c2_info[25].fileTime2 = 0U;
  c2_info[26].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mpower.m";
  c2_info[26].name = "ndims";
  c2_info[26].dominantType = "double";
  c2_info[26].resolved = "[B]ndims";
  c2_info[26].fileLength = 0U;
  c2_info[26].fileTime1 = 0U;
  c2_info[26].fileTime2 = 0U;
  c2_info[27].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mpower.m";
  c2_info[27].name = "power";
  c2_info[27].dominantType = "double";
  c2_info[27].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/power.m";
  c2_info[27].fileLength = 5380U;
  c2_info[27].fileTime1 = 1228097498U;
  c2_info[27].fileTime2 = 0U;
  c2_info[28].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/power.m";
  c2_info[28].name = "eml_scalar_eg";
  c2_info[28].dominantType = "double";
  c2_info[28].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalar_eg.m";
  c2_info[28].fileLength = 3068U;
  c2_info[28].fileTime1 = 1240265610U;
  c2_info[28].fileTime2 = 0U;
  c2_info[29].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalar_eg.m/any_enums";
  c2_info[29].name = "false";
  c2_info[29].dominantType = "";
  c2_info[29].resolved = "[B]false";
  c2_info[29].fileLength = 0U;
  c2_info[29].fileTime1 = 0U;
  c2_info[29].fileTime2 = 0U;
  c2_info[30].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalar_eg.m";
  c2_info[30].name = "isstruct";
  c2_info[30].dominantType = "double";
  c2_info[30].resolved = "[B]isstruct";
  c2_info[30].fileLength = 0U;
  c2_info[30].fileTime1 = 0U;
  c2_info[30].fileTime2 = 0U;
  c2_info[31].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalar_eg.m/zerosum";
  c2_info[31].name = "cast";
  c2_info[31].dominantType = "double";
  c2_info[31].resolved = "[B]cast";
  c2_info[31].fileLength = 0U;
  c2_info[31].fileTime1 = 0U;
  c2_info[31].fileTime2 = 0U;
  c2_info[32].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalar_eg.m/zerosum";
  c2_info[32].name = "plus";
  c2_info[32].dominantType = "double";
  c2_info[32].resolved = "[B]plus";
  c2_info[32].fileLength = 0U;
  c2_info[32].fileTime1 = 0U;
  c2_info[32].fileTime2 = 0U;
  c2_info[33].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/power.m";
  c2_info[33].name = "eml_scalexp_alloc";
  c2_info[33].dominantType = "double";
  c2_info[33].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalexp_alloc.m";
  c2_info[33].fileLength = 808U;
  c2_info[33].fileTime1 = 1230498300U;
  c2_info[33].fileTime2 = 0U;
  c2_info[34].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_scalexp_alloc.m";
  c2_info[34].name = "minus";
  c2_info[34].dominantType = "double";
  c2_info[34].resolved = "[B]minus";
  c2_info[34].fileLength = 0U;
  c2_info[34].fileTime1 = 0U;
  c2_info[34].fileTime2 = 0U;
  c2_info[35].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/power.m";
  c2_info[35].name = "eml_scalar_floor";
  c2_info[35].dominantType = "double";
  c2_info[35].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_floor.m";
  c2_info[35].fileLength = 260U;
  c2_info[35].fileTime1 = 1209334390U;
  c2_info[35].fileTime2 = 0U;
  c2_info[36].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/power.m";
  c2_info[36].name = "ne";
  c2_info[36].dominantType = "double";
  c2_info[36].resolved = "[B]ne";
  c2_info[36].fileLength = 0U;
  c2_info[36].fileTime1 = 0U;
  c2_info[36].fileTime2 = 0U;
  c2_info[37].context = "";
  c2_info[37].name = "abs";
  c2_info[37].dominantType = "double";
  c2_info[37].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/abs.m";
  c2_info[37].fileLength = 566U;
  c2_info[37].fileTime1 = 1221270734U;
  c2_info[37].fileTime2 = 0U;
  c2_info[38].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/abs.m";
  c2_info[38].name = "ischar";
  c2_info[38].dominantType = "double";
  c2_info[38].resolved = "[B]ischar";
  c2_info[38].fileLength = 0U;
  c2_info[38].fileTime1 = 0U;
  c2_info[38].fileTime2 = 0U;
  c2_info[39].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/abs.m";
  c2_info[39].name = "islogical";
  c2_info[39].dominantType = "double";
  c2_info[39].resolved = "[B]islogical";
  c2_info[39].fileLength = 0U;
  c2_info[39].fileTime1 = 0U;
  c2_info[39].fileTime2 = 0U;
  c2_info[40].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/abs.m";
  c2_info[40].name = "zeros";
  c2_info[40].dominantType = "double";
  c2_info[40].resolved = "[B]zeros";
  c2_info[40].fileLength = 0U;
  c2_info[40].fileTime1 = 0U;
  c2_info[40].fileTime2 = 0U;
  c2_info[41].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/abs.m";
  c2_info[41].name = "eml_scalar_abs";
  c2_info[41].dominantType = "double";
  c2_info[41].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_abs.m";
  c2_info[41].fileLength = 461U;
  c2_info[41].fileTime1 = 1203451560U;
  c2_info[41].fileTime2 = 0U;
  c2_info[42].context = "";
  c2_info[42].name = "sin";
  c2_info[42].dominantType = "double";
  c2_info[42].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sin.m";
  c2_info[42].fileLength = 324U;
  c2_info[42].fileTime1 = 1203451642U;
  c2_info[42].fileTime2 = 0U;
  c2_info[43].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sin.m";
  c2_info[43].name = "eml_scalar_sin";
  c2_info[43].dominantType = "double";
  c2_info[43].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_sin.m";
  c2_info[43].fileLength = 601U;
  c2_info[43].fileTime1 = 1209334392U;
  c2_info[43].fileTime2 = 0U;
  c2_info[44].context = "";
  c2_info[44].name = "uplus";
  c2_info[44].dominantType = "double";
  c2_info[44].resolved = "[B]uplus";
  c2_info[44].fileLength = 0U;
  c2_info[44].fileTime1 = 0U;
  c2_info[44].fileTime2 = 0U;
  c2_info[45].context = "";
  c2_info[45].name = "times";
  c2_info[45].dominantType = "double";
  c2_info[45].resolved = "[B]times";
  c2_info[45].fileLength = 0U;
  c2_info[45].fileTime1 = 0U;
  c2_info[45].fileTime2 = 0U;
  c2_info[46].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[46].name = "le";
  c2_info[46].dominantType = "double";
  c2_info[46].resolved = "[B]le";
  c2_info[46].fileLength = 0U;
  c2_info[46].fileTime1 = 0U;
  c2_info[46].fileTime2 = 0U;
  c2_info[47].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[47].name = "eml_index_class";
  c2_info[47].dominantType = "";
  c2_info[47].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_index_class.m";
  c2_info[47].fileLength = 909U;
  c2_info[47].fileTime1 = 1192470382U;
  c2_info[47].fileTime2 = 0U;
  c2_info[48].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[48].name = "ones";
  c2_info[48].dominantType = "char";
  c2_info[48].resolved = "[B]ones";
  c2_info[48].fileLength = 0U;
  c2_info[48].fileTime1 = 0U;
  c2_info[48].fileTime2 = 0U;
  c2_info[49].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/ops/mtimes.m";
  c2_info[49].name = "eml_xdotu";
  c2_info[49].dominantType = "int32";
  c2_info[49].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/eml_xdotu.m";
  c2_info[49].fileLength = 1453U;
  c2_info[49].fileTime1 = 1209334452U;
  c2_info[49].fileTime2 = 0U;
  c2_info[50].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/eml_xdotu.m";
  c2_info[50].name = "eml_xdot";
  c2_info[50].dominantType = "int32";
  c2_info[50].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/eml_xdot.m";
  c2_info[50].fileLength = 1330U;
  c2_info[50].fileTime1 = 1209334450U;
  c2_info[50].fileTime2 = 0U;
  c2_info[51].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/external/eml_blas_xdot.m";
  c2_info[51].name = "eml_refblas_xdot";
  c2_info[51].dominantType = "int32";
  c2_info[51].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/refblas/eml_refblas_xdot.m";
  c2_info[51].fileLength = 343U;
  c2_info[51].fileTime1 = 1211219644U;
  c2_info[51].fileTime2 = 0U;
  c2_info[52].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/refblas/eml_refblas_xdot.m";
  c2_info[52].name = "eml_refblas_xdotx";
  c2_info[52].dominantType = "int32";
  c2_info[52].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/refblas/eml_refblas_xdotx.m";
  c2_info[52].fileLength = 1605U;
  c2_info[52].fileTime1 = 1236260880U;
  c2_info[52].fileTime2 = 0U;
  c2_info[53].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/refblas/eml_refblas_xdotx.m";
  c2_info[53].name = "isequal";
  c2_info[53].dominantType = "char";
  c2_info[53].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elmat/isequal.m";
  c2_info[53].fileLength = 180U;
  c2_info[53].fileTime1 = 1226580872U;
  c2_info[53].fileTime2 = 0U;
  c2_info[54].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elmat/isequal.m";
  c2_info[54].name = "eml_isequal_core";
  c2_info[54].dominantType = "char";
  c2_info[54].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_isequal_core.m";
  c2_info[54].fileLength = 3981U;
  c2_info[54].fileTime1 = 1236260872U;
  c2_info[54].fileTime2 = 0U;
  c2_info[55].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_isequal_core.m";
  c2_info[55].name = "isnumeric";
  c2_info[55].dominantType = "char";
  c2_info[55].resolved = "[B]isnumeric";
  c2_info[55].fileLength = 0U;
  c2_info[55].fileTime1 = 0U;
  c2_info[55].fileTime2 = 0U;
  c2_info[56].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_isequal_core.m/same_size";
  c2_info[56].name = "true";
  c2_info[56].dominantType = "";
  c2_info[56].resolved = "[B]true";
  c2_info[56].fileLength = 0U;
  c2_info[56].fileTime1 = 0U;
  c2_info[56].fileTime2 = 0U;
  c2_info[57].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/refblas/eml_refblas_xdotx.m";
  c2_info[57].name = "eml_index_minus";
  c2_info[57].dominantType = "int32";
  c2_info[57].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_index_minus.m";
  c2_info[57].fileLength = 277U;
  c2_info[57].fileTime1 = 1192470384U;
  c2_info[57].fileTime2 = 0U;
  c2_info[58].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/blas/refblas/eml_refblas_xdotx.m";
  c2_info[58].name = "eml_index_plus";
  c2_info[58].dominantType = "int32";
  c2_info[58].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_index_plus.m";
  c2_info[58].fileLength = 272U;
  c2_info[58].fileTime1 = 1192470386U;
  c2_info[58].fileTime2 = 0U;
  c2_info[59].context = "";
  c2_info[59].name = "sign";
  c2_info[59].dominantType = "double";
  c2_info[59].resolved = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sign.m";
  c2_info[59].fileLength = 408U;
  c2_info[59].fileTime1 = 1203451642U;
  c2_info[59].fileTime2 = 0U;
  c2_info[60].context = "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/sign.m";
  c2_info[60].name = "eml_scalar_sign";
  c2_info[60].dominantType = "double";
  c2_info[60].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_sign.m";
  c2_info[60].fileLength = 543U;
  c2_info[60].fileTime1 = 1203451612U;
  c2_info[60].fileTime2 = 0U;
  c2_info[61].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_sign.m";
  c2_info[61].name = "isnan";
  c2_info[61].dominantType = "double";
  c2_info[61].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elmat/isnan.m";
  c2_info[61].fileLength = 506U;
  c2_info[61].fileTime1 = 1228097410U;
  c2_info[61].fileTime2 = 0U;
  c2_info[62].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/elfun/eml_scalar_sign.m";
  c2_info[62].name = "eml_guarded_nan";
  c2_info[62].dominantType = "char";
  c2_info[62].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_guarded_nan.m";
  c2_info[62].fileLength = 485U;
  c2_info[62].fileTime1 = 1192470380U;
  c2_info[62].fileTime2 = 0U;
  c2_info[63].context =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_guarded_nan.m";
  c2_info[63].name = "eml_is_float_class";
  c2_info[63].dominantType = "char";
  c2_info[63].resolved =
    "[ILX]$matlabroot$/toolbox/eml/lib/matlab/eml/eml_is_float_class.m";
  c2_info[63].fileLength = 226U;
  c2_info[63].fileTime1 = 1197854042U;
  c2_info[63].fileTime2 = 0U;
}

static const mxArray *c2_d_sf_marshall(void *c2_chartInstance, void *c2_u)
{
  const mxArray *c2_y = NULL;
  boolean_T c2_b_u;
  const mxArray *c2_b_y = NULL;
  c2_y = NULL;
  c2_b_u = *((boolean_T *)c2_u);
  c2_b_y = NULL;
  sf_mex_assign(&c2_b_y, sf_mex_create("y", &c2_b_u, 11, 0U, 0U, 0U, 0));
  sf_mex_assign(&c2_y, c2_b_y);
  return c2_y;
}

static void init_dsm_address_info(void)
{
}

/* SFunction Glue Code */
void sf_c2_gustlaw_get_check_sum(mxArray *plhs[])
{
  ((real_T *)mxGetPr((plhs[0])))[0] = (real_T)(2333262027U);
  ((real_T *)mxGetPr((plhs[0])))[1] = (real_T)(2963207465U);
  ((real_T *)mxGetPr((plhs[0])))[2] = (real_T)(2298922277U);
  ((real_T *)mxGetPr((plhs[0])))[3] = (real_T)(1237578412U);
}

mxArray *sf_c2_gustlaw_get_autoinheritance_info(void)
{
  const char *autoinheritanceFields[] = { "checksum", "inputs", "parameters",
    "outputs" };

  mxArray *mxAutoinheritanceInfo = mxCreateStructMatrix(1,1,4,
    autoinheritanceFields);

  {
    mxArray *mxChecksum = mxCreateDoubleMatrix(4,1,mxREAL);
    double *pr = mxGetPr(mxChecksum);
    pr[0] = (double)(191940328U);
    pr[1] = (double)(2348022545U);
    pr[2] = (double)(3260367368U);
    pr[3] = (double)(2053722777U);
    mxSetField(mxAutoinheritanceInfo,0,"checksum",mxChecksum);
  }

  {
    const char *dataFields[] = { "size", "type", "complexity" };

    mxArray *mxData = mxCreateStructMatrix(1,4,3,dataFields);

    {
      mxArray *mxSize = mxCreateDoubleMatrix(1,2,mxREAL);
      double *pr = mxGetPr(mxSize);
      pr[0] = (double)(4);
      pr[1] = (double)(1);
      mxSetField(mxData,0,"size",mxSize);
    }

    {
      const char *typeFields[] = { "base", "fixpt" };

      mxArray *mxType = mxCreateStructMatrix(1,1,2,typeFields);
      mxSetField(mxType,0,"base",mxCreateDoubleScalar(10));
      mxSetField(mxType,0,"fixpt",mxCreateDoubleMatrix(0,0,mxREAL));
      mxSetField(mxData,0,"type",mxType);
    }

    mxSetField(mxData,0,"complexity",mxCreateDoubleScalar(0));

    {
      mxArray *mxSize = mxCreateDoubleMatrix(1,2,mxREAL);
      double *pr = mxGetPr(mxSize);
      pr[0] = (double)(1);
      pr[1] = (double)(1);
      mxSetField(mxData,1,"size",mxSize);
    }

    {
      const char *typeFields[] = { "base", "fixpt" };

      mxArray *mxType = mxCreateStructMatrix(1,1,2,typeFields);
      mxSetField(mxType,0,"base",mxCreateDoubleScalar(10));
      mxSetField(mxType,0,"fixpt",mxCreateDoubleMatrix(0,0,mxREAL));
      mxSetField(mxData,1,"type",mxType);
    }

    mxSetField(mxData,1,"complexity",mxCreateDoubleScalar(0));

    {
      mxArray *mxSize = mxCreateDoubleMatrix(1,2,mxREAL);
      double *pr = mxGetPr(mxSize);
      pr[0] = (double)(1);
      pr[1] = (double)(1);
      mxSetField(mxData,2,"size",mxSize);
    }

    {
      const char *typeFields[] = { "base", "fixpt" };

      mxArray *mxType = mxCreateStructMatrix(1,1,2,typeFields);
      mxSetField(mxType,0,"base",mxCreateDoubleScalar(10));
      mxSetField(mxType,0,"fixpt",mxCreateDoubleMatrix(0,0,mxREAL));
      mxSetField(mxData,2,"type",mxType);
    }

    mxSetField(mxData,2,"complexity",mxCreateDoubleScalar(0));

    {
      mxArray *mxSize = mxCreateDoubleMatrix(1,2,mxREAL);
      double *pr = mxGetPr(mxSize);
      pr[0] = (double)(1);
      pr[1] = (double)(1);
      mxSetField(mxData,3,"size",mxSize);
    }

    {
      const char *typeFields[] = { "base", "fixpt" };

      mxArray *mxType = mxCreateStructMatrix(1,1,2,typeFields);
      mxSetField(mxType,0,"base",mxCreateDoubleScalar(10));
      mxSetField(mxType,0,"fixpt",mxCreateDoubleMatrix(0,0,mxREAL));
      mxSetField(mxData,3,"type",mxType);
    }

    mxSetField(mxData,3,"complexity",mxCreateDoubleScalar(0));
    mxSetField(mxAutoinheritanceInfo,0,"inputs",mxData);
  }

  {
    mxSetField(mxAutoinheritanceInfo,0,"parameters",mxCreateDoubleMatrix(0,0,
                mxREAL));
  }

  {
    const char *dataFields[] = { "size", "type", "complexity" };

    mxArray *mxData = mxCreateStructMatrix(1,1,3,dataFields);

    {
      mxArray *mxSize = mxCreateDoubleMatrix(1,2,mxREAL);
      double *pr = mxGetPr(mxSize);
      pr[0] = (double)(4);
      pr[1] = (double)(1);
      mxSetField(mxData,0,"size",mxSize);
    }

    {
      const char *typeFields[] = { "base", "fixpt" };

      mxArray *mxType = mxCreateStructMatrix(1,1,2,typeFields);
      mxSetField(mxType,0,"base",mxCreateDoubleScalar(10));
      mxSetField(mxType,0,"fixpt",mxCreateDoubleMatrix(0,0,mxREAL));
      mxSetField(mxData,0,"type",mxType);
    }

    mxSetField(mxData,0,"complexity",mxCreateDoubleScalar(0));
    mxSetField(mxAutoinheritanceInfo,0,"outputs",mxData);
  }

  return(mxAutoinheritanceInfo);
}

static mxArray *sf_get_sim_state_info_c2_gustlaw(void)
{
  const char *infoFields[] = { "chartChecksum", "varInfo" };

  mxArray *mxInfo = mxCreateStructMatrix(1, 1, 2, infoFields);
  char *infoEncStr[] = {
    "100 S1x2'type','srcId','name','auxInfo'{{M[1],M[5],T\"dx\",},{M[8],M[0],T\"is_active_c2_gustlaw\",}}"
  };

  mxArray *mxVarInfo = sf_mex_decode_encoded_mx_struct_array(infoEncStr, 2, 10);
  mxArray *mxChecksum = mxCreateDoubleMatrix(1, 4, mxREAL);
  sf_c2_gustlaw_get_check_sum(&mxChecksum);
  mxSetField(mxInfo, 0, infoFields[0], mxChecksum);
  mxSetField(mxInfo, 0, infoFields[1], mxVarInfo);
  return mxInfo;
}

static void chart_debug_initialization(SimStruct *S, unsigned int
  fullDebuggerInitialization)
{
  if (!sim_mode_is_rtw_gen(S)) {
    if (ssIsFirstInitCond(S) && fullDebuggerInitialization==1) {
      /* do this only if simulation is starting */
      {
        unsigned int chartAlreadyPresent;
        chartAlreadyPresent = sf_debug_initialize_chart(_gustlawMachineNumber_,
          2,
          1,
          1,
          5,
          0,
          0,
          0,
          0,
          0,
          &(chartInstance.chartNumber),
          &(chartInstance.instanceNumber),
          ssGetPath(S),
          (void *)S);
        if (chartAlreadyPresent==0) {
          /* this is the first instance */
          init_script_number_translation(_gustlawMachineNumber_,
            chartInstance.chartNumber);
          sf_debug_set_chart_disable_implicit_casting(_gustlawMachineNumber_,
            chartInstance.chartNumber,1);
          sf_debug_set_chart_event_thresholds(_gustlawMachineNumber_,
            chartInstance.chartNumber,
            0,
            0,
            0);

          {
            unsigned int dimVector[1];
            dimVector[0]= 4;
            _SFD_SET_DATA_PROPS(0,1,1,0,SF_DOUBLE,1,&(dimVector[0]),0,0,0,0.0,
                                1.0,0,"x",0,(MexFcnForType)c2_sf_marshall);
          }

          {
            unsigned int dimVector[1];
            dimVector[0]= 4;
            _SFD_SET_DATA_PROPS(1,2,0,1,SF_DOUBLE,1,&(dimVector[0]),0,0,0,0.0,
                                1.0,0,"dx",0,(MexFcnForType)c2_sf_marshall);
          }

          _SFD_SET_DATA_PROPS(2,1,1,0,SF_DOUBLE,0,NULL,0,0,0,0.0,1.0,0,"V",0,
                              (MexFcnForType)c2_b_sf_marshall);
          _SFD_SET_DATA_PROPS(3,1,1,0,SF_DOUBLE,0,NULL,0,0,0,0.0,1.0,0,
                              "nlssFlag",0,(MexFcnForType)c2_b_sf_marshall);
          _SFD_SET_DATA_PROPS(4,1,1,0,SF_DOUBLE,0,NULL,0,0,0,0.0,1.0,0,
                              "nlsdFlag",0,(MexFcnForType)c2_b_sf_marshall);
          _SFD_STATE_INFO(0,0,2);
          _SFD_CH_SUBSTATE_COUNT(0);
          _SFD_CH_SUBSTATE_DECOMP(0);
        }

        _SFD_CV_INIT_CHART(0,0,0,0);

        {
          _SFD_CV_INIT_STATE(0,0,0,0,0,0,NULL,NULL);
        }

        _SFD_CV_INIT_TRANS(0,0,NULL,NULL,0,NULL);

        /* Initialization of EML Model Coverage */
        _SFD_CV_INIT_EML(0,1,0,0,0,0,0,0);
        _SFD_CV_INIT_EML_FCN(0,0,"eML_blk_kernel",0,-1,3381);
        _SFD_TRANS_COV_WTS(0,0,0,1,0);
        if (chartAlreadyPresent==0) {
          _SFD_TRANS_COV_MAPS(0,
                              0,NULL,NULL,
                              0,NULL,NULL,
                              1,NULL,NULL,
                              0,NULL,NULL);
        }

        {
          real_T (*c2_x)[4];
          real_T (*c2_dx)[4];
          real_T *c2_V;
          real_T *c2_nlssFlag;
          real_T *c2_nlsdFlag;
          c2_dx = (real_T (*)[4])ssGetOutputPortSignal(chartInstance.S, 1);
          c2_nlsdFlag = (real_T *)ssGetInputPortSignal(chartInstance.S, 3);
          c2_x = (real_T (*)[4])ssGetInputPortSignal(chartInstance.S, 0);
          c2_nlssFlag = (real_T *)ssGetInputPortSignal(chartInstance.S, 2);
          c2_V = (real_T *)ssGetInputPortSignal(chartInstance.S, 1);
          _SFD_SET_DATA_VALUE_PTR(0U, c2_x);
          _SFD_SET_DATA_VALUE_PTR(1U, c2_dx);
          _SFD_SET_DATA_VALUE_PTR(2U, c2_V);
          _SFD_SET_DATA_VALUE_PTR(3U, c2_nlssFlag);
          _SFD_SET_DATA_VALUE_PTR(4U, c2_nlsdFlag);
        }
      }
    } else {
      sf_debug_reset_current_state_configuration(_gustlawMachineNumber_,
        chartInstance.chartNumber,chartInstance.instanceNumber);
    }
  }
}

static void sf_opaque_initialize_c2_gustlaw(void *chartInstanceVar)
{
  chart_debug_initialization(chartInstance.S,0);
  initialize_params_c2_gustlaw();
  initialize_c2_gustlaw();
}

static void sf_opaque_enable_c2_gustlaw(void *chartInstanceVar)
{
  enable_c2_gustlaw();
}

static void sf_opaque_disable_c2_gustlaw(void *chartInstanceVar)
{
  disable_c2_gustlaw();
}

static void sf_opaque_gateway_c2_gustlaw(void *chartInstanceVar)
{
  sf_c2_gustlaw();
}

static mxArray* sf_opaque_get_sim_state_c2_gustlaw(void *chartInstanceVar)
{
  mxArray *st = (mxArray *) get_sim_state_c2_gustlaw();
  return st;
}

static void sf_opaque_set_sim_state_c2_gustlaw(void *chartInstanceVar, const
  mxArray *st)
{
  set_sim_state_c2_gustlaw(sf_mex_dup(st));
}

static void sf_opaque_terminate_c2_gustlaw(void *chartInstanceVar)
{
  if (sim_mode_is_rtw_gen(chartInstance.S) || sim_mode_is_external
      (chartInstance.S)) {
    sf_clear_rtw_identifier(chartInstance.S);
  }

  finalize_c2_gustlaw();
}

extern unsigned int sf_machine_global_initializer_called(void);
static void mdlProcessParameters_c2_gustlaw(SimStruct *S)
{
  int i;
  for (i=0;i<ssGetNumRunTimeParams(S);i++) {
    if (ssGetSFcnParamTunable(S,i)) {
      ssUpdateDlgParamAsRunTimeParam(S,i);
    }
  }

  if (sf_machine_global_initializer_called()) {
    initialize_params_c2_gustlaw();
  }
}

static void mdlSetWorkWidths_c2_gustlaw(SimStruct *S)
{
  if (sim_mode_is_rtw_gen(S) || sim_mode_is_external(S)) {
    int_T chartIsInlinable =
      (int_T)sf_is_chart_inlinable("gustlaw","gustlaw",2);
    ssSetStateflowIsInlinable(S,chartIsInlinable);
    ssSetRTWCG(S,sf_rtw_info_uint_prop("gustlaw","gustlaw",2,"RTWCG"));
    ssSetEnableFcnIsTrivial(S,1);
    ssSetDisableFcnIsTrivial(S,1);
    ssSetNotMultipleInlinable(S,sf_rtw_info_uint_prop("gustlaw","gustlaw",2,
      "gatewayCannotBeInlinedMultipleTimes"));
    if (chartIsInlinable) {
      ssSetInputPortOptimOpts(S, 0, SS_REUSABLE_AND_LOCAL);
      ssSetInputPortOptimOpts(S, 1, SS_REUSABLE_AND_LOCAL);
      ssSetInputPortOptimOpts(S, 2, SS_REUSABLE_AND_LOCAL);
      ssSetInputPortOptimOpts(S, 3, SS_REUSABLE_AND_LOCAL);
      sf_mark_chart_expressionable_inputs(S,"gustlaw","gustlaw",2,4);
      sf_mark_chart_reusable_outputs(S,"gustlaw","gustlaw",2,1);
    }

    sf_set_rtw_dwork_info(S,"gustlaw","gustlaw",2);
    ssSetHasSubFunctions(S,!(chartIsInlinable));
    ssSetOptions(S,ssGetOptions(S)|SS_OPTION_WORKS_WITH_CODE_REUSE);
  }

  ssSetChecksum0(S,(131725940U));
  ssSetChecksum1(S,(3606602159U));
  ssSetChecksum2(S,(47303521U));
  ssSetChecksum3(S,(1181887059U));
  ssSetmdlDerivatives(S, NULL);
  ssSetExplicitFCSSCtrl(S,1);
}

static void mdlRTW_c2_gustlaw(SimStruct *S)
{
  if (sim_mode_is_rtw_gen(S)) {
    sf_write_symbol_mapping(S, "gustlaw", "gustlaw",2);
    ssWriteRTWStrParam(S, "StateflowChartType", "Embedded MATLAB");
  }
}

static void mdlStart_c2_gustlaw(SimStruct *S)
{
  chartInstance.chartInfo.chartInstance = NULL;
  chartInstance.chartInfo.isEMLChart = 1;
  chartInstance.chartInfo.chartInitialized = 0;
  chartInstance.chartInfo.sFunctionGateway = sf_opaque_gateway_c2_gustlaw;
  chartInstance.chartInfo.initializeChart = sf_opaque_initialize_c2_gustlaw;
  chartInstance.chartInfo.terminateChart = sf_opaque_terminate_c2_gustlaw;
  chartInstance.chartInfo.enableChart = sf_opaque_enable_c2_gustlaw;
  chartInstance.chartInfo.disableChart = sf_opaque_disable_c2_gustlaw;
  chartInstance.chartInfo.getSimState = sf_opaque_get_sim_state_c2_gustlaw;
  chartInstance.chartInfo.setSimState = sf_opaque_set_sim_state_c2_gustlaw;
  chartInstance.chartInfo.getSimStateInfo = sf_get_sim_state_info_c2_gustlaw;
  chartInstance.chartInfo.zeroCrossings = NULL;
  chartInstance.chartInfo.outputs = NULL;
  chartInstance.chartInfo.derivatives = NULL;
  chartInstance.chartInfo.mdlRTW = mdlRTW_c2_gustlaw;
  chartInstance.chartInfo.mdlStart = mdlStart_c2_gustlaw;
  chartInstance.chartInfo.mdlSetWorkWidths = mdlSetWorkWidths_c2_gustlaw;
  chartInstance.chartInfo.extModeExec = NULL;
  chartInstance.chartInfo.restoreLastMajorStepConfiguration = NULL;
  chartInstance.chartInfo.restoreBeforeLastMajorStepConfiguration = NULL;
  chartInstance.chartInfo.storeCurrentConfiguration = NULL;
  chartInstance.S = S;
  ssSetUserData(S,(void *)(&(chartInstance.chartInfo)));/* register the chart instance with simstruct */
  if (!sim_mode_is_rtw_gen(S)) {
    init_dsm_address_info();
  }

  chart_debug_initialization(S,1);
}

void c2_gustlaw_method_dispatcher(SimStruct *S, int_T method, void *data)
{
  switch (method) {
   case SS_CALL_MDL_START:
    mdlStart_c2_gustlaw(S);
    break;

   case SS_CALL_MDL_SET_WORK_WIDTHS:
    mdlSetWorkWidths_c2_gustlaw(S);
    break;

   case SS_CALL_MDL_PROCESS_PARAMETERS:
    mdlProcessParameters_c2_gustlaw(S);
    break;

   default:
    /* Unhandled method */
    sf_mex_error_message("Stateflow Internal Error:\n"
                         "Error calling c2_gustlaw_method_dispatcher.\n"
                         "Can't handle method %d.\n", method);
    break;
  }
}
