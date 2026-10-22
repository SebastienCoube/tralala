#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>
using namespace Rcpp;

// log(exp(a) + exp(b)), stable. Returns -Inf when both are -Inf (the R version gives NaN).
inline double addLogProbs(double a, double b) {
  double m = std::max(a, b);
  return std::log(std::exp(a - m) + std::exp(b - m)) + m;
}

// [[Rcpp::export]]
void transition(SEXP filtering_log_probs,              // double vector, MODIFIED IN PLACE
                          SEXP out_logprobs,                     // double vector (length K-1), MODIFIED IN PLACE
                          const NumericMatrix& value_at_btf_knots,   // n_knots x K
                          const NumericMatrix& btf_multiplicators,   // (n_knots-1) x K; 0 rows if depth == 1
                          const NumericVector& btf_knots,        // BTF_per_state[[latent_state]]
                          int depth) {
  // Wrapping a REALSXP does not copy; any other type would be silently coerced
  // to a copy and the caller would see no change, so refuse it.
  if (TYPEOF(filtering_log_probs) != REALSXP || TYPEOF(out_logprobs) != REALSXP)
    stop("filtering_log_probs and out_logprobs must be double vectors");

  NumericVector filt(filtering_log_probs);
  NumericVector out(out_logprobs);

  const int K = value_at_btf_knots.ncol();
  if (filt.size() < depth) stop("filtering_log_probs is shorter than depth");
  if (out.size() != K - 1) stop("out_logprobs must have length ncol(value_at_btf_knots) - 1");

  std::vector<double> tr_prob(K), btf_mult(K, 1.0);
  for (int k = 0; k < K; ++k) tr_prob[k] = value_at_btf_knots(0, k);

  for (int time_spent = depth; time_spent >= 1; --time_spent) {
    // match(time_spent, btf_knots), 0-based, -1 if no match
    int knot_match = -1;
    for (int j = 0; j < btf_knots.size(); ++j) {
      if (btf_knots[j] == time_spent) { knot_match = j; break; }
    }

    if (knot_match >= 0) {
      // exactly at a knot: fetch exact probabilities and the next multiplicator
      for (int k = 0; k < K; ++k) tr_prob[k] = value_at_btf_knots(knot_match, k);
      if (time_spent > 1)
        for (int k = 0; k < K; ++k) btf_mult[k] = btf_multiplicators(knot_match - 1, k);
    } else {
      // between knots: multiply and renormalize
      double s = 0.0;
      for (int k = 0; k < K; ++k) { tr_prob[k] *= btf_mult[k]; s += tr_prob[k]; }
      for (int k = 0; k < K; ++k) tr_prob[k] /= s;
    }

    const double f = filt[time_spent - 1];
    // outflow: accumulate log(tr_prob[-1]) + filtering into out_logprobs
    for (int k = 1; k < K; ++k)
      out[k - 1] = addLogProbs(out[k - 1], f + std::log(tr_prob[k]));
    // in-place update of the remaining probability
    filt[time_spent - 1] = f + std::log(tr_prob[0]);
  }
}


namespace {

// Reproduces valueAtBtfKnots() + btf_multiplicators from R.
//   transition_param : double array, dim = c(n_knots, p, K-1), optional dimnames
//   explanatory_var  : double vector of length p (matched by names to dimnames(...)[[2]]
//                      if both are present, otherwise taken in the same order)
// Outputs (row-major):
//   vals[i*K + k]      = softmax probabilities at knot i (n_knots x K)
//   mult[(i-1)*K + k]  = exp(-(log vals[i,k] - log vals[i-1,k]) / (knots[i]-knots[i-1]))
//                        (only filled when depth > 1)
void computeBtfTables(const NumericVector& transition_param,
                      const NumericVector& explanatory_variable,
                      const IntegerVector& btf_knots,
                      int depth,
                      int& K,
                      std::vector<double>& vals,
                      std::vector<double>& mult) {
  SEXP dimS = Rf_getAttrib(transition_param, R_DimSymbol);
  if (Rf_length(dimS) != 3)
    stop("transition_param must be a 3-dimensional array (n_knots x p x (K-1))");
  const int nk = INTEGER(dimS)[0];
  const int p  = INTEGER(dimS)[1];
  const int m  = INTEGER(dimS)[2];
  K = m + 1;
  if (nk < 1) stop("transition_param must have at least one knot");
  if (btf_knots.size() != nk) stop("length(btf_knots) must equal dim(transition_param)[1]");

  // Match explanatory variables to the 2nd dimension of transition_param
  std::vector<int> xi(p);
  SEXP dn = Rf_getAttrib(transition_param, R_DimNamesSymbol);
  SEXP pn = (dn != R_NilValue) ? VECTOR_ELT(dn, 1) : R_NilValue;
  SEXP xn = Rf_getAttrib(explanatory_variable, R_NamesSymbol);
  if (pn != R_NilValue && xn != R_NilValue) {
    CharacterVector pnames(pn), xnames(xn);
    for (int j = 0; j < p; ++j) {
      int found = -1;
      for (int l = 0; l < xnames.size(); ++l)
        if (std::string(pnames[j]) == std::string(xnames[l])) { found = l; break; }
      if (found < 0)
        stop("explanatory variable '%s' not found in names(explanatory_variable)",
             std::string(pnames[j]).c_str());
      xi[j] = found;
    }
  } else {
    if (explanatory_variable.size() != p)
      stop("explanatory_variable must have length dim(transition_param)[2] "
             "(or be named to match dimnames)");
    for (int j = 0; j < p; ++j) xi[j] = j;
  }

  // Linear predictors + softmax with a leading reference category (logit 0)
  vals.assign(static_cast<size_t>(nk) * K, 0.0);
  std::vector<double> eta(m);
  for (int i = 0; i < nk; ++i) {
    double mx = 0.0;
    for (int k = 0; k < m; ++k) {
      double s = 0.0;
      for (int j = 0; j < p; ++j)
        s += transition_param[i + static_cast<R_xlen_t>(nk) * (j + static_cast<R_xlen_t>(p) * k)]
      * explanatory_variable[xi[j]];
      eta[k] = s;
      if (s > mx) mx = s;
    }
    double denom = std::exp(-mx);
    vals[static_cast<size_t>(i) * K] = std::exp(-mx);
    for (int k = 0; k < m; ++k) {
      const double e = std::exp(eta[k] - mx);
      vals[static_cast<size_t>(i) * K + k + 1] = e;
      denom += e;
    }
    for (int k = 0; k < K; ++k) vals[static_cast<size_t>(i) * K + k] /= denom;
  }

  // Multipliers between consecutive knots
  mult.clear();
  if (depth > 1 && nk > 1) {
    mult.assign(static_cast<size_t>(nk - 1) * K, 1.0);
    for (int i = 1; i < nk; ++i) {
      const double dk = static_cast<double>(btf_knots[i] - btf_knots[i - 1]);
      for (int k = 0; k < K; ++k)
        mult[static_cast<size_t>(i - 1) * K + k] =
          std::exp(-(std::log(vals[static_cast<size_t>(i) * K + k]) -
          std::log(vals[static_cast<size_t>(i - 1) * K + k])) / dk);
    }
  }
}

} // namespace

// [[Rcpp::export]]
void transition_several(
    SEXP filtering_log_probs,                 // double vector, MODIFIED IN PLACE
    SEXP out_logprobs,                        // double vector (length K-1), MODIFIED IN PLACE
    SEXP filtering_log_probs_emission_diff_f, // double array, dim[0] = length(filtering), MODIFIED IN PLACE
    SEXP filtering_log_probs_emission_diff_b, // same dim as _f, MODIFIED IN PLACE
    SEXP out_logprobs_emission_diff_f,        // double array, dim[0] = length(out), MODIFIED IN PLACE
    SEXP out_logprobs_emission_diff_b,        // same dim as out_..._f, MODIFIED IN PLACE
    const NumericVector& transition_param,    // transition_parameters[[latent_state]], array n_knots x p x (K-1)
    const NumericVector& explanatory_variable,// data_seq$explanatory_variables[time_idx, ]
    const IntegerVector& btf_knots,
    int depth,
    int last_idx) {                           // in [1, depth]; log-prob rows are visited
  // last_idx..1, then depth..last_idx+1

  if (TYPEOF(filtering_log_probs) != REALSXP || TYPEOF(out_logprobs) != REALSXP ||
      TYPEOF(filtering_log_probs_emission_diff_f) != REALSXP ||
      TYPEOF(filtering_log_probs_emission_diff_b) != REALSXP ||
      TYPEOF(out_logprobs_emission_diff_f) != REALSXP ||
      TYPEOF(out_logprobs_emission_diff_b) != REALSXP)
    stop("all six in-place arguments must be double (not integer) objects");

  if (depth < 1) stop("depth must be >= 1");
  if (last_idx < 1 || last_idx > depth) stop("last_idx must be between 1 and depth");

  int K = 0;
  std::vector<double> vals, mult;
  computeBtfTables(transition_param, explanatory_variable, btf_knots, depth, K, vals, mult);

  NumericVector filt(filtering_log_probs);
  NumericVector out(out_logprobs);
  if (filt.size() < depth) stop("filtering_log_probs is shorter than depth");
  if (out.size() != K - 1) stop("out_logprobs must have length K - 1 (dim(transition_param)[3])");

  const R_xlen_t nrowF = filt.size(), nrowO = out.size();
  const R_xlen_t P = Rf_xlength(filtering_log_probs_emission_diff_f) / std::max<R_xlen_t>(nrowF, 1);
  if (Rf_xlength(filtering_log_probs_emission_diff_f) != nrowF * P ||
      Rf_xlength(filtering_log_probs_emission_diff_b) != nrowF * P ||
      Rf_xlength(out_logprobs_emission_diff_f)        != nrowO * P ||
      Rf_xlength(out_logprobs_emission_diff_b)        != nrowO * P)
    stop("array sizes do not match: first dim must be length(filtering_log_probs) or "
           "length(out_logprobs), and trailing dims must be identical");

  double* GF_f = REAL(filtering_log_probs_emission_diff_f);
  double* GF_b = REAL(filtering_log_probs_emission_diff_b);
  double* GO_f = REAL(out_logprobs_emission_diff_f);
  double* GO_b = REAL(out_logprobs_emission_diff_b);

  std::vector<double> tr_prob(K), btf_mult(K, 1.0), ltr(K);
  for (int k = 0; k < K; ++k) tr_prob[k] = vals[k];            // row 0

  for (int step = 0; step < depth; ++step) {
    // BTF indexation: unchanged (depth, depth-1, ..., 1)
    const int time_spent = depth - step;

    int knot_match = -1;
    for (int j = 0; j < btf_knots.size(); ++j)
      if (btf_knots[j] == time_spent) { knot_match = j; break; }

    if (knot_match >= 0) {
      for (int k = 0; k < K; ++k) tr_prob[k] = vals[static_cast<size_t>(knot_match) * K + k];
      if (time_spent > 1) {
        if (knot_match < 1 || mult.empty())
          stop("btf_knots must be increasing and contain at least 2 knots when depth > 1");
        for (int k = 0; k < K; ++k)
          btf_mult[k] = mult[static_cast<size_t>(knot_match - 1) * K + k];
      }
    } else {
      double s = 0.0;
      for (int k = 0; k < K; ++k) { tr_prob[k] *= btf_mult[k]; s += tr_prob[k]; }
      for (int k = 0; k < K; ++k) tr_prob[k] /= s;
    }
    for (int k = 0; k < K; ++k) ltr[k] = std::log(tr_prob[k]);

    // Log-prob row: last_idx, last_idx-1, ..., 1, depth, depth-1, ..., last_idx+1
    int idx = (last_idx - 1 - step) % depth;
    if (idx < 0) idx += depth;
    const int row = idx;            // 0-based row = (1-based index) - 1

    // vector part
    const double f = filt[row];
    for (int k = 1; k < K; ++k)
      out[k - 1] = addLogProbs(out[k - 1], f + ltr[k]);

    // array part
    for (R_xlen_t c = 0; c < P; ++c) {
      const double ff = GF_f[row + nrowF * c];
      const double fb = GF_b[row + nrowF * c];
      for (int k = 1; k < K; ++k) {
        double& of = GO_f[(k - 1) + nrowO * c];
        double& ob = GO_b[(k - 1) + nrowO * c];
        of = addLogProbs(of, ff + ltr[k]);
        ob = addLogProbs(ob, fb + ltr[k]);
      }
      GF_f[row + nrowF * c] = ff + ltr[0];
      GF_b[row + nrowF * c] = fb + ltr[0];
    }

    filt[row] = f + ltr[0];
  }
}

