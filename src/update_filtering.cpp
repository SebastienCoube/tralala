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

// Maps each column j of transition_param (2nd dim) to an index in explanatory_variable.
void matchExplanatory(const NumericVector& transition_param,
                      const NumericVector& explanatory_variable,
                      int p,
                      std::vector<int>& xi) {
  xi.assign(p, 0);
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
}

  // Softmax with a leading reference category (logit 0). eta has length K-1, out has length K.
  void softmaxRef(const std::vector<double>& eta, int K, double* out) {
    const int m = K - 1;
    double mx = 0.0;
    for (int k = 0; k < m; ++k) if (eta[k] > mx) mx = eta[k];
    double denom = std::exp(-mx);
    out[0] = denom;
    for (int k = 0; k < m; ++k) {
      const double e = std::exp(eta[k] - mx);
      out[k + 1] = e;
      denom += e;
    }
    for (int k = 0; k < K; ++k) out[k] /= denom;
  }

  // mult[k] = exp(-(log hi[k] - log lo[k]) / dk)
  void multFromRows(const double* lo, const double* hi, double dk, int K, double* out) {
    for (int k = 0; k < K; ++k)
      out[k] = std::exp(-(std::log(hi[k]) - std::log(lo[k])) / dk);
  }

  // Reproduces valueAtBtfKnots() + btf_multiplicators from R.
  //   transition_param : double array, dim = c(n_knots, p, K-1), optional dimnames
  //   explanatory_var  : double vector of length p
  // Outputs (row-major):
  //   vals[i*K + k]      = softmax probabilities at knot i (n_knots x K)
  //   mult[(i-1)*K + k]  = exp(-(log vals[i,k] - log vals[i-1,k]) / (knots[i]-knots[i-1]))
  //                        (only filled when depth > 1)
  //   xi[j]              = index in explanatory_variable of column j of transition_param
  void computeBtfTables(const NumericVector& transition_param,
                        const NumericVector& explanatory_variable,
                        const IntegerVector& btf_knots,
                        int depth,
                        int& K,
                        std::vector<double>& vals,
                        std::vector<double>& mult,
                        std::vector<int>& xi) {
    SEXP dimS = Rf_getAttrib(transition_param, R_DimSymbol);
    if (Rf_length(dimS) != 3)
      stop("transition_param must be a 3-dimensional array (n_knots x p x (K-1))");
    const int nk = INTEGER(dimS)[0];
    const int p  = INTEGER(dimS)[1];
    const int m  = INTEGER(dimS)[2];
    K = m + 1;
    if (nk < 1) stop("transition_param must have at least one knot");
    if (btf_knots.size() != nk) stop("length(btf_knots) must equal dim(transition_param)[1]");

    matchExplanatory(transition_param, explanatory_variable, p, xi);

    vals.assign(static_cast<size_t>(nk) * K, 0.0);
    std::vector<double> eta(m);
    for (int i = 0; i < nk; ++i) {
      for (int k = 0; k < m; ++k) {
        double s = 0.0;
        for (int j = 0; j < p; ++j)
          s += transition_param[i + static_cast<R_xlen_t>(nk) * (j + static_cast<R_xlen_t>(p) * k)]
        * explanatory_variable[xi[j]];
        eta[k] = s;
      }
      softmaxRef(eta, K, &vals[static_cast<size_t>(i) * K]);
    }

    mult.clear();
    if (depth > 1 && nk > 1) {
      mult.assign(static_cast<size_t>(nk - 1) * K, 1.0);
      for (int i = 1; i < nk; ++i) {
        const double dk = static_cast<double>(btf_knots[i] - btf_knots[i - 1]);
        multFromRows(&vals[static_cast<size_t>(i - 1) * K], &vals[static_cast<size_t>(i) * K],
                     dk, K, &mult[static_cast<size_t>(i - 1) * K]);
      }
    }
  }

} // namespace


// [[Rcpp::export]]
void transition_several(
    SEXP filtering_log_probs,                   // double vector, MODIFIED IN PLACE
    SEXP out_logprobs,                          // double vector (length K-1), MODIFIED IN PLACE
    SEXP filtering_log_probs_emission_diff_f,   // double array, dim[0] = length(filtering), MODIFIED IN PLACE
    SEXP filtering_log_probs_emission_diff_b,   // same dim as _f, MODIFIED IN PLACE
    SEXP out_logprobs_emission_diff_f,          // double array, dim[0] = length(out), MODIFIED IN PLACE
    SEXP out_logprobs_emission_diff_b,          // same dim as out_..._f, MODIFIED IN PLACE
    SEXP filtering_log_probs_transition_diff_f, // named list of double arrays, MODIFIED IN PLACE
    SEXP filtering_log_probs_transition_diff_b, // named list of double arrays, MODIFIED IN PLACE
    SEXP out_logprobs_transition_diff_f,        // named list of double arrays, MODIFIED IN PLACE
    SEXP out_logprobs_transition_diff_b,        // named list of double arrays, MODIFIED IN PLACE
    const std::string& latent_state_name,       // name of the list member differentiated w.r.t. transition_param
    const NumericVector& transition_param,      // transition_parameters[[latent_state]], array n_knots x p x (K-1)
    const NumericVector& explanatory_variable,  // data_seq$explanatory_variables[time_idx, ]
    const IntegerVector& btf_knots,
    int depth,
    int last_idx,                               // in [1, depth]
    double eps = 1e-6) {                        // increment applied to one coefficient of transition_param

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
  std::vector<int> xi;
  computeBtfTables(transition_param, explanatory_variable, btf_knots, depth, K, vals, mult, xi);

  SEXP dimS = Rf_getAttrib(transition_param, R_DimSymbol);
  const int nk = INTEGER(dimS)[0];
  const int p  = INTEGER(dimS)[1];
  const int m  = K - 1;
  const bool has_mult = !mult.empty();

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

  // ---- transition-diff lists: loop over the names of the filtering _f list ----
  struct TransEntry {
    double* Ff; double* Fb;   // filtering-like arrays, nrowF x Pt
    double* Of; double* Ob;   // out-like arrays, nrowO x Pt
    R_xlen_t Pt;
    bool target;              // true if name == latent_state_name
  };
  std::vector<TransEntry> tr;
  R_xlen_t n_target = 0;       // number of components Pt of the target entry
  bool found_target = false;
  {
    List lFf(filtering_log_probs_transition_diff_f);
    List lFb(filtering_log_probs_transition_diff_b);
    List lOf(out_logprobs_transition_diff_f);
    List lOb(out_logprobs_transition_diff_b);
    CharacterVector nms = lFf.names();
    for (R_xlen_t i = 0; i < nms.size(); ++i) {
      const std::string key = as<std::string>(nms[i]);
      NumericVector aFf = lFf[key];
      NumericVector aFb = lFb[key];
      NumericVector aOf = lOf[key];
      NumericVector aOb = lOb[key];
      TransEntry e;
      e.Ff = REAL(aFf); e.Fb = REAL(aFb); e.Of = REAL(aOf); e.Ob = REAL(aOb);
      e.Pt = aFf.size() / nrowF;
      e.target = (key == latent_state_name);
      if (e.target) {
        found_target = true;
        n_target = e.Pt;
      }
      tr.push_back(e);
    }
  }
  if (!found_target)
    stop("latent_state_name '%s' not found in names(filtering_log_probs_transition_diff_f)",
         latent_state_name.c_str());
  if (n_target != transition_param.size())
    stop("the array for '%s' must have as many columns (%d) as length(transition_param) (%d)",
         latent_state_name.c_str(), (int) n_target, (int) transition_param.size());

  // ---- perturbed BTF quantities for the target entry (chain rule: only knot i changes) ----
  // Column c of the target arrays <-> transition_param[i, j, kk] with c = i + nk*(j + p*kk)
  // (column-major, as R flattens the trailing dimensions).
  // For direction d = 0 (_f: +eps) and d = 1 (_b: -eps), only the following change:
  //   - the probabilities at knot i              : pv
  //   - mult between knots (i-1, i)  [index i-1] : pmLo   (if i >= 1)
  //   - mult between knots (i, i+1)  [index i]   : pmHi   (if i <= nk-2)
  // Everything else is shared with the unperturbed tables.
  const R_xlen_t Pt = n_target;
  const size_t stride = static_cast<size_t>(K);
  std::vector<double> pv(Pt * 2 * stride), pmLo(Pt * 2 * stride, 1.0), pmHi(Pt * 2 * stride, 1.0);
  std::vector<double> trP(Pt * 2 * stride), bmP(Pt * 2 * stride, 1.0), ltrP(Pt * 2 * stride);
  {
    // unperturbed linear predictors at every knot
    std::vector<double> etaKnot(static_cast<size_t>(nk) * m);
    for (int i = 0; i < nk; ++i)
      for (int k = 0; k < m; ++k) {
        double s = 0.0;
        for (int j = 0; j < p; ++j)
          s += transition_param[i + static_cast<R_xlen_t>(nk) * (j + static_cast<R_xlen_t>(p) * k)]
        * explanatory_variable[xi[j]];
        etaKnot[static_cast<size_t>(i) * m + k] = s;
      }
    std::vector<double> eta(m);
    for (R_xlen_t c = 0; c < Pt; ++c) {
      const int i  = static_cast<int>(c % nk);
      const int j  = static_cast<int>((c / nk) % p);
      const int kk = static_cast<int>(c / (static_cast<R_xlen_t>(nk) * p));
      const double xj = explanatory_variable[xi[j]];
      for (int d = 0; d < 2; ++d) {
        const double delta = (d == 0) ? eps : -eps;
        for (int k = 0; k < m; ++k) eta[k] = etaKnot[static_cast<size_t>(i) * m + k];
        eta[kk] += delta * xj;
        double* pvc = &pv[(c * 2 + d) * stride];
        softmaxRef(eta, K, pvc);
        if (has_mult) {
          if (i >= 1) {
            const double dk = static_cast<double>(btf_knots[i] - btf_knots[i - 1]);
            multFromRows(&vals[static_cast<size_t>(i - 1) * K], pvc, dk, K,
                         &pmLo[(c * 2 + d) * stride]);
          }
          if (i + 1 < nk) {
            const double dk = static_cast<double>(btf_knots[i + 1] - btf_knots[i]);
            multFromRows(pvc, &vals[static_cast<size_t>(i + 1) * K], dk, K,
                         &pmHi[(c * 2 + d) * stride]);
          }
        }
        // initial state: row 0 of the (perturbed) vals table
        const double* v0 = (i == 0) ? pvc : &vals[0];
        for (int k = 0; k < K; ++k) trP[(c * 2 + d) * stride + k] = v0[k];
      }
    }
  }

  std::vector<double> tr_prob(K), btf_mult(K, 1.0), ltr(K);
  for (int k = 0; k < K; ++k) tr_prob[k] = vals[k];            // row 0

  for (int step = 0; step < depth; ++step) {
    // BTF indexation: unchanged (depth, depth-1, ..., 1)
    const int time_spent = depth - step;

    int knot_match = -1;
    for (int j = 0; j < btf_knots.size(); ++j)
      if (btf_knots[j] == time_spent) { knot_match = j; break; }

    // ---- unperturbed trajectory (used by everything but the target entry) ----
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

    // ---- perturbed trajectories, one per component c and direction d ----
    for (R_xlen_t c = 0; c < Pt; ++c) {
      const int i = static_cast<int>(c % nk);
      for (int d = 0; d < 2; ++d) {
        const size_t off = (c * 2 + d) * stride;
        double* t  = &trP[off];
        double* bm = &bmP[off];
        if (knot_match >= 0) {
          const double* src = (knot_match == i) ? &pv[off]
          : &vals[static_cast<size_t>(knot_match) * K];
          for (int k = 0; k < K; ++k) t[k] = src[k];
          if (time_spent > 1) {
            const int a = knot_match - 1;           // mult index, between knots a and a+1
            const double* ms = (a == i - 1) ? &pmLo[off]
            : (a == i)     ? &pmHi[off]
            : &mult[static_cast<size_t>(a) * K];
            for (int k = 0; k < K; ++k) bm[k] = ms[k];
          }
        } else {
          double s = 0.0;
          for (int k = 0; k < K; ++k) { t[k] *= bm[k]; s += t[k]; }
          for (int k = 0; k < K; ++k) t[k] /= s;
        }
        for (int k = 0; k < K; ++k) ltrP[off + k] = std::log(t[k]);
      }
    }

    // Log-prob row: last_idx, last_idx-1, ..., 1, depth, depth-1, ..., last_idx+1
    int idx = (last_idx - 1 - step) % depth;
    if (idx < 0) idx += depth;
    const int row = idx;            // 0-based row = (1-based index) - 1

    // vector part
    const double f = filt[row];
    for (int k = 1; k < K; ++k)
      out[k - 1] = addLogProbs(out[k - 1], f + ltr[k]);

    // emission-diff array part
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

    // transition-diff array part:
    //  - name != latent_state_name : same treatment as above (ltr)
    //  - name == latent_state_name : column c uses the trajectory obtained by moving
    //    transition_param[i,j,kk] by +eps (_f arrays) or -eps (_b arrays)
    for (size_t t = 0; t < tr.size(); ++t) {
      const TransEntry& e = tr[t];
      for (R_xlen_t c = 0; c < e.Pt; ++c) {
        const double* lf = e.target ? &ltrP[(c * 2    ) * stride] : ltr.data();
        const double* lb = e.target ? &ltrP[(c * 2 + 1) * stride] : ltr.data();
        const double ff = e.Ff[row + nrowF * c];
        const double fb = e.Fb[row + nrowF * c];
        for (int k = 1; k < K; ++k) {
          double& of = e.Of[(k - 1) + nrowO * c];
          double& ob = e.Ob[(k - 1) + nrowO * c];
          of = addLogProbs(of, ff + lf[k]);
          ob = addLogProbs(ob, fb + lb[k]);
        }
        e.Ff[row + nrowF * c] = ff + lf[0];
        e.Fb[row + nrowF * c] = fb + lb[0];
      }
    }

    filt[row] = f + ltr[0];
  }
}
