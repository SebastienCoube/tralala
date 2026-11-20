#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>
using namespace Rcpp;
// [[Rcpp::plugins(openmp)]]

// log(exp(a) + exp(b)), stable. Returns -Inf when both are -Inf (the R version gives NaN).

inline double addLogProbs(double a, double b) {
  if (a < b) std::swap(a, b);            // a >= b
  if (b == -INFINITY) return a;          // also covers both -Inf
  return a + std::log1p(std::exp(b - a));
}

//' @export
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

  // A "segment" is a maximal run of steps governed by the same knot match:
  // it starts at the step where knot m matches (m = -1 for the initial run of steps
  // before any knot matches) and ends right before the next match.
  struct Seg { int start; int end; int m; };

  // Processes one column of the four arrays for all steps. Column-by-column access keeps the
  // column (nrowF / nrowO contiguous doubles) in cache. lf / lb are depth x K tables of
  // log transition probabilities (step-major) used for the _f and _b arrays.
  inline void processColumn(const int* rows, int depth, int K,
                            double* Ff, double* Fb, double* Of, double* Ob,
                            const double* lf, const double* lb) {
    for (int s = 0; s < depth; ++s) {
      const int row = rows[s];
      const double ff = Ff[row];
      const double fb = Fb[row];
      const double* a = lf + static_cast<size_t>(s) * K;
      const double* b = lb + static_cast<size_t>(s) * K;
      for (int k = 1; k < K; ++k) {
        Of[k - 1] = addLogProbs(Of[k - 1], ff + a[k]);
        Ob[k - 1] = addLogProbs(Ob[k - 1], fb + b[k]);
      }
      Ff[row] = ff + a[0];
      Fb[row] = fb + b[0];
    }
  }

} // namespace


//' @export
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
  R_xlen_t n_target = 0;
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

  // ======================================================================================
  // Step 1: unperturbed trajectory, computed ONCE for all steps and all columns.
  //   rows[s]        : 0-based row of the log-prob vectors visited at step s
  //   ltrT[s*K + k]  : log transition probability at step s (step-major)
  //   segs           : segments of steps governed by the same knot match
  // All checks that can stop() happen here, before anything is modified in place.
  // ======================================================================================
  std::vector<int> rows(depth);
  std::vector<double> ltrT(static_cast<size_t>(depth) * K);
  std::vector<Seg> segs;
  {
    std::vector<double> tr_prob(K), btf_mult(K, 1.0);
    for (int k = 0; k < K; ++k) tr_prob[k] = vals[k];            // row 0
    for (int step = 0; step < depth; ++step) {
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
        if (!segs.empty()) segs.back().end = step;
        segs.push_back(Seg{step, depth, knot_match});
      } else {
        if (segs.empty()) segs.push_back(Seg{0, depth, -1});
        double s = 0.0;
        for (int k = 0; k < K; ++k) { tr_prob[k] *= btf_mult[k]; s += tr_prob[k]; }
        for (int k = 0; k < K; ++k) tr_prob[k] /= s;
      }
      for (int k = 0; k < K; ++k)
        ltrT[static_cast<size_t>(step) * K + k] = std::log(tr_prob[k]);

      // Log-prob row: last_idx, last_idx-1, ..., 1, depth, depth-1, ..., last_idx+1
      int idx = (last_idx - 1 - step) % depth;
      if (idx < 0) idx += depth;
      rows[step] = idx;
    }
  }

  // ======================================================================================
  // Step 2: vector part (sequential over steps; each row is visited exactly once).
  // ======================================================================================
  for (int s = 0; s < depth; ++s) {
    const int row = rows[s];
    const double* l = &ltrT[static_cast<size_t>(s) * K];
    const double f = filt[row];
    for (int k = 1; k < K; ++k)
      out[k - 1] = addLogProbs(out[k - 1], f + l[k]);
    filt[row] = f + l[0];
  }

  // ======================================================================================
  // Step 3: emission-diff arrays, column by column, using the shared unperturbed table.
  // ======================================================================================
  for (R_xlen_t c = 0; c < P; ++c)
    processColumn(rows.data(), depth, K,
                  GF_f + nrowF * c, GF_b + nrowF * c, GO_f + nrowO * c, GO_b + nrowO * c,
                  ltrT.data(), ltrT.data());

  // ======================================================================================
  // Step 4: transition-diff arrays.
  //   - name != latent_state_name : same shared table as the emission arrays.
  //   - name == latent_state_name : column c <-> transition_param[i,j,kk], c = i + nk*(j + p*kk).
  //     Moving this coefficient by +eps (_f) / -eps (_b) changes only vals[i], hence only
  //     mult[i-1] and mult[i]. The trajectory therefore differs from the unperturbed one
  //     only on the segments
  //        * m == i      (whole segment: start probabilities vals[i] and multiplier mult[i-1])
  //        * m == i + 1  (non-match steps: multiplier mult[i])
  //        * the initial segment (m == -1) when i == 0 (it uses row 0 of vals)
  //     On every other step the shared unperturbed table is reused as is: scratch tables are
  //     copies of ltrT in which only these segments are overwritten (and restored afterwards).
  // ======================================================================================
  std::vector<double> etaKnot, scrF, scrB;
  std::vector<std::vector<int>> segsOf;           // segsOf[i] = indices (in segs) affected by knot i
  {
    etaKnot.assign(static_cast<size_t>(nk) * m, 0.0);
    for (int i = 0; i < nk; ++i)
      for (int k = 0; k < m; ++k) {
        double s = 0.0;
        for (int j = 0; j < p; ++j)
          s += transition_param[i + static_cast<R_xlen_t>(nk) * (j + static_cast<R_xlen_t>(p) * k)]
        * explanatory_variable[xi[j]];
        etaKnot[static_cast<size_t>(i) * m + k] = s;
      }
    scrF = ltrT;
    scrB = ltrT;
    segsOf.assign(nk, std::vector<int>());
    for (size_t q = 0; q < segs.size(); ++q) {
      const int sm = segs[q].m;
      if (sm == -1) segsOf[0].push_back(static_cast<int>(q));
      else {
        segsOf[sm].push_back(static_cast<int>(q));                     // m == i
        if (sm >= 1) segsOf[sm - 1].push_back(static_cast<int>(q));    // m == i + 1
      }
    }
  }

  std::vector<double> eta(m), pv(K), pmLo(K), pmHi(K), t(K), bm(K);

  for (size_t e_idx = 0; e_idx < tr.size(); ++e_idx) {
    const TransEntry& e = tr[e_idx];

    if (!e.target) {
      for (R_xlen_t c = 0; c < e.Pt; ++c)
        processColumn(rows.data(), depth, K,
                      e.Ff + nrowF * c, e.Fb + nrowF * c, e.Of + nrowO * c, e.Ob + nrowO * c,
                      ltrT.data(), ltrT.data());
      continue;
    }

    for (R_xlen_t c = 0; c < e.Pt; ++c) {
      const int i  = static_cast<int>(c % nk);
      const int j  = static_cast<int>((c / nk) % p);
      const int kk = static_cast<int>(c / (static_cast<R_xlen_t>(nk) * p));
      const double xj = explanatory_variable[xi[j]];

      for (int d = 0; d < 2; ++d) {
        const double delta = (d == 0) ? eps : -eps;
        double* scr = (d == 0) ? scrF.data() : scrB.data();

        // perturbed knot probabilities and the two multipliers that involve knot i
        for (int k = 0; k < m; ++k) eta[k] = etaKnot[static_cast<size_t>(i) * m + k];
        eta[kk] += delta * xj;
        softmaxRef(eta, K, pv.data());
        if (!mult.empty()) {
          if (i >= 1) {
            const double dk = static_cast<double>(btf_knots[i] - btf_knots[i - 1]);
            multFromRows(&vals[static_cast<size_t>(i - 1) * K], pv.data(), dk, K, pmLo.data());
          }
          if (i + 1 < nk) {
            const double dk = static_cast<double>(btf_knots[i + 1] - btf_knots[i]);
            multFromRows(pv.data(), &vals[static_cast<size_t>(i + 1) * K], dk, K, pmHi.data());
          }
        }

        // recompute the perturbed log table on the affected segments only
        for (size_t u = 0; u < segsOf[i].size(); ++u) {
          const Seg& sg = segs[segsOf[i][u]];
          // starting state of the segment
          if (sg.m == -1) {                                   // initial segment: row 0 of vals
            const double* v0 = (i == 0) ? pv.data() : &vals[0];
            for (int k = 0; k < K; ++k) { t[k] = v0[k]; bm[k] = 1.0; }
          } else {
            const double* v = (sg.m == i) ? pv.data() : &vals[static_cast<size_t>(sg.m) * K];
            for (int k = 0; k < K; ++k) t[k] = v[k];
            if (depth - sg.start > 1) {                        // time_spent > 1: multiplier is used
              const double* ms = (sg.m == i) ? pmLo.data() : pmHi.data();   // m == i or i + 1
              for (int k = 0; k < K; ++k) bm[k] = ms[k];
            }
          }
          for (int s = sg.start; s < sg.end; ++s) {
            if (!(sg.m >= 0 && s == sg.start)) {               // non-match step: multiply + normalize
              double sum = 0.0;
              for (int k = 0; k < K; ++k) { t[k] *= bm[k]; sum += t[k]; }
              for (int k = 0; k < K; ++k) t[k] /= sum;
            }
            for (int k = 0; k < K; ++k) scr[static_cast<size_t>(s) * K + k] = std::log(t[k]);
          }
        }
      }

      processColumn(rows.data(), depth, K,
                    e.Ff + nrowF * c, e.Fb + nrowF * c, e.Of + nrowO * c, e.Ob + nrowO * c,
                    scrF.data(), scrB.data());

      // restore the scratch tables on the segments that were overwritten
      for (size_t u = 0; u < segsOf[i].size(); ++u) {
        const Seg& sg = segs[segsOf[i][u]];
        const size_t a = static_cast<size_t>(sg.start) * K, b = static_cast<size_t>(sg.end) * K;
        std::copy(ltrT.begin() + a, ltrT.begin() + b, scrF.begin() + a);
        std::copy(ltrT.begin() + a, ltrT.begin() + b, scrB.begin() + a);
      }
    }
  }
}
