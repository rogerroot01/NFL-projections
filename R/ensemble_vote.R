# One vote per eligible signal. Exact-line predictions abstain; the average
# projection breaks a tied vote only. These rules apply to every season.
ensemble_majority_pick <- function(picks, projection, line) {
  picks <- picks[is.finite(picks) & picks != 0]
  if (!length(picks)) return(NA_real_)
  balance <- sum(picks > 0) - sum(picks < 0)
  if (balance != 0) return(sign(balance))
  if (!is.finite(projection) || !is.finite(line) || projection == line) return(NA_real_)
  sign(projection - line)
}

ensemble_vote_agreement <- function(picks) {
  picks <- picks[is.finite(picks) & picks != 0]
  if (!length(picks)) return(NA_real_)
  max(mean(picks > 0), mean(picks < 0))
}
