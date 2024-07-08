# Run the following 5 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/risky_trials_filter.R")
# source("r-scripts/exp-1a/risky_trials_analysis.R")
#-------------------------------------------------------------------------------


# Main effects
# -----------------------------------------------------------------------------
risky_main <- data.frame(risky_main)[, 2:ncol(risky_main)]

# Inverse Bayes Factor
delta_BIC <- risky_main$BIC[2:nrow(risky_main)] - 
  risky_main$BIC[1:(nrow(risky_main) - 1)]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
risky_main$BF_10 <- c(NA, BF10)

# Degrees of Freedom
df_diffs <- c(NA, diff(risky_main$df))

# L-Ratio
l_rats <- round(risky_main$L.Ratio, 2)

# p-val
p_vals <- ifelse(risky_main$p.value < .001, '< .001', 
                 round(risky_main$p.value, 3))

# # r-squared
# r_sqs <- round(risky_main$R2, 2)

# Inverse Bayes Factor
bfs <-
  ifelse(risky_main$BF_10 > 150, '> 150',
    ifelse(risky_main$BF_10 < .01, '< .01',
      round(risky_main$BF_10, 3)
    )
  )


