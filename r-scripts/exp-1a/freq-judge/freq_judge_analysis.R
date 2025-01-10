# Run the following 4 lines to execute this script independently
# setwd('../../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/freq-judge/freq_judge_filter.R")
#-------------------------------------------------------------------------------

# 2 x 3 mixed ANOVA
#-------------------------------------------------------------------------------
# Note: data is qualitative, thus the validity of this method is questionable.
# E.g., One person's 40 may be another persons 60.

fj_long <- filter(
  fj_long,
  fj_outcome %in% c("0", "+80")
)

# Contrasts
` 8020_vs_50 ` <- c(0, 1, 0)
` 2080_vs_50 ` <- c(0, 0, 1)
contrasts(fj_long$group) <- cbind(` 8020_vs_50 `, ` 2080_vs_50 `)


fj_long$fj_value <- factor(fj_long$fj_value, levels = c("High", "Low"))
` high_vs_low ` <- c(1, -1)
contrasts(fj_long$fj_value) <- ` high_vs_low `

fj_long$subject <- factor(fj_long$subject)

# Models
base <- lme(fj_resp ~ 1,
  random = ~ 1 | subject / fj_value,
  method = "ML",
  data = fj_long
)

cond_mod <- lme(fj_resp ~ group,
  random = ~ 1 | subject / fj_value,
  method = "ML",
  data = fj_long
)

HL_mod <- lme(fj_resp ~ group + fj_value,
  random = ~ 1 | subject / fj_value,
  method = "ML",
  data = fj_long
)

int_mod <- lme(fj_resp ~ group + fj_value + group:fj_value,
  random = ~ 1 | subject / fj_value,
  method = "ML",
  data = fj_long
)

# Main effects and interactions
aov_2x3 <- data.frame(anova(base, cond_mod, HL_mod, int_mod))
aov_2x3 <- aov_2x3[, 2:ncol(aov_2x3)]

# Inverse Bayes Factor
delta_BIC <- aov_2x3$BIC[2:nrow(aov_2x3)] - aov_2x3$BIC[1:(nrow(aov_2x3) - 1)]
BF01 <- exp(delta_BIC / 2)
BF10 <- 1 / BF01
aov_2x3$BF_10 <- c(NA, BF10)

# Planned comparisons
pc <- as.data.frame(summary(int_mod)$tTable)

pc$r_effect <- sqrt((pc$`t-value`^2) / (pc$`t-value`^2 + pc$DF))
