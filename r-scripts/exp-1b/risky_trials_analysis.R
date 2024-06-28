# setwd('../..') #Running this R script alone requires being in the main dir.
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1b/subj_stats.R")
# source("r-scripts/exp-1b/risky_trials_filter.R")
#-------------------------------------------------------------------------------

# risky_res

#Factor Block
risky_res$block <- factor(risky_res$block)

# Comparisons

# Group Comparisons
` g1_vs_g3 ` <- c(1, 0, 0)
` g2_vs_g3 ` <- c(0, 1, 0)
contrasts(risky_res$group) <- cbind(` g1_vs_g3 `, ` g2_vs_g3 `)

# Choice Value Comparisons
` high_vs_low ` <- c(1, -1)
contrasts(risky_res$choice_value) <- ` high_vs_low `

# Block Comparisons (Group 2 and 5 removed)
` b1_vs_b6 ` <- c(1, 0, 0, 0, 0, 0)
` b2_vs_b6 ` <- c(0, 1, 0, 0, 0, 0)
` b3_vs_b6 ` <- c(0, 0, 1, 0, 0, 0)
` b4_vs_b6 ` <- c(0, 0, 0, 1, 0, 0)
` b5_vs_b6 ` <- c(0, 0, 0, 0, 1, 0)
contrasts(risky_res$block) <- cbind(
  ` b1_vs_b6 `,
  ` b2_vs_b6 `,
  ` b3_vs_b6 `,
  ` b4_vs_b6 `,
  ` b5_vs_b6 `
)

# Conventional ANOVA
# library(ez)
# conv_ANOVA <- ezANOVA(risky_res,
#   dv = .(cp), wid = .(subject),
#   between = .(group),
#   within = .(choice_value, block),
#   type = 3, detailed = TRUE
# )
# conv_ANOVA

# Multilevel model
base_mod <- lme(cp ~ 1,
  random = ~ 1 | subject / choice_value / block,
  method = "ML", data = risky_res
)

  #Main effects
grp_mod <- update(base_mod, .~. + group)
value_mod <- update(grp_mod, .~. + choice_value)
blk_mod <- update(value_mod, .~. + block)

  # Interactions
grp_val <- update(blk_mod, .~. + group:choice_value)
grp_blk <- update(grp_val, .~. + group:block)
val_blk <- update(grp_blk, .~. + choice_value:block)
grp_val_blk <- update(val_blk, .~. + group:choice_value:block)

# Main Effects
risky_main <- anova(base_mod, grp_mod, value_mod, blk_mod,
      grp_val, grp_blk, val_blk, grp_val_blk)

risky_main

# Planned Comparisons
summary(grp_val_blk)






























