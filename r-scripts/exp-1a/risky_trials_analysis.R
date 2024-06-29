# Run the following 3 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/risky_trials_filter.R")
#-------------------------------------------------------------------------------


# Experiment 1a
#-------------------------------------------------------------------------------

# Low Value Options
# Fixed: 20 (Door 3)
# Risky: 0 or 40 (Door 4)

# High Value Options
# Fixed: 60 (Door 1)
# Risky: 40 or 80 (Door 2)

# Six blocks of 80 trials.
# Decision Trials: (fixed low vs risky low) & (fixed high vs risky high) - 24 per block (12 low, 12 high)
# Single door trials: one door to click - 40 per block
# Catch trials: high vs low - 16 per block

# Group 1: 
# Block 1: 80% extreme values (0 low, 80 high)
# Block 2: 50% extreme values.
# Block 3: 20% extreme values (0 low, 80 high)
# Block 4-6: Block 2: 50% extreme values.

# Group 2: 
# Block 1: 20% extreme values (0 low, 80 high)
# Block 2: 50% extreme values.
# Block 3: 80% extreme values (0 low, 80 high)
# Block 4-6: Block 2: 50% extreme values.

# Group 3 (control): 
# All blocks: 50% extreme values.

#-------------------------------------------------------------------------------

# Factor Data
risky_res$subject <- factor(risky_res$subject)
risky_res$block <- factor(risky_res$block)
risky_res$choice_value <- factor(risky_res$choice_value, 
                                  levels = c('High', 'Low'))

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






























