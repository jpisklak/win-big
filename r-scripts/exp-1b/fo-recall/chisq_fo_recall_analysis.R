# Run the following 5 lines to execute this script independently
# setwd('../../..') # assumes working dir is ./r-scripts/exp-1b/fo-recall
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1b/subj_stats.R")
# source("r-scripts/exp-1b/fo-recall/fo_recall_filter.R")
#-------------------------------------------------------------------------------

# Remove anyone who gave a "other/incorrect" response.
other <- fo %>% filter(fo_cat == "Other")
n_other <- length(unique(other$subject))
fo <- fo %>% filter(!(subject %in% other$subject ))
n_fo <- length(unique(fo$subject))

# Create category column for results
fo <- fo %>%
  mutate(fo_eval = case_when(
    q_door_val == "risky_low" & fo_resp == 0 ~ "Yes",
    q_door_val == "risky_high" & fo_resp == 80 ~ "Yes",
    q_door_val == "risky_low" & fo_resp != 0 ~ "No",
    q_door_val == "risky_high" & fo_resp != 80 ~ "No",
  ))

# Goodness of Fit test
#-------------------------------------------------------------------------------
gf_high <- fo |> filter(fo$fo_cat != "0" & fo_value == "High")
gf_high_tab <- xtabs(~ fo_cat, data = gf_high)
chisq.test(gf_high_tab, p = c(0.5, 0.5))

gf_low <- fo |> filter(fo$fo_cat != "+80" & fo_value == "Low")
gf_low_tab <- xtabs(~ fo_cat, data = gf_low)
chisq.test(gf_low_tab, p = c(0.5, 0.5))

# 2 X 3 Pearson’s Chi-squared Test
#-------------------------------------------------------------------------------

fo_high <- fo %>% filter(fo_value == "High")
fo_high_tab <- xtabs(~ group + fo_eval, data = fo_high)

fo_low <- fo %>% filter(fo_value == "Low")
fo_low_tab <- xtabs(~ group + fo_eval, data = fo_low)

fo_high_test <- chisq.test(fo_high_tab)
fo_high_eff <- cramerV(fo_high_tab, digits = 4)

fo_low_test <- chisq.test(fo_low_tab)
fo_low_eff <- cramerV(fo_low_tab, digits = 4)

# Post Hoc Analysis 1 (preferred)-----------------------------------------------
# Standardized Residuals
std_res_high <- chisq.test(fo_high_tab)$stdres
std_res_low <- chisq.test(fo_low_tab)$stdres

# P-values
p_high <- pnorm(abs(std_res_high), lower.tail = FALSE) * 2
p_low <- pnorm(abs(std_res_low), lower.tail = FALSE) * 2

# Post Hoc Analysis 2-----------------------------------------------------------

# Pairwise Fisher-Exact tests - High Value

# BEST 50-50 vs BEST 80-20
h_50_v_8020 <- fo %>% 
  filter(fo_value == "High" & group != "BEST 20-80") %>%
  droplevels()
h_50_v_8020 <- xtabs(~ group + fo_eval, data = h_50_v_8020)
h_50_v_8020 <- fisher.test(h_50_v_8020)

# BEST 50-50 vs BEST 20-80
h_50_v_2080 <- fo %>% 
  filter(fo_value == "High" & group != "BEST 80-20") %>%
  droplevels()
h_50_v_2080 <- xtabs(~ group + fo_eval, data = h_50_v_2080)
h_50_v_2080 <- fisher.test(h_50_v_2080)

# BEST 80-20 vs BEST 20-80
h_8020_v_2080 <- fo %>% 
  filter(fo_value == "High" & group != "BEST 50-50") %>%
  droplevels()
h_8020_v_2080 <- xtabs(~ group + fo_eval, data = h_8020_v_2080)
h_8020_v_2080 <- fisher.test(h_8020_v_2080)



# Pairwise Fisher-Exact tests - Low Value

# BEST 50-50 vs BEST 80-20
l_50_v_8020 <- fo %>% 
  filter(fo_value == "Low" & group != "BEST 20-80") %>%
  droplevels()
l_50_v_8020 <- xtabs(~ group + fo_eval, data = l_50_v_8020)
l_50_v_8020 <- fisher.test(l_50_v_8020)

# BEST 50-50 vs BEST 20-80
l_50_v_2080 <- fo %>% 
  filter(fo_value == "Low" & group != "BEST 80-20") %>%
  droplevels()
l_50_v_2080 <- xtabs(~ group + fo_eval, data = l_50_v_2080)
l_50_v_2080 <- fisher.test(l_50_v_2080)

# BEST 80-20 vs BEST 20-80
l_8020_v_2080 <- fo %>% 
  filter(fo_value == "Low" & group != "BEST 50-50") %>%
  droplevels()
l_8020_v_2080 <- xtabs(~ group + fo_eval, data = l_8020_v_2080)
l_8020_v_2080 <- fisher.test(l_8020_v_2080)


# Dataframe of Fisher tests

fish_tests <- tibble(
  value = c(rep("High", 3), rep("Low", 3)),
  comparison = rep(c(
    "BEST 50-50 : BEST 80-20",
    "BEST 50-50 : BEST 20-80",
    "BEST 80-20 : BEST 20-80"
  ), 2),
  odds_ratio = c(
    h_50_v_8020$estimate, h_50_v_2080$estimate, h_8020_v_2080$estimate,
    l_50_v_8020$estimate, l_50_v_2080$estimate, l_8020_v_2080$estimate
  ),
  p_value = c(
    h_50_v_8020$p.value, h_50_v_2080$p.value, h_8020_v_2080$p.value,
    l_50_v_8020$p.value, l_50_v_2080$p.value, l_8020_v_2080$p.value
  ),
  CI_low = c(
    h_50_v_8020$conf.int[1], h_50_v_2080$conf.int[1], h_8020_v_2080$conf.int[1],
    l_50_v_8020$conf.int[1], l_50_v_2080$conf.int[1], l_8020_v_2080$conf.int[1]
  ),
  CI_upper = c(
    h_50_v_8020$conf.int[2], h_50_v_2080$conf.int[2], h_8020_v_2080$conf.int[2],
    l_50_v_8020$conf.int[2], l_50_v_2080$conf.int[2], l_8020_v_2080$conf.int[2]
  ),
)

fish_tests$p_value_adj <- p.adjust(fish_tests$p_value, method = "holm")


# https://rcompanion.org/handbook/H_04.html
# https://www.statology.org/interpret-cramers-v/
