# Run the fjllowing 3 lines to execute this script independently
# setwd('../../..') # assumes working dir is ./r-scripts/exp-1b
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1b/subj_stats.R")
#-------------------------------------------------------------------------------

fj <- data_1b %>%
  filter(
    !(subject %in% exclude$subject), # remove catch exclusions
    trial_type == "freq_judge"
  ) %>%
  select(
    -(chosen_stim:ITI),
    -(fo_resp:fo_rt)
  )


# Create value column
fj <- fj %>%
  mutate(fj_value = case_when(
    q_door_val %in% c("risky_low", "fixed_low") ~ "Low",
    q_door_val %in% c("risky_high", "fixed_high") ~ "High"
  ))

# Pivot data to long
fj_long <- fj %>% 
  select(!ends_with(".RT")) %>% 
  pivot_longer(
    cols = ends_with(".RESP"),
    names_to = "fj_stim_outcome",
    values_to = "fj_resp")
  
# Remove invalid responses
fj_long <-fj_long %>%
  filter((q_door_val == "risky_high" & fj_stim_outcome %in%
    c("Resp40.RESP", "Resp80.RESP")) |
    (q_door_val == "risky_low" & fj_stim_outcome %in%
      c("Resp0.RESP", "Resp40.RESP"))
    )

fj_long <- fj_long %>%
  mutate(fj_outcome = case_when(
    fj_stim_outcome == "Resp80.RESP" ~ "+80",
    fj_stim_outcome == "Resp40.RESP" ~ "+40",
    fj_stim_outcome == "Resp0.RESP" ~ "0"
  ))


# Note: Three subjects had a missing value for one response.
# fj_long %>% filter(is.na(fj_resp))
