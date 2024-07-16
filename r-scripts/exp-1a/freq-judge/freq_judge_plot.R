# Run the following 4 lines to execute this script independently
# setwd('../../..') # assumes working dir is ./r-scripts/freq-judge
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/freq-judge/freq_judge_filter.R")
#-------------------------------------------------------------------------------

fj_long$fj_outcome <- factor(fj_long$fj_outcome,
                             levels = c("0", "+40", "+80"))

# Plot (Means and 95% CI)
#-------------------------------------------------------------------------------
# Note data is qualitative and thus probably shouldn't be plotted this way.
dodge <- position_dodge(.9)

plt_fj_means <- ggplot(fj_long, 
                       aes(x = fj_outcome, y = fj_resp,
                           fill = group, group = group
                           )
                       ) +
  geom_hline(yintercept = 50, linetype = 3) +
  geom_bar(
    stat = "summary", fun = mean,
    colour = "black",
    linewidth = 1,
    position = dodge
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    fun.args = list(conf.int = .95),
    width = 0.2,
    colour = "black",
    linewidth = 1,
    position = dodge
  ) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  facet_grid2(fj_value ~ group, scales = 'free_x', independent = 'x') +
  coord_cartesian(ylim = c(0, 100)) +
  xlab("") +
  ylab("Judged Percentage") +
  labs(fill = "Group") +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 28),
    strip.text.x = element_text(size = 26),
    strip.text.y = element_text(
      size = 26,
      margin = unit(c(0, 0, 0, 5), "mm")),
    legend.position = "none",
    panel.spacing.x = unit(4, "lines"),
    panel.spacing.y = unit(2, "lines")
  )

# Save Plot
ggsave("plots/freq-judge/plt_fj_means.png",
  plot = plt_fj_means,
  units = "in", width = 11, height = 8,
  dpi = 500
)

ggsave("plots/freq-judge/plt_fj_means.svg",
  plot = plt_fj_means,
  units = "in", width = 11, height = 8
)

# Acommpanying table
fj_bar_tab <- fj_long %>%
  group_by(fj_value, fj_outcome, group) %>%
  summarise(
    n = length(fj_resp),
    mean = mean(fj_resp, na.rm = TRUE),
    sd = sd(fj_resp, na.rm = TRUE),
    median = median(fj_resp, na.rm = TRUE),
    IQR = IQR(fj_resp, na.rm = TRUE)
  )

lookup <- c(value = "fj_value", outcome = "fj_outcome")
fj_bar_tab <- rename(fj_bar_tab, all_of(lookup))
