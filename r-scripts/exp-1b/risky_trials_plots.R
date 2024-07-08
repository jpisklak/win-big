# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/exp-1b
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1b/subj_stats.R")
# source('r-scripts/exp-1b/risky_trials_filter.R')

#-------------------------------------------------------------------------------

# Conventional EO Plot by Block
#-------------------------------------------------------------------------------
dodge <- 0.25
plt_risky_blk <- ggplot(risky_res, aes(
  x = block, y = cp,
  group = choice_value,
  shape = choice_value,
  fill = choice_value
)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    aes(colour = choice_value),
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    # fun.data = "mean_cl_boot",
    fun.args = list(conf.int = .95),
    # fun.args = list(conf.int = .95, B = 5000),
    width = 0.2,
    # aes(group = group),
    colour = "black",
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_point(
    stat = "summary", fun = mean,
    stroke = 1.5,
    size = 5,
    position = position_dodge(width = dodge)
  ) +
  facet_wrap(~group) +
  annotate(
    "segment",
    x = -Inf, xend = -Inf, y = -Inf, yend = Inf,
    linewidth = 1
  ) +
  scale_colour_manual(values = c("#ff6961", "#77dd77")) +
  scale_fill_manual(values = c("#ff6961", "#77dd77")) +
  scale_shape_manual(values = 21:23) +
  scale_x_continuous(breaks = 1:6) +
  coord_cartesian(ylim = c(0, 0.8)) +
  xlab("Block") +
  ylab("p(Risky)") +
  labs(
    shape = "Choice Value:",
    fill = "Choice Value:",
    colour = "Choice Value:"
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 28),
    strip.text = element_text(size = 30),
    legend.position = "bottom"
  )

# Save Plot
ggsave("plots/exp-1b/plt_risky_blk.png",
  plot = plt_risky_blk,
  units = "in", width = 11, height = 7,
  dpi = 500
)

ggsave("plots/exp-1b/plt_risky_blk.svg",
  plot = plt_risky_blk,
  units = "in", width = 11, height = 7
)
