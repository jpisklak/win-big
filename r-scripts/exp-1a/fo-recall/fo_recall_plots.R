# Run the following 3 lines to execute this script independently
# setwd('../../..') # assumes wd is main dir.
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/fo-recall/fo_recall_filter.R")
#-------------------------------------------------------------------------------

props$fo_value <- factor(props$fo_value, levels = c("High", "Low"))
props$fo_cat <- factor(props$fo_cat, levels = c("0", "+40", "+80", "Other"))

# New Colour levels
props$colour_col <- paste(props$group, props$fo_cat, sep = "_")
props$colour_col <- factor(props$colour_col)

levels(props$colour_col)

col_palette <- c("white", "#7570B3", "#7570B3", "grey",
                 "white", "#1B9E77", "#1B9E77", "grey",
                 "white", "#D95F02", "#D95F02", "grey")

# Plot
plt_fo_prop <- ggplot(props, aes(x = fo_cat, y = prop, group = group)) +
  #geom_hline(yintercept = 0.5, linetype = 3) +
  geom_bar(
    stat = "identity",
    aes(fill = colour_col),
    colour = "black",
    linewidth = 1,
    position = "dodge"
  ) +
  #facet_wrap(fo_value ~ group, scales = 'free_x') +
  facet_grid2(fo_value ~ group, scales = 'free_x', independent = 'x') +
  scale_fill_manual(values = col_palette) +
  coord_cartesian(ylim = c(0, 0.85)) +
  xlab("Outcome") +
  ylab("p(Reported)") +
  labs(fill = "group") +
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
ggsave("plots/exp-1a/fo-recall/plt_fo_prop.png",
       plot = plt_fo_prop,
       units = "in", width = 11, height = 8,
       dpi = 500
)

ggsave("plots/exp-1a/fo-recall/plt_fo_prop.svg",
       plot = plt_fo_prop,
       units = "in", width = 11, height = 8
)

