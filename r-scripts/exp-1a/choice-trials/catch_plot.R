# Run the following 3 lines to execute this script independently
# setwd('../../..') # assumes wd is main dir.
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
#-------------------------------------------------------------------------------

#Plot
dodge <- position_dodge(.3)

plt_catch <- ggplot(ctch_res, aes(x = block, y = cp, group = group)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    linewidth = 2,
    position = dodge,
    aes(colour = group)
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    fun.args = list(conf.int = .95),
    width = 0.2,
    aes(group = group),
    colour = "black",
    linewidth = 1,
    position = dodge
  ) +
  geom_point(
    stat = "summary", fun = mean,
    aes(fill = group, shape = group),
    size = 7,
    stroke = 2,
    position = dodge
  ) +
  scale_shape_manual(values = 21:23) +
  scale_colour_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_fill_manual(values = brewer.pal(n = 8, name = "Dark2")) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +

  # coord_cartesian(ylim = c(0, 1)) +
  xlab("Block") +
  ylab("p(High Value)") +
  labs(
    colour = "",
    shape = "",
    fill = ""
  ) +
  theme_custom() +
  guides(shape = guide_legend(position = 'inside')) +
  theme(legend.position.inside = c(0.7, 0.4))

#Save Plot  
ggsave('plots/exp-1a/choice-trials/catch_results.png',
       plot = plt_catch,
       units = 'in', width = 11, height = 8, 
       dpi = 500)

ggsave('plots/exp-1a/choice-trials/catch_results.svg',
       plot = plt_catch,
       units = 'in', width = 11, height = 8)
    
    
