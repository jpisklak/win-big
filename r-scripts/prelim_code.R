# Install and Load Necessary Packages
pkgs <- c(
  "rcompanion",
  "nlme",
  "Hmisc",
  "RColorBrewer",
  "tidyverse",
  "ggh4x"
)

installed_pkgs <- pkgs %in% rownames(installed.packages())

if (any(installed_pkgs == FALSE)) {
  install.packages(pkgs[!installed_pkgs])
}

invisible(lapply(pkgs, library, character.only = TRUE))

# Set tibble defaults
options(
  pillar.print_min = 35,
  pillar.print_max = 35,
  pillar.width = Inf
)

# Load custom ggplot theme
source("r-scripts/theme_custom.R")

