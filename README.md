# README: win-big

## Overview

This repository contains the data and R code associated with Experiments 1a and 1b from the study *Effects of Initial Experiences on Risky Choice* by Elliot A. Ludvig, Neil McMillan, Jeffrey M. Pisklak, Nick Simonsen, Alice Mason, Jason Long, Marcia L. Spetch, and Christopher R. Madan.

**GitHub Repository:**

- [https://github.com/jpisklak/win-big](https://github.com/jpisklak/win-big)

**Associated OSF Project:**

- [https://osf.io/d85eu/](https://osf.io/d85eu/)

The most accessible form of the data to download is `exp_1a_data.csv` and `exp_1b_data.csv` respectively, located within the `./data` directory.

To view the results document, download `win-big-results--yyyy-mm-dd.html` and open it in a web browser.

## Contents

The directory, file, and variable names within this repository are designed to be self-explanatory.

- `generate_markdown.R`: An R script used to run all the files necessary to produce the HTML results document `win-big-results--yyyy-mm-dd.html`.

- `/data`: Contains cleaned versions of the raw data generated with `/r-scripts/exp-1a/exp_1a_data_filter.R` and `/r-scripts/exp-1b/exp_1b_data_filter.R`

- `/data/raw-data`: Contains all the raw data merged from the e-prime files.

- `/r-scripts`: Contains scripts for specific tasks such as merging raw data files, plotting risky choice results, etc. These files are sourced in the markdown generation. To run one of these scripts independently, adjust the working directory to `/win-big` and run the dependencies listed at the top of the script.

- `/plots`: Stores all generated plots in both .png and .svg formats.

- `/markdown`: Contains the R markdown files used to generate the HTML results document.
