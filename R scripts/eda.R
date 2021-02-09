#!/usr/bin/env Rscript
setwd("~/Desktop/Spotify_eda/")

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(JJHmisc)
  library(latex2exp)
  library(ggplot2)
  library(magrittr)
  library(feather)
  library(tidyverse)
  require(lubridate)
  library(broom)
  library(ggrepel)
  library(hrbrthemes)
})