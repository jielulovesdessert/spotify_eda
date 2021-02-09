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
  library(skimr)
  library(cowplot)
  library(reshape2)
})

# feature skim
df.data <- fread("data/data.csv")
skim(df.data)

# boxplot of most features
pdf(file="image/boxplot.pdf", width=20, heigh=10)
df.data %>%
  select(-c(acousticness, id, name, year, release_date, artists, duration_ms)) %>%
  boxplot()
dev.off()
# sadly the box plot is not very helpful to skim 

# 1
# distribution of all numeric features
df.data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + 
  ggtitle("Distribution of all numeric features")

ggsave("image/1.0_feature_distribution.pdf")

# it might be woth to do some log transformation for features such as duration_ms, speechiness if we're heading for modeling
# example - log duration
# hist(log(df.data$duration_ms))

# 2
# heatmap to show the correlation
df.data %>%
  keep(is.numeric) -> df.numeric

cormat <- round(cor(df.numeric), 2)
melted_cormat <- melt(cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# reorder the correlation matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# part 2 final chart
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  ggtitle("An organized correlation heatmap")

ggsave("image/2.0_correlation_heatmap.pdf")

# This chart clearly showed some trend:
# When time passes, songs are becoming less accoustic, while meanwhile 
# louder, more energetic, and more popular
# Note that this correlations doesn't indicate any causation; 
# eg a wrong statement: songs are becoming more populare because they are less accoustic

# 3
# line chart



