library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(scales)

data <- data %>% mutate(age = replace_na(age, 25))
data <- data %>% mutate(boxplot = replace_na(boxplot, 2))
data %>% 
  select(age, barplot, fav_plot, boxplot, linegraph, stackedgraph) %>% 
  group_by(fav_plot) %>% 
  summarise(avg_age=mean(age), avg_barplot=mean(barplot),avg_boxplot=mean(boxplot), avg_linegraph=mean(linegraph), avg_stackedgraph=mean(stackedgraph)) %>% 
  ggplot() +
  geom_point(aes(x=fav_plot, y=avg_barplot, fill="Barplot", color="Barplot", shape="Barplot"), size=5, alpha=0.7) +
  geom_line(aes(x=as.numeric(fav_plot), y=avg_barplot, color="Barplot"), linetype="dotdash") +
  geom_point(aes(x=fav_plot, y=avg_boxplot, fill="Boxplot",color="Boxplot", shape="Boxplot"), size=5, alpha=0.7) +
  geom_line(aes(x=as.numeric(fav_plot), y=avg_boxplot,color="Boxplot"), linetype="dotdash") +
  geom_point(aes(x=fav_plot, y=avg_linegraph, fill="Line Graph",color="Line Graph",shape="Line Graph"), size=5, alpha=0.7) +
  geom_line(aes(x=as.numeric(fav_plot), y=avg_linegraph, color="Linegraph"), linetype="dotdash") +
  geom_point(aes(x=fav_plot, y=avg_stackedgraph, fill="Stacked Area Graph", color="Stacked Area Graph", shape="Stacked Area Grpah"), size=5, alpha=0.7) +
  geom_line(aes(x=as.numeric(fav_plot), y=avg_stackedgraph, color="Stacked Area Graph"), linetype="dotdash") +
  xlab("\nFavorite Type of Plot") +
  ylab("Difficulty to Make a Plot\n") +
  ggtitle("Average Difficulty Score to Make a plot Per Favorite Plot\n") +
  scale_color_discrete(name="Types of Plot",
                    breaks=c("Barplot", "Boxplot", "Line Graph", "Stacked Area Graph"),
                      labels=c("Barplot", "Boxplot", "Line Graph", "Stacked Area Graph")) +
  scale_fill_discrete(name="Types of Plot",
                    breaks=c("Barplot", "Boxplot", "Line Graph", "Stacked Area Graph"),
                      labels=c("Barplot", "Boxplot", "Line Graph", "Stacked Area Graph")) +
  scale_shape_discrete(name="Types of Plot",
                      labels=c("Barplot", "Boxplot", "Line Graph", "Stacked Area Graph")) +
  theme_classic()
