#load library
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(dbplyr)
library(RSQLite)
library(DBI)
library (plyr)

#load data from combined.csv
surveys_combined <- read.csv("data/combined.csv")

#clear the NA / empty data from combined.csv
surveys_combined_clear<- surveys_combined %>% filter(!is.na(sex), 
                                                         sex != "",
                                                         !is.na(hindfoot_length), 
                                                         hindfoot_length != "",
                                                         !is.na(weight), 
                                                         weight != "") 

weight_control <- surveys_combined_clear %>% filter(plot_type == 'Control') %>% select(weight)
weight_ltk_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Long-term Krat Exclosure') %>% select(weight)

mat=matrix(data=c(weight_control, weight_ltk_exclosure),ncol=2)

l=list(control = weight_control, ltx_exclousure = weight_ltk_exclosure)
ldata = do.call(rbind.data.frame, l)
df <- ldply (l, data.frame)

mean_weight <- surveys_combined_clear %>% group_by(plot_type, genus) %>% 
  select(plot_type, genus, species_id, weight)

surveys_wide_mean_weight <- surveys_mean_weight %>%
  spread(key = genus, value = mean_weight, fill = 0)



ggplot(data = surveys_combined_clear) + 
  geom_point(mapping = aes(x=plot_type, y = weight))

ggscatter(surveys_combined_clear, x = "plot_type", y = "weight",title = "Relationship between weight and plot type", xlab = "Plot Type", ylab = "weight(g)",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE # Add confidence interval
)

stat_cor(method = "pearson", label.x = 10, label.y = 300)






#create variable to show weight in plot type : control
weight_control <- surveys_combined_clear %>% filter(plot_type == 'Control') %>% select(weight)
weight_ltk_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Long-term Krat Exclosure') %>% select(weight)

weight_rodent_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Rodent Exclosure') %>% select(weight)
weight_stk__exclosure <- surveys_combined_clear %>% filter(plot_type == 'Short-term Krat Exclosure') %>% select(weight)
weight_s_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Spectab exclosure') %>% select(weight)

weights <- data.frame(unlist(weight_control), unlist(weight_ltk_exclosure))


Reduce(full_join, list(weight_control, weight_ltk_exclosure))


vec1 <- c(weight_control)  # numeric vector
vec2 <- c(weight_ltk_exclosure)  # character vector




vec3 <- c(TRUE, FALSE, TRUE, TRUE)  # logical vector
vec4 <- gl(4, 1, 4, label = c("l1", "l2", "l3", "l4"))  # factor with 4 levels


