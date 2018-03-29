install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggplot2)
#connect to .csv
#plots <- read.csv("data/plots.csv")
#species <- read.csv("data/species.csv")
#surveys <- read.csv("data/surveys.csv")
surveys_combined <- read.csv("data/combined.csv")
#portal_rodents <- read.csv("data/Portal_rodents_19772002_scinameUUIDs.csv")

#connect to DB
#install.packages(c("dbplyr","RSQLite"))
#connect to DB
#mammal_db <- DBI::dbConnect(RSQLite::SQLite(),"data/portal_mammals.sqlite")
#library(dbplyr)
#surveys_db <- tbl(mammal_db, "surveys")
#plots_db <- tbl(mammal_db, "plots")
#species_db <- tbl(mammal_db, "species")
surveys_complete <- surveys_combined %>% filter(!is.na(weight), !is.na(hindfoot_length), !is.na(sex), !is.na(species_id), species_id != "", sex != "") 
species_count <- surveys_complete %>% group_by(species_id, sex) %>% tally()  %>% filter(n>=50)

surveys_weight <- surveys_complete %>% group_by(genus, sex) %>% summarize(				
  mean_weight = round(mean(weight, na.rm = TRUE), digits = 2), 
  min_weight = round(min(weight, na.rm = TRUE), digits = 2),
  max_weight = round(max(weight, na.rm = TRUE), digits = 2))

ggplot(surveys_weight, aes(x = genus, y = mean_weight, color=sex)) + geom_boxplot(alpha = 1)

R
#ggplot(surveys_weight, aes(x = mean_weight, color=sex)) + geom_histogram(fill="white",alpha=0.5)

supria123
hevayumi


#any row where species_id is in this count 

surveys_complete <- surveys_complete %>% filter(species_id %in% species_count$species_id)

#plotting with ggplot 
ggplot(surveys_complete, aes(x = weight, y = hindfoot_length)) + 
  geom_point(alpha = 0.1, aes(color=species_id))



ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) + geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.3, color = "tomato")

ggplot(surveys_complete, aes(x = species_id, y = weight)) + 
  geom_jitter(alpha = .1, color = "blue") + geom_boxplot(alpha = .5)

year_counts <- surveys_complete %>% group_by(year, species_id) %>% tally()

ggplot(year_counts, aes(x=year, y=n, color=species_id))+geom_line()

#20180321 - 
#plotting with boxplot 
ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +   geom_boxplot()

ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) + geom_jitter(alpha = .1,aes(color=plot_id)) +  geom_boxplot()

ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) + geom_jitter(alpha = .1,aes(color=as.factor(plot_id))) +  geom_boxplot()

#yearly count 
year_counts <- surveys_complete %>% group_by(year, species_id) %>% tally()

ggplot(year_counts, aes(x=year, y=n, color=species_id))+geom_line()
#make a graph for each species separately 
ggplot(year_counts, aes(x=year, y=n, color=species_id))+geom_line()+facet_wrap(~species_id)

year_counts_bysex <- surveys_complete %>% group_by(year, species_id, sex) %>% tally()
ggplot(year_counts_bysex, aes(x=year, y=n, color=sex))+geom_line()+facet_wrap(~species_id)


ggplot(year_counts_bysex, aes(x=year, y=n, color=sex))+
  geom_line()+facet_wrap(~species_id)+theme_classic()

ggplot(year_counts_bysex, aes(x=year, y=n, color=sex))+
  geom_line()+facet_wrap(~species_id)+
  theme(axis.text.x = element_text(angle=90), text = element_text(size = 16))
#arrange lot 
#put this at the top of your script 
#install.packages("gridExtra")
library(gridExtra)
spp_weight_boxplot <- ggplot(surveys_complete, aes(x=species_id, y=weight))+
  geom_boxplot()+xlab("Species")+ylab("Weight (g)") +
  scale_y_log10()
spp_count_plot <- ggplot(year_counts, aes(x=year, y=n, color=species_id))+
  geom_line()+xlab("Year")+ylab("Abundance")
grid.arrange(spp_weight_boxplot, spp_count_plot)
combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol=2, widths=c(4,6))
ggsave("figs/combo_plot_weight.png", combo_plot, width=10, dpi=300)

