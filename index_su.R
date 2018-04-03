#load library
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(dbplyr)
library(RSQLite)
library(DBI)
library(ggpubr)
 
#load data from combined.csv
surveys_combined <- read.csv("data/combined.csv")

#clear the NA / empty data from combined.csv
surveys_combined_clear<- surveys_combined %>% filter(!is.na(sex), 
                                                         sex != "",
                                                         !is.na(hindfoot_length), 
                                                         hindfoot_length != "",
                                                         !is.na(weight), 
                                                         weight != "") 

#Check the correlation between species id, the min, max and mean of species_id
surveys_hindfoot <- surveys_combined %>%
  group_by(species_id, hindfoot_length) %>%
  summarise(mean_w = mean(hindfoot_length),
            min_w = min(hindfoot_length),
            max_w = max(hindfoot_length))

#Clear the NA of hindfoot_Length, the min, max and mean
surveys_hindfoot %>% filter(!is.na(hindfoot_length))
      
#write surveys_combined.csv
write_csv(surveys_combined_clear, path = "data/output/surveys_combined.csv")

#connect to database portal_mammals.sqlite
mammal_db <- dbConnect(dbDriver("SQLite"), "data/portal_mammals.sqlite")

#get all the tables
tables <- dbListTables(mammal_db)
head(tables)

##make a query from surveys, plots, species where is not NA or empty
surveys_db <- dbGetQuery(mammal_db, "select surveys.*, species.species, species.taxa, species.genus,
                                      plots.plot_type
                                      from surveys, species, plots
                                      where surveys.species_id = species.species_id
                                      and surveys.plot_id = plots.plot_id
                                      and surveys.sex <> 'NA'
                                      and surveys.hindfoot_length <> 'NA'
                                      and surveys.weight <> 'NA'
                                    ")
#write surveys_combined_db.csv
write_csv(surveys_db, path = "data/output/surveys_combined_db.csv")

#disconnect database portal_mammal
dbDisconnect(mammal_db)

#create distribution of 1 variable

#create histogram to show distribution of hindfoot_length
hist(surveys_combined_clear$hindfoot_length, 
     main="Histogram of Length", xlab ="Hindfoot Length")

#create boxplot to show distribution of hindfoot_length
boxplot(surveys_combined_clear$hindfoot_length, 
     main="Boxplot of Length", xlab ="Hindfoot Length")


#create histogram to show distribution of weight
hist(surveys_combined_clear$weight, 
     main="Distribution of Weight", xlab ="Weight")

#create histogram to show distribution of weight
hist(surveys_combined_clear$weight, 
     main="Distribution of Weight", xlab ="Weight")


# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
sp <- ggscatter(surveys_combined_clear, x = "hindfoot_length", y = "weight",title = "Relationship between weight and length of hindfoot", xlab = "length of hindfoot (mm)", ylab = "Weight of animal(gm)",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
)
# Add correlation coefficient
final.plot<- sp + stat_cor(method = "pearson", label.x = 10, label.y = 200)
#save plot into image
ggsave("image/plot3.jpg", final.plot, width=10, dpi=300)


# Extend the regression lines beyond the domain of the data
##weight changes over the year

wg<- ggscatter(data=surveys_combined_clear, x='year',y='weight', color ="plot_type",shape = "plot_type",
               title = "Weight changes over the year based on each plot type", xlab = "Year", ylab = "Weight of animal(gm)",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue","red","green","yellow","pink", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE # Add confidence interval
 )  
# Add correlation coefficient
final.wg <- wg + stat_cor(method = "pearson", label.x = 1995, label.y = 200)
final.wg
ggsave("image/plot4.jpg", final.wg, width=10, dpi=300)



# Extend the regression lines beyond the domain of the data
##hindfoot changes over the year

hd<- ggscatter(data=surveys_combined_clear, x='year',y='hindfoot_length', color ="plot_type",shape = "plot_type",
               title = "Hindfoot length changes over the year based on each plot type", xlab = "Year", ylab = "length of Hindfoot(mm)",
               add = "reg.line",  # Add regressin line
               add.params = list(color = "blue","red","green","yellow","pink", fill = "lightgray"), # Customize reg. line
               conf.int = TRUE # Add confidence interval
)  
# Add correlation coefficient
final.hd<- hd + stat_cor(method = "pearson", label.x = 1995, label.y = 200)
final.hd
ggsave("image/plot5.jpg", final.wg, width=10, dpi=300)
#create line chart plot type per year
year_plot_type <- surveys_combined_clear %>% group_by(year, plot_type) %>% tally()
line_chart <- ggplot(year_plot_type, aes(x=year, y=n, color=plot_type)) + 
  geom_point() + xlab("Year") + ylab("Plot Type")  
line_chart + stat_cor(method = "pearson", label.x = 1995, label.y = 900)

p <- ggplot(data = year_plot_type, aes(x = year, y = n, color =plot_type)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()
p + stat_cor(method = "pearson",aes(color = plot_type),label.sep = ", ", label.x = 1995, label.y = 900)
#create bar chart sex per year
year_sex <- surveys_combined_clear %>% group_by(year, sex) %>% tally()
bar_chart <- ggplot(year_sex, aes(x=year, y=n, color=sex)) + 
  geom_bar(stat="identity") + xlab("Year") + ylab("Sex")

#put chart to grid
timeseries_plot <- grid.arrange(bar_chart, line_chart, ncol=2, widths=c(4,6))

#save plot into image
ggsave("image/plot1.jpg", timeseries_plot, width=10, dpi=300)

#create boxplot chart weight per plot_type
boxplot_chart_weight <- ggplot(surveys_combined_clear, aes(x=plot_type, y=weight))+
  geom_boxplot()+xlab("plot type")+ylab("Weight (g)") +  geom_jitter() +
  scale_y_log10()

#create boxplot chart hindfoot length per plot_type
boxplot_chart_length <- ggplot(surveys_combined_clear, aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot()+xlab("plot type")+ylab("Length") + geom_jitter()
  scale_y_log10()

#put chart to grid
frequency_plot <- grid.arrange(boxplot_chart_weight, boxplot_chart_length, ncol=2, widths=c(5,5))

#save plot into image
ggsave("image/plot2.jpg", frequency_plot, width=10, dpi=300)

#todo list 
#find the correlation and p-value & R-square
#make RMarkup