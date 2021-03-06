#load library
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(dbplyr)
library(RSQLite)
library(DBI)
#install.packages("ggpmisc")
library(ggpmisc)
library(rattle)

#load data from combined.csv
surveys_combined <- read.csv("data/combined.csv")

#clear the NA / empty data from combined.csv
surveys_combined_clear<- surveys_combined %>% filter(!is.na(sex), 
                                                         sex != "",
                                                         !is.na(hindfoot_length), 
                                                         hindfoot_length != "",
                                                         !is.na(weight), 
                                                         weight != "") 


#Check the correlation each plot type and hindfootlength 
## Weight and Hindfoot_length in Control plot
lw_control <- surveys_combined %>% 
  filter(!is.na(hindfoot_length), !is.na(weight)) %>%
  select(hindfoot_length, weight , genus, plot_type) %>%
  filter(plot_type == "Control")

#- Check names 
names(lw_control)
#- Attach the data
attach(lw_control)
#- Check the type of variable for weight and hindfoot_length in control plot
class(weight)
class(hindfoot_length)
plot(weight, hindfoot_length, main = "The relationship weight and hindfoot_length in control plottype")
cor(weight, hindfoot_length)
lw_controlstat <- lm(hindfoot_length ~ weight)
summary(lw_controlstat)

## Weight and Hindfoot_length in Long-term Krat Exclosure plot (2)
lw_longterm <- surveys_combined %>% 
  filter(!is.na(hindfoot_length), !is.na(weight)) %>%
  select(hindfoot_length, weight , genus, plot_type) %>%
  filter(plot_type == "Long-term Krat Exclosure")

#- Check & Statistics
names(lw_longterm)
attach(lw_longterm)
class(weight)
class(hindfoot_length)
plot(weight, hindfoot_length, main = "The relationship weight and hindfoot_length in longterm plottype")
cor(weight, hindfoot_length)
lw_longtermstat <- lm(hindfoot_length ~ weight)
summary(lw_longtermstat)


## Weight and Hindfoot_length in Rodent Exclosure plot (3)
lw_rodent <- surveys_combined %>% 
  filter(!is.na(hindfoot_length), !is.na(weight)) %>%
  select(hindfoot_length, weight , genus, plot_type) %>%
  filter(plot_type == "Rodent Exclosure")

#- Check & Statistics
names(lw_rodent)
attach(lw_rodent)
class(weight)
class(hindfoot_length)
plot(weight, hindfoot_length, main = "The relationship weight and hindfoot_length in rodent plottype")
cor(weight, hindfoot_length)
lw_rodentstat <- lm(hindfoot_length ~ weight)
summary(lw_rodentstat)

## Weight and Hindfoot_length in Short Term Exclosure plot (4)
lw_shortterm <- surveys_combined %>% 
  filter(!is.na(hindfoot_length), !is.na(weight)) %>%
  select(hindfoot_length, weight , genus, plot_type) %>%
  filter(plot_type == "Short-term Krat Exclosure")

#- Check & Statistics
names(lw_shortterm)
attach(lw_shortterm)
class(weight)
class(hindfoot_length)
plot(weight, hindfoot_length, main = "The relationship weight and hindfoot_length in shortterm plottype")
cor(weight, hindfoot_length)
lw_shorttermstat <- lm(hindfoot_length ~ weight)
summary(lw_shorttermstat)

## Weight and Hindfoot_length in Spectab Exclosure plot (5)
lw_spectab <- surveys_combined %>% 
  filter(!is.na(hindfoot_length), !is.na(weight)) %>%
  select(hindfoot_length, weight , genus, plot_type) %>%
  filter(plot_type == "Spectab exclosure")

#- Check & Statistics
names(lw_spectab)
attach(lw_spectab)
class(weight)
class(hindfoot_length)
plot(weight, hindfoot_length, main = "The relationship weight and hindfoot_length in spectab plottype")
cor(weight, hindfoot_length)
lw_spectabstat <- lm(hindfoot_length ~ weight)
summary(lw_spectabstat)


lengthplot <- ggplot(data = length_control, x='genus', y='hindfoot_length', color ="genus") + 
theme(axis.line = element_line(size = 1.5), axis.text.x = element_text(angle = 45, vjust = 0.5)
      
ggsave("lengthplot.jpg")


length_control <- surveys_combined %>% 
  filter(!is.na(hindfoot_length)) %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Control")

length_Longterm <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Long-term Krat Exclosure")

length_rodent <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Rodent Exclosure")

length_shortterm <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Short-term Krat Exclosure")

length_spectab <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Spectab exclosure")

#Check the correlation statistics each plot
length_control <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Control")

ggplot(length_control, aes(x=plot_type, y=hindfoot_length)) + 
  geom_point(color='green', size = 4) +
  geom_smooth(method = lm, se=FALSE, fullrange=TRUE, color='red')
length_control <- cor(plot_type, hindfoot_length, method = 'pearson')



length_Longterm <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Long-term Krat Exclosure")

length_rodent <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Rodent Exclosure")

length_shortterm <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Short-term Krat Exclosure")

length_spectab <- surveys_hindfoot %>%
  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Spectab exclosure") 

#Ggplot hindfoot_length and plot_type
rel_length_plot <- ggplot() +
  geom_point(data = length_control, aes(x=plot_type, y=hindfoot_length), color = 'green') +
  geom_point(data = length_Longterm, aes(x=plot_type, y=hindfoot_length), color = 'red') +
  geom_point(data = length_rodent, aes(x=plot_type, y=hindfoot_length), color = 'blue') +
  geom_point(data = length_shortterm, aes(x=plot_type, y=hindfoot_length), color = 'yellow') +
  geom_point(data = length_spectab, aes(x=plot_type, y=hindfoot_length), color = 'pink') 

ggsave("image/rellengthplot.jpg", rel_length_plot, width=11, dpi=300)

  select(hindfoot_length, plot_type , genus) %>%
  filter(plot_type == "Control")

# Add correlation coefficient
wg + stat_cor(method = "pearson", label.x = 1995, label.y = 200)
# Extend the regression lines beyond the domain of the data

View(length_plottype)
#From the table, it gives relatively the same results, No correlation between hindfoot_length and plot_type".
#1 Whatever the size of the hindfoot_length, we can use any other plot_type and it will give almost relatively result

install.packages("ggpubr")
library("ggpubr")

fit<- lm(surveys$weight~surveys_combined_clear$hindfoot_length)
summary(fit)


#Clear the NA of hindfoot_Length, the min, max and mean
surveys_hindfoot %>% filter(!is.na(hindfoot_length))

#Make plotting -> ggplot of hindfoot_lenght, min, max and mean of species_id
ggplot(surveys_hindfoot, aes(x = hindfoot_length, y = species_id)) +
  geom_boxplot()+xlab("length") + ylab("species_id")

      
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

#create relationship of 2 variable 
plot(surveys_combined_clear$hindfoot_length, surveys_combined_clear$weight, 
    main="Relationship Length and Weight", 
    xlab = "Hindfoot Length", ylab = "Weight")


#correlation weight per plot type 

#create variable to show weight in plot type : control
weight_ltk_control <- surveys_combined_clear %>% filter(plot_type == 'Control' | plot_type == 'Long-term Krat Exclosure') 
weight_control <- surveys_combined_clear %>% filter(plot_type == 'Control') %>% select(weight)
weight_ltk_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Long-term Krat Exclosure') %>% select(weight)
weight_rodent_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Rodent Exclosure') %>% select(weight)
weight_stk__exclosure <- surveys_combined_clear %>% filter(plot_type == 'Short-term Krat Exclosure') %>% select(weight)
weight_s_exclosure <- surveys_combined_clear %>% filter(plot_type == 'Spectab exclosure') %>% select(weight)

temp <- left_join(weight_control, weight_ltk_exclosure)


weight.plotype<- data.frame(weight= surveys_combined_clear$weight, plot_type = surveys_combined_clear$plot_type)
wg.control <- weight.plotype %>% filter (plot_type == "Control")
wg.rodent <- weight.plotype %>% filter (plot_type == 'Rodent Exclosure') 


weight <- list(weight_control, weight_ltk_exclosure, weight_rodent_exclosure, weight_stk__exclosure, weight_s_exclosure)

### combine the year columns into a single column with separate rows for each year; assign to new vector
pop_long <- gather(pop_wide,year,population,-country)

cc3 <- as.data.frame(table(unlist(weight_control, weight_ltk_exclosure, weight_rodent_exclosure, weight_stk__exclosure, weight_s_exclosure)))

weight <- join(weight_control, weight_ltk_exclosure)


weight <- matrix(c(weight_control, weight_ltk_exclosure, weight_rodent_exclosure, weight_s_exclosure ),ncol=4,byrow=TRUE)
colnames(weight) <- c("control","Long-term Krat Exclosure","Rodent Exclosure", "Short-term Krat Exclosure", "Spectab exclosure")

weight <- paste(weight_control, weight_ltk_exclosure, weight_rodent_exclosure, weight_s_exclosure)

ggplot(weight_ltk_control, group_by(plot_type,weight), aes(color=plot_type, y=weight))

ggplot(weight_ltk_control, aes(x=plot_type, y=weight))+
  geom_point(size=2, shape=23)+xlab("plot type")+ylab("Weight (g)")






##find R-squared and P-value 
fit<- lm(surveys_combined_clear$weight~surveys_combined_clear$hindfoot_length)
summary(fit)

<<<<<<< HEAD
=======
##
>>>>>>> 76f77eb44d648998c0577d251f4d5c35676a4d6a
#create line chart plot type per year
year_plot_type <- surveys_combined_clear %>% group_by(year, plot_type) %>% tally()
line_chart <- ggplot(year_plot_type, aes(x=year, y=n, color=plot_type)) + 
  geom_line() + xlab("Year") + ylab("Plot Type")

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
