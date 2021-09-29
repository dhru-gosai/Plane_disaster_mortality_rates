#------------------------------#
# MA5820 Assignment 3: Capstone
# Author: Dhruvisha Gosai
#------------------------------#

RStudio.Version()

#Loading relevant R packages

library(car)
library(datasets)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(qqplotr)
library(ggfortify)
library(ggmap)
library(ggthemes)
library(hrbrthemes)
library(viridis)
library(tidyr)
library(MASS)
library(lmtest)
library(boot)
library(plyr)
library(readr)
library(data.table)
library(crosstalk)
library(plotly)
library(shiny)
library(leaflet)
library(zoo)
library(cluster)
library(UsingR)
library(cowplot)
library(sqldf)
library(moments)
library(ggpubr)

#==============================================================================#

#-------------------------------#
#          IMPORT DATA          #
#-------------------------------#

HDI <- read.csv("D:/Dhru Folder/JCU - Master of Data science/MA5820 - Statistical Methods and data analysis/Assignment 3/hdi_human_development_index.csv"
                ,header = TRUE
                ,sep=",")

plane_affected <- read.csv("D:/Dhru Folder/JCU - Master of Data science/MA5820 - Statistical Methods and data analysis/Assignment 3/plane_crash_affected_annual_number.csv"
                ,header = TRUE
                ,sep=",")

plane_death <- read.csv("D:/Dhru Folder/JCU - Master of Data science/MA5820 - Statistical Methods and data analysis/Assignment 3/plane_crash_deaths_annual_number.csv"
                     ,header = TRUE
                     ,sep=",")

#==============================================================================#

#---------------------------------------------------------#
#  Data Transformation: Clean and transform as required   #
#---------------------------------------------------------#

# View dataset
view(HDI)
view(plane_affected)
View(plane_death)

# Summary
str(HDI)
str(plane_affected)
str(plane_death)

summary(HDI)
summary(plane_affected)
summary(plane_death)

describe(HDI)
describe(plane_affected)
describe(plane_death)

#---------------------------
# Transpose the data to join
HDI_transpose             <- HDI            %>% pivot_longer(-country, names_to = "year", values_to = "HDI") %>%
                                                  transform(year_clean = substr(year,2,5))
plane_affected_transpose  <- plane_affected %>% pivot_longer(-country, names_to = "year", values_to = "Affected_Per_Year") %>%
                                                  transform(year_clean = substr(year,2,5))
plane_death_transpose     <- plane_death    %>% pivot_longer(-country, names_to = "year", values_to = "Deaths_Per_Year") %>%
                                                  transform(year_clean = substr(year,2,5))

# Creating year variable as numeric and dropping year_clean column
HDI_transpose$year <- as.numeric(HDI_transpose$year_clean,replace = T)
HDI_transpose      <- subset(HDI_transpose, select = -c(year_clean)) #Drop column

plane_affected_transpose$year <- as.numeric(plane_affected_transpose$year_clean,replace = T)
plane_affected_transpose      <- subset(plane_affected_transpose, select = -c(year_clean)) #Drop column

plane_death_transpose$year <- as.numeric(plane_death_transpose$year_clean,replace = T)
plane_death_transpose      <- subset(plane_death_transpose, select = -c(year_clean)) #Drop column

#---------------------------
# Creating a single table with data for HDI, deaths and affected -> cleaning the year to remove x
full_data_x1 <- inner_join(HDI_transpose, plane_affected_transpose, by=c("year" = "year","country"="country")) %>%
                  inner_join(.,plane_death_transpose, by=c("year" = "year","country"="country"))


# Checking to see if year is numeric and all variables
summary(full_data_x1)

#---------------------------
# Creating Char HDI variable
full_data_x2 <- full_data_x1 %>%
                  filter(!is.na(HDI)) %>%
                  mutate(HDI_Band    = case_when(HDI <= 0.333 ~ "1",
                                                 HDI > 0.333 & HDI <= 0.666  ~ "2",
                                                 HDI > 0.666 & HDI <= 1      ~ "3",
                                                 HDI > 1 ~ "4"),
                         year_band   = case_when(year >= 1990 & year <= 1994 ~ "1990 - 1994",
                                                 year >= 1995 & year <= 1999 ~ "1995 - 1999",
                                                 year >= 2000 & year <= 2004 ~ "2000 - 2004",
                                                 year >= 2005                ~ "2005 +"),
                         decade_band = case_when(year >= 1990 & year <= 1999 ~ "1990 - 1999",
                                                 year >= 2000 & year <= 2008 ~ "2000 - 2008"))

full_data_x2$Injured_Per_Year <- full_data_x2$Affected_Per_Year - full_data_x2$Deaths_Per_Year

full_data_x2$Survived_flag <- ifelse((full_data_x2$Affected_Per_Year - full_data_x2$Deaths_Per_Year) > 0, "Survived", "None Survived")


#==============================================================================#

#---------------------#
# Data Visualisation  #
#---------------------#

#------ Time series to visualise datasets: Plane deaths ------#

TimeSeries <- plane_death_transpose %>%
  group_by(year) %>%
  summarise(Deaths_Per_Year = sum(Deaths_Per_Year)) %>%
  select(year,Deaths_Per_Year)

ggplot(data=TimeSeries, aes(x=year, y=Deaths_Per_Year)) +
  geom_line( color="steelblue") +
  geom_point() +
  xlab("Time Period") +
  ylab("Number of Deaths") +
  ggtitle("Time series of number of deaths from 1970 to 2008") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#------ Visualise datasets: Full merge 2 by HDI ------#
TimeSeries2 <- full_data_x2 %>%
  group_by(year, HDI_Band) %>%
  summarise(Deaths_Per_Year = sum(Deaths_Per_Year)) %>%
  select(year, HDI_Band ,Deaths_Per_Year)

ggplot(data=TimeSeries2, aes(x=year, y=Deaths_Per_Year, fill=HDI_Band, color=HDI_Band)) +
  geom_bar(stat="identity", width=.65, position = position_dodge(width=0.9))  +
  xlab("Time Period") +
  ylab("Number of Deaths") +
  ggtitle("Number of deaths by HDI between 1990 and 2008") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1))

#-------- Top 20 Countries with highest deaths ------#

Top20_Countries_death <- plane_death_transpose %>%
  filter(between(year, 1999, 2008)) %>%
  group_by(country) %>%
  summarise(Deaths_Per_Year = sum(Deaths_Per_Year)) %>%
  select(country,Deaths_Per_Year) %>%
  arrange(desc(Deaths_Per_Year))

Top20_Countries_death <- data.frame(head(Top20_Countries_death, n = 10))

ggplot(data=Top20_Countries_death, aes(x = reorder(country, Deaths_Per_Year), y = Deaths_Per_Year)) +
  geom_bar(stat = 'identity', width = 0.5) +
  geom_text(aes(label = Deaths_Per_Year), stat = 'identity', data = Top20_Countries_death, hjust = -0.1, size = 3.5) +
  coord_flip() +
  xlab('Top 10 Countries') +
  ylab('Number of Deaths') +
  ggtitle('Top 10 countries by deaths in plane crash between 1990 and 2008') +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 12, face = "bold"))

HDI_2008 <- full_data_x2 %>% filter(year=="2008") %>% select(HDI_Band, country)
Top20_Countries_death_HDI <- inner_join(Top20_Countries_death, HDI_2008, by="country")
#==============================================================================#
#------------------#
# Data Exploration #
#------------------#

#Need to remove observations with deaths per year as 0
full_data_x3 <- full_data_x2 %>%
  filter(Deaths_Per_Year > 0 | Affected_Per_Year > 0)

#Check for distribution for number of deaths
ggplot1 <- ggplot(full_data_x3, aes(x=Deaths_Per_Year)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) +
  geom_density(colour = 'blue')+
  xlab("Death count") +
  ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 12, face = "bold"))


#****** Log Transformation *****
full_data_x3$Log_Deaths_Per_Year   <- log(full_data_x3$Deaths_Per_Year)
full_data_x3$Log_Affected_Per_Year <- log(full_data_x3$Affected_Per_Year)
full_data_x3$Injured_Per_Year      <- log(full_data_x3$Injured_Per_Year)


#Check for distribution for number of deaths
ggplot2 <- ggplot(full_data_x3, aes(x=Log_Deaths_Per_Year)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) +
  geom_density(colour = 'blue')+
  xlab("Log Transformed Death count") +
  ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 12, face = "bold"))

plot_grid(ggplot1, ggplot2, labels = "Before and After Log transform of Number of Deaths")

# Need to make sure there are no outliers - NONE FOUND
boxplot(full_data_x3$Log_Deaths_Per_Year, main = "Boxplot of log transformed number of deaths")
outlier <- boxplot.stats(full_data_x3$Log_Deaths_Per_Year)$out     #Identifying outlier


#------ Confirmation that the transformation worked
# Skewness test before transformation
skewness(full_data_x3$Deaths_Per_Year)     #Before log transform
skewness(full_data_x3$Log_Deaths_Per_Year) #After log transform
#---------------------------------------------------#
#---------------------------------------------------#

# Conduct Shapiro test for normality
shapiro.test(full_data_x3$Log_Deaths_Per_Year)


# Create a random sample
seed <- set.seed(1122)
Random_Sample <- sample(1:nrow(full_data_x3), 150)

full_data_x4 <- full_data_x3[Random_Sample, ]
view(full_data_x4)
summary(full_data_x4)

# Skewness test before transformation
skewness(full_data_x3$Deaths_Per_Year)     #Before log transform
skewness(full_data_x3$Log_Deaths_Per_Year) #After log transform
#==============================================================================#
# Hypothesis Testing
#==============================================================================#

#------- Objective 1: Mean deaths by HDI group --------#
# H0: Number of deaths by HDI groups are equal
# H1: Number of deaths by HDI groups are not equal
#------------------------------------------------------#

ggplot3 <- ggqqplot(full_data_x4$Deaths_Per_Year,
         ylab="Deaths from Plane Crash") #distribution check

ggplot4 <- ggqqplot(full_data_x4$Log_Deaths_Per_Year,
         ylab="Log Deaths from Plane Crash") #distribution check

plot_grid(ggplot3, ggplot4,
          labels = c('Q-Q Plot of Deaths (Sample Data)', 'Q-Q Plot of Log Deaths (Sample Data)'),
          ncol = 1)

shapiro.test(full_data_x4$Log_Deaths_Per_Year) #Normality test - Not normally distributed

HDI_2 <- subset(full_data_x4, HDI_Band == "2")
HDI_3 <- subset(full_data_x4, HDI_Band == "3")

par(mfrow=c(1,2))
qqnorm(HDI_2$Log_Deaths_Per_Year)
qqline(HDI_2$Log_Deaths_Per_Year)
qqnorm(HDI_3$Log_Deaths_Per_Year)
qqline(HDI_3$Log_Deaths_Per_Year)


shapiro.test(HDI_2$Log_Deaths_Per_Year) #Normality test - Not normally distributed
shapiro.test(HDI_3$Log_Deaths_Per_Year) #Normality test - Not normally distributed

HDI_2_3 <- subset(full_data_x4, HDI_Band != "1")

wilcox.test(Log_Deaths_Per_Year ~ HDI_Band, data=HDI_2_3,alternative="two.sided",mu=0)

# RESULT: With significance level of 0.05, there is not enough evidence to reject null hypothesis with p-value of 0.5689
#         and conclude that mean is same for each HDI banding which is consistent with histogram we generated (HDI 2, 3 are almost identical)
#------------------------------------

#==============================================================================#
# Hypothesis Testing
#==============================================================================#

#------- Objective 2: Dependency of Year banding and HDI banding (2 and 3) --------#
# H0: Year banding and survival are independent for number of deaths
# H1: Year banding and survival are dependent for number of deaths
#----------------------------------------------------------------------------------------#

#Create subset
Objective2_chisq <- subset(full_data_x4, select = c(year_band, Survived_flag, Deaths_Per_Year)) #keep column

Objective2_chisq <- Objective2_chisq %>%
                        group_by(year_band, Survived_flag) %>%
                          summarise(Count = n())
view(Objective2_chisq)

#Require a matrix of number of deaths for chisq test
Not_Survived <- c(30,27,27, 20)
Survived <- c(7,11,16,12)

chisq_table = cbind(Not_Survived, Survived)
rownames(chisq_table) = c("1990 - 1994", "1995 - 1999", "2000 - 2004", "2005 +")
chisq_table

overall_chisq <- chisq.test(chisq_table)
overall_chisq

overall_chisq$expected

# RESULT: With p-value of 0.259, we conclude that null hypothesis is not rejected and conclude year and HDI banding are dependent for number of deaths.
#------------------------------------

#==============================================================================#

#------- Objective 3: --------#
# H0:
# H1: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are NOT equal
#-----------------------------#

#-----------------------------#
# Logistic regression
#-----------------------------#
all(complete.cases(full_data_x4))

ggplot(full_data_x4, aes(x=HDI)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) +
  geom_density(colour = 'blue')+
  ggtitle("Histogram of HDI Score")+
  xlab("HDI score") +
  ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 12, face = "bold"))

#-----------------------------#
# Estimate the parameters
#-----------------------------#
## Model Creation
full_data_x4$Survived_flag_Binary <- ifelse(full_data_x4$Survived_flag =="Survived", 1,0)

model <- glm(Survived_flag_Binary~HDI+year_band,data = full_data_x4, family = "binomial")  #REJECT
summary(model)
