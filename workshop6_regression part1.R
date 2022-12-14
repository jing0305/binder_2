#workshop6
#install packages
install.packages("Hmisc")
install.packages("performance")
install.packages("data.table")
#load packages
library(tidyverse)
library(Hmisc)
library(performance)
library(data.table)
#load data
crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")
head(crime)

crime_tidied <- crime %>%
  separate(col = `City, State`, into = c("City", "State")) %>%
  rename(House_price=index_nsa) %>%
  rename(Violent_Crimes="Violent Crimes")
head(crime_tidied)

#ggplot population and violent_crimes 
#lm = linear model 
crime_tidied %>%
  ggplot(aes(x=Population,y=Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm",se=FALSE) +
  theme_minimal() +
  theme(text = element_text(size=13)) +
  labs(x="population",
       y="violent crimes")

#R
rcorr(crime_tidied$Population, crime_tidied$Violent_Crimes)
#r=0.81 p<0.001

#filter out population greater than 2,000,000
crime_filtered <- filter(crime_tidied, Population < 2000000)

#new ggplot
crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) + 
  geom_point(alpha = .25) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population", 
       y = "Violent Crimes")

#new R
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)
#r=0.69

crime_filtered <- filter(crime_filtered,Year == 2015)

crime_filtered %>%
  ggplot(aes(x=Population, y=Violent_Crimes,
             label=City))+
           geom_point()+
           geom_text(nudge_y = 500, check_overlap = TRUE) + 
           geom_smooth(method="lm",se=FALSE) +
           xlim(0,1800000) +
           theme_minimal()+
           theme(text = element_text(size=13)) +
           labs(x="Population", y="Violent Crimes")
rcorr(crime_filtered$Population,crime_filtered$Violent_Crimes)           
         #r=0.65 p<0.001

#build a model
#why 1
model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)
check_model(model2)

#anova
anova(model1,model2)

summary(model2)

#challenge
#You now have three tasks:
#1. Check whether the same relationship holds for population size and robberies in 2015.

#robberies in 2015
crime_tidied_2015 <-filter(crime_tidied,Year == 2015)

rcorr(crime_tidied_2015$Population,crime_tidied_2015$Robberies)
#r=0.85
model3 <-lm(Robberies ~ Population,crime_tidied_2015)
anova(model3)
summary(model3)

#f(critical value)

#y=intercept + bx+c
#2. Are house prices predicted by the number of violent crimes in 2015?
rcorr(crime_tidied_2015$House_price,crime_tidied_2015$Violent_Crimes)
#r=-0.14,p<0.389 not statistical significant 

#3. Are house prices predicted by population size in 2015?
rcorr(crime_tidied_2015$House_price,crime_tidied_2015$Population)
#r=0.05 p<0.732 not statistical significant 


#One of the most commonly used regression type is OLS 
#(ordinary least squares) 
#which works by minimizing the distance (deviation) 
#between the observed data and the linear model.

#
crime %>%
filter(Population < "mean_sdl(Population,mult=2,na.rm=TRUE)")


crime_1 <- crime %>%
  filter(Population<mean_sdl(Population,mult=2,na.rm=TRUE)
head(crime_1)

#remove outliers based on sd
remove_sd_outlier(crime,cols=`Violent Crimes`, n_sigmas=2,verbose=TRUE)
col_mean <- mean(`Violent Crimes`)
col_sd <- sd(`Violent Crimes`)
col_vals <- runif(1000)
