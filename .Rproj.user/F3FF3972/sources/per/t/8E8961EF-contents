#logistic regression
#used for TRUE/FALSE variables like: election prediction, mortality, graduate school admission, classification
setwd("../../Data_Course/Data")
library(tidyverse)
library(modelr)
library(easystats)

df <- read_csv("GradSchool_Admissions.csv")
df$admit <- as.logical(df$admit)

df%>%
  ggplot(aes(x = gpa, y = admit, color = rank))+
  geom_point()

df%>%
  ggplot(aes(x = gpa, fill = admit))+
  geom_density(alpha = .5)

#we need an S curve to make logistic shape


mod <- glm(data = df, 
           formula = admit ~ gpa + rank,
           family = binomial) #MEANS WE ARE DOING LOGISTIC REGRESSION
summary(mod)   #ESTIMATES FOR SUMMARY DON'T MAKE SENSE, IGNORE FOR NOW
                #they are called log odds, just pay attention to p- values

performance(mod)
#tjur r2 - it isn't linear, so you can't actually get regression. its "bootstrapped"
#

predict <- add_predictions(df, mod, type = "response")
  ggplot(predict, aes( x = gpa, y = pred, color = factor(rank)))+
  geom_smooth()
#prediction column is giving a chance of getting in 0 being no chance, 1 being 100% sure
# on this graph we are seeing people with a similar gpa having a higher chance of getting into 
  #grad school just because they attend a higher rank undergraduate school. We need to test if 
  #the difference is real or not
  mod2 <- glm(data = df, 
             formula = admit ~ gpa * rank,
             family = binomial)
summary(mod2)  
gather_predictions(df,mod,mod2) %>%
  ggplot(aes(x=gpa, y= pred, color = factor(rank)))+
  geom_smooth() +
  facet_wrap(~model)

#we can try mixing everything together

mod3 <- glm(data = df, 
            formula = admit ~ gpa + rank + gre,
            family = binomial)
summary(mod3)
gather_predictions(df,mod,mod3, type = "response") %>%
  ggplot(aes(x=gpa, y= pred, color = factor(rank)))+
  geom_smooth() +
  facet_wrap(~model)
#type = "response" translates log odds to probability

gather_predictions(df,mod,mod3, type = "response") %>%
  ggplot(mapping = aes(x=gre, y= pred, color = gpa))+
  geom_point() +
  facet_wrap(~rank) +
  scale_color_viridis_c()
# 2 things to remember when doing logistics
# type = response
#family = binomial

library(carData)
MplsDemo
MplsStops %>% glimpse
stops <- MplsStops
stops

stops <- stops %>%
  mutate(suspicious = case_when(problem == "suspicious" ~ TRUE,
                                TRUE ~ FALSE))
mod1 <- glm(data = stops,
            formula = suspicious ~ race,
            family = "binomial")

add_predictions(stops, mod1, type = "response")%>%
  ggplot(aes(x = race, y = pred)) +
  geom_point()

stops %>% glimpse
ggplot(stops, aes(x = lat, y = long, color = race)) +
  geom_density2d()

ggplot(stops, aes(x = lat, y = long, color = race)) +
  geom_point()
