df <- read_csv("asldfkjasdlfkjasdlkfjj", skip = 3) #skip = 3 means that you would skip the first 3 rows

library(tidyverse)
library(carData)

 MplsStops
 MplsDemo
 mpls <- full_join(MplsStops,MplsDemo)

 df <- df %>%
   (CITATION = case_when(citationIssued == "YES" ~ TRUE,
                                TRUE ~ FALSE)
 mod <- glm(data = mpls,
            formula = CITATION~ poverty + gender +race,
            family = "binomial")
library(modelr) 
mpls %>%
  add_predictions(mod, type = "response") %>%
  ggplot(aes(x = poverty, y = pred, color = gender)) +
  geom_smooth()+
  facet_wrap(~race,scales = "free")
              )