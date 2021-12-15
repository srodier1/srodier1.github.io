library(moments)
library(dplyr)
library(tidyverse)
library(janitor)
library(plotly)
library(broom)
library(lme4)
options(scipen = 999)

df <- read.csv("merged_data.csv")
phd <- read.csv("cvss/phd_awards.csv")
names(df2)


length(unique(df2$entity))




length(unique(x$entity))

eu <- df2 %>% 
  filter(continent == "Europe") %>% select(entity) %>% as.list()


asia <- df2 %>% 
  filter(continent == "Asia") %>% select(entity)%>% as.list()

north_a <- df2 %>% 
  filter(continent == "North America") %>% select(entity)%>% as.list()

south_a <- df2 %>% 
  filter(continent == "South America") %>% select(entity)%>% as.list()

afr <- df2 %>% 
  filter(continent == "Africa") %>% select(entity)%>% as.list()

oc <- df2 %>% 
  filter(continent == "Oceania") %>% select(entity)%>% as.list()

abc <- df2 %>% 
  mutate("cont" = case_when(df2$entity %in% eu ~ "Europe", 
                             df2$entity %in% asia ~ "Asia",
                             df2$entity %in% north_a ~ "North_Ameria",
                             df2$entity %in% south_a ~ "South_America",
                          df2$entity %in% afr ~ "Africa",
                        TRUE ~ "Oceania"))

unique(abc$cont)
         

asia <- c(asia)
abc <- df2 %>% #rowwise() %>% 
  mutate("cont" = case_when((df2$entity %in% asia) ~ "Asia",
                            df2$entity %in% eu ~ "Europe",
                            TRUE ~ "Oceania"))
unique(abc$cont)



asia <- c("Afghanistan", "Bangladesh",  "China",       "India" ,      "Israel"  ,   "Japan",      "Jordan",    
          "Laos"  ,     "Lebanon"  ,  "Malaysia",   "Mongolia"  , "Nepal",      "Pakistan",   "Palestine", 
           "Saudi_Arabia", "Singapore",  "South_Korea","Taiwan",     "Thailand",   "Turkey",     "Vietnam",   
          "Hong_Kong"   )

eu <- c( "Andorra" ,       "Austria"      ,  "Belarus"  ,      "Belgium"    ,    "Cyprus"   ,      "Estonia" ,      
          "Finland"      ,  "France"   ,      "Germany"  ,      "Hungary" ,       "Iceland" ,       "Ireland"   ,    
          "Luxembourg"  ,   "Norway",         "Poland"   ,      "Portugal",       "Russia",         "Slovakia",      
          "Slovenia",       "Spain",          "Sweden",         "Switzerland",    "United_Kingdom",
         "Czechoslovakia")
north_a <- c("Belize"  ,           "Bermuda"   ,         "Cuba"   ,            "Dominican_Republic", "Greenland",         
             "Grenada",            "Haiti",              "Honduras",           "Mexico",             "Nicaragua",         
              "Puerto_Rico" ,       "United_States",      "Aruba")
south_a <- c("Brazil",  "Ecuador", "Peru",    "Uruguay")
afr <- c("Cameroon", "Congo",    "Egypt",    "Ghana",    "Kenya",    "Morocco",  "Somalia",  "Zimbabwe",
         "Zanzibar")

df2 %>% 
  mutate("cont" = case_when(df2$entity %in% eu ~ "Europe", 
                            df2$entity %in% asia ~ "Asia",
                            df2$entity %in% north_a ~ "North_Ameria",
                            df2$entity %in% south_a ~ "South_America",
                            df2$entity %in% afr ~ "Africa",
                            TRUE ~ "Oceania"))



