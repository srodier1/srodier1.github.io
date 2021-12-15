library(tidyverse)
library(modelr)
df <- read.csv("merged_data.csv")
unique(df$entity)

#does an apple/day keep the doctor away?
  
  #what will an apple/day mean?
  #fruit consumed kgs/capita/year, cal from plnt protein, calories from animal protein, calories from fat
  
  #what does keep the doctor away mean? 
  #it can mean dalys, cancer prevalence, depressive disorders, life expectancy
df %>% 
  filter(entity == "Afghanistan") %>% 
  select(year) %>% 
  unique()


#problems
#1)continent is NA for all except 2015
#2)unique(year [for afghanistan]) has less values than 

df %>% 
  filter(entity == "Afghanistan") %>% 
  ggplot(aes(x = year))+
  geom_histogram()



af <- df %>% 
  filter(entity == "Afghanistan") 
  
df$year %>% table %>% plot
df %>% glimpse
names(df)

#Each entity only has one year with an entry for continent.
#How an I add that same continent to each observation for that entity(country)

#take all NA values in column and assign them the value in continent for year 2015
 for (x in is.na(df$continent)) {
 x <- df %>%
   filter()
    
 }

is.it.true <- function(x) {
  
  if(x == TRUE) {x = !is.na(continent)}
}






mod1 <- glm(data = df,
    formula = life_expectancy ~ kg_apple_prod_capita_year*fruit_consumed_kg_pc_year*cal_frm_plnt_prot)
summary(mod1)

mod1 <- glm(data = df,
            formula = life_expectancy ~ fruit_consumed_kg_pc_year+kg_apple_prod_capita_year+cal_frm_plnt_prot)
summary(mod1)

p <- df %>%
  filter(year == 2000) %>% 
  ggplot(aes(x = cal_frm_plnt_prot, y = life_expectancy))+
  geom_point()
p

df %>%
  ggplot(aes(x = gdp_capita_year, y = life_expectancy))+
  geom_point()
modgdp_le <- glm(data = df,
                 formula = life_expectancy ~ gdp_capita_year)

summary(modgdp_le)
add_predictions(mod1, modgdp_le) %>%
  ggplot(aes(x = fruit_consumed_kg_pc_year, gdp_capita_year))+
  geom_boxplot()




library(plotly)
ggplotly(p)

library(MASS)
stepAIC(mod1)
  



