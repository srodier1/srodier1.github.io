library(janitor)
library(tidyverse)
path <- "cvss"
outpath <- "merged_data.csv"

dat <- list.files(path, full.names = TRUE, pattern = ".csv$") %>%
  as.list()
read_fnl_data <- function(x){
  read_csv(x,
           col_types = cols(
             Entity = col_character(),
             Continent = col_character(),
             Year = col_double()
           ))
}

poop <- map(dat, read_fnl_data)
df <- reduce(poop,full_join)

df %>%
  write_csv(file=outpath)
df <- read.csv(outpath, check.names = TRUE )



df <- df%>%
  rename("Total_Life_Satis" = "Life.satisfaction.in.Cantril.Ladder..World.Happiness.Report.2021.",
          "Proportion_Wmn_Snr_Uppr_Mgmnt" = "X5.5.2...Proportion.of.women.in.senior.and.middle.management.positions.......IC_GEN_MGTN",
          "Fruit_consumed(kg_pc/year)" = "Fruits...Excluding.Wine...Food.supply.quantity..kg.capita.yr...FAO..2020.",
          "Fruit_consumed(kg_pc/year)" = "Fruits...Excluding.Wine...Food.supply.quantity..kg.capita.yr...FAO..2020.",
          "Kg_Banana/Capita/Year"= "Food.Supply...Crops.Primary.Equivalent...Bananas...2615...Food.supply.quantity..kg.capita.yr....645...kg",
          "Kg_Dates/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Dates...2619...Food.supply.quantity..kg.capita.yr....645...kg",
          "Kg_Citrus_Other/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Citrus..Other...2614...Food.supply.quantity..kg.capita.yr....645...kg",
          "Kg_Orng_Mand/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Oranges..Mandarines...2611...Food.supply.quantity..kg.capita.yr....645...kg" ,
          "Kg_Apple+prod/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Apples.and.products...2617...Food.supply.quantity..kg.capita.yr....645...kg",
          "Kg_Lemon_Lime/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Lemons..Limes.and.products...2612...Food.supply.quantity..kg.capita.yr....645...kg", 
          "Kg_Grape/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Grapes.and.products..excl.wine....2620...Food.supply.quantity..kg.capita.yr....645...kg", 
          "Kg_Grapefrt_prod/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Grapefruit.and.products...2613...Food.supply.quantity..kg.capita.yr....645...kg",
          "Kg_Pineapple+prod/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Pineapples.and.products...2618...Food.supply.quantity..kg.capita.yr....645...kg", 
          "Kg_Plantains/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Plantains...2616...Food.supply.quantity..kg.capita.yr....645...kg",
          "Kg_Other_frt/Capita/Year" = "Food.Supply...Crops.Primary.Equivalent...Fruits..Other...2625...Food.supply.quantity..kg.capita.yr....645...kg",
          "GDP/Capita/Year" = "GDP.per.capita..PPP..constant.2017.international...",
          "Population/sq_kg_Land" = "Population.density..people.per.sq..km.of.land.area.",
          "num_males_born/100fml_born" = "Sex.ratio.at.birth..male.births.per.female.births.",
          "Kg_Vgtbl/Capita/Year" = "Vegetables...Food.supply.quantity..kg.capita.yr...FAO..2020.",
          "%fml" = "Population..female....of.total.population.",
         "total_pop" = "Total.population..Gapminder..HYDE...UN.",
         "cal_frm_animl_prot" = "Calories.from.animal.protein..FAO..2017..",
          "cal_frm_plnt_prot" ="Calories.from.plant.protein..FAO..2017..",
           "cal_frm_fat" = "Calories.from.fat..FAO..2017..",
          "cal_frm_carbs" ="Calories.from.carbohydrates..FAO..2017..",
          "DALY" = "DALYs..Disability.Adjusted.Life.Years....All.causes...Sex..Both...Age..Age.standardized..Rate.",
        "%prevalence_depressive_disorders" = "Prevalence...Depressive.disorders...Sex..Both...Age..Age.standardized..Percent.",
        "life_expectancy" = "Life.expectancy",
        "bladder_cancer_deaths" = "Bladder.cancer..deaths.",
       "brain_nvss_cancer_deaths" = "Brain.and.nervous.system.cancer..deaths.",
      "breast_cancer_deaths"= "Breast.cancer..deaths.",
      "cervical_cancer_deaths" = "Cervical.cancer..deaths.",
      "colon_rect_cancer_deaths" = "Colon.and.rectum.cancer..deaths.",
     "esophageal_cancer_deaths" = "Esophageal.cancer..deaths.")

     
newnames <- df %>% names() %>%  make_clean_names()
names(df) <- newnames

names(df) = gsub(pattern = "_sex_both_age_all_ages_number", replacement = "", x = names(df))
names(df) = gsub(pattern = "_age_all_ages_number", replacement = "", x = names(df))
names(df) = gsub(pattern = "_age_age_standardized_percent", replacement = "", x = names(df))
names(df) = gsub(pattern = "_sex_both", replacement = "", x = names(df))
df$entity = gsub(pattern = " ", replacement = "_", df$entity) 
names(df)
#get rid of years before 1990!

df <- df %>%
  filter(year > 1900)
range(df$year)

#take away spaces in continents
unique(df$continent)
str_replace(df$continent, " ", "_")

#what countries do I have data for apples from?
wha <- df %>%
  select("entity", "kg_apple_prod_capita_year") %>%
  filter(!is.na(kg_apple_prod_capita_year))
unique(wha$entity)
summary(wha)
#I have it for all countries

#limit the entities that I need, by selecting the only ones I want
df <- df %>% filter(entity %in% c("Scotland","England","Zimbabwe","Zanzibar", "World", "Vietnam","Uruguay", "United_States", "United_Kingdom",
"Turkey", "Thailand", "Taiwan","Sweden", "Switzerland", "Spain" , "South_Korea", "Somalia","Slovenia" ,
"Slovakia", "Singapore", "Saudi_Arabia", "Samoa", "Russia", "Puerto_Rico",
"Portugal", "Poland", "Peru", "Palestine", "Pakistan","Norway", 
"Nicaragua","Nepal", "Morocco", "Mongolia" , "Mexico" , "Malaysia" , "Luxembourg" ,
"Lebanon",  "Laos", "Jordan", "Japan", "Israel","Kenya", "Ireland","India" , "Iceland", 
"Hungary", "Hong_Kong", "Honduras", "Haiti", "Guam" ,"Grenada", "Greenland", "Ghana",
"Germany" , "France" , "Finland", "Estonia" , "Egypt", "Ecuador","Dominican_Republic",
"Czechoslovakia" , "Cyprus", "Cuba", "Congo" , "China" , "Ch♥ile","Cameroon","Brazil" ,
"Bermuda", "Belize" ,"Belgium","Belarus" , "Bangladesh" , "Austria" , "Aruba",  "Afghanistan","Andorra"))

#select columns that I DO WANT
names(df)

df <- df %>%
  select(entity,                                                                                       
          year,
          total_pop,                                                                                    
          continent,
         kg_apple_prod_capita_year,
         phd_awards,
          cal_frm_animl_prot,                                                                          
          cal_frm_plnt_prot,                                                                            
          cal_frm_fat,                                                                                  
          cal_frm_carbs,
         fruit_consumed_kg_pc_year,                                                                    
          gdp_capita_year,
         life_expectancy,
         deaths_neoplasms,
         daly,
         percent_prevalence_depressive_disorders,)


#fill in country names
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

df <- df %>% 
  mutate("continent" = case_when(df2$entity %in% eu ~ "Europe", 
                            df2$entity %in% asia ~ "Asia",
                            df2$entity %in% north_a ~ "North_Ameria",
                            df2$entity %in% south_a ~ "South_America",
                            df2$entity %in% afr ~ "Africa",
                            TRUE ~ "Oceania"))
                                                                                     
          

#make new csv
write_csv(df, "merged_data.csv")
 