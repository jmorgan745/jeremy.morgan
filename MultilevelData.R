##########
# Author: Brian Anderson
# Last Update: 16 April 2018
# Purpose: R script to create multilevel dataframe for classroom use
##########

# Load two datasets
library(tidyverse)
kauffman.ds <- read_csv("Main Street Entrepreneurship.csv")
pcgdp.ds <- read_csv("gmpPCGDP.csv")

# Filter and Tidy the Kauffman Data we need
survive.df <- kauffman.ds %>% 
  select(Location, 'Index Year', Location_name, Survival_Rate) %>% 
  rename(Year = 'Index Year') %>% 
  filter(!is.na(Survival_Rate)) %>% 
  arrange(Location_name, Year)


# Tidy the GDP data from BEA
pcgdp.df <- pcgdp.ds %>% 
  filter(!is.na(GeoName)) %>% 
  select(GeoFIPS, GeoName, `2001`:`2016`) %>% 
  gather(Year, GDP_PC, `2001`:`2016`) %>% 
  filter(GeoFIPS != "00998") %>% 
  mutate(GeoName = str_remove(GeoName, ",.*")) %>% 
  rename(Location = GeoFIPS,
         Location_name = GeoName) %>% 
  mutate(Location = as.integer(Location),
         Year = as.integer(Year)) %>% 
  filter(Year >= 2001) %>% 
  filter(Location > 56) %>% 
  arrange(Location_name, Year)

# Join the data
survival.df <- inner_join(survive.df, pcgdp.df, by = c("Location", "Year"))

# Tidy the joined data
survival.df <- survival.df %>% 
  select(-Location_name.y) %>% 
  rename(MSA = Location_name.x) %>% 
  mutate(MSA = as.factor(MSA)) %>% 
  mutate(logGDP_PC = log(GDP_PC))

summary(lm(Survival_Rate ~ logGDP_PC, data = survival.df))

library(plm)
random.model <- plm(Survival_Rate ~ logGDP_PC, data = survival.df,
                    index = c("Location", "Year"), model = "random")
summary(random.model)

fixed.model <- plm(Survival_Rate ~ logGDP_PC, data = survival.df,
                   index = c("Location", "Year"), model = "within")
summary(fixed.model)

phtest(fixed.model, random.model)

plmtest(fixed.model, c("time"), type=("bp"))

library(lme4)
library(lmerTest)
library(sjstats)
intercept.model <- lmer(Survival_Rate ~ (1 | Location), data = survival.df)
summary(intercept.model)

