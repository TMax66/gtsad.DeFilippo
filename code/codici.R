library(tidyverse)
library(readxl)
library(openxlsx)
library(here)


dt <- read_excel(here("data","datipupe.xlsx"))


dt %>% 
  group_by(specie,`larval diet`, day ) %>% 
  summarise(weight= mean(`pupal weight`)) %>% 
  
  ggplot()+
  aes(x=day, y = weight, color= `larval diet`)+
  geom_point()+geom_line()+
  facet_wrap(~specie, scales = "free")
  
  



