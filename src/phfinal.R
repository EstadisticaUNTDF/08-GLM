library(tidyverse)

phrepro <- read.csv("data/phleum_bio-repro.csv", header = T)
phdemo <- read.csv("data/phleum_demo.csv", header = T)

phrepro <- phrepro %>% 
  rename(ID = Chapa, Trat = Ubicación)

phfinal <- phdemo %>% select(1:6) %>% 
  filter(Fecha == 7) %>% 
  full_join(phrepro, by = "ID") %>% 
  select(Macollos, Circunferencia, Peso, Inflorescencias, Trat) %>% 
  rename(Circ = Circunferencia, Biomasa = Peso, Ambiente = Trat) %>% 
  filter(complete.cases(.))
if(!dir.exists("cache")) dir.create("cache")
write.csv(phfinal, "cache/phfinal.csv", row.names = FALSE)

