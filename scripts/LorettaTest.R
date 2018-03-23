
library(tidyverse)

flavourData <- read_csv("../rawData/bcfa_C17_fa.csv")

str(flavourData)
head(flavourData)

tidyFlavour <- flavourData %>% 
  mutate(obsId = 1:n()) %>% 
  gather("FattyAcids", "value", 1:4)
tidyFlavour

ggplot(tidyFlavour, aes(x=obsId, y=value, colour=FattyAcids)) +
  geom_point()


ggplot(tidyFlavour, aes(x=obsId, y=value, colour=FattyAcids)) +
  geom_point() +
  facet_grid(Year ~ .)

tidyFlavour %>% count(Year)
flavourData %>% count(MOA > MNA)
flavourData %>% count(MNA > EOA)
flavourData %>% count(EOA > C17.0)

ggplot(tidyFlavour, aes(x=FattyAcids, y=value)) +
  geom_boxplot() +
  facet_grid(~Year)

ggplot(flavourData, aes(x=MOA, y=MNA, colour=as.factor(Year))) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) 

ggplot(flavourData, aes(x=MOA, y=EOA, colour=as.factor(Year))) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) 




fit_MOA_MNA <- lm(MOA ~ MNA, data=flavourData)

summary(fit_MOA_MNA)

library(broom)
fit_MOA_MNA.df <- tidy(fit_MOA_MNA) %>% 
  filter(term == "MNA")
fit_MOA_MNA.df

