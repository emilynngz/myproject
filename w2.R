#task 1
library(foreign)
hse_and_shes12 <- read.dta("2012_hse_and_shes_combined.dta")
library(ggplot2)
subd <- subset(hse_and_shes12, eqvinc>0 & wemwbs>0 & age>=18)
#the linear model
wemwbs.model <- lm(wemwbs ~ eqvinc+age+Sex, data=subd)
summary(wemwbs.model)
#the polynomial model (graph)
ggplot(subd, aes(x=eqvinc, y=wemwbs)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) 
#the polynomial model
wemwbs.modelq <- lm(wemwbs~eqvinc+I(eqvinc^2)+age+Sex, data=subd)
summary(wemwbs.modelq)
#is there improvement with the quadratic relationship
anova(wemwbs.model, wemwbs.modelq)
#the second model (with q) has smaller p value, better

#task 2
ge <- read.csv("ge2019.csv")
aps <- read.csv("aps19.csv")
ge19aps19 <- merge(ge, aps,by="ons_id")
ge19aps19$brexit.per <- ge19aps19$brexit/ge19aps19$valid_votes
ge19aps19$brexit.per[ge19aps19$brexit.per = 0] <- NA
