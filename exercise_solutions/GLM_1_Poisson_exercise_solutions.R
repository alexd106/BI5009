## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
sp<- read.table(file= "./data/species.txt", header= TRUE, 
                stringsAsFactors = TRUE)
str(sp)

sp$logSp<- log(sp$Species)
sp$pH<- factor(sp$pH, levels= c("low", "mid", "high"))

VOI<- c("logSp", "Biomass", "pH")
pairs(sp[, VOI])
# Biomass tends to increase with pH, which could generate
# some collinearity between these explanatory variables in a model.
# but still plenty of variation in Biomass within each pH, 
# so hopefully this won't be an issue.

coplot(logSp ~ Biomass | pH, data= sp)
# the relationships look clean and well distinct between treatments, 
# supporting the idea of an interaction.
# looking promising.


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
sp.glm1<- glm(Species ~ Biomass, family= poisson, data= sp)


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
summary(sp.glm1)

# Model description:
# Species_i ~ Poisson(mu_i)
# log(mu_i) = 3.18 - 0.064*Biomass_i



## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
# On the link scale:
3.18 - 0.064*5 # 2.86
# On the response scale (species count):
exp(3.18 - 0.064*5) # 17.46


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
sp.glm2<- glm(Species ~ Biomass * pH, family= poisson, data= sp)


## ----Q7-9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------
summary(sp.glm2)
anova(sp.glm2, test= "Chisq")

# anova is useful for getting an idea of the relative contributions
# of each term, but beware that order does matter for all but the last term!

# drop1 would provide the same answer in this case, since there are
# no other interactions involved (and interactions are always last). 
# In general this would be the method of choice if our purpose is 
# to perform model simplification.

drop1(sp.glm2, test= "Chisq")

# the ANOVA table provides a global test across pH levels. 
# However the significance of the interactions is also unambiguous
# from the summary table, in this case, although the null hypothesis
# is different (coefficient different from zero in the latter 
# vs. significant proportion of variation explained in the former).



## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------
library(car)
vif(sp.glm2)

# GVIF is high for all the terms.
# This is expected though, because all terms are part of an interaction. 
# Any change in one coefficient will have a strong impact on the others
# This might be exacerbated by the pre-existing correlation between 
# the predictors. But all components are very clearly needed here.


