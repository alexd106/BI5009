## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------
sp<- read.table(file= "./data/species.txt", header= TRUE)
str(sp)

sp$logSp<- log(sp$Species)
sp$pH<- factor(sp$pH, levels= c("low", "mid", "high"))

# make a list of the variables of interest, for convenience:
VOI<- c("logSp", "Biomass", "pH")
pairs(sp[, VOI])
# Biomass tends to increase with pH, which could generate
# some collinearity between these explanatory variables in a model.
# but still plenty of variation in Biomass within each pH, 
# so hopefully this won't be an issue.

coplot(logSp ~ Biomass | pH, data= sp)
# the relationships look clean and well distinct between treatments, 
# supporting the idea of an interaction.



## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------
sp.glm1<- glm(Species ~ Biomass, family= poisson, data= sp)


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------
summary(sp.glm1)

# Model description:
# Species_i ~ Poisson(mu_i)
# log(mu_i) = 3.18 - 0.064*Biomass_i



## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------
# On the link scale:
3.18 - 0.064*5 # 2.86
# On the response scale (species count):
exp(3.18 - 0.064*5) # 17.46


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------
sp.glm2<- glm(Species ~ Biomass * pH, family= poisson, data= sp)


## ----Q7-9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------
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



## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------
# "(Intercept)" is the predicted value on the link (log) scale for
# Biomass = 0, and pH= "low"

# "Biomass" is the slope of Biomass for the low pH category.
# It is negative, so assumes a linear decrease (on the log scale)

# "pHmid" is the estimated difference (on the log scale) between
# the "pHmid" and the reference level, "pHlow".
# Positive means higher on average than pHlow.

# "Biomass:pHmid" is the difference between the slope of Biomass for pHmid
# compared to the slope of Biomass for the reference level, "pHlow".
# Positive means that the decrease is slower in "pHmid" (although this is
# for the linear relationships on the log scale, but it may not look
# the same on the response scale - see the graphical interpretation below)

# A mathematical description of the model
# (more or less how I would present it in the methods section of a paper):
# Species ~ Poisson(mu)
# log(mu) = 2.95 - 0.26 * Biomass
#    + 0.48 * pHmid
#    + 0.82 * pHhigh
#    + 0.12 * Biomass * pHmid
#    + 0.16 * Biomass * pHhigh

# The coefficients are on the log scale,
# so cannot be interpreted directly as counts. 
# They are interpreted as changes in log-counts:

# For a biomass of zero, the log of the number of species at medium pH
# is increased by 0.48 compared to the low pH.
# This is equivalent to saying that the counts increase by exp(0.48),
# hence that they are multiplied by 1.62.


