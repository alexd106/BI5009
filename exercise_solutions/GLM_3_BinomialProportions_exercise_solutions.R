## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
Mites<- read.delim("./data/DrugsMites.txt", 
                   stringsAsFactors = TRUE)
str(Mites)

Mites$fToxic<- factor(Mites$Toxic)
str(Mites)




## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
Mites$Living_mites<- Mites$Total - Mites$Dead_mites


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
M1<- glm(cbind(Living_mites, Dead_mites) ~ Concentration + fToxic + Concentration : fToxic, family= binomial, data= Mites)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
summary(M1)

## Mathematical model description:

# Prop_surviving ~ Binomial(Ntot, P) 
# log(P / (1-P)) = 
#         1.43 - 3.80*Concentration 
#         + 0.0085*fToxic2 + 0.014*fToxic3 + 0.27*fToxic4 
#         - 6.18*Concentration:fToxic2
#         - 1.90*Concentration*fToxic3 
#         + 1.48*Concentration*fToxic4

## "biological" hypotheses:

# (Intercept):     the mean (on the logit scale) for Toxic 1 when its
# concentration is 0 (null hypothesis not very useful, however
# the value is quite interesting, when back-transformed on the proportion
# scale: this is the proportion that survive the experiment in the absence
# of toxic chemical. Shouldn't differ between chemicals, presumably?)

# Concentration: slope of Concentration, or change in survival per unit
# increase in Toxic 1 concentration (on logit link), or change in log-odds
# of survival (on proportion scale)

# fToxic2:       difference between intercepts of Toxic 2 and Toxic 1
# fToxic3:       difference between intercepts of Toxic 3 and Toxic 1
# fToxic4:       difference between intercepts of Toxic 4 and Toxic 1

# Concentration:fToxic2  difference between Conc. slopes for Toxic 2 and
# Toxic 1. Negative means a more lethal effect than Toxic 1

# Concentration:fToxic3  difference between Conc. slopes for Toxic 2 and Toxic 1
# Concentration:fToxic4  difference between Conc. slopes for Toxic 2 and Toxic 1


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
# Yes, because this is a binomial variable with more than 1 trial per observation
# Residual deviance: 117.67  on 107  degrees of freedom
# -> negligible overdispersion


