## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE)
str(loyn)

loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)


## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
# Example:

# Rank	|	Predictor	|	Biological effect
# 1	    |	LOGAREA	  |	Large patches containing proportionally
#                     more core habitat: enable persistence of
#                     species with larger home ranges. 
# 3	    |	LOGDIST	  |	?
# ?	    |	LOGLDIST	|	?
# 2	    |	YR.ISOL	  |	?
# ?	    |	ALT		    |	?
# ?	    |	FGRAZE	  |	?

# expect different researchers to come up with different predictions!
# The main limitation is our lack of expert knowledge of the
# study system and area, of course.


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
## VOI<- c("ABUND", "LOGAREA", "LOGDIST", "LOGLDIST", "YR.ISOL", "ALT", "FGRAZE")
## pairs(loyn[, VOI])
## 
## # There is variable degrees of imbalance (correlation) between predictors
## # such as:
## # LOGAREA and FGRAZE,
## # LOGDIST and LOGLDIST (quite expected),
## # YR.ISOL and other variables like LOGAREA or FGRAZE,
## # LOGAREA and ALT,
## # but overall a decent spread of observations across these pairs of predictors.
## 
## # The relationship between the response variable ABUND and all the predictors
## # is visible in the top row:
## #  Some potential correlations present like with LOGAREA (positive),
## # YR.ISOl (positive), maybe ALT (positive) and FGRAZE (negative).


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
M1 <- lm(ABUND ~ LOGDIST + LOGLDIST +
                 YR.ISOL + ALT + LOGAREA * FGRAZE,
         data = loyn)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
library(car)
vif(M1)

# Potential problem with FGRAZE (as seen from data exploration) with LOGAREA
# or YR.ISOL
# Ignore for the moment, but we will have to take this into account in our
# interpretation of the results.
# The high VIF for the terms involved in an
# interaction is expected and can be ignored.


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
# Wait: why did we not use 'summary' or 'anova' for this?
summary(M1)
anova(M1)

# 'summary' tests if the coefficient for each predictor is significantly
# different from zero.
# 'anova' tests for the significance of the proportion of variation explained
# by a particular term in the model.
# The ANOVA allows testing the overall significance of categorical predictors,
# which involves several coefficients together, thus is more handy. 
# But the results of this ANOVA depend on the order of the variables,
# which is arbitrary here.
# We could change the order but there are too many possible permutations
# Summary p-values don't suffer that problem but test different hypotheses
# It would be useful to use an ANOVA that doesn't depend on the order
# of inclusion of the variables, like 'drop1'

drop1(M1, test = "F")

# LOGDIST is the least significant, hence makes the least 
# contribution to the variability explained by the model, 
# with respect to the number of degrees of freedom it uses (1)

# In retrospect, this model selection step effectively amounts to a test of the
# significance of the LOGDIST effect (although it is a relatively weak test,
# because this was not specific: in reality we were testing
# multiple hypotheses at once)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
M2 <- lm(ABUND ~ LOGLDIST + # removing LOGDIST here
                 YR.ISOL + ALT + LOGAREA * FGRAZE,
         data = loyn)

# or use the shortcut:

M2<- update(M1, formula = . ~ . - LOGDIST) # "." means all previous variables
drop1(M2, test = "F")

# LOGLDIST is now the least significant, hence makes the least 
# contribution to the variability explained by the model, 
# with respect to the number of degrees of freedom it uses (1)

M3 <- update(M2, formula = . ~ . - LOGLDIST)
drop1(M3, test = "F")

# YR.ISOL and ALT now the least significant. Choosing on the basis of
# p-values this similar is really quite arbitrary, so would be best guided
# by expert knowledge if we have it.
# in the absence of strong a-priori expertise, we'll go for YR.ISOL

M4 <- update(M3, formula = . ~ . - YR.ISOL)
drop1(M4, test = "F")

# and finally drop ALT from the model
# I am writing the model in full for more clarity:
M5 <- lm(ABUND ~ LOGAREA * FGRAZE, data = loyn) 
# Here we are, back to a
# familiar version of the model!


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
# If the goal of the study is simply to test the FGRAZE * LOGAREA interaction, then all we need is the associated significance test.
# If the model is intended to be used for further inference (like
# prediction), then we will try to simplify it as much as is justifiable to do.

# We can use drop1 for this, but we don't need to, in this simple case:
# when an interaction is significant, the main effects should be left in,
# irrespective of significance, because the interaction cannot be
# interpreted correctly without its main effect.

# Likewise, when an interaction is non-significant it must go first,
# and only then the evidence for the main effects can be assessed.

# Because R always includes interactions *after* their main effects
# in the models, the anova of the model returns the same result as drop1, 
# in our simple model which has no interactions with other terms

# Demo:
anova(M5)
drop1(M5, test= "F") 
# drop1 is clever enough that it doesn't let you
# see the p-values for the main effect, in the presence of their interaction.

# In this case the interaction is not significant, and thus isn't needed
# a better model would thus be the additive-only model:
# lm(ABUND ~ LOGAREA + FGRAZE , data = loyn)
# By applying 'drop1' we have effectively tested the effect of the interaction,
# i.e. the hypothesis that the effect of grazing level depends on patch size (or vice versa).


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")------
M6 <- lm(ABUND ~ LOGAREA + FGRAZE, data = loyn) 
# first split the plotting device into 2 rows and 2 columns
par(mfrow = c(2,2))

# now create the residuals plots
plot(M6)

# To test the normality of residuals assumption we use the Normal Q-Q plot. 
# The central residuals are not too far from the Q-Q line but the extremes
# are too extreme (the tails of the distribution are too long). Some
# observations, both high and low, are poorly explained by the model.

# The plot of the residuals against the fitted values suggests these
# extreme residuals happen for intermediate fitted values.

# Looking at the homogeneity of variance assumption (Residuals vs
# Fitted and Scale-Location plot),
# the graphs are mostly messy, with no clear pattern emerging. There is
# a hint of smaller variance with the lowest fitted values, which is not ideal.
# This could mean that the homogeneity of variance assumption is not met
# (i.e. the variances are not the same). 

# The observations with the highest leverage don't appear to be overly
# influential, according to the Cook's distances in the Residuals vs
# Leverage plot.  


## ----Q9extra, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------
## # (Extra)
## # ABUND being bounded by zero, it wouldn't be too surprising if the variance increases with the mean abundance.
## # This is often improved by log-transforming the response
## loyn$logABUND<- log(loyn$ABUND + 1) # here the natural log
## M6log <- lm(logABUND ~ LOGAREA + FGRAZE, data = loyn)
## par(mfrow = c(2,2))
## plot(M6log)
## 
## # Not this time! Lots of extreme negative residuals generated.
## 
## # Back to `M6`, then. The other issue was the extreme residuals.
## # This could be due to missing important predictors from the model, either
## # unknown predictors, or interactions.


## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------
anova(M6)
# null hypothesis 1: There is no effect of LOGAREA on ABUND
# (the proportion of variation explained by LOGAREA is zero)
# null hypothesis 2: There is no effect of FGRAZE on ABUND
# (no significant proportion of variation explained by grazing
# levels *after* the effect of LOGAREA)
# the p values are all very small therefore reject both null hypotheses.

# (Supplement, for info)
# You can work out the proportions of variation explained by each predictor
# we can extract the Sums of squares like this:
str(anova(M6)) # examine the structure of the returned object
M6.SS<- anova(M6)$'Sum Sq' # store values of interest
M6.SS # checking that we have extracted the Sums of squares as intended

# compute SST (total sums of squares):
M6.SST<- sum(anova(M6)$'Sum Sq') 

# proportion of variation in the data explained by the model:
(M6.SST - M6.SS[3]) / M6.SST # 0.7269773

# proportion of variation in the data explained by predictors:
(M6.SS[1:2]) / M6.SST # 0.5476530 0.1793243

# So that's 55% of variation in the data explained by LOGAREA and 18% for FGRAZE after LOGAREA


summary(M6)

# From the summary table, the model equation is
# ABUND = 15.72*(Intercept) + 7.25*LOGAREA + 0.38*FGRAZE2 - 0.19*FGRAZE3
# - 1.59*FGRAZE4 - 11.89*FGRAZE5
# Note that (Intercept) = 1 always


## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------
# Biologically: confirming what we already found out in the previous LM exercises:
# There is a significant effect of grazing levels, especially the highest
# level with a negative effect on bird abundance

# There is a significant positive effect of patch area, too.

# The relative importance of patch area and grazing is not entirely clear, 
# as these are correlated, with smaller patches also having higher grazing
# intensity on average, and larger patches lower grazing intensity.

# Some observations are poorly predicted (fitted) using the set of
# available predictors.

# Interpretation: 
# Bird abundance may increase with patch area due to populations being more
# viable in large patches (e.g. less prone to extinction), 
# or perhaps because there is proportionally less edge effect in larger
# patches, and this in turn provides more high quality habitat for species 
# associated with these habitat patches

# The negative effect of grazing may be due to grazing decreasing resource
# availability for birds, for example plants or seeds directly, or insects
# associated with the grazed plants.


# Methodologically:
# Doing model selection is difficult without an intrinsic / expert knowledge
# of the system, to guide what variables to include.
# Even with this data set, many more models could have been formulated.
# For example, for me, theory would have suggested to test an interaction 
# between YR.ISOL and LOGDIST (or LOGLDIST?), 
# because LOGDIST will affect the dispersal of birds between patches 
# (hence the colonisation rate), 
# and the time since isolation of the patch may affect how important
# dispersal has been to maintain or rescue populations 
# (for recently isolated patches, dispersal, and hence distance to nearest
# patches may have a less important effect)


## ----Q12, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------
# This time, we are not doing any specific hypothesis testing, so there's
# no need to force the LOGAREA * FGRAZE into the model until the
# end of the model selection.
# But we should still start with a "most complex model considered" 
# which we deem plausible.

M.start<- lm(ABUND ~ LOGLDIST + YR.ISOL + ALT + LOGAREA * FGRAZE, data= loyn)

step(M.start)

# or 

drop1(M.start)
# lowest AIC would be obtained by removing LOGLDIST

M.step2<- lm(ABUND ~ YR.ISOL + ALT + LOGAREA * FGRAZE, data= loyn)
drop1(M.step2)
# lowest AIC would be obtained by removing YR.ISOL

M.step3<- lm(ABUND ~ ALT + LOGAREA * FGRAZE, data= loyn)
drop1(M.step3)
# lowest AIC would be obtained by removing ALT

M.step4<- lm(ABUND ~ LOGAREA * FGRAZE, data= loyn)
drop1(M.step4)
# lowest AIC is the current model, including the interaction.
# unlike the F-test, the AIC favors retaining the interaction


# one way of constructing a summary table, for reporting the results:
# (we could include the model without interaction, for reference)
Model.formulas<- c("LOGLDIST + YR.ISOL + ALT + LOGAREA * FGRAZE",
	"YR.ISOL + ALT + LOGAREA * FGRAZE",
	"ALT + LOGAREA * FGRAZE",
	"LOGAREA * FGRAZE",
	"LOGAREA + FGRAZE")
Model.AIC<- c(AIC(M.start),
	AIC(M.step2),
	AIC(M.step3),
	AIC(M.step4),
	AIC(M6))

summary.table<- data.frame(Model= Model.formulas,
	AIC= round(Model.AIC, 2))

# Sorting models from lowest AIC (preferred) to highest (least preferred):
summary.table<- summary.table[order(summary.table$AIC), ]

# Adding AIC differences with respect to best model:
summary.table$deltaAIC<- summary.table$AIC - summary.table$AIC[1]

require(knitr)
kable(summary.table)


# Note that this rather automatic way of doing model selection
# misses a number of plausible combinations of the terms of 
# interest. This is somewhat arbitrary and there is a chance to
# miss the "best" model in that way.

# The function `step` tries to reduce this problem with the
# options `direction= "both"`. This means that after achieving a
# minimal model by applying "backwards" steps of model
# reduction, `step` will eventually apply one or more
# "forward" steps, where previously deleted terms are added
# again in case they lead to an improvement of the minimal model.

# However, this is no replacement for a fully manual and 
# thought-through model selection, informed by the understanding 
# of theory in the research area, and of the research questions.



## ----A1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
birds.add.2 <- lm(ABUND ~ FGRAZE + LOGAREA, data = loyn)
anova(birds.add.2)

# null hypothesis 1: There is no effect of FGRAZE on ABUND
# (no difference between grazing levels)
# null hypothesis 2: There is no effect of LOGAREA on ABUND
# (the proportion of variation in ABUND explained by LOGAREA is
# zero *after* accounting for the effect of FGRAZE)
# the p values are all very small therefore reject both null hypotheses.

# we can also extract the Sums of squares like this:
birds.add.2.SS<- anova(birds.add.2)$'Sum Sq' # store values of interest
birds.add.2.SST<- sum(anova(birds.add.2)$'Sum Sq') # compute SST

# proportion of variation in the data explained by the model:
(birds.add.2.SST - birds.add.2.SS[3]) / birds.add.2.SST
# the same as the previous model

# proportion of variation in the data explained by predictors:
(birds.add.2.SS[1:2]) / birds.add.2.SST # 0.5449233 0.1820540

# So that's 54% for FGRAZE and 18% for LOGAREA after FGRAZE,
# essentially reversing the contributions found in the first
# model (for reference, see the solution code chunk for Question 19).

# Conclusion: here the order matters a lot. This is not too surprising since
# the design is unbalanced and FGRAZE and LOGAREA covary (they are correlated)


## ----A2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
# ABUND = 15.72*(Intercept) + 7.25*LOGAREA + 0.38*FGRAZE2 - 0.19*FGRAZE3
# - 1.59*FGRAZE4 - 11.89*FGRAZE5
# Note that (Intercept) = 1 always

# expected abundance for (A): replace LOGAREA by -0.5, and all FGRAZE2...5 by 0
15.72*1 + 7.25*(-0.5) + 0.38*0 - 0.19*0 - 1.59*0 - 11.89*0 # 12.095

# could also extract the model coefficients like this:
M6.coef<- coef(M6)
M6.coef
# and multiply by the predictor values to obtain the equivalent prediction
# (difference due to my own roundings of estimates above)
sum(M6.coef * c(1, -0.5, 0, 0, 0, 0)) # 12.09278 

# expected abundance for (B): input 1 for the GRAZE3 variable
15.72*1 + 7.25*(-0.5) + 0.38*0 - 0.19*1 - 1.59*0 - 11.89*0 # 11.905
# or
sum(M6.coef * c(1, -0.5, 0, 1, 0, 0)) # 11.90349

# the difference (B) 11.90349 - (A) 12.09278 should correspond to
# the estimate for the GRAZE3 coefficient

# expected abundance for (C)
sum(M6.coef * c(1, 0.5, 0, 1, 0, 0)) # 19.15072

# the difference (C) 19.15072 - (B) 11.90349 should coincide with the
# estimate of the slope for the LOGAREA effect, since there is just one
# LOGAREA unit of difference and the slope is the change in expected
# abundance for a 1-unit increase in the predictor.


## ----A3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------
birds.inter.1 <- lm(ABUND ~ FGRAZE * LOGAREA , data = loyn)
# ABUND = 21.243*(Intercept) - 6.165*FGRAZE2 - 7.215*FGRAZE3 - 17.910*FGRAZE4
# - 17.043*FGRAZE5 + 4.144*LOGAREA + 4.368*FGRAZE2:LOGAREA
# + 4.989*FGRAZE3:LOGAREA + 15.235*FGRAZE4:LOGAREA + 1.996*FGRAZE5:LOGAREA
# Note that (Intercept) = 1 always

# expected abundance for (A): replace LOGAREA by 2.5, and all terms involving
# FGRAZE2...5 by 0
21.243*1 - 6.165*0 - 7.215*0 - 17.910*0 - 17.043*0 + 4.144*2.5 + 4.368*0 + 4.989*0 + 15.235*0 + 1.996*0 # 31.603

# much lower risk of error by extracting the model coefficients like this:
birds.inter.1.coef<- coef(birds.inter.1)
birds.inter.1.coef # check coefficients and their order
# and multiply by the predictor values to obtain the equivalent prediction
sum(birds.inter.1.coef * c(1, 0, 0, 0, 0, 2.5, 0, 0, 0, 0)) # 31.60296 

# expected abundance for (B): input 1 for the GRAZE5 variable and -0.5 for
# variables LOGAREA and FGRAZE5:LOGAREA
21.243*1 - 6.165*0 - 7.215*0 - 17.910*0 - 17.043*1 + 4.144*(-0.5) + 4.368*0 + 4.989*0 + 15.235*0 + 1.996*(-0.5) # 1.13
# or
sum(birds.inter.1.coef * c(1, 0, 0, 0, 1, -0.5, 0, 0, 0, -0.5)) # 1.130203 

# Well done if you got there!


## ----A4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------
## # first split the plotting device into 2 rows and 2 columns
## par(mfrow = c(2,2))
## 
## # create the residuals plots for the additive model
## plot(M6)
## 
## # create the residuals plots for the interactive model
## plot(birds.inter.1)
## 
## # Not a great deal of an improvement! Just marginally better in every respect,
## # thanks to increasing the fit slightly by throwing lots of
## # (unnecessary?) new model parameters at the data.
## 
## # By increasing the complexity of the model, we have improved
## # the fit, but there is little evidence that what we have
## # additionally captured is of biological interest. It most
## # likely is just noise in the data, because the sample size is
## # too low for the model (and us) to be able to distinguish true
## # patterns from noise (See plots from Question 8).
