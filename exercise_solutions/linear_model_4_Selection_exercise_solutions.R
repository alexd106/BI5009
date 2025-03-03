## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
loyn <- read.table("data/loyn.txt", header = TRUE)
str(loyn)

loyn$LOGAREA <- log10(loyn$AREA)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)

# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------
# # define the panel.cor function from ?pairs
# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
# {
#     usr <- par("usr")
#     par(usr = c(0, 1, 0, 1))
#     r <- abs(cor(x, y))
#     txt <- format(c(r, 0.123456789), digits = digits)[1]
#     txt <- paste0(prefix, txt)
#     if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
#     text(0.5, 0.5, txt, cex = cex.cor * r)
# }
# 
# # subset the variables of interest
# VOI<- c("ABUND", "LOGAREA", "LOGDIST", "LOGLDIST", "YR.ISOL", "ALT", "FGRAZE")
# pairs(loyn[, VOI], lower.panel = panel.cor)
# 
# # There are varying degrees of correlation between explanatory variables which
# # might indicate some collinearity, i.e. LOGAREA and FGRAZE (0.48), LOGDIST and
# # LOGLDIST (0.59) and YR.ISOL and FGRAZE (0.56). However, the relationships
# # between these explanatory variables are quite weak so we can probably
# # include these variables in the same model (but keep an eye on things).
# # There also seems to be a reasonable spread of observations across these
# # pairs of explanatory variables.
# 
# # The relationship between the response variable ABUND and all the explanatory
# # variables is visible in the top row:
# # Some potential relationships present like with LOGAREA (positive),
# # maybe ALT (positive) and FGRAZE (negative).


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
M1 <- lm(ABUND ~ LOGDIST + LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + 
           FGRAZE:LOGAREA, data = loyn)


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
summary(M1)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
# Wait: why can't we use information from the 'summary(M1)' or 'anova(M1)' functions
# to do this?

# the 'summary' table tests if the coefficient for each explanatory variable 
# is significantly different from zero.

# the 'anova' tests for the significance of the proportion of variation explained
# by a particular term in the model. 

# The ANOVA table also allows testing the overall significance of a categorical explanatory
# variable (like FGRAZE) which involves several parameters together (one for each level), 
# which is quite is handy. But the results of this ANOVA are based on sequential 
# sums of squares and therefore the order of the variables in the model
# (which is arbitrary here) matters.

# We could change the order but there are too many possible permutations.
# Summary P values don't suffer from this problem but tests different hypotheses.
# It would be useful to use an ANOVA that doesn't depend on the order
# of inclusion of the variables, this is effectively what 'drop1' does.

drop1(M1, test = "F")

# LOGLDIST is the least significant (p = 0.88), and therefore makes the least 
# contribution to the variability explained by the model, with respect to 
# the number of degrees of freedom it uses (1)


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
# new model removing LOGLDIST 
M2 <- lm(ABUND ~ LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
           LOGAREA:FGRAZE, data = loyn) 

# or use a shortcut with the update() function:
M2 <- update(M1, formula = . ~ . - LOGLDIST) # "." means all previous variables

# now redo drop1() on the new model
drop1(M2, test = "F")

# YR.ISOL is now the least significant (p = 0.859), hence makes the least 
# contribution to the variability explained by the model, 
# with respect to the number of degrees of freedom it uses (1)


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
M3 <- update(M2, formula = . ~ . - YR.ISOL)

drop1(M3, test = "F")

# LOGDIST now the least significant (p = 0.714) and should be removed from 
# the next model.


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
M4 <- update(M3, formula = . ~ . - LOGDIST)
drop1(M4, test = "F")

# ALT is not significant (p = 0.331)


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------
# and finally drop ALT from the model
M5 <- update(M4, formula = . ~ . - ALT)
drop1(M5, test = "F")

# the LOGAREA:FGRAZE term represents the interaction between LOGAREA and
# FGRAZE. This is significant (p = 0.005) and so our model selection
# process comes to an end.


## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
# As the interaction between LOGAREA and FGRAZE was significant at each step of
# model selection process the main effects should be left in our model,
# irrespective of significance. This is because it is quite difficult to 
# interpret an interaction without the main effects. The drop1 
# function is clever enough that it doesn't let you see the P values for the 
# main effects, in the presence of their significant interaction.

# Also note, because R always includes interactions *after* their main effects
# the P value of the interaction term (p = 0.005) from the model selection 
# is the same as P value if we use the anova() function on our final model

# Check this:
anova(M5)
drop1(M5, test= "F") 


## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
# Biologically: confirming what we already found out in the previous exercise:
# There is a significant interaction between the area of the patch and the level 
# of grazing 

# However, some observations are poorly predicted (fitted) using the set of
# available explanatory variables (i.e. the two very large forest patches)

# Interpretation: 
# Bird abundance might increase with patch area due to populations being more
# viable in large patches (e.g. less prone to extinction), 
# or perhaps because there is proportionally less edge effect in larger
# patches, and this in turn provides more high quality habitat for species 
# associated with these habitat patches

# The negative effect of grazing may be due to grazing decreasing resource
# availability for birds, for example plants or seeds directly, or insects
# associated with the grazed plants. There may also be more disturbance of birds
# in highly grazed forest patches resulting in fewer foraging opportunities
# or chances to mate (this is all speculation mind you!).

# Methodologically:
# Doing model selection is difficult without intrinsic / expert knowledge
# of the system, to guide what variables to include.
# Even with this data set, many more models could have been formulated.
# For example, for me, theory would have suggested to test an interaction 
# between YR.ISOL and LOGDIST (or LOGLDIST?), 
# because LOGDIST will affect the dispersal of birds between patches 
# (hence the colonisation rate), and the time since isolation of the patch may 
# affect how important dispersal has been to maintain or rescue populations 
# (for recently isolated patches, dispersal, and hence distance to nearest
# patches may have a less important effect)


## ----QA1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
drop1(M1)
#Removing the term 'LOGLDIST' gives the lowest AIC of 226.23
#So refit your model with this term removed. Run `drop1()` again on your updated model. Perhaps call this new model `M2.AIC`.
M2.AIC <- update(M1, formula = . ~ . - LOGLDIST)
drop1(M2.AIC) #removing which term gives the lowest AIC?
#Continue in this manner until removing any terms INCREASES the AIC. Then we have our minimal adequate model (which should be the same as the final model we finished with last time)


## ----QA2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
#Fit each model using the terms described above, giving each a unique and sensible name
M.AIC.1<- lm(ABUND ~ LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE, data= loyn)

M.AIC.2<- lm(ABUND ~ LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGLDIST:YR.ISOL + LOGAREA:FGRAZE, data= loyn)

M.AIC.3<- lm(ABUND ~ YR.ISOL + LOGAREA + FGRAZE, data= loyn)

M.AIC.4<- lm(ABUND ~ LOGAREA + FGRAZE + LOGAREA:FGRAZE, data= loyn)

M.AIC.5<- lm(ABUND ~ LOGAREA + FGRAZE, data= loyn)


## ----QA3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
AIC(M.AIC.1)
#AIC of 418.54
AIC(M.AIC.2)
#AIC of 420.34
AIC(M.AIC.3)
#AIC of 424.60
AIC(M.AIC.4)
#AIC of 413.71
AIC(M.AIC.5)
#AIC of 422.61

#So the model with the lowest AIC (= 413.71) is M.AIC.4, with the terms 'LOGAREA + FGRAZE + LOGAREA:FGRAZE'


## ----QA2a, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------
# This is one way of constructing a summary table for reporting the results:

# create a vector of the formulas for all the models compared during our model selection (remember to add any custom models you have have added yourself!):

model.formulas<- c(
  "LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE", 
  "LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGLDIST:YR.ISOL + LOGAREA:FGRAZE", 
  "YR.ISOL + LOGAREA + FGRAZE", 
  "LOGAREA + FGRAZE + LOGAREA:FGRAZE", 
  "LOGAREA + FGRAZE")

#Collect the AIC values for each model (note they will be the 2nd column of the object 'model.AIC')

model.AIC = AIC(M.AIC.1, M.AIC.2, M.AIC.3, M.AIC.4, M.AIC.5) #these are the 5 models you already fitted in step A2 (plus any extra you added yourself)

# create a dataframe of models and AIC values

summary.table<- data.frame(Model = model.formulas,
	AIC= round(model.AIC[,2], 2))

# Sort the models from lowest AIC (preferred) to highest (least preferred)

summary.table<- summary.table[order(summary.table$AIC), ]

# Add the difference in AIC with respect to best model

summary.table$deltaAIC<- summary.table$AIC - summary.table$AIC[1]

# print the dataframe to the console

summary.table


## ----Q12c, eval=TRUE, echo=FALSE--------------------------------------------------------------------
knitr::kable(summary.table, "html", align = "lcr", row.names = FALSE)

