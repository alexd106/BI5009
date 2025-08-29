## ----Q1, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# loyn <- read.table("data/loyn.txt", header = TRUE)
# str(loyn)
# 
# loyn$LOGAREA <- log10(loyn$AREA)
# loyn$LOGDIST <- log10(loyn$DIST)
# loyn$LOGLDIST <- log10(loyn$LDIST)
# 
# # create factor GRAZE as it was originally coded as an integer
# loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
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
# # pairs of explanatory variables which is a good thing.
# 
# # The relationship between the response variable ABUND and all the explanatory
# # variables is visible in the top row:
# # Some potential relationships present like with LOGAREA (positive),
# # maybe ALT (positive) and FGRAZE (negative).


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# M1 <- lm(ABUND ~ LOGDIST + LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
#            FGRAZE:LOGAREA, data = loyn)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# summary(M1)


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# # Wait: why can't we use information from the 'summary(M1)' or 'anova(M1)' functions
# # to do this?
# 
# # the 'summary' table tests if the coefficient for each explanatory variable
# # is significantly different from zero.
# 
# # the 'anova' tests for the significance of the proportion of variation explained
# # by a particular term in the model.
# 
# # The ANOVA table also allows testing the overall significance of a categorical explanatory
# # variable (like FGRAZE) which involves several parameters together (one for each level),
# # which is quite is handy. But the results of this ANOVA are based on sequential
# # sums of squares and therefore the order of the variables in the model
# # (which is arbitrary here) matters.
# 
# # We could change the order but there are too many possible permutations.
# # Summary P values don't suffer from this problem but tests different hypotheses.
# # It would be useful to use an ANOVA that doesn't depend on the order
# # of inclusion of the variables, this is effectively what 'drop1' does.
# 
# drop1(M1, test = "F")
# 
# # LOGLDIST is the least significant (p = 0.88), and therefore makes the least
# # contribution to the variability explained by the model, with respect to
# # the number of degrees of freedom it uses (1). This variable is a good candidate
# # to remove from the model


## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# # new model removing LOGLDIST
# M2 <- lm(ABUND ~ LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
#            LOGAREA:FGRAZE, data = loyn)
# 
# # or use a shortcut with the update() function:
# M2 <- update(M1, formula = . ~ . - LOGLDIST) # "." means all previous variables
# 
# # now redo drop1() on the new model
# drop1(M2, test = "F")
# 
# # YR.ISOL is now the least significant (p = 0.859), hence makes the least
# # contribution to the variability explained by the model,
# # with respect to the number of degrees of freedom it uses (1)


## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# M3 <- update(M2, formula = . ~ . - YR.ISOL)
# 
# drop1(M3, test = "F")
# 
# # LOGDIST now the least significant (p = 0.714) and should be removed from
# # the next model.


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# M4 <- update(M3, formula = . ~ . - LOGDIST)
# drop1(M4, test = "F")
# 
# # ALT is not significant (p = 0.331)


## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------------------------------------------
# # and finally drop ALT from the model
# M5 <- update(M4, formula = . ~ . - ALT)
# drop1(M5, test = "F")
# 
# # the LOGAREA:FGRAZE term represents the interaction between LOGAREA and
# # FGRAZE. This is significant (p = 0.005) and so our model selection
# # process comes to an end.


## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # As the interaction between LOGAREA and FGRAZE was significant at each step of
# # model selection process the main effects should be left in our model,
# # irrespective of significance. This is because it is quite difficult to
# # interpret an interaction without the main effects. The drop1
# # function is clever enough that it doesn't let you see the P values for the
# # main effects, in the presence of their significant interaction.
# 
# # Also note, because R always includes interactions *after* their main effects
# # the P value of the interaction term (p = 0.005) from the model selection
# # is the same as P value if we use the anova() function on our final model
# 
# # Check this:
# anova(M5)
# drop1(M5, test= "F")


## ----Q11, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # Biologically: confirming what we already found out in the previous exercise:
# # There is a significant interaction between the area of the patch and the level
# # of grazing
# 
# # However, some observations are poorly predicted (fitted) using the set of
# # available explanatory variables (i.e. the two very large forest patches)
# 
# # Interpretation:
# # Bird abundance might increase with patch area due to populations being more
# # viable in large patches (e.g. less prone to extinction),
# # or perhaps because there is proportionally less edge effect in larger
# # patches, and this in turn provides more high quality habitat for species
# # associated with these habitat patches
# 
# # The negative effect of grazing may be due to grazing decreasing resource
# # availability for birds, for example plants or seeds directly, or insects
# # associated with the grazed plants. There may also be more disturbance of birds
# # in highly grazed forest patches resulting in fewer foraging opportunities
# # or chances to mate (this is all speculation mind you!).
# 
# # Methodologically:
# # Doing model selection is difficult without intrinsic / expert knowledge
# # of the system, to guide what variables to include.
# # Even with this data set, many more models could have been formulated.
# # For example, for me, theory would have suggested to test an interaction
# # between YR.ISOL and LOGDIST (or LOGLDIST?),
# # because LOGDIST will affect the dispersal of birds between patches
# # (hence the colonisation rate), and the time since isolation of the patch may
# # affect how important dispersal has been to maintain or rescue populations
# # (for recently isolated patches, dispersal, and hence distance to nearest
# # patches may have a less important effect)


## ----QA1, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # This time, we are not doing any specific hypothesis testing, rather we are
# # attempting to select a model with the 'best' goodness of fit with the minimal
# # number of estimated parameters.
# 
# # We will start with a reasonably complex but PLAUSABLE model (this is the same
# # model we started with using F test based model selection above.
# 
# M.start.AIC<- lm(ABUND ~ LOGLDIST + LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
#                LOGAREA:FGRAZE, data = loyn)
# 
# drop1(M.start.AIC)


## ----QA2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # So, our starting model with no variables removed has an AIC of 228.20. If we
# # remove the interaction term `LOGAREA:FGRAZE` from the model then this results
# # in a big increase in AIC (238.02 - 228.20 = 9.82) so this suggests that there
# # is substantial evidence that the interaction should remain in the model. The
# # models without `LOGLDIST`, `LOGDIST`, `YR.ISOL` all have pretty much the
# # same AIC value (around 226) so in practice we could remove any of them. Let's
# # remove the term that results in the model with the lowest AIC which is the
# # `LOGLDIST` variable (AIC 226.23).
# 
# M2.AIC <- update(M.start.AIC, formula = . ~ . - LOGLDIST)
# drop1(M2.AIC)


## ----QA3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # Ok, as the model without the variable `YR.ISOL` has the lowest AIC (224.27)
# # let's update our model and remove this variable.
# 
# M3.AIC <- update(M2.AIC, formula = . ~ . - YR.ISOL)
# drop1(M3.AIC)


## ----QA4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # So, now the model without `LOGDIST` has the lowest AIC (222.43) so we should
# # refit the model without this variable and run `drop1()` again.
# 
# M4.AIC <- update(M3.AIC, formula = . ~ . - LOGDIST)
# drop1(M4.AIC)


## ----QA5, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------------------------------------
# # And the model without the variable `ALT` has an AIC of 221.57 which is about
# # the same as the model with `ALT` (AIC 222.43), so let's remove this variable
# # from the model as this suggests that the simpler model fits our data just as
# # well as the more complicated model.
# 
# M5.AIC <- update(M4.AIC, formula = . ~ . - ALT)
# drop1(M5.AIC)
# 
# # OK, so now we have a model with the main effects of LOGAREA, FGRAZE and the
# # interaction term LOGAREA:FGRAZE. When we remove the interaction term the
# # AIC value increases by 8.9 (230.47-221.57) and this suggests that if we
# # remove the interaction term the model fit is significantly worse. Therefore we
# # should leave it in and finish our model selection here.


## ----QA2a, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------------
# # one way of constructing a summary table for reporting the results:
# 
# # create a vector of all the models compared during out model selection
# 
# model.formulas<- c(
#   "LOGLDIST + LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
# 	"LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
# 	"ALT + LOGDIST + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
# 	"ALT + LOGAREA + FGRAZE + LOGAREA:FGRAZE",
# 	"LOGAREA + FGRAZE + LOGAREA:FGRAZE")
# 
# # fit each model. Need to use the noquote() function to remove the
# # quotations around our model formula otherwise you will get an error when
# # using the lm() function.
# 
# # You will also need to paste the response variable 'ABUND ~ '
# # together with out explanatory variables to create a valid model formula
# 
# M.start <- lm(noquote(paste('ABUND ~', model.formulas[1])), data = loyn)
# M.step2 <- lm(noquote(paste('ABUND ~', model.formulas[2])), data = loyn)
# M.step3 <- lm(noquote(paste('ABUND ~', model.formulas[3])), data = loyn)
# M.step4 <- lm(noquote(paste('ABUND ~', model.formulas[4])), data = loyn)
# M.step5 <- lm(noquote(paste('ABUND ~', model.formulas[5])), data = loyn)
# 
# # obtain the AIC values for each model. Note: these will be different
# # than those obtained with the drop1() function due to a small difference in
# # how AIC is calculated. This isn;t a problem, just don't mix and match
# # the AIC values form drop1 and AIC functions.
# 
# model.AIC<- c(AIC(M.start),
# 	AIC(M.step2),
# 	AIC(M.step3),
# 	AIC(M.step4),
# 	AIC(M.step5))
# 
# # create a dataframe of models and AIC values
# 
# summary.table<- data.frame(Model = model.formulas,
# 	AIC= round(model.AIC, 2))
# 
# # Sort the models from lowest AIC (preferred) to highest (least preferred)
# 
# summary.table<- summary.table[order(summary.table$AIC), ]
# 
# # Add the difference in AIC with respect to best model
# 
# summary.table$deltaAIC<- summary.table$AIC - summary.table$AIC[1]
# 
# # print the dataframe to the console
# 
# summary.table


## ----Q12c, eval=SOLUTIONS, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
# # Or if you prefer a prettier version to include directly in
# # your paper / thesis. You will need to install the knitr
# # package before you can do this. See the 'Installing R Markdown'
# # in [Appendix A](https://intro2r.com/install-rm.html)
# # in the Introduction to R book for more details.
# 
# knitr::kable(summary.table, "html", align = "lcr", row.names = FALSE)

