## ----Q1, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
loyn <- read.table("data/loyn.txt", header = TRUE)
str(loyn)

loyn$LOGAREA <- log10(loyn$AREA)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)

# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# define the panel.cor function from ?pairs
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
    op <- par(usr = c(0, 1, 0, 1))   # set new plotting region, keep the old
    on.exit(par(op))                 # and restore the old one on exit
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

# subset the variables of interest. Note we use the numeric GRAZE here, not the factor
# FGRAZE: a correlation coefficient is only meaningful between numeric variables, and
# GRAZE is an ordered index of grazing intensity so a correlation with it is
# interpretable. It is still FGRAZE that goes into the model.
VOI<- c("ABUND", "LOGAREA", "LOGDIST", "LOGLDIST", "YR.ISOL", "ALT", "GRAZE")
pairs(loyn[, VOI], lower.panel = panel.cor)

# There are varying degrees of correlation between explanatory variables which might
# indicate some collinearity, i.e. LOGAREA and GRAZE (0.48), LOGDIST and LOGLDIST (0.59)
# and YR.ISOL and GRAZE (0.56). However, the relationships between these explanatory
# variables are quite weak so we can probably include these variables in the same model
# (but keep an eye on things). There also seems to be a reasonable spread of
# observations across these pairs of explanatory variables which is a good thing.

# The relationship between the response variable ABUND and all the explanatory variables
# is visible in the top row: Some potential relationships present like with LOGAREA
# (positive), maybe ALT (positive) and GRAZE (negative).


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
M1 <- lm(ABUND ~ LOGDIST + LOGLDIST + YR.ISOL + ALT + LOGAREA + FGRAZE + 
           FGRAZE:LOGAREA, data = loyn)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
summary(M1)

# No, not all of the P values are less than 0.05, which is what prompts us to simplify
# the model over the next few questions.

# As for the question about chance: at a 0.05 threshold you expect about 1 test in every
# 20 to fall below it even when nothing at all is going on. So in a table of 14 tests
# you would expect roughly one 'significant' result even if none of these variables
# mattered in the slightest. Bear that in mind whenever somebody hands you a large table
# of tests. A single starred coefficient in a table this size is weak evidence on its
# own, and hunting through a big model for whatever happens to sit below 0.05 is a poor
# way to do science.

# It's also part of the reason why the selection procedure we are about to use follows a
# stated rule, applied consistently, rather than picking out whatever happens to look
# interesting.


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# Wait: why can't we use information from the 'summary(M1)' or 'anova(M1)' functions to
# do this?

# the 'summary' table tests if the coefficient for each explanatory variable is
# significantly different from zero.

# the 'anova' tests for the significance of the proportion of variation explained by a
# particular term in the model.

# The ANOVA table also allows testing the overall significance of a categorical
# explanatory variable (like FGRAZE) which involves several parameters together (one for
# each level), which is quite handy. But the results of this ANOVA are based on
# sequential sums of squares and therefore the order of the variables in the model
# (which is arbitrary here) matters.

# We could change the order but there are too many possible permutations. Summary P
# values don't suffer from this problem but test different hypotheses. It would be
# useful to use an ANOVA that doesn't depend on the order of inclusion of the variables,
# this is effectively what 'drop1' does.

drop1(M1, test = "F")

# LOGLDIST is the least significant (p = 0.88), and therefore makes the least
# contribution to the variability explained by the model, with respect to the number of
# degrees of freedom it uses (1). This variable is a good candidate to remove from the
# model




## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# new model removing LOGLDIST 
M2 <- lm(ABUND ~ LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
           LOGAREA:FGRAZE, data = loyn) 

# or use a shortcut with the update() function:
M2 <- update(M1, formula = . ~ . - LOGLDIST) # "." means all previous variables

# now redo drop1() on the new model
drop1(M2, test = "F")

# YR.ISOL is now the least significant (p = 0.859), hence makes the least contribution
# to the variability explained by the model, with respect to the number of degrees of
# freedom it uses (1)




## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
M3 <- update(M2, formula = . ~ . - YR.ISOL)

drop1(M3, test = "F")

# LOGDIST now the least significant (p = 0.714) and should be removed from the next
# model.




## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
M4 <- update(M3, formula = . ~ . - LOGDIST)
drop1(M4, test = "F")

# ALT has the largest P value (p = 0.332)




## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# and finally drop ALT from the model
M5 <- update(M4, formula = . ~ . - ALT)
drop1(M5, test = "F")

# the LOGAREA:FGRAZE term represents the interaction between LOGAREA and FGRAZE. This is
# significant (p = 0.006) and so our model selection process comes to an end.




## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# As the interaction between LOGAREA and FGRAZE was significant at each step of model
# selection process the main effects should be left in our model, irrespective of
# significance. This is because it is quite difficult to interpret an interaction
# without the main effects. The drop1 function is clever enough that it doesn't let you
# see the P values for the main effects, in the presence of their significant
# interaction.

# Also note, because R always includes interactions *after* their main effects the P
# value of the interaction term (p = 0.005) from the model selection is the same as P
# value if we use the anova() function on our final model

# Check this:
anova(M5)
drop1(M5, test= "F") 


## ----Q11, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# the estimates and their confidence intervals for the final model
summary(M5)
confint(M5)

# and remember from the previous exercise that everything here except the intercept and
# LOGAREA is a DIFFERENCE from graze level 1, not a value for that grazing level. If you
# want the intercept and slope for a particular grazing level you need to add the
# relevant contrast onto the baseline.

# Biologically: confirming what we already found out in the previous exercise: the
# relationship between bird abundance and patch area depends on the level of grazing
# (interaction F_4,57 = 4.09, p = 0.006). The fitted slopes range from 4.14 birds per 1
# unit increase in LOGAREA at graze level 1 up to 17.78 at graze level 4, so the effect
# of patch area varies roughly fourfold across the grazing levels. No single number
# describes 'the effect of area' in these data

# However, some observations are poorly predicted (fitted) using the set of available
# explanatory variables (i.e. the two very large forest patches)

# Interpretation: Bird abundance might increase with patch area due to populations being
# more viable in large patches (e.g. less prone to extinction), or perhaps because there
# is proportionally less edge effect in larger patches, and this in turn provides more
# high quality habitat for species associated with these habitat patches

# The negative effect of grazing may be due to grazing decreasing resource availability
# for birds, for example plants or seeds directly, or insects associated with the grazed
# plants. There may also be more disturbance of birds in highly grazed forest patches
# resulting in fewer foraging opportunities or chances to mate (this is all speculation
# mind you!).

# Methodologically: Model selection is difficult without expert knowledge of the system
# to guide which variables to include in the first place. Plenty of other models could
# have been formulated from this dataset. Theory might have suggested an interaction
# between YR.ISOL and LOGDIST, for instance, since distance affects dispersal between
# patches and time since isolation affects how much dispersal has mattered.

# AND NOW THAT CAVEAT, which is the answer to the question at the end of Q11.

# The P values and confidence intervals printed for M5 are too optimistic.

# They are calculated as if we had specified M5 in advance. We didn't. We fitted a 14
# parameter model and then dropped terms one at a time based on what these same data
# told us. Selection kept exactly those terms that happened to look strongest in this
# particular sample, so the surviving estimates are pushed away from zero and their
# intervals are narrower than they should be.

# This is called post-selection inference. There are ways of dealing with it, none of
# which we'll cover here. What matters for you is that the problem exists, and that you
# say so plainly whenever you report a model you arrived at by selection. Most published
# papers using stepwise selection don't.

# One thing to be clear about before you go on. You will have noticed that the model we
# ended up with here is the same one we fitted in the previous exercise. That is a
# consequence of how I set these exercises up, not a finding. In the previous exercise I
# simply told you to fit LOGAREA + FGRAZE + LOGAREA:FGRAZE, on the assumption that
# this was the hypothesis the researchers set out to test, because it was the clearest
# way to teach you what an interaction between a continuous and a categorical variable
# looks like.

# So don't read anything into the two agreeing, and in your own work you would not fit
# both and then compare them. You either specify a model in advance from your hypotheses
# and report that, or you select one from the data and report the selection with the
# caveat above. Doing both on the same data and picking whichever you prefer is the one
# thing neither approach allows.


## ----Q12report, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------
# Same health warning: my version, not the only version.

# METHODS. Bird abundance was modelled against log10 patch area, log10 distance to the
# nearest patch, log10 distance to the nearest larger patch, year of isolation, altitude
# and grazing intensity (a five level factor), plus the area by grazing interaction.
# Distance and area variables were log10 transformed after graphical exploration showed
# strong right skew. Terms were removed by backward selection using single term
# deletions and F tests, with main effects retained wherever they appeared in a retained
# interaction.

# RESULTS. Four terms were removed in turn, leaving patch area, grazing intensity and
# their interaction (Table 1). Bird abundance increased with patch area at every grazing
# level, but at a rate that varied more than fourfold with grazing intensity
# (interaction F 4,57 = 4.09, p = 0.006; adjusted R2 = 0.77, n = 67 patches). As this
# model was arrived at by selection rather than specified in advance, the P values and
# intervals reported here are likely to be optimistic.

# (that's about 150 words, so there was room to spare)

# That last sentence is doing real work, so don't cut it. It is one of the two things
# Q12 asked you to include, and without it a reader has no way of knowing that the model
# was searched for rather than proposed. It costs you 20 words. If a supervisor or
# reviewer tells you to drop it, ask them what the reader is supposed to do instead.

# Notice how much work the table did there. Without it you would have had to spell out
# every deletion and its P value in prose, which would eat most of your word limit and
# be tedious to read. With it, one bracketed "(Table 1)" points the reader at the whole
# selection history, and your text is free to say what the result actually means. This
# is how model selection should be reported, and it is not how most papers do it.

# The two things a naive account would have left out are the selection history and that
# last sentence. Without the history a reader can't tell how much searching went on, and
# a model reported bare looks as though it was specified in advance on theoretical
# grounds, which carries far more weight than one arrived at by trying things and
# discarding them. Without the caveat they would take the P values at face value.



## ----QA1, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# This time, we are not doing any specific hypothesis testing, rather we are attempting
# to select a model with the 'best' goodness of fit with the minimal number of estimated
# parameters.

# We will start with a reasonably complex but PLAUSABLE model (this is the same model we
# started with using F test based model selection above.

M.start.AIC<- lm(ABUND ~ LOGLDIST + LOGDIST + YR.ISOL + ALT + LOGAREA + FGRAZE +
               LOGAREA:FGRAZE, data = loyn)

drop1(M.start.AIC)




## ----QA2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# So, our starting model with no variables removed has an AIC of 228.20. If we remove
# the interaction term `LOGAREA:FGRAZE` from the model then this results in a big
# increase in AIC (238.02 - 228.20 = 9.82) so this suggests that there is substantial
# evidence that the interaction should remain in the model. The models without
# `LOGLDIST`, `LOGDIST`, `YR.ISOL` all have pretty much the same AIC value (around 226)
# so in practice we could remove any of them. Let's remove the term that results in the
# model with the lowest AIC which is the `LOGLDIST` variable (AIC 226.23).

M2.AIC <- update(M.start.AIC, formula = . ~ . - LOGLDIST)
drop1(M2.AIC)




## ----QA3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# Ok, as the model without the variable `YR.ISOL` has the lowest AIC (224.27) let's
# update our model and remove this variable.

M3.AIC <- update(M2.AIC, formula = . ~ . - YR.ISOL)
drop1(M3.AIC)




## ----QA4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# So, now the model without `LOGDIST` has the lowest AIC (222.43) so we should refit the
# model without this variable and run `drop1()` again.

M4.AIC <- update(M3.AIC, formula = . ~ . - LOGDIST)
drop1(M4.AIC)




## ----QA5, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# And the model without the variable `ALT` has an AIC of 221.57 which is about the same
# as the model with `ALT` (AIC 222.43), so let's remove this variable from the model as
# this suggests that the simpler model fits our data just as well as the more
# complicated model.

M5.AIC <- update(M4.AIC, formula = . ~ . - ALT)
drop1(M5.AIC)

# OK, so now we have a model with the main effects of LOGAREA, FGRAZE and the
# interaction term LOGAREA:FGRAZE. When we remove the interaction term the AIC value
# increases by 8.9 (230.47-221.57) and this suggests that if we remove the interaction
# term the model fit is significantly worse. Therefore we should leave it in and finish
# our model selection here.

