## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
loyn <- read.table("data/loyn.txt", header = TRUE, 
                   stringsAsFactors = TRUE)
str(loyn)


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
coplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)

# or library(lattice) xyplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)

# - Within a grazing level, abundance seems to increase with the log patch area
#   in a more or less linear fashion
# - Overall, the mean abundance seems to decrease as grazing levels increase.
#   This is most noticeable in the highest grazing level.
# - Some of the slopes of the relationships (imagine a straight line) appear to be 
#   somewhat different for the different graze levels. The slopes for graze levels
#   1 and 2 are similar, but different for graze levels 3, 4, and 5. We will 
#   need to test this with a model.


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
birds_inter1 <- lm(ABUND ~ FGRAZE + LOGAREA + FGRAZE:LOGAREA, data = loyn)

# Or use the 'shortcut' - it's equivalent to the model above
# birds_inter1 <- lm(ABUND ~ FGRAZE * LOGAREA, data = loyn)


## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
# first split the plotting device into 2 rows and 2 columns
par(mfrow = c(2,2))

# now create the residuals plots
plot(birds_inter1)

# To test the normality of residuals assumption we use the Normal Q-Q plot. The central
# residuals are not too far from the Q-Q line but the extremes are too extreme (the
# tails of the distribution are too long). Some observations, both high and low, are
# poorly explained by the model.

# The plot of the residuals against the fitted values suggests these extreme residuals
# happen for intermediate fitted values.

# Looking at the homogeneity of variance assumption (Residuals vs Fitted and
# Scale-Location plot), the graphs are mostly messy, with no clear pattern emerging.

# The observations with the highest leverage don't appear to be overly influential,
# according to the Cook's distances in the Residuals vs Leverage plot (all < 0.5).


## ----Q6b, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# now the residuals against each explanatory variable in turn

# extract the residuals and keep them in the dataframe, so they stay lined up
# with the variables we want to plot them against
loyn$resid_inter <- resid(birds_inter1)

par(mfrow = c(1, 2))

plot(resid_inter ~ LOGAREA, data = loyn, pch = 16,
     xlab = "LOGAREA", ylab = "Residuals")
abline(h = 0, lty = 2)

boxplot(resid_inter ~ FGRAZE, data = loyn,
        xlab = "Grazing level", ylab = "Residuals")
abline(h = 0, lty = 2)

# WHAT ARE WE LOOKING FOR HERE? Not an overall trend. If a variable is in the
# model then least squares guarantees the residuals average out to zero against
# it, so there is nothing to find on that score. Check it for yourself if you
# like: cor(loyn$resid_inter, loyn$LOGAREA) is exactly 0, and the mean residual
# is exactly 0 within every level of FGRAZE.

# What these plots CAN show you is shape and spread.

# Against LOGAREA: the scatter is fairly even across the range with no obvious
# curve, so there is no sign that we need a quadratic term for patch area.

# Against FGRAZE: the middle of each box sits on zero, as it must, but the boxes
# are not the same height. Graze level 3 is much more variable than graze levels
# 2 and 4. The residual variances are 21.8, 10.3, 48.5, 6.1 and 12.0, so the
# largest is about eight times the smallest. That is a hint that the homogeneity
# of variance assumption is shakier than the Scale-Location plot made it look.
# Don't over-react to it though: with only 11 to 17 patches per level these
# variance estimates are themselves quite imprecise.


## ----Q6c, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# The other, and arguably more useful, version of this check is to plot the
# residuals against variables that are NOT in the model. If something you left
# out shows a clear pattern here, it may well belong in the model.

# we only created LOGAREA and FGRAZE earlier in this exercise, so make the
# other two transformed variables now (same log10 transformation you used in
# the graphical data exploration exercise)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)

par(mfrow = c(2, 2))
plot(resid_inter ~ LOGDIST, data = loyn, pch = 16, ylab = "Residuals")
abline(h = 0, lty = 2)
plot(resid_inter ~ LOGLDIST, data = loyn, pch = 16, ylab = "Residuals")
abline(h = 0, lty = 2)
plot(resid_inter ~ YR.ISOL, data = loyn, pch = 16, ylab = "Residuals")
abline(h = 0, lty = 2)
plot(resid_inter ~ ALT, data = loyn, pch = 16, ylab = "Residuals")
abline(h = 0, lty = 2)

# All four are patternless (the correlations are 0.02, -0.02, -0.03 and 0.11),
# so nothing we have left out is crying out to be added. That is reassuring, and
# it is worth remembering when we come to do formal model selection in the next
# exercise.


## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
anova(birds_inter1)

# The null hypothesis is that there is no significant interaction between FGRAZE and
# LOGAREA. As the P value is smaller than our cutoff of 0.05 (p = 0.005) we reject the
# null hypothesis and conclude that there is a significant interaction.

# This means that there is a significant relationship between bird abundance and log
# area, and that this relationship is different for different levels of graze (at least
# one of them is different). Put another way, the slopes of the relationship between
# abundance and log area for each level of graze are different.

# As there is a significant interaction, it's difficult to interpret the main effects of
# FGRAZE and LOGAREA as by definition the effect of one variable is dependent on the
# value of the other variable.


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
summary(birds_inter1)

# and the 95% confidence intervals for each of those estimates
confint(birds_inter1)

# One thing first, and it's the same point we made in the previous exercise. Apart from
# the intercept and LOGAREA, every estimate in this table is a DIFFERENCE. FGRAZE2 is
# not graze level 2's intercept, it is how different graze level 2's intercept is from
# graze level 1's. FGRAZE2:LOGAREA is not graze level 2's slope, it is how different
# that slope is from graze level 1's. To get the actual intercept and slope for a
# grazing level, add the relevant contrast onto the baseline:

#   graze level 2 intercept = 21.24 + (-6.06) = 15.18
#   graze level 2 slope     =  4.14 +   4.27  =  8.41

# Keep graze level 1's own slope of 4.14 in mind as you read on. Several of the
# differences below are estimated against it, and they only make sense once you can see
# how small it is.

# (Intercept) Here the Intercept (baseline) is the predicted ABUND when LOGAREA = 0, for
# FGRAZE level 1. Note that LOGAREA = 0 is a patch of 1 hectare (because log10(1) = 0),
# so this is a real patch size and not an extrapolation. We estimate 21.24 birds (95%
# CI: 14.37 to 28.12 birds). the null hypothesis for the intercept is that the intercept
# = 0 As the P value < 0.05 (7.09e-08) we reject this null hypothesis, although 'are
# there no birds at all in a one hectare patch?' was never a question that anyone needed
# answering.

# LOGAREA Represents the slope of the relationship between ABUND and LOGAREA, specific
# to FGRAZE = 1. The null hypothesis is that the slope of the relationship between
# LOGAREA and ABUND = 0, for level FGRAZE = 1 only. So, for graze 1 the slope is 4.14.
# This means that for a 1 unit increase in LOGAREA (i.e. a tenfold increase in patch
# area) we get a corresponding increase of 4.14 birds on average (95% CI: 0.60 to 7.69
# birds). As the P value (0.022) is < 0.05 we conclude that the slope is different from
# 0. Notice how wide that interval is though: these data are consistent with anything
# from a barely detectable increase of 0.6 birds up to a substantial 7.7 birds. We are
# reasonably confident about the direction of this effect but not at all confident about
# its size.

# FGRAZE2 The difference between the intercept of graze level 2 and that of graze level
# 1. The null hypothesis is that this difference = 0. In a one hectare patch, graze
# level 2 is estimated to hold 6.06 fewer birds than graze level 1 (95% CI: -13.72 to
# +1.60, p = 0.118). The interval includes zero, so we fail to reject the null
# hypothesis.

# Stop there. Do NOT go on to say the two are 'the same'. Look at what this interval
# still allows: anything from 13.7 fewer birds to 1.6 more. Nearly 14 fewer birds is a
# bigger difference than the raw gap between these two grazing levels in the previous
# exercise (9.2 birds), so it is not a small possibility we are dismissing.

# With only 11 patches at graze level 2, this is a precision problem, not a null result.
# The honest conclusion is 'these data can't tell us', and that is a perfectly
# legitimate thing to report.

# FGRAZE3, FGRAZE4, FGRAZE5
# Same interpretation as FGRAZE2: each is the difference in intercept from
# graze level 1.
# FGRAZE3: -12.32 birds (95% CI: -20.77 to -3.88, p = 0.005)
# FGRAZE4: -15.41 birds (95% CI: -24.65 to -6.17, p = 0.001)
# FGRAZE5: -17.04 birds (95% CI: -24.64 to -9.44, p < 0.001)
# All three intervals lie entirely below zero, so here we do have clear
# evidence of lower abundance than at graze level 1. But notice how wide they
# still are. We know the direction and that all three reductions are
# substantial; we do not know the size to better than about 15 birds either way.

# FGRAZE2:LOGAREA This represents the difference in the slope of the relationship
# between ABUND and LOGAREA between graze level 2 and graze level 1. The null hypothesis
# is that the difference in slopes between graze level 1 and 2 = 0 (i.e. no difference).
# The estimated difference in slopes is 4.27 (95% CI: -0.56 to 9.10, p = 0.082). The
# interval includes zero, so no clear evidence that the slopes differ, and once again
# that is not the same as saying they are the same. Graze level 1's own slope is only
# 4.14, so an interval reaching 9.10 is consistent with graze level 2 responding to
# patch area more than twice as steeply.

# FGRAZE3:LOGAREA This represents the difference in the slope of the relationship
# between ABUND and LOGAREA between graze level 3 and graze level 1. The null hypothesis
# is that the difference in slopes between graze level 1 and 3 = 0 (i.e. no difference).
# The estimated difference in slopes is 9.06 (95% CI: 2.89 to 15.22) with a P value of
# 0.005. The whole interval lies above zero, so we do have clear evidence that the
# relationship between bird abundance and patch area is steeper in graze level 3 than it
# is in graze level 1.

# FGRAZE4:LOGAREA Same interpretation again: the estimated difference in slopes between
# graze level 4 and graze level 1 is 13.63 (95% CI: 5.33 to 21.94, p = 0.002). The whole
# interval is above zero, so we have clear evidence of a steeper relationship in graze
# level 4 than in graze level 1, and this is the largest slope difference in the model.

# FGRAZE5:LOGAREA The estimated difference in slopes between graze level 5 and graze
# level 1 is 2.00 (95% CI: -4.30 to 8.29, p = 0.528). With a P value that unremarkable
# it is very tempting to write this off as 'no difference'. But the interval runs from
# 4.3 shallower than graze level 1's slope to 8.3 steeper, against a graze level 1 slope
# of only 4.14. So these data are consistent with graze level 5 having essentially no
# relationship with patch area, and equally consistent with it having one three times
# steeper. We have learned almost nothing here, which is a very different statement from
# having learned there is nothing there.

# One general point before you move on. Parameters whose intervals exclude zero and
# those whose intervals include it are not two different kinds of result. They sit on a
# continuum of evidence. FGRAZE2:LOGAREA (p = 0.08) and FGRAZE3:LOGAREA (p = 0.005)
# differ in degree, not in kind.

# The Multiple R-square value is 0.79, so 79% of the variation in the data is explained
# by the model. This is quite a bit more than the models with only LOGAREA and FGRAZE as
# single explanatory variables. Remember though that R-squared tells you how well this
# model fits these data, and nothing more than that.


## ----Q9a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------------------------------------
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data = loyn, col = GRAZE, pch = 16)
# Note: # colour 1 means black in R colour 2 means red in R colour 3 means green in R
# colour 4 means blue in R colour 5 means cyan in R

# FGRAZE1 create a sequence of increasing LOGAREA within the observed range
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 1]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 1]),
					length = 20)

# create data frame for prediction
dat4pred <- data.frame(FGRAZE = "1", LOGAREA = LOGAREA.seq)

# predict for new data
P1 <- predict(birds_inter1, newdata = dat4pred)

# add the predictions to the plot of the data
lines(dat4pred$LOGAREA, P1, col = 1, lwd = 2)

# FGRAZE2
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 2]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 2]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "2", LOGAREA = LOGAREA.seq)

P2 <- predict(birds_inter1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P2, col = 2, lwd = 2)

# FGRAZE3
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 3]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 3]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "3", LOGAREA = LOGAREA.seq)

P3 <- predict(birds_inter1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P3, col = 3, lwd = 2)

# FGRAZE4
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 4]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 4]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "4", LOGAREA = LOGAREA.seq)

P4 <- predict(birds_inter1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P4, col = 4, lwd = 2)

# FGRAZE5
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 5]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 5]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "5", LOGAREA = LOGAREA.seq)

P5 <- predict(birds_inter1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P5, col = 5, lwd = 2)

legend("topleft", 
 legend = paste("Graze = ", 5:1), 
 col = c(5:1), bty = "n",
 lty = c(1, 1, 1), 
 lwd = c(1, 1, 1))

# Now, about those fitted lines for graze levels 1 and 2 (the black and red lines). They
# are not on top of each other. The graze level 2 line starts lower and climbs more
# steeply, and the two lines cross. That is precisely what the estimates in Question 8
# said: an intercept 6.06 birds lower and a slope 4.27 steeper, which is the graze level
# 2 row of the table you made in Q8 (intercept 15.18, slope 8.41). What the large P
# values tell us is that we can't rule out the possibility that the two lines really are
# identical, given how few patches we have in graze level 2 (11 of them) and how much
# scatter there is around each line. They do not tell us that the two lines ARE
# identical, and the plot makes that difference easy to see. The estimated pattern is
# right there in front of you; it just isn't estimated precisely enough for us to be
# sure of it.


## ----Q9b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------------------------------------
# Okay, that was a long-winded way of doing this. If, like me, you prefer more compact
# code and less risks of errors, you can use a loop, to save repeating the sequence 5
# times:
par(mfrow = c(1, 1))
plot(ABUND ~ LOGAREA, data = loyn, col = GRAZE, pch = 16)

for(g in levels(loyn$FGRAZE)){ # g will take the values "1", "2",..., "5" in turn
	LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == g]),
										to = max(loyn$LOGAREA[loyn$FGRAZE == g]),
														length = 20)
	dat4pred <- data.frame(FGRAZE = g, LOGAREA = LOGAREA.seq)
	predicted <- predict(birds_inter1, newdata = dat4pred)
	lines(dat4pred$LOGAREA, predicted, col = as.numeric(g), lwd = 2)
}
legend("topleft", 
 legend = paste("Graze = ", 5:1), 
 col = c(5:1), bty= "n",
 lty = c(1, 1, 1), 
 lwd = c(1, 1, 1))


## ----Q9c, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------------------------------------
# install.packages('ggplot2', dep = TRUE)
library(ggplot2)

ggplot(loyn, aes(x = LOGAREA, y = ABUND, colour = FGRAZE) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)


## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# Same health warning as before: my version, not the only version.

# The relationship between bird abundance and forest patch area depended on grazing
# intensity (interaction F 4,57 = 4.09, p = 0.006; adjusted R2 = 0.77, n = 67 patches).
# Abundance increased with patch area at every grazing level, but the rate of increase
# varied more than fourfold, from 4.1 birds per tenfold increase in area at the lowest
# grazing intensity to 17.8 at grazing level 4 (Fig. 1). In a one hectare patch,
# abundance at grazing level 2 was an estimated 6.1 birds lower than at level 1 (95% CI:
# 13.7 lower to 1.6 higher) and its slope 4.3 steeper (95% CI: 0.6 shallower to 9.1
# steeper); with only 11 patches at that grazing level these data cannot resolve either
# comparison. Patch area therefore mattered least in the most lightly grazed patches,
# which already held high numbers of birds even when small, and most at intermediate to
# high grazing intensities.

# Right, so what's going on in that paragraph? Let's take my three questions in turn.

# 1. No, you can't write that sentence on its own, and this is the big one.

#    Once you have an interaction in the model, "bird abundance increases with
#    patch area" is an incomplete sentence, because HOW MUCH it increases
#    depends on the grazing level. The honest version always carries the
#    condition with it: "abundance increased with patch area at every grazing
#    level, but the rate varied fourfold". Get into the habit of never letting
#    a main effect out on its own when it sits inside an interaction.

# 2. Look at the two clauses in the middle of my paragraph.

#    They give the estimate, they give the interval, and then they say plainly
#    that we can't resolve the comparison. What they never do is claim the two
#    grazing levels are the same. I've also told the reader WHY we can't
#    resolve it (only 11 patches), which turns 'we don't know' from something
#    that sounds like a failure into a useful piece of information about the
#    study. "We cannot tell from these data" is a legitimate result and it is
#    perfectly publishable.

# 3. No, and it's worth being clear about why.

#    It is NOT that the baseline is uninteresting. Graze level 1 is the lightest
#    grazing intensity, so it's a perfectly sensible reference to compare the
#    others against, and a reader will care about it. The problem is simply that
#    ten numbers written out in prose is unreadable. Nobody can hold five
#    intercepts and five slopes in their head from a block of text.

#    So report the interaction test, the range of fitted slopes, the figure,
#    and the adjusted R2 and n, then let your figure do the heavy lifting. If a
#    journal wants the full coefficient table it goes in a supplement. Your
#    figure from Q9 does far more work than the table ever will: five fitted
#    lines whose slopes visibly differ IS the finding.

# Two last things I did on purpose that are worth copying.

# I said 'adjusted R2' rather than just 'R2'. With 10 parameters the two differ
# noticeably here (0.80 against 0.77), and a reader can't check your number if they
# don't know which one you quoted.

# I wrote 'per tenfold increase in area' rather than 'per unit log10 area'. Both are
# correct, but only one of them means anything to an ecologist reading quickly. Wherever
# you've transformed a variable, try to hand the reader the effect back on a scale they
# can picture.

