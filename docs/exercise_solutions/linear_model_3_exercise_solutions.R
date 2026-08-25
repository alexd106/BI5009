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

# or
# library(lattice)
# xyplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)

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

# To test the normality of residuals assumption we use the Normal Q-Q plot. 
# The central residuals are not too far from the Q-Q line but the extremes
# are too extreme (the tails of the distribution are too long). Some
# observations, both high and low, are poorly explained by the model.

# The plot of the residuals against the fitted values suggests these
# extreme residuals happen for intermediate fitted values.

# Looking at the homogeneity of variance assumption (Residuals vs
# Fitted and Scale-Location plot),
# the graphs are mostly messy, with no clear pattern emerging.

# The observations with the highest leverage don't appear to be overly
# influential, according to the Cook's distances in the Residuals vs
# Leverage plot (all < 0.5).  


## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
anova(birds_inter1)

# The null hypothesis is that there is no significant interaction between 
# FGRAZE and LOGAREA. 
# As the P value is smaller than our cutoff of 0.05 (p = 0.005) we reject the 
# null hypothesis and conclude that there is a significant interaction. 

# This means that there is a significant relationship between bird abundance 
# and log area, and that this relationship is different for different levels of 
# graze (at least one of them is different). Put another way, the slopes of the 
# relationship between abundance and log area for each level of graze are different.

# As there is a significant interaction, it's difficult to interpret the main
# effects of FGRAZE and LOGAREA as by definition the effect of one variable is 
# dependent on the value of the other variable.


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------
summary(birds_inter1)

# and the 95% confidence intervals for each of those estimates
confint(birds_inter1)

# Before working through these one at a time, be clear about one thing, and it's
# the same point we made in the previous exercise. Apart from the intercept and
# LOGAREA, every estimate in this table is a DIFFERENCE. FGRAZE2 is not graze
# level 2's intercept, it's how far graze level 2's intercept sits from graze
# level 1's. FGRAZE2:LOGAREA is not graze level 2's slope, it's how far graze
# level 2's slope sits from graze level 1's. To get the actual intercept and
# slope for a grazing level you add the relevant contrast onto the baseline.

# for graze level 2, for example:
#   intercept = 21.24 + (-6.06) = 15.18
#   slope     =  4.14 +   4.27  =  8.41

# and here are all five at once
# (unname() just strips the coefficient names off, otherwise the rows come out
# labelled FGRAZE2, FGRAZE3 and so on, which is the very thing we're trying to
# stop you thinking)
b <- unname(coef(birds_inter1))
round(data.frame(FGRAZE    = 1:5,
                 intercept = c(b[1], b[1] + b[2:5]),
                 slope     = c(b[6], b[6] + b[7:10])), 2)

# Those five intercepts and five slopes are exactly the five lines you'll plot
# in Q9. Keep them to hand, because some of the interpretations below only make
# sense once you can see how small graze level 1's own slope (4.14) is next to
# the differences we're estimating against it.

# (Intercept)
# Here the Intercept (baseline) is the predicted ABUND when LOGAREA = 0,
# for FGRAZE level 1. Note that LOGAREA = 0 is a patch of 1 hectare (because
# log10(1) = 0), so this is a real patch size and not an extrapolation.
# We estimate 21.24 birds (95% CI: 14.37 to 28.12 birds).
# the null hypothesis for the intercept is that the intercept = 0
# As the P value < 0.05 (7.09e-08) we reject this null hypothesis, although
# 'are there no birds at all in a one hectare patch?' was never a question
# that anyone needed answering.

# LOGAREA 
# Represents the slope of the relationship between ABUND and LOGAREA, 
# specific to FGRAZE = 1.
# The null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0, for level FGRAZE = 1 only. 
# So, for graze 1 the slope is 4.14. This means that for a 1 unit increase in
# LOGAREA (i.e. a tenfold increase in patch area) we get a corresponding
# increase of 4.14 birds on average (95% CI: 0.60 to 7.69 birds).
# As the P value (0.022) is < 0.05 we conclude that the slope is different
# from 0. Notice how wide that interval is though: these data are consistent
# with anything from a barely detectable increase of 0.6 birds up to a
# substantial 7.7 birds. We are reasonably confident about the direction of
# this effect but not at all confident about its size.

# FGRAZE2
# Is the estimated difference (contrasts) between the *intercept* of FGRAZE level
# 2 and the reference level intercept, FGRAZE = 1.
# The null hypothesis associated with this estimate is that the difference
# in the intercepts between graze level 1 and graze level 2 = 0.
# The estimated difference is -6.06 birds (95% CI: -13.72 to +1.60) and the
# P value is 0.118. The interval includes zero, so we fail to reject the null
# hypothesis. In other words, we have no clear evidence that these two
# intercepts differ.

# And that is exactly where we stop. It is very tempting to take one more step
# and conclude that the intercepts for graze level 1 and graze level 2 'are the
# same', but we can't. A P value is calculated on the assumption that the null
# hypothesis is true, so it can never be turned round into evidence that the
# null hypothesis IS true. A large P value has two quite different causes and
# it cannot tell them apart: either the effect really is negligible, or our
# study simply isn't able to resolve it.

# So look at what this interval does NOT rule out. These data are compatible
# with graze level 2 having nearly 14 fewer birds in a one hectare patch. That
# is larger than the raw difference in group means between graze levels 1 and 2
# (9.2 birds, from the previous exercise) and it is about 1.4 standard
# deviations of bird abundance. They are equally compatible with graze level 2
# having slightly more birds. With only 11 forest patches in graze level 2 this
# is a precision problem, not a null result. The honest conclusion is 'we can't
# tell from these data', and that is a perfectly legitimate thing to report.

# FGRAZE3, FGRAZE4, FGRAZE5
# The parameter estimates have the same interpretation as for FGRAZE2 (above). 
# They are all estimates of the difference between FGRAZE at the appropriate level 
# and FGRAZE 1 (Intercept).
# FGRAZE3: -12.32 birds (95% CI: -20.77 to -3.88, p = 0.005)
# FGRAZE4: -15.41 birds (95% CI: -24.65 to -6.17, p = 0.001)
# FGRAZE5: -17.04 birds (95% CI: -24.64 to -9.44, p < 0.001)
# For all three the entire interval lies below zero, so here we do have clear
# evidence that the intercepts are lower than that of graze level 1. Notice
# that these intervals are wide as well. We know the direction, and we know
# that all three are substantial reductions, but the size is only pinned down
# to within roughly 15 birds either way.

# FGRAZE2:LOGAREA
# This represents the difference in the slope of the relationship between ABUND 
# and LOGAREA between graze level 2 and graze level 1.
# The null hypothesis is that the difference in slopes between graze level 1 
# and 2 = 0 (i.e. no difference).
# The estimated difference in slopes is 4.27 (95% CI: -0.56 to 9.10) with a
# P value of 0.082. The interval includes zero, so we have no clear evidence
# that these two slopes differ. And once again, that is not the same thing as
# saying that they are the same. The graze level 1 slope is itself only 4.14,
# so an interval running all the way up to 9.10 is consistent with graze level
# 2 responding to patch area more than twice as steeply as graze level 1.
# Writing those two slopes off as identical would be a serious misreport of
# what these data actually show.

# FGRAZE3:LOGAREA
# This represents the difference in the slope of the relationship between ABUND 
# and LOGAREA between graze level 3 and graze level 1.
# The null hypothesis is that the difference in slopes between graze level 1 
# and 3 = 0 (i.e. no difference).
# The estimated difference in slopes is 9.06 (95% CI: 2.89 to 15.22) with a
# P value of 0.005. The whole interval lies above zero, so we do have clear
# evidence that the relationship between bird abundance and patch area is
# steeper in graze level 3 than it is in graze level 1.

# FGRAZE4:LOGAREA
# Same interpretation again: the estimated difference in slopes between graze
# level 4 and graze level 1 is 13.63 (95% CI: 5.33 to 21.94, p = 0.002). The
# whole interval is above zero, so we have clear evidence of a steeper
# relationship in graze level 4 than in graze level 1, and this is the largest
# slope difference in the model.

# FGRAZE5:LOGAREA
# The estimated difference in slopes between graze level 5 and graze level 1 is
# 2.00 (95% CI: -4.30 to 8.29) with a P value of 0.528. This is the least
# informative parameter in the whole model, and a P value of 0.53 is about as
# unremarkable as they come, so it is very tempting indeed to write it off as
# 'no difference'. But look at the interval. It runs from a slope 4.3 shallower
# than graze level 1's to one 8.3 steeper, and graze level 1's own slope is
# only 4.14. These data are therefore consistent with graze level 5 having
# essentially no relationship with patch area at all, and equally consistent
# with it having a relationship three times steeper than graze level 1. We have
# learned almost nothing about this particular comparison, which is a very
# different statement from having learned that there is nothing there.

# One general point that is worth pausing over before you move on. The
# parameters whose intervals exclude zero and those whose intervals include it
# are not two different kinds of result. They sit on a continuum of evidence.
# FGRAZE2:LOGAREA (p = 0.08) and FGRAZE3:LOGAREA (p = 0.005) differ in degree,
# not in kind, and drawing a line at 0.05 and sorting them into 'real' and 'not
# real' throws that information away.

# The Multiple R-square value is 0.79, so 79% of the variation in the data is 
# explained by the model. This is quite a bit more than the models with only
# LOGAREA and FGRAZE as single explanatory variables. Remember though that
# R-squared tells you how well this model fits these data, and nothing more
# than that.


## ----Q9a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------------------------------------
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data = loyn, col = GRAZE, pch = 16)
# Note: # colour 1 means black in R
# colour 2 means red in R
# colour 3 means green in R
# colour 4 means blue in R
# colour 5 means cyan in R

# FGRAZE1
# create a sequence of increasing LOGAREA within the observed range
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

# Now, about those fitted lines for graze levels 1 and 2 (the black and red
# lines). They are not on top of each other. The graze level 2 line starts
# lower and climbs more steeply, and the two lines cross. That is precisely
# what the estimates in Question 8 said: an intercept 6.06 birds lower and a
# slope 4.27 steeper, which is the graze level 2 row of the table you made in
# Q8 (intercept 15.18, slope 8.41). What the large P values tell us is that we can't rule
# out the possibility that the two lines really are identical, given how few
# patches we have in graze level 2 (11 of them) and how much scatter there is
# around each line. They do not tell us that the two lines ARE identical, and
# the plot makes that difference easy to see. The estimated pattern is right
# there in front of you; it just isn't estimated precisely enough for us to be
# sure of it.


## ----Q9b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------------------------------------
# Okay, that was a long-winded way of doing this.
# If, like me, you prefer more compact code and less risks of errors,
# you can use a loop, to save repeating the sequence 5 times:
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

