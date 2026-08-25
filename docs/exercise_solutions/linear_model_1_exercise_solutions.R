## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
loyn <- read.table("data/loyn.txt", header = TRUE, 
                   stringsAsFactors = TRUE)
str(loyn)

# 67 observations and 8 variables (from str())


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
# let's first log10 transform the AREA variable due to the couple 
# of unusually large forest area values (check you data exploration)
# to remind yourself

loyn$LOGAREA <- log10(loyn$AREA)

# make a scatterplot of bird abundance and log10 Area

plot(loyn$LOGAREA, loyn$ABUND, xlab = "log10 forest area", 
     ylab = "bird abundance", ylim = c(0, 55))

# now fit the linear model

loyn_lm <- lm(ABUND ~ LOGAREA, data = loyn)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
# ANOVA table
anova(loyn_lm)

# The null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0
# i.e. there is no relationship

# The p value is very small (3.81e-14) therefore we 
# reject this null hypothesis (i.e. the slope is different
# from 0)


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
summary(loyn_lm)

# the estimate of the intercept = 10.4
# the estimate of the slope = 9.78

# word equation
# ABUND = 10.40 + 9.78 * LOGAREA

# the estimates on their own tell you nothing about how precisely they have
# been measured, so let's also get their 95% confidence intervals
confint(loyn_lm)

# the 95% confidence interval for the slope is 7.76 to 11.81. In words, for
# every 1 unit increase in LOGAREA these data are consistent with an increase
# of somewhere between about 7.8 and 11.8 birds (and a 1 unit increase in
# LOGAREA is a tenfold increase in patch area on the original scale). Our best
# estimate is 9.78 birds, but it's the interval that tells us how precisely
# we've measured it.

# notice that both ends of this interval describe a substantial effect (bird
# abundance in these data ranges from 1.5 to 39.6 birds), so our ecological
# conclusion, that forest patch area matters and matters a lot, holds right
# across the whole interval. This is a well resolved result. You'll meet a
# much less well resolved one in the linear model 3 exercise.

# the 95% confidence interval for the intercept is 8.05 to 12.76. The intercept
# is the predicted bird abundance when LOGAREA = 0, which is a patch of 1
# hectare (because log10(1) = 0), so these data are consistent with somewhere
# between about 8 and 13 birds in a one hectare patch.


## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------

# the null hypothesis for the intercept is that the intercept = 0
# the p value is very small (certainly less than the (not so magic) 0.05)
# therefore we reject this null hypothesis and conclude that the intercept
# is different from zero.

# the null hypothesis for the slope is that the slope = 0
# the p value is very small (3.81e-14)
# therefore we reject this null hypothesis and conclude that the slope
# is different from zero (i.e. there is a significant relationship between
# LOGAREA and ABUND).

# and now the same two findings without the word 'significant'

# Bird abundance increases with forest patch area. We estimate an increase of
# 9.78 birds (95% CI: 7.76 to 11.81) for every 1 unit increase in LOGAREA
# (i.e. for every tenfold increase in patch area).
# In a one hectare patch we estimate a bird abundance of 10.41 birds
# (95% CI: 8.05 to 12.76).

# the second version is a bit longer, but it tells a reader by how much bird
# abundance changes, in what units, and how precisely we know it. The word
# 'significant' only tells them that we were able to detect the effect at all.
# It says nothing whatsoever about whether the effect is large or whether it
# is ecologically important.


## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
summary(loyn_lm)

# The multiple R-squared value is 0.588 and therefore 58.8% of
# the variation in ABUND is explained by LOGAREA

# One word of caution about R-squared. It measures how well the model fits
# these data and nothing more. A high R-squared doesn't mean that the
# relationship is important, that LOGAREA causes the change in ABUND, or that
# the model would predict well for a new set of forest patches. Equally, a low
# R-squared doesn't mean that an effect is unimportant, as plenty of real and
# useful effects sit inside very noisy systems.


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
# first split the plotting device into 2 rows and 2 columns
par(mfrow = c(2,2))

# now create the residuals plots
plot(loyn_lm)

# The normal Q-Q plot is used to assess normality of the residuals (top right).
# If perfectly normal then all residuals would lie exactly on the reference line.
# In reality this is never going to happen so we are looking for obvious large 
# departures. In this plot they don't look too bad, although the lower quantiles
# do seem to deviate. All in all I would be cautiously optimistic.

# To check homogeneity of variance assumption we look at both the Residuals vs Fitted (top left)
# and the Scale-Location plots (bottom left). 
# There doesn't appear to be any obvious patterns of the residuals in these plots, 
# although you can see the two residuals associated with the two large forest patch areas 
# are below the zero line (negative residuals) on the right hand side of these plots
# despite the log transformation. Perhaps something to bear in mind as we progress. 
# But in short, the homogeneity of variance assumption looks ok.

# From the Residuals vs Leverage plot (bottom right) you can see that there are no residuals 
# with a Cook's distance greater than 1. In fact they are all well below 0.5. If you want to 
# produce a plot of just Cook's distance (perhaps this is clearer)
par(mfrow = c(1,1))
plot(loyn_lm, which = 4)

# Going back to the Residuals vs Leverage plot (bottom right) you can see 
# three residuals that are somewhat unusual in terms of their leverage as 
# they stick out a bit to the right compared to the rest of the residuals. 
# Two of these residuals are our two large forest patch areas again. However,
# leverage still seems to be on the low side, so at least on the scale of the
# log transformation things look ok.


## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
# to predict bird abundance if AREA == 100

# if you log base 10 transformed the AREA variable
bird_abundance100 = 10.4 + (9.78 * log10(100))
bird_abundance100

# if you used the natural log (i.e. log()) then you would use log()
# not log10()


## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# need to create a dataframe object with a column of LOGAREA values to predict from.
# note you need to call the column the same as used in the model
my_data <- data.frame(LOGAREA = seq(from = min(loyn$LOGAREA),
                                  to = max(loyn$LOGAREA),
                                  length = 50))

# use predict function to calculate predicted values of abundance based on the 
# new LOGAREA values in the data frame my.data (use the newdata argument)
pred_vals <- predict(loyn_lm, newdata = my_data)


## ----Q11, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# plot the lines on the plot. The x values are the new LOGAREA values from the my.data
# dataframe, the predicted values are from pred.vals 
plot(loyn$LOGAREA, loyn$ABUND, xlab = "Log10 Patch Area", 
     ylab = "Bird Abundance", ylim = c(0, 55))

lines(my_data$LOGAREA, pred_vals, lty = 1,col = "firebrick")

# for those of you who are into plotting using the ggplot2 package,
# this is one of those occasions where things are a little simpler!
# Don't forget, you will need to install the ggplot2 package if
# you've never used it before

# install.packages('ggplot2')

library(ggplot2)
ggplot(mapping = aes(x = LOGAREA, y = ABUND), data = loyn) +
    geom_point() +
    xlab("Log10 Patch Area") +
    ylab("Bird Abundance") +
    geom_smooth(method = "lm", se = FALSE, colour = "firebrick") +
    theme_classic()


## ----Q12, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
pred_vals_se <- predict(loyn_lm, newdata = my_data, se.fit = TRUE) # note the use of the se.fit argument

# check out the structure of pred_vals_se
# you can see we now have 4 vectors in this object
# $fit = fitted values
# $se.fit = standard error of fitted values
# $df = degrees of freedom
# $residual.scale = residual standard error

# so we will need to access our fitted values and standard errors using 
# pred.val.se$fit and pred.vals.se$se.fit respectively
str(pred_vals_se)

# now create the plot
plot(x = loyn$LOGAREA, y = loyn$ABUND,
     xlab = "Log10 Patch Area",
     ylab = "Bird abundance", ylim = c(0, 55))
    

# add the fitted values as before but now we need to use 
# pred.vals.se$fit
lines(my_data$LOGAREA, pred_vals_se$fit, lty = 1,col = "firebrick")

# add the upper 95% confidence interval
lines(my_data$LOGAREA, pred_vals_se$fit + (1.96 * pred_vals_se$se.fit), lty = 2, col = "firebrick")

# add the lower 95% confidence interval
lines(my_data$LOGAREA, pred_vals_se$fit - (1.96 * pred_vals_se$se.fit), lty = 2, col = "firebrick")

# And the ggplot way of doing things

ggplot(mapping = aes(x = LOGAREA, y = ABUND), data = loyn) +
    geom_point() +
    xlab("Log10 Patch Area") +
    ylab("Bird Abundance") +
    geom_smooth(method = "lm", se = TRUE, colour = "firebrick") +
    theme_classic()


## ----Q13, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# back transformed LOGAREA and 95% confidence intervals

# re-plot but this time use the original untransformed AREA variable
plot(x = loyn$AREA, y = loyn$ABUND, ylim = c(0,55),
     xlab = "Patch Area",
     ylab = "Bird abundance")

# now add the fitted lines and upper and lower 95% confidence intervals
# remember we only log10 transformed the LOGAREA variable so this is the only
# variable that we need to back-transform

lines(10^(my_data$LOGAREA), pred_vals_se$fit, lty = 1,col = "firebrick")
lines(10^(my_data$LOGAREA), pred_vals_se$fit + (1.96 * pred_vals_se$se.fit), lty = 2, col = "firebrick")
lines(10^(my_data$LOGAREA), pred_vals_se$fit - (1.96 * pred_vals_se$se.fit), lty = 2, col = "firebrick")

# the model doesn't look too great now! The fit is poor at both ends. Those 4 to 8
# hectare patches holding 24 to 28 birds are under-predicted by about 8 birds, and on
# this plot they're squashed against the left hand axis where they're easy to miss.

# Notice the gap too. Patches run from 0.1 to 144 hectares and then jump to 973 and 1771,
# so the right hand half of this plot rests on two points (the same two that had high
# leverage in Q8) and the line across it is an assumption. You could restrict the analysis
# to the smaller patches, which is defensible if your question is really about small
# remnants, but it doesn't fix the misfit at the low end, the gap is much less severe on
# the log10 scale the model is actually fitted on, and dropping data because a plot looks
# wrong is a decision you would have to declare.

# And the thing we promised back in Q5: the fitted line is straight on the log scale and
# curved here, so there is no single 'birds per hectare' slope to quote. A hectare added
# to a 1 hectare patch is worth about 2.9 birds, a hectare added to a 1000 hectare patch
# about 0.004. What stays constant is the multiplicative step, 9.78 birds per tenfold
# increase in area, which is why we reported the estimate per 1 unit of LOGAREA in Q5.


## ----Q14, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------
# Before you read mine, a health warning. This is how I would write it up, and it's
# one reasonable version among many. Reporting conventions vary a good deal between
# journals, between subfields and between supervisors. What matters is that the
# estimate, its uncertainty and the sample size are all in there, that you haven't
# claimed anything the data don't support, and that somebody could reproduce what you
# did from what you've written. Compare yours against mine, and against the papers
# you're reading. Where they differ, ask yourself which is clearer and which is more
# honest.

# Bird abundance increased with forest patch area. A linear model of bird abundance
# against log10 transformed patch area estimated an increase of 9.78 birds (95% CI:
# 7.76 to 11.81) for every 1 unit increase in log10 area, which is a tenfold increase
# in patch area (F 1,65 = 92.85, p < 0.001). The model explained 58.8% of the variation
# in bird abundance (n = 67 patches).

# Notice that two of those numbers didn't come from summary(). The confidence interval
# came from confint() and the sample size you had to go and look up. Always give n. Two
# studies reporting the same estimate with n = 67 and n = 6700 are not making the same
# claim, and it's the confidence interval that gives the game away.

