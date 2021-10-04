## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE)
str(loyn)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------
## coplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)
## 
## # or
## library(lattice)
## xyplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)
## 
## # There is a lot of variation in there, but:
## # The mean abundance seems to decrease as grazing levels increase.
## # This is most noticeable in the highest grazing level.
## # Within a grazing level, abundance seems to increase with the log-patch area.
## # It is unclear from this if the slope of the log-area effect is
## # different between grazing levels


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
birds.inter.1 <- lm(ABUND ~ FGRAZE * LOGAREA , data = loyn)


## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------
## # first split the plotting device into 2 rows and 2 columns
## par(mfrow = c(2,2))
## 
## # now create the residuals plots
## plot(birds.inter.1)
## 
## # To test the normality of residuals assumption we use the Normal Q-Q plot.
## # The central residuals are not too far from the Q-Q line but the extremes
## # are too extreme (the tails of the distribution are too long). Some
## # observations, both high and low, are poorly explained by the model.
## 
## # The plot of the residuals against the fitted values suggests these
## # extreme residuals happen for intermediate fitted values.
## 
## # Looking at the homogeneity of variance assumption (Residuals vs
## # Fitted and Scale-Location plot),
## # the graphs are mostly messy, with no clear pattern emerging. There is
## # a hint of smaller variance with the lowest fitted values, which is not ideal.
## # This could mean that the homogeneity of variance assumption is not met
## # (i.e. the variances are not the same), but with this amount of data we
## # can't tell. ABUND being bounded by zero, it wouldn't be too
## # surprising that the variance increases with the mean abundance.
## 
## # The observations with the highest leverage don't appear to be overly
## # influential, according to the Cook's distances in the Residuals vs
## # Leverage plot.


## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------
summary(birds.inter.1)

# Here the intercept (baseline) is the predicted `ABUND` for LOGAREA = 0,
# for FGRAZE level 1.
# the null hypothesis for the intercept is that the intercept = 0
# (not biologically relevant).

# LOGAREA represents the slope for LOGAREA, specific to level FGRAZE = 1.
# The null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0, for level FGRAZE = 1 only. 

# FGRAZE2...5 estimate differences (contrasts) between the *intercept* of
# each level and the *intercept* of the reference level, FGRAZE = 1. 

# FGRAZE2...5:LOGAREA estimate differences (contrasts) between the *slope*
# of LOGAREA for each level and the *slope* of LOGAREA for the reference
# level, FGRAZE = 1. 


# The Multiple R-square value is 0.76, so 76% of the variation in the data is explained by the model.


## ----Q8a, eval=TRUE, echo=TRUE, collapse=FALSE------------------------------------------------------------
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data= loyn, col= GRAZE, pch= 16)
# Note: # color 1 means black in R
# color 2 means red in R
# color 3 means green in R
# color 4 means blue in R
# color 5 means cyan in R

# FGRAZE1
# create a sequence of increasing Biomass within the observed range
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 1]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 1]),
					length= 20)
# create data frame for prediction
dat4pred<- data.frame(FGRAZE= "1", LOGAREA= LOGAREA.seq)
# predict for new data
dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
# add the predictions to the plot of the data
lines(predicted ~ LOGAREA, data= dat4pred, col= 1, lwd= 2)

# FGRAZE2
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 2]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 2]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "2", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 2, lwd= 2)

# FGRAZE3
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 3]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 3]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "3", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 3, lwd= 2)

# FGRAZE4
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 4]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 4]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "4", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 4, lwd= 2)

# FGRAZE5
LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == 5]),
					to= max(loyn$LOGAREA[loyn$FGRAZE == 5]),
					length= 20)
dat4pred<- data.frame(FGRAZE= "5", LOGAREA= LOGAREA.seq)
dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
lines(predicted ~ LOGAREA, data= dat4pred, col= 5, lwd= 2)

legend("topleft", 
 legend= paste("Graze = ", 5:1), 
 col= c(5:1), bty= "n",
 lty= c(1, 1, 1), 
 lwd= c(1, 1, 1))


## ----Q8b, eval=TRUE, echo=TRUE, collapse=FALSE------------------------------------------------------------
# Okay, that was a long-winded way of doing this.
# If, like me, you prefer more compact code and less risks of errors,
# you can use a loop, to save repeating the sequence 5 times:
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data= loyn, col= GRAZE, pch= 16)

for(g in levels(loyn$FGRAZE)){# `g` will take the values "1", "2",..., "5" in turn
	LOGAREA.seq<- seq(from= min(loyn$LOGAREA[loyn$FGRAZE == g]),
										to= max(loyn$LOGAREA[loyn$FGRAZE == g]),
														length= 20)
	dat4pred<- data.frame(FGRAZE= g, LOGAREA= LOGAREA.seq)
	dat4pred$predicted<- predict(birds.inter.1, newdata= dat4pred)
	lines(predicted ~ LOGAREA, data= dat4pred, col= as.numeric(g), lwd= 2)
}
legend("topleft", 
 legend= paste("Graze = ", 5:1), 
 col= c(5:1), bty= "n",
 lty= c(1, 1, 1), 
 lwd= c(1, 1, 1))


## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------
## # INTERACTIVE MODEL
## # The slopes of the LOGAREA effect across grazing levels are all over the
## # place, without any coherent pattern (for instance, they could have been
## # increasing or decreasing gradually from low to high grazing intensity).
## # From a model like this, it appears that abundance is broadly increasing
## # with patch size, however it is challenging to learn anything general
## # about the nature of the relationships between birds, in relation to
## # patch size and grazing.
## 
## # The interaction is non-significant, so isn't supported statistically either.
## 
## # Time to revert to a simpler, or an altogether different model?
## # More on this in the next exercise!
## 
## # Some observations are poorly predicted (fitted) using the current set
## # of predictors.
## 

