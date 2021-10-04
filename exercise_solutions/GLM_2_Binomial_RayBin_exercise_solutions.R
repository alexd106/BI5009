## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------
dat<- read.csv("./data/RayMaturity.csv", stringsAsFactors= T)
str(dat)


## ----Q3a, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE---------------------------------------------------------------------
# count the number of observations per month, and per year:
table(dat$Month)
# majority of the data are from April-May-June
table(dat$Year)

# count observations per year/month combination and represent as mosaicplot
Obs.Per.Year.Month<- table(dat$Year, dat$Month)
# See what's in the table:
Obs.Per.Year.Month
# Year in rows, Months in columns

# mosaic plot (this is the default plot for a contingency table in R)
plot(Obs.Per.Year.Month)
# fewer observations in 2012 (column is narrow)
# variable sample size from each month from April to Sept (height / area of the squares varies)

# but overall a fairly "random" representation of all months across years,
# so should not be problematic
# (for example, an issue might arise if all observations from Spring
# came from one year and all Summer observations from a different
# year, hence not comparable maybe)



## ----Q3b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")---------------
Obs.Per.Sex.Area<- table(dat$Sex, dat$Area)
plot(Obs.Per.Sex.Area)
# most data are from males and from the English Channel
# we would thus expect to be able to estimate the effects for
# the males more precisely than for the females.

# if we include data from all sexes in the model and do not account
# for sex effects, the result would be biased towards males.
# Likewise for Area.

# Is length correlated with Month?
dat$fMonth<- factor(dat$Month)
boxplot(Lg ~ fMonth, data= dat)
# A little maybe, but not that clear. Shouldn't be a problem



## ----Q3c, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE---------------------------------------------------------------------
# Maturity in relation to Sex
mean.per.Sex<- tapply(dat$Mature, list(dat$Sex), mean)
barplot(mean.per.Sex, ylim= c(0, 1),
		 xlab= "Sex", ylab= "proportion mature")
# Probability maybe higher in males?



## ----Q3d, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")---------------
# Maturity in relation to Area
mean.per.Area<- tapply(dat$Mature, list(dat$Area), mean)
barplot(mean.per.Area, ylim= c(0, 1),
		  xlab= "Area", ylab= "proportion mature")
# Probability higher in the English Channel?

# Maturity in relation to length
mean.per.Lg<- tapply(dat$Mature, list(as.factor(dat$Lg)), mean)
barplot(mean.per.Lg, ylim= c(0, 1),
		  xlab= "Length class", ylab= "proportion mature")
# Probability increases massively with length, as expected

# Can we ignore the year effect?
mean.per.Year<- tapply(dat$Mature, list(dat$Year), mean)
barplot(mean.per.Year, ylim= c(0, 1),
		  xlab= "Length class", ylab= "proportion mature")
# not much going on, probably safe to ignore

# Can we ignore the month effect?
mean.per.Month<- tapply(dat$Mature, list(dat$Month), mean)
barplot(mean.per.Month, ylim= c(0, 1),
		  xlab= "Length class", ylab= "proportion mature")
# some trend here, not ideal given that most data are from April-May-June
# Maybe Month should be included



## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------
datM<- dat[dat$Sex == "M", ]
Mat1<- glm(Mature ~ Lg + Area + Lg:Area, family= binomial, data= datM)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------
summary(Mat1)

# "(Intercept)" is the predicted value on the link (logit) scale for
# Lg = 0, females (Sex= "F") on the Atlantic coast (Area= "ATL")

# "Lg" is the slope of Length for females on the Atlantic coast.
# Assumes a linear increase (on the logit scale)

# "AreaECH" is the estimated difference (on the logit scale) between
# the Channel and the Atlantic coast.
# Positive means higher odds of being mature in the Channel

# "Lg:AreaECH" is the difference between the slope of Length in the Channel
# compared to the slope of Length in the Atlantic.
# Negative means that the increase is slower in the Channel

# A mathematical description of the model
# (more or less how I would present it in the methods section of a paper):
# maturity ~ Bernoulli(p)  or maturity ~ Binomial(N= 1, p)
# log(p / (1-p)) = -41.97 + 0.52*Length
#    + 15.53 * AeraECH
#    -0.19 * AeraECH * Length 

# The coefficients are on the logit scale,
# so cannot be interpreted directly as probabilities. 
# They are interpreted as changes in log-odds:
# For a given body length, a ray in the English Channel has
# log-odds increased by 15.53 compared to a ray in the Atlantic.
# This is equivalent to saying that the odds increase by exp(15.53).

# None of this is particularly intuitive, so we will mostly
# rely on predictions on the probability (response) scale
# for the interpretation of the model



## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------------

drop1(Mat1, test= "Chisq")
# the interaction of length by area is significant: nothing to simplify.

# out of interest, the total proportion of deviance explained is 
(Mat1$null.deviance - Mat1$deviance) / Mat1$null.deviance
# 33%, pretty good for a binomial model!


## ----Q7a, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
par(mfrow= c(2, 2))
plot(Mat1) # not very useful

# to make sense of what we are seeing, we can add colors: red
# for residuals of mature individuals, black for residuals of immatures
plot(Mat1, col= datM$Mature + 1)
# Still not very telling

# let's plot against predictors:
res2.p<- resid(Mat1, type= "pearson")

par(mfrow= c(2, 2))
plot(res2.p ~ datM$Sex) # boxplot (x axis is a factor)

plot(res2.p ~ datM$Area) # boxplot

plot(res2.p ~ datM$Lg, col= datM$Mature + 1) # scatterplot

# Can't see anything useful.


## ----Q8a, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE, fig.height=5, fig.width=5------------------------------------------
library(arm)
par(mfrow= c(1, 1))
binnedplot(x= datM$Month, y= res2.p, xlab= "Month", nclass = 6)
# nclass= 6 because we have 6 months in the data



## ----Q8b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
par(mfrow= c(2, 2))
binnedplot(x= as.numeric(datM$Area), y= res2.p, xlab= "Area")
binnedplot(x= datM$Month, y= res2.p, xlab= "Month", nclass = 6)
# nclass= 6 because we have 6 months in the data
binnedplot(x= datM$Lg, y= res2.p, xlab= "Length", nclass= 20)
# could use more or less than 20



## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------------------------------------------------------
## # things look fine with Area, with residuals well within
## # the expected limits (grey lines)
## 
## # Same with length, except for the extreme lengths,
## # for which the residuals are slightly too low and too high, respectively.
## # We'll see in the graphs of the predictions if this is a big concern.
## 


## ----Q10a, eval=TRUE, echo=TRUE, results=TRUE, collapse=TRUE, fig.height=5-------------------------------------------------------
# create a sequence of increasing lengths
Seq.Length<- 74:100

# PREDICTIONS FOR MALES ON THE ATLANTIC COAST
dat.new.ATL<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ATL")

Mat1.pred.ATL<- predict(Mat1, dat.new.ATL, type= "link", se.fit= T)

# Add them to the prediction data frame, to keep the workspace tidy
dat.new.ATL$fit<- Mat1.pred.ATL$fit

# Same approach for MALES IN THE ENGLISH CHANNEL
dat.new.ECH<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ECH")
Mat1.pred.ECH<- predict(Mat1, dat.new.ECH, type= "link", se.fit= T)
dat.new.ECH$fit<- Mat1.pred.ECH$fit

# PLOTTING THE TWO AREAS TOGETHER
par(mfrow= c(1, 1))
# plot predictions for Atlantic coast
plot(x= Seq.Length, y= dat.new.ATL$fit, 
      type= "l", lwd= 2, xlab= "Length (cm)",
		  ylab= "Fitted value (link scale)",
      main= "Predictions for Males", col= "navy")

# Add predictions for English Channel
lines(x= Seq.Length, y= dat.new.ECH$fit, lwd= 2, col= "cyan")

legend(x= 87, y= 0.3, legend= c("Atlantic coast", "English Channel"), 
          lwd= 2, col= c("navy", "cyan"), bty= "n")



## ----Q10b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=5----
# create a sequence of increasing lengths
Seq.Length<- 74:100

# PREDICTIONS FOR MALES ON THE ATLANTIC COAST
dat.new.ATL<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ATL")
Mat1.pred.ATL<- predict(Mat1, dat.new.ATL, type= "link", se.fit= T)

# Convert predictions to the response (probability) scale.
# And add them to the prediction data frame to keep the workspace tidy
dat.new.ATL$fit.resp<- exp(Mat1.pred.ATL$fit)/(1+exp(Mat1.pred.ATL$fit)) 
# or 
dat.new.ATL$fit.resp<- plogis(Mat1.pred.ATL$fit)

# PREDICTIONS FOR MALES IN THE ENGLISH CHANNEL
dat.new.ECH<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ECH")
Mat1.pred.ECH<- predict(Mat1, dat.new.ECH, type= "link", se.fit= T)
dat.new.ECH$fit.resp<- plogis(Mat1.pred.ECH$fit)

# PLOTTING IT TOGETHER
par(mfrow= c(1, 1))
# plot predictions for Atlantic coast
plot(x= Seq.Length, y= dat.new.ATL$fit.resp, 
      type= "l", lwd= 2, xlab= "Length (cm)",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for Males", col= "navy")

# Add predictions for English Channel
lines(x= Seq.Length, y= dat.new.ECH$fit.resp, lwd= 2, col= "cyan")

legend(x= 87, y= 0.3, legend= c("Atlantic coast", "English Channel"), 
          lwd= 2, col= c("navy", "cyan"), bty= "n")




## ----Q11, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=5----
# create a sequence of increasing lengths
Seq.Length<- 74:100

# PREDICTIONS FOR MALES ON THE ATLANTIC COAST
dat.new.ATL<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ATL")
Mat1.pred.ATL<- predict(Mat1, dat.new.ATL, type= "link", se.fit= T)

# Convert predictions to the response (probability) scale.
# And add them to the prediction data frame to keep the workspace tidy
dat.new.ATL$fit.resp<- exp(Mat1.pred.ATL$fit)/(1+exp(Mat1.pred.ATL$fit)) 
# or dat.new.ATL$fit.resp<- plogis(Mat1.pred.ATL$fit)

# Calculate the confidence limits on the link scale and transform onto the response scale:
# lower 95% CI
dat.new.ATL$LCI<- plogis(Mat1.pred.ATL$fit - 1.96*Mat1.pred.ATL$se.fit)
# upper 95% CI
dat.new.ATL$UCI<- plogis(Mat1.pred.ATL$fit + 1.96*Mat1.pred.ATL$se.fit)

# PREDICTIONS FOR MALES IN THE ENGLISH CHANNEL
dat.new.ECH<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ECH")
Mat1.pred.ECH<- predict(Mat1, dat.new.ECH, type= "link", se.fit= T)
dat.new.ECH$fit.resp<- plogis(Mat1.pred.ECH$fit)
dat.new.ECH$LCI<- plogis(Mat1.pred.ECH$fit - 1.96*Mat1.pred.ECH$se.fit)
dat.new.ECH$UCI<- plogis(Mat1.pred.ECH$fit + 1.96*Mat1.pred.ECH$se.fit)

# PLOTTING IT ALL TOGETHER
par(mfrow= c(1, 1))
# plot predictions for Atlantic coast
plot(x= Seq.Length, y= dat.new.ATL$fit.resp, 
      type= "l", lwd= 2, xlab= "Length (cm)",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for Males", col= "navy")
# Add the upper and lower confidence limits of predictions
lines(x= Seq.Length, y= dat.new.ATL$UCI, lty= 3, col= "navy")
lines(x= Seq.Length, y= dat.new.ATL$LCI, lty= 3, col= "navy")

# Add predictions for English Channel
lines(x= Seq.Length, y= dat.new.ECH$fit.resp, lwd= 2, col= "cyan")
# Add the upper and lower confidence limits
lines(x= Seq.Length, y= dat.new.ECH$UCI, lty= 3, col= "cyan")
lines(x= Seq.Length, y= dat.new.ECH$LCI, lty= 3, col= "cyan")

legend(x= 87, y= 0.3, legend= c("Atlantic coast", "English Channel"), 
          lwd= 2, col= c("navy", "cyan"), bty= "n")



## ----Q12, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------------
## # The model seems to fit the data satisfortorily, base on the validation plots.
## 
## # Ignoring the effect of Month may bias the results slightly,
## # but hopefully not much (if you wish you could check this yourself)
## 
## # With respect to the large residuals at extreme body lengths,
## # we can see on the graphs that the predicted probabilities are already
## # very high and low, respectively (close to 0 or 1).
## # Therefore, they wouldn't change very much even if we improved the model.
## # So, no much concern here.
## 
## # We infer from the graph of the predictions that:
## 
## # a minority of males is mature at 75 cm
## # almost all males are mature after 85 cm
## # 50% of males have reached maturity at a length around
## # 78-80 cm, however maturity is reached
## # earlier in the Channel than on the Atlantic coastal area.
## 
## # We could speculate that the difference in the rate of maturation
## # between the two areas is due to a higher fishing pressure on the
## # species in the English Channel. This is correlative.
## # More research needed if we want to establish the cause!
## 


## ----binnedplot_alternative, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
par(mfrow= c(1, 1))
# plot the residuals against length
plot(res2.p ~ datM$Lg, col= datM$Mature + 1)
# get the mean of the residuals for each 1 cm of length
lg.means<- tapply(res2.p, list(datM$Lg), mean)
# convert ordered bin labels into numbers (1 to 365)
lg.vals<- as.numeric(names(lg.means))
lines(lg.means ~ lg.vals, col= 3)
abline(h= 0, lty= 3, col= grey(0.5))


## ----Optional1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=5----
Mat2<- glm(Mature ~ Lg + Area + Sex + Lg:Area + Lg:Sex, family= binomial, data= dat)
drop1(Mat2, test= "Chisq")
# interation with Sex not significant -> simplify
Mat3<- glm(Mature ~ Lg + Area + Sex + Lg:Area, family= binomial, data= dat)
drop1(Mat3, test= "Chisq")
# All effects significant -> noting to drop.

# I skip the model validation here, as it is quite similar to previously

# PREDICTIONS
# create a sequence of increasing lengths
Seq.Length<- 74:100

# PREDICTIONS FOR MALES ON THE ATLANTIC COAST
dat.new.ATL.M<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ATL")
Mat3.pred.ATL.M<- predict(Mat3, dat.new.ATL, type= "link", se.fit= T)
# fitted values on the response scale:
dat.new.ATL.M$fit.resp<- plogis(Mat3.pred.ATL.M$fit)
# lower 95% CI
dat.new.ATL.M$LCI<- plogis(Mat3.pred.ATL.M$fit - 1.96*Mat3.pred.ATL.M$se.fit)
# upper 95% CI
dat.new.ATL.M$UCI<- plogis(Mat3.pred.ATL.M$fit + 1.96*Mat3.pred.ATL.M$se.fit)

# PREDICTIONS FOR MALES IN THE ENGLISH CHANNEL
dat.new.ECH.M<- data.frame(Sex= "M", Lg= Seq.Length, Area= "ECH")
Mat3.pred.ECH.M<- predict(Mat3, dat.new.ECH.M, type= "link", se.fit= T)
dat.new.ECH.M$fit.resp<- plogis(Mat3.pred.ECH.M$fit)
dat.new.ECH.M$LCI<- plogis(Mat3.pred.ECH.M$fit - 1.96*Mat3.pred.ECH.M$se.fit)
dat.new.ECH.M$UCI<- plogis(Mat3.pred.ECH.M$fit + 1.96*Mat3.pred.ECH.M$se.fit)

# PREDICTIONS FOR FEMALES ON THE ATLANTIC COAST
dat.new.ATL.F<- data.frame(Sex= "F", Lg= Seq.Length, Area= "ATL")
Mat3.pred.ATL.F<- predict(Mat3, dat.new.ATL.F, type= "link", se.fit= T)
dat.new.ATL.F$fit.resp<- exp(Mat3.pred.ATL.F$fit)/(1+exp(Mat3.pred.ATL.F$fit)) 
dat.new.ATL.F$LCI<- plogis(Mat3.pred.ATL.F$fit - 1.96*Mat3.pred.ATL.F$se.fit)
dat.new.ATL.F$UCI<- plogis(Mat3.pred.ATL.F$fit + 1.96*Mat3.pred.ATL.F$se.fit)

# PREDICTIONS FOR FEMALES IN THE ENGLISH CHANNEL
dat.new.ECH.F<- data.frame(Sex= "F", Lg= Seq.Length, Area= "ECH")
Mat3.pred.ECH.F<- predict(Mat3, dat.new.ECH.F, type= "link", se.fit= T)
dat.new.ECH.F$fit.resp<- plogis(Mat3.pred.ECH.F$fit)
dat.new.ECH.F$LCI<- plogis(Mat3.pred.ECH.F$fit - 1.96*Mat3.pred.ECH.F$se.fit)
dat.new.ECH.F$UCI<- plogis(Mat3.pred.ECH.F$fit + 1.96*Mat3.pred.ECH.F$se.fit)

## PLOTTING MALES AND FEMALES TOGETHER:
# PLOTTING MALES
par(mfrow= c(1, 1))
plot(x= Seq.Length, y= dat.new.ATL.M$fit.resp, 
      type= "l", lwd= 2, xlab= "Length (cm)",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for all groups", col= "navy")
lines(x= Seq.Length, y= dat.new.ATL.M$UCI, lty= 3, col= "navy")
lines(x= Seq.Length, y= dat.new.ATL.M$LCI, lty= 3, col= "navy")

lines(x= Seq.Length, y= dat.new.ECH.M$fit.resp, lwd= 2, col= "cyan")
lines(x= Seq.Length, y= dat.new.ECH.M$UCI, lty= 3, col= "cyan")
lines(x= Seq.Length, y= dat.new.ECH.M$LCI, lty= 3, col= "cyan")

# ADDING FEMALES TO THE PLOT
# plot predictions for Atlantic coast
lines(x= Seq.Length, y= dat.new.ATL.F$fit.resp, lwd= 2, col= "darkgreen")
# Add upper and lower prediction confidence limits
lines(x= Seq.Length, y= dat.new.ATL.F$UCI, lty= 3, col= "darkgreen")
lines(x= Seq.Length, y= dat.new.ATL.F$LCI, lty= 3, col= "darkgreen")
# Add predictions for English Channel
lines(x= Seq.Length, y= dat.new.ECH.F$fit.resp, lwd= 2, col= "green")
# Add upper and lower prediction confidence limits
lines(x= Seq.Length, y= dat.new.ECH.F$UCI, lty= 3, col= "green")
lines(x= Seq.Length, y= dat.new.ECH.F$LCI, lty= 3, col= "green")

legend(x= 87, y= 0.4, legend= c("Males Atlantic coast", "Males English Channel", "Females Atlantic coast", "Females English Channel"), 
          lwd= 2, col= c("navy", "cyan", "darkgreen", "green"), bty= "n")


## ----Optional2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------------------------
## # The model seems to be fitting the data satisfactorily.
## 
## # We infer from the graph of the predictions that:
## 
## # As previously,
## # a minority of males is mature at 75 cm
## # almost all males are mature at 85 cm
## # 50% of males have reached maturity at a length around
## # 78-80 cm, however maturity is reached
## # earlier in the Channel than on the Atlantic coastal area.
## 
## # In addition,
## # Females mature at a larger length than males:
## # a minority of females is mature at 75 cm
## # almost all females are mature at 90 cm
## # 50% of females have reached maturity at a length around
## # 83 cm, without a clear difference between the Channel
## # and the Atlantic coastal area (but note that the confidence
## # intervals are wider than males, due to the smaller sample size).
## 
## # We could speculate that the difference in the rate of maturation
## # between the two areas is due to a higher fishing pressure on the
## # species in the English Channel, although the pattern found in
## # females does not support this so clearly. More data and research needed?

