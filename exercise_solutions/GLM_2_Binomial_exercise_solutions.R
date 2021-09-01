## ----Q2, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE-------------------

dat<- read.csv("./data/dolphin.csv", stringsAsFactors= T)

dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2"))
# reordering chronologically

str(dat)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide")----
# count observations per year/month combination and represent as mosaicplot
plot(table(dat$year, dat$mon))
# CPOD failure in Feb-April 2012 and Dec 2012-March 2013

plot(table(dat$mh))
# fairly even representation of hours
# (that's on the random sample; Almost perfectly balanced on the full dataset)
# we should have no problem using 'mh' as a predictor in the model

plot(table(dat$Tide4, dat$mh))
# even representation of tides
# time of day and tidal phase not independent (but not a linear correlation)
# This is balanced enough that we should have no problem using 'mh', Tide4
# or their interaction as predictors in the model.

#### Now, investigating variation in probability of encounter:
#
# presence in relation to time of day
mean.per.mh<- tapply(dat$presence, list(dat$mh), mean)
plot(mean.per.mh, type= "l", ylim= c(0, 1),
		 xlab= "time of day", ylab= "proportion of hours present")
# Probability slightly lower in the middle of the day

# are there seasonal patterns?
mean.per.mon<- tapply(dat$presence, list(dat$mon), mean)
plot(mean.per.mon, type= "l",
				ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present")
# Probability of presence lower in Jan-March?

# presence in relation to tide
mean.per.Tide4<- tapply(dat$presence, list(dat$Tide4), mean)
plot(mean.per.Tide4, type= "b", ylim= c(0, 1),
     xlab= "tidal phase",
		 ylab= "proportion of hours present")
# No obvious effect of tidal phase on average?


# If interested, we could also ask more complex questions, involving interactions between predictors, for example:

# are seasonal patterns similar between years?
# let's calculate the mean per month for each year,
# and plot the seasonal pattern lines for individual years together
mean.per.month.year<- tapply(dat$presence, list(dat$mon, dat$year), mean)
# (month in rows, years in columns)

# matplot draws one line per column (year)
matplot(mean.per.month.year, type= "l",
				ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present")

legend(x= "topleft", legend= colnames(mean.per.month.year),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.year),
       lty= 1:ncol(mean.per.month.year),
       title= "Year")

# This suggests broadly similar seasonal patterns of variation across years,
# with very low probability of presence from Jan to March

# We could also explore if the effect of some predictors changes between seasons:
# Seasonal variation in diel pattern
mean.per.month.Time6<- tapply(dat$presence, list(dat$mon, dat$Time6), mean)
matplot(mean.per.month.Time6, type= "l", 
        ylim= c(0, 1), 
				xlab= "month", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.per.month.Time6),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.Time6),
       lty= 1:ncol(mean.per.month.Time6),
       title= "Time6")

# stronger diel pattern in later part of the year:
# the lines for different parts of the day diverge quite
# strongly from Sept to Jan.

# are seasonal patterns similar between Tide4 levels?
# let's calculate the mean per month for each tidal stage,
mean.per.month.Tide4<- tapply(dat$presence, list(dat$mon, dat$Tide4), mean)
matplot(mean.per.month.Tide4, type= "l",
       ylim= c(0, 1),
			 xlab= "month", ylab= "proportion of hours present", lty= 1)

legend(x= "topleft", legend= colnames(mean.per.month.Tide4),
       bty= "n", # no bounding box for the legend
       col= 1:ncol(mean.per.month.Tide4),
       lty= 1:ncol(mean.per.month.Tide4),
       title= "Tide4")

# no dramatic change in pattern of tide use across seasons, 
# as all the lines follow a broadly similar trajectory:
# the probability of sighting is mostly affected by season.
# There are variations among tide levels but more subtle.
# Would such an interaction turn out to be significant in a model?



## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
PA1<- glm(presence ~ tideangle_deg + mh + julianday, family= binomial, data= dat)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
summary(PA1)

# Model description:
# presence ~ Bernoulli(p)  or presence ~ Binomial(N= 1, p)
# log(p / (1-p)) = 
#       -1.40*(Intercept) + -0.00044*tideangle_deg + 0.0012*mh 
#       + 0.0022*julianday 

# "(Intercept)" general intercept
# "tideangle_deg" main effect of tide angle, assumes a linear decrease 
# (negative coefficient) of probability of presence from high to flood stages
# "mh" main effect of time of day, assumes a linear  increase 
# of probability of presence from the first hour of the day to the last
# "julianday" main effect of day of year, assumes a linear increase 
# of probability of presence from 1st Jan to 31st Dec

# Note that interpretations above are linear effects on the link scale,
# but sigmoidal on the probability scale, thanks to the logit link



## ----Q6a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
library(car)
vif(PA1)
# No concern.

par(mfrow= c(2, 2))
plot(PA1, col= dat$presence + 1) # red is presence, black is absence
# Not very useful or pretty statistical art. Not worth framing.

# plot against predictors:
res1.p<- resid(PA1, type= "pearson")

par(mfrow= c(2, 2))
plot(res1.p ~ dat$tideangle_deg, col= dat$presence + 1)

plot(res1.p ~ dat$mh, col= dat$presence + 1)

plot(res1.p ~ dat$julianday, col= dat$presence + 1)

# Can't see anything useful.

# Use arm if you can:
library(arm)
par(mfrow= c(2, 2))
binnedplot(x= dat$tideangle_deg, y= res1.p, xlab= "Tide angle", nclass= 100)
binnedplot(x= dat$mh, y= res1.p, xlab= "hour")
binnedplot(x= dat$julianday, y= res1.p, xlab= "Day of the year", nclass= 100)


## ----Q6b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
# clearly some unwanted patterns, especially in mh and julianday
# but possibly in tide angle, too
# all pointing at non-linear effects of the predictors on the response

# This is rather expected indeed: for example, it wouldn't make biological 
# sense for the probability of presence to go up from 00:01 am to 23:59 pm,
# and then drop suddently after midnight to start low again at
# 00:01 the next day. Dolphins don't evaporate at midnight.
# The same reasoning applies for the other cycles, tide and season



## ----Q6c, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
par(mfrow= c(2, 2))
# plot the residuals against tideangle_deg
plot(res1.p ~ dat$tideangle_deg, col= dat$presence + 1)
# get the mean of the residuals for each 1 degree bin of tideangle_deg
tide.means<- tapply(res1.p, list(dat$tideangle_deg), mean)
# convert ordered bin labels into numbers (1 to 360)
tide.vals<- as.numeric(names(tide.means))
# plot residual means against bin number
lines(tide.means ~ tide.vals, col= 3)
# add horizontal line at y= 0 for reference
abline(h= 0, lty= 3, col= grey(0.5))

# same idea for hour of the day:
plot(res1.p ~ dat$mh, col= dat$presence + 1)
hour.means<- tapply(res1.p, list(dat$mh), mean)
lines(hour.means ~ as.numeric(names(hour.means)), col= 3)
abline(h= 0, lty= 3, col= grey(0.5))

# same for julianday:
plot(res1.p ~ dat$julianday, col= dat$presence + 1)
day.means<- tapply(res1.p, list(dat$julianday), mean)
lines(day.means ~ as.numeric(names(day.means)), col= 3)
abline(h= 0, lty= 3, col= grey(0.5))

# Same story.


## ----Q7a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
# Please take the time to think before unfolding the next code chunk



## ----Q7b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------
# The issue is that the effects of these predictors are not linear
# on the logit (link) scale.

# There are several ways the non-linearity could be addressed. 
# one of the most straightforward with glm() is to discretize
# continuous predictors into bins and to treat them as factors.
# In this way, a mean is estimated per category of the variable,
# and no assumption is made about the shape of the relationship.

# Each of the predictors we started with already has one or more 
# categorical counterpart in the data set.
# I suggest you try fTide4 + fMonth + Time6, with fTide4 and
# fMonth being the factor version of Tide4 and mon (both need creating).



## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------
# convert numerically coded categorical variables into factors:
dat$fTide4<- factor(dat$Tide4)
dat$fMonth<- factor(dat$mon)

PA10<- glm(presence ~ fTide4 + fMonth + Time6, family= binomial, data= dat)

drop1(PA10, test= "Chisq")
# all terms significant; nothing to drop

# out of interest, the total proportion of deviance explained is 
(PA10$null.deviance - PA10$deviance) / PA10$null.deviance # 7%


## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
# plot against predictors:
res10.p<- resid(PA10, type= "pearson")

boxplot(res10.p ~ dat$fMonth, xlab= "month")
# boxplot is not a great way to diagnose issues, due to the odd distribution of residuals in the logistic regression

library(arm)
par(mfrow= c(2, 2))
binnedplot(x= dat$tideangle_deg, y=  res10.p, xlab= "Tide angle", nclass= 100)
# okay
binnedplot(x= dat$mh, y=  res10.p, xlab= "hour")
# okay
binnedplot(x= dat$julianday, y=  res10.p, xlab= "Day of the year", nclass= 100)
# julianday is not strictly a predictor in the model, 
# but there is no certainty that month is a good way
# to describe temporal variation (a calendar month is
# quite an arbitrary from a dolphin's point of view). 
# Plotting against julian day allows to check this further.
# All good so far

# We can also check if there might be more complex patterns not accounted 
# for by the model, for example interactions between the variables.
# Check seasonal variation in diel pattern again ("time by season" interaction):
matplot(tapply(res10.p, list(dat$mon, dat$Time6), mean), type= "l", 
        xlab= "month", ylab= "proportion of hours present", lty= 1)
# still residual variation in diel pattern in later part of the year
# (not surprising as there is nothing in the model aiming at capturing this)



## ----Q10b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE-------------
PA10.dat4pred<- data.frame(Time6= levels(dat$Time6),
                                fMonth= "10", fTide4= "1")

PA10.pred<- predict(PA10, PA10.dat4pred, type= "link", se.fit= T)

# Convert predictions to the response (probability) scale.
# And add them to the prediction data frame (that bit is optional)
PA10.dat4pred$fit.resp<- exp(PA10.pred$fit)/(1+exp(PA10.pred$fit)) 
# or plogis(PA10.pred$fit)

# lower 95% CI
PA10.dat4pred$LCI<- plogis(PA10.pred$fit - 1.96*PA10.pred$se.fit)
# upper 95% CI
PA10.dat4pred$UCI<- plogis(PA10.pred$fit + 1.96*PA10.pred$se.fit)


## ----Q10c, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=5----
par(mfrow= c(1, 1))
plot(x= 1:6, y= PA10.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Section of day",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for time of day\n(assuming Tide = 1 and Month = 10)")

arrows(x0= 1:6, x1= 1:6,
          y0= PA10.dat4pred$LCI, y1= PA10.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)



## ----Q11, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
par(mfrow= c(2, 2)) # we will need 3 plots

# repeat plotting of predictions for Time6
PA10.dat4pred<- data.frame(Time6= levels(dat$Time6),
                                fMonth= "10", fTide4= "1")

PA10.pred<- predict(PA10, PA10.dat4pred, type= "link", se.fit= T)

PA10.dat4pred$fit.resp<- plogis(PA10.pred$fit) 
# lower 95% CI
PA10.dat4pred$LCI<- plogis(PA10.pred$fit - 1.96*PA10.pred$se.fit)
# upper 95% CI
PA10.dat4pred$UCI<- plogis(PA10.pred$fit + 1.96*PA10.pred$se.fit)

plot(x= 1:6, y= PA10.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Section of day",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for time of day\n(assuming Tide = 1 and Month = 10)",
      xaxt= "n") # supress automatic x axis (we will draw our own improved axis)

arrows(x0= 1:6, x1= 1:6,
          y0= PA10.dat4pred$LCI, y1= PA10.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)

axis(side= 1, at= 1:6, label= levels(dat$Time6))

# plotting of predictions for fMonth
PA10.dat4pred<- data.frame(fMonth= levels(dat$fMonth),
                              Time6 = "PM2", fTide4= "1")

PA10.pred<- predict(PA10, PA10.dat4pred, type= "link", se.fit= T)

PA10.dat4pred$fit.resp<- plogis(PA10.pred$fit) 
# lower 95% CI
PA10.dat4pred$LCI<- plogis(PA10.pred$fit - 1.96*PA10.pred$se.fit)
# upper 95% CI
PA10.dat4pred$UCI<- plogis(PA10.pred$fit + 1.96*PA10.pred$se.fit)

plot(x= 1:12, PA10.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Month",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions per month\n(assuming Time = PM2 and Tide = 1)")

arrows(x0= 1:12, x1= 1:12,
          y0= PA10.dat4pred$LCI, y1= PA10.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)

# plotting of predictions for fTide4
PA10.dat4pred<- data.frame(fTide4= levels(dat$fTide4),
                              Time6 = "PM2", fMonth= "10")

PA10.pred<- predict(PA10, PA10.dat4pred, type= "link", se.fit= T)

PA10.dat4pred$fit.resp<- plogis(PA10.pred$fit) 
# lower 95% CI
PA10.dat4pred$LCI<- plogis(PA10.pred$fit - 1.96*PA10.pred$se.fit)
# upper 95% CI
PA10.dat4pred$UCI<- plogis(PA10.pred$fit + 1.96*PA10.pred$se.fit)

plot(1:4, PA10.dat4pred$fit.resp, 
      pch= 16, cex= 1.4, xlab= "Tidal phase",
		  ylab= "Fitted probability", ylim= c(0, 1),
      main= "Predictions for tide\n(assuming Time = PM2 and Month = 10)",
      xaxt= "n") # supress x axis (we will draw our own)

axis(side= 1, at= 1:4, label= levels(dat$fTide4))

arrows(x0= 1:4, x1= 1:4,
          y0= PA10.dat4pred$LCI, y1= PA10.dat4pred$UCI,
          length= 0.02, angle= 90, code= 3)





## ----Q13, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------
PA20<- glm(presence ~ fTide4 * fMonth + fTide4 * Time6 + fMonth * Time6, family= binomial, data= dat)

summary(PA20) # inspect the model coefficients
anova(PA20, test= "Chisq") # gauge significance of predictors
# all terms significant; nothing to drop




## ----Q14a, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
# Because I don't have specific predictions here, and I suspect the presence
# of dolphins at Sutors may respond to all combinations of the 3 environmental
# factors we have, I'll go for a quite systematic exploration here (you may
# have a different logic, knowledge about dolphins, more specific predictions, 
# and a more restricted list of models you consider plausible: that would be
# entirely fine)

# remove interactions one-at-a-time
PA21<- glm(presence ~ fMonth + fTide4 + Time6 + fMonth:fTide4 + fMonth:Time6, family= binomial, data= dat)
PA22<- glm(presence ~ fMonth + fTide4 + Time6 + fMonth:Time6 + fTide4:Time6, family= binomial, data= dat)
PA23<- glm(presence ~ fMonth + fTide4 + Time6 + fMonth:fTide4 + fTide4:Time6, family= binomial, data= dat)
# remove interactions two-at-a-time
PA24<- glm(presence ~ fMonth + fTide4 + Time6 + fMonth:Time6, family= binomial, data= dat)
PA25<- glm(presence ~ fMonth + fTide4 + Time6 + fTide4:Time6, family= binomial, data= dat)
PA26<- glm(presence ~ fMonth + fTide4 + Time6 + fMonth:fTide4, family= binomial, data= dat)
# remove all interactions
PA27<- glm(presence ~ fMonth + fTide4 + Time6, family= binomial, data= dat)

AIC(PA20)
AIC(PA21)
AIC(PA22)
AIC(PA23)
AIC(PA24)
AIC(PA25)
AIC(PA26)
AIC(PA27)

# PA24 seems best so far

# remove the main effect not involved in the interaction of PA24 (fTide4)
PA28<- glm(presence ~ fMonth + Time6 + fMonth:Time6, family= binomial, data= dat)
AIC(PA28)

# we could simplify further, but it is unlikely that simpler models 
# would be supported based on what we know, so I'll stop here

# Constructing a table 
# (you can do this with pen and paper, or in R for example like this)
ModelStructure<- c(
  "fTide4 * fMonth + fTide4 * Time6 + fMonth * Time6",     
  "fMonth + fTide4 + Time6 + fMonth:fTide4 + fMonth:Time6",
  "fMonth + fTide4 + Time6 + fMonth:Time6 + fTide4:Time6" ,
  "fMonth + fTide4 + Time6 + fMonth:fTide4 + fTide4:Time6",
  "fMonth + fTide4 + Time6 + fMonth:Time6",
  "fMonth + fTide4 + Time6 + fTide4:Time6",
  "fMonth + fTide4 + Time6 + fMonth:fTide4",
  "fMonth + fTide4 + Time6",
  "fMonth + Time6 + fMonth:Time6")

AICval<- c(AIC(PA20),
            AIC(PA21),
            AIC(PA22),
            AIC(PA23),
            AIC(PA24),
            AIC(PA25),
            AIC(PA26),
            AIC(PA27),
            AIC(PA28))

ModelName<- c("PA20",
            "PA21",
            "PA22",
            "PA23",
            "PA24",
            "PA25",
            "PA26",
            "PA27",
            "PA28")

# combine models and AIC values in a table
ModSelTab<- data.frame(model= ModelName, structure= ModelStructure, AIC= AICval)

# sort the table by increasing AIC value
ModSelTab<- ModSelTab[order(ModSelTab$AIC), ]

# compute AIC differences with best model, and round to 2 decimals
ModSelTab$delta<- round(ModSelTab$AIC - ModSelTab$AIC[1], 2)

# compute AIC weight, and round to 2 decimals
ModSelTab$weight<- round(exp(-ModSelTab$delta / 2) / sum(exp(-ModSelTab$delta / 2)), 2)


## ----Q14b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE--------
require(knitr)
kable(ModSelTab)

# Model PA24 is far ahead, and attracts almost all the AIC weight, accordingly.

# Depending on the set of models you decided to compare, 
# your best model and AIC weights may vary from mine.



## ----Q15, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, fig.width=8----
res24.p<- resid(PA24, type= "pearson")

library(arm)
par(mfrow= c(2, 2))
binnedplot(x= dat$tideangle_deg, y=  res24.p, xlab= "Tide angle", nclass= 100)
# okay
binnedplot(x= dat$mh, y=  res24.p, xlab= "hour")
# okay
binnedplot(x= dat$julianday, y=  res24.p, xlab= "Day of the year", nclass= 100)
# okay
# Check seasonal variation in diel pattern again ("time by season" interaction):
matplot(tapply(res24.p, list(dat$mon, dat$Time6), mean), type= "l", 
        xlab= "month", ylab= "proportion of hours present", lty= 1)
# Residual variation is tiny (check y-axis and compare with Question 9); 
# no consistent pattern of variation. Pretty good!

# There are few assumptions for the Bernoulli distribution other than 
# observations being zeros and ones.
# Some assumptions valid for all models still apply here, such as: model 
# correctly specified; independent
# residuals. The latter is violated in this data set, due to consecutive 
# measurements in time. This issue
# is explored in the linked paper, using mixed models for non-independent data 
# (covered in the course BI5302). 
# The paper also uses GAMs for avoiding the discretization of 
# continuous variables, and
# accounting for the cyclicity of the preditors (estimates at each end should 
# match, e.g. 31st Dec-1st Jan, or 23:59 - 00:00)



## ----Q16, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.show= ifelse(SOLUTIONS, "asis", "hide"), fig.height=8, , fig.width=8----

# Model PA24 assumes that dolphin presence (actually, probability of recording) 
# changes according to time of year, time of day and tidal stage, and that the
# effect of time of day changes with the time of year (fMonth:Time6).

summary(PA24)
# This is tricky to interpret due to the number of coefficients (75), 
# and the presence of interactions. Best to use plots!

# Let's illustrate the differences in predictions for two different months, 
# for example June and December (this could be done for each of the 12 months)

par(mfrow= c(1, 2))
# June
dat4pred<- expand.grid(Time6= levels(dat$Time6),
                fMonth= "6",
								fTide4= "1")
PA24.pred<- predict(PA24, dat4pred, type= "link", se.fit= T)
dat4pred$fit.resp<- plogis(PA24.pred$fit)
dat4pred$LCI<- plogis(PA24.pred$fit - 1.96*PA24.pred$se.fit)
dat4pred$UCI<- plogis(PA24.pred$fit + 1.96*PA24.pred$se.fit)
plot(as.numeric(dat4pred$Time6), dat4pred$fit.resp, pch= 16, 
	  cex= 1.4, main= paste("Month =", 6),
    col= 1, xlab= "Section of day",
	  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))
arrows(x0= as.numeric(dat4pred$Time6), x1= as.numeric(dat4pred$Time6),
    y0= dat4pred$LCI, y1= dat4pred$UCI,
	  col= grey(0.5), length= 0.02, angle= 90, code= 3)

# December
dat4pred<- expand.grid(Time6= levels(dat$Time6),
                fMonth= "12",
								fTide4= "1")
PA24.pred<- predict(PA24, dat4pred, type= "link", se.fit= T)
dat4pred$fit.resp<- plogis(PA24.pred$fit)
dat4pred$LCI<- plogis(PA24.pred$fit - 1.96*PA24.pred$se.fit)
dat4pred$UCI<- plogis(PA24.pred$fit + 1.96*PA24.pred$se.fit)
plot(as.numeric(dat4pred$Time6), dat4pred$fit.resp, pch= 16, 
	  cex= 1.4, main= paste("Month =", 12),
    col= 1, xlab= "Section of day",
	  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))
arrows(x0= as.numeric(dat4pred$Time6), x1= as.numeric(dat4pred$Time6),
    y0= dat4pred$LCI, y1= dat4pred$UCI,
	  col= grey(0.5), length= 0.02, angle= 90, code= 3)

# In June, dolphins seem to have a similar probability of being recorded
# at any time of the day and night, whereas in December they are mostly
# using the site by night (ca. 40% chances of being present against 
# only ca. 10% in the middle of the day)

# (FOR THE R GEEKS OUT THERE)
# For plots of predictions for all 12 months, it is possible to automate
# the process using a loop:
par(mfrow= c(3, 4))
for(month in 1:12){ 
  # at the first iteration, the variable `month` will take value 1, then
  # at the second iteration, the variable `month` will take value 2, 
  # etc... until iteraction 12
	dat4pred<- expand.grid(Time6= levels(dat$Time6),
                                fMonth= as.character(month),
								fTide4= "1")
	PA24.pred<- predict(PA24, dat4pred, type= "link", se.fit= T)
	dat4pred$fit.resp<- plogis(PA24.pred$fit)
	dat4pred$LCI<- plogis(PA24.pred$fit - 1.96*PA24.pred$se.fit)
	dat4pred$UCI<- plogis(PA24.pred$fit + 1.96*PA24.pred$se.fit)
	plot(as.numeric(dat4pred$Time6), dat4pred$fit.resp, pch= 16, 
		  cex= 1.4, main= paste("Month =", month),
          col= 1, xlab= "Section of day",
		  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))
	arrows(x0= as.numeric(dat4pred$Time6), x1= as.numeric(dat4pred$Time6),
          y0= dat4pred$LCI, y1= dat4pred$UCI,
		  col= grey(0.5), length= 0.02, angle= 90, code= 3)
} # end of one iteration, start of the next

# According to the better supported model, they tend to use the site
# more in May, June and 
# July day and night, visit mostly by night from October to December,
# and seldom from Jan to March.

# Of note is the low proportion of deviance explained by the model,
# despite its complexity (75 parameters):
(PA24$null.deviance - PA24$deviance) /
	PA24$null.deviance 
# 10%. This is quite normal with Bernoulli data.


