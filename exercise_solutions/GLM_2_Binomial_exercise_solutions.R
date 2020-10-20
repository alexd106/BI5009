## ----Q2, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE------------------------------------------------------

dat<- read.csv("./data/dolphin.csv", stringsAsFactors= T)

# re-ordering factor levels for convenience:
dat$Per2<- factor(dat$Per2, levels= c("RestOfYear", "MayJun"))
# (making "RestOfYear" the reference level)
dat$Per4<- factor(dat$Per4, levels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3"))
dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2")) # reordering chronologically

str(dat)


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------
## # count observations per year/month combination and represent as mosaicplot
## plot(table(dat$year, dat$mon))
## # CPOD failure in Feb-April 2012 and Dec 2012-March 2013
## 
## plot(table(dat$mh))
## # fairly even representation of hours
## # (that's on the random sample; Almost perfectly balanced on the full dataset)
## 
## plot(table(dat$Tide4, dat$mh))
## # even representation of tides
## # time of day and tidal phase not independent (but not a linear correlation)
## 
## # presence in relation to time of day
## plot(tapply(dat$presence, list(dat$mh), mean), type= "l", ylim= c(0, 1), xlab= "time of day", ylab= "proportion of hours present")
## 
## # are seasonal patterns similar between years?
## matplot(tapply(dat$presence, list(dat$mon, dat$year), mean), type= "l", ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present")
## 
## # Presence in relation to tide
## plot(tapply(dat$presence, list(dat$Tide4), mean), type= "b", ylim= c(0, 1), xlab= "tidal phase", ylab= "proportion of hours present")
## 
## matplot(tapply(dat$presence, list(dat$mon, dat$Tide4), mean), type= "l", ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present", lty= 1)
## # no change in pattern of tide use across seasons
## 
## # Seasonal variation in diel pattern
## matplot(tapply(dat$presence, list(dat$mon, dat$Time6), mean), type= "l", ylim= c(0, 1), xlab= "month", ylab= "proportion of hours present", lty= 1)
## # stronger diel pattern in later part of the year
## 
## # Variation in diel pattern between May-June (red) and the rest of the year
## matplot(tapply(dat$presence, list(dat$mh, dat$Per2), mean), type= "l", ylim= c(0, 1), xlab= "time of day", ylab= "proportion of hours present", lty= 1)
## # less nocturnal in spring?
## 
## # with more categories in spring
## matplot(tapply(dat$presence, list(dat$mh, dat$Per4), mean), type= "l", ylim= c(0, 1), xlab= "time of day", ylab= "proportion of hours present", lty= 1)
## # no obvious systematic difference between the 3 portions of May-June
## 


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
PA1<- glm(presence ~ tideangle_deg * mh * julianday, family= binomial, data= dat)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
summary(PA1)

# Model description:
# presence_i ~ Bernoulli(p_i)  or presence_i ~ Binomial(N= 1, p_i)
# log(p_i / (1-p_i)) = 
#       -1.36*(Intercept) + 0.000108*tideangle_deg - 0.0012*mh 
#       + 0.0031*julianday - 4.95e-05*tideangle_deg*mh 
#       - 8.90e-06*tideangle_deg*julianday - 7.96e-05*mh*julianday 
#       + 7.64e-07*tideangle_deg*mh*julianday

# "(Intercept)" general intercept
# "tideangle_deg" main effect of tide angle
# "mh" main effect of time of day
# "julianday" main effect of day of year
# "tideangle_deg:mh" does effect of tide angle change with time of day?
# "tideangle_deg:julianday" does effect of tide angle change with day of year?
# "mh:julianday" does effect of time of day change with day of year?
# "tideangle_deg:mh:julianday" does the change in the effect of (eg) tide angle
#       with time of day change across seasons? 
#       (you can permute all the 3 terms as you please in the sentence)



## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
drop1(PA1, test= "Chisq")
# drop the triple interaction

PA2<- update(PA1, . ~ . - tideangle_deg:mh:julianday)
drop1(PA2, test= "Chisq")
# drop tideangle_deg:julianday

PA3<- update(PA2, . ~ . - tideangle_deg:julianday)
drop1(PA3, test= "Chisq")
# drop mh:julianday

PA4<- update(PA3, . ~ . - mh:julianday)
drop1(PA4, test= "Chisq")
# nothing left to drop.

summary(PA4)


## ----Q7a, eval=SOLUTIONS, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------------------------------------
## library(car)
## vif(PA4)
## # High for terms involved in the interaction, as expected. No concern.
## 
## par(mfrow= c(2, 2))
## plot(PA4, col= dat$presence + 1) # red is presence, black is absence
## # Not very useful statistical art. Not worth framing either.
## 
## # plot against predictors:
## res4.p<- resid(PA4, type= "pearson")
## 
## par(mfrow= c(2, 2))
## plot(res4.p ~ dat$tideangle_deg, col= dat$presence + 1)
## 
## plot(res4.p ~ dat$mh, col= dat$presence + 1)
## 
## plot(res4.p ~ dat$julianday, col= dat$presence + 1)
## 
## # Can't see anything useful.
## 
## # Use arm if you can:
## library(arm)
## par(mfrow= c(2, 2))
## binnedplot(x= dat$tideangle_deg, y= res4.p, xlab= "Tide angle", nclass= 100)
## binnedplot(x= dat$mh, y= res4.p, xlab= "hour")
## binnedplot(x= dat$julianday, y= res4.p, xlab= "Day of the year", nclass= 100)


## ----Q7b, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
## # clearly some unwanted patterns, especially in mh and julianday
## # but possibly in tide angle, too
## # all pointing at non-linear effects of the predictors on the response
## 


## ----Q7c, eval=SOLUTIONS, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------------------------------------
## par(mfrow= c(2, 2))
## plot(res4.p ~ dat$tideangle_deg, col= dat$presence + 1)
## tide.means<- tapply(res4.p, list(dat$tideangle_deg), mean)
## tide.vals<- as.numeric(names(tide.means))
## lines(tide.means ~ tide.vals, col= 3)
## abline(h= 0, lty= 3, col= grey(0.5))
## 
## plot(res4.p ~ dat$mh, col= dat$presence + 1)
## hour.means<- tapply(res4.p, list(dat$mh), mean)
## lines(hour.means ~ as.numeric(names(hour.means)), col= 3)
## abline(h= 0, lty= 3, col= grey(0.5))
## 
## plot(res4.p ~ dat$julianday, col= dat$presence + 1)
## day.means<- tapply(res4.p, list(dat$julianday), mean)
## lines(day.means ~ as.numeric(names(day.means)), col= 3)
## abline(h= 0, lty= 3, col= grey(0.5))
## 
## # Same story.


## ----Q8, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE--------------------------------------------------
# there are several ways the non-linearity could be addressed. 
# one of the most straightforward with glm() is to discretize
# continuous predictors into bins and to treat them as factors.

# Each of the predictors we started with already has one or more 
# categorical counterpart in the data set.
# I suggest you try Tide4 * Per2 * Time6
# You can choose something else or cut your own predictors, too.



## ----Q9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------------
# convert numerically coded categorical variables into factors:
dat$fTide4<- factor(dat$Tide4)

PA10<- glm(presence ~ fTide4 * Per2 * Time6, family= binomial, data= dat)

drop1(PA10, test= "Chisq")
# drop the triple interaction Tide4:Per2:Time6

PA11<- glm(presence ~ fTide4 * Per2 + Per2 * Time6 + fTide4:Time6, family= binomial, data= dat)

drop1(PA11, test= "Chisq")
# drop fTide4:Time6

PA12<- glm(presence ~ fTide4 * Per2 + Per2 * Time6, family= binomial, data= dat)

drop1(PA12, test= "Chisq")
# drop fTide4:Per2

PA13<- glm(presence ~ fTide4 + Per2 * Time6, family= binomial, data= dat)

drop1(PA13, test= "Chisq")
# nothing else to drop

summary(PA13)
anova(PA13, test= "Chisq")
# fTide4 contributes minimally

# Total proportion of deviance explained is 
(PA13$null.deviance - PA13$deviance) / PA13$null.deviance # 3%


## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
## # plot against predictors:
##  res13.p<- resid(PA13, type= "pearson")
## 
## library(arm)
## par(mfrow= c(2, 2))
## binnedplot(x= dat$tideangle_deg, y=  res13.p, xlab= "Tide angle", nclass= 100)
## # okay
## binnedplot(x= dat$mh, y=  res13.p, xlab= "hour")
## # okay
## binnedplot(x= dat$julianday, y=  res13.p, xlab= "Day of the year", nclass= 100)
## # less than good (model only allows for difference between May-June
## # and rest of the year)
## 
## # Check seasonal variation in diel pattern again ("time by season" interaction):
## matplot(tapply(res13.p, list(dat$mon, dat$Time6), mean), type= "l",
##         xlab= "month", ylab= "proportion of hours present", lty= 1)
## # still residual variation in diel pattern in later part of the year
## # (not surprising as there is nothing in the model aiming at capturing this)
## 
## # residuals suggest a finer binning of time of the year
## # is required for the predictors. Or an approach that circumvents the issues
## # with binning (see reference at the end for an alternative approach).
## 


## ----Q11a, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE------------------------------------------------
PA13.dat4pred<- expand.grid(Time6= levels(dat$Time6),
                                Per2= levels(dat$Per2),
								fTide4= "1")


## ----Q11b, eval=TRUE, echo=TRUE, results=SOLUTIONS, collapse=TRUE------------------------------------------------
PA13.pred<- predict(PA13, PA13.dat4pred, type= "link", se.fit= T)

PA13.dat4pred$fit.resp<- exp(PA13.pred$fit)/(1+exp(PA13.pred$fit)) 
# or plogis(PA13.pred$fit)

# lower 95% CI
PA13.dat4pred$LCI<- plogis(PA13.pred$fit - 1.96*PA13.pred$se.fit)
# upper 95% CI
PA13.dat4pred$UCI<- plogis(PA13.pred$fit + 1.96*PA13.pred$se.fit)


## ----Q11c, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=5------------------------
## par(mfrow= c(1, 1))
## plot(as.numeric(PA13.dat4pred$Time6), PA13.dat4pred$fit.resp, pch= 16, cex= 1.4,
##           col= PA13.dat4pred$Per2, xlab= "Section of day",
## 		  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))
## 
## arrows(x0= as.numeric(PA13.dat4pred$Time6), x1= as.numeric(PA13.dat4pred$Time6),
##           y0= PA13.dat4pred$LCI, y1= PA13.dat4pred$UCI,
## 		  col= PA13.dat4pred$Per2, length= 0.02, angle= 90, code= 3)


## ----Q12, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE---------------------------------------
## PA10.MAM.stepAIC<- step(PA10)
## 
## anova(PA10.MAM.stepAIC, test= "Chisq")
## # same model as PA13
## 
## # convert 'mon' into factor
## dat$fMonth<- factor(dat$mon)
## # fit new model
## PA20<- glm(presence ~ fTide4 * fMonth * Time6, family= binomial, data= dat)
## 
## PA20.MAM.stepAIC<- step(PA20)
## 
## anova(PA20.MAM.stepAIC, test= "Chisq")
## # Same structure selected: tide, plus season by time of day
## AIC(PA10.MAM.stepAIC)
## AIC(PA20.MAM.stepAIC)
## # Monthly model vastly favoured despite the 60 extra parameters
## 
## anova(PA10.MAM.stepAIC, PA20.MAM.stepAIC, test= "Chisq")
## # also clearly favoured by likelihood ratio test
## 
## # residual analysis:
## res20.d<- resid(PA20.MAM.stepAIC, type= "pearson")
## library(arm)
## par(mfrow= c(2, 2))
## binnedplot(x= dat$tideangle_deg, y=   res20.d, xlab= "Tide angle", nclass= 100)
## binnedplot(x= dat$mh, y=   res20.d, xlab= "hour")
## binnedplot(x= dat$julianday, y=   res20.d, xlab= "Day of the year", nclass= 100)
## # Check seasonal variation in diel pattern again ("time by season" interaction):
## matplot(tapply( res20.d, list(dat$mon, dat$Time6), mean), type= "l", xlab= "month", ylab= "proportion of hours present", lty= 1)
## # much improved
## 


## ----Q13, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8, , fig.width=8----------
## # dolphins have a weak but apparently stable preference for certain tidal states in Sutors.
## 
## # According to model PA13, they are more likely to be seen during the day in
## # May/June than in other months where they are more nocturnal (more detail
## # could be obtained by plotting the predicted hour of day effect per month
## # from PA20.MAM.stepAIC), like this:
## 
## par(mfrow= c(3, 4))
## for(month in 1:12){
## 	dat4pred<- expand.grid(Time6= levels(dat$Time6),
##                                 fMonth= as.character(month),
## 								fTide4= "1")
## 	PA20.pred<- predict(PA20.MAM.stepAIC, dat4pred, type= "link", se.fit= T)
## 	dat4pred$fit.resp<- plogis(PA20.pred$fit)
## 	dat4pred$LCI<- plogis(PA20.pred$fit - 1.96*PA20.pred$se.fit)
## 	dat4pred$UCI<- plogis(PA20.pred$fit + 1.96*PA20.pred$se.fit)
## 	plot(as.numeric(dat4pred$Time6), dat4pred$fit.resp, pch= 16,
## 		  cex= 1.4, main= paste("Month =", month),
##           col= 1, xlab= "Section of day",
## 		  ylab= "Fitted probability (assuming Tide = 1)", ylim= c(0, 1))
## 	arrows(x0= as.numeric(dat4pred$Time6), x1= as.numeric(dat4pred$Time6),
##           y0= dat4pred$LCI, y1= dat4pred$UCI,
## 		  col= grey(0.5), length= 0.02, angle= 90, code= 3)
## }
## 
## # According to the better supported model, they tend to use the site more in May, June and July day and night, visit mostly by night from October to December, and seldom from Jan to March.
## 
## # There are few assumptions for the Bernoulli distribution other than observations being zeros and ones.
## # Some assumptions valid for all models still apply here, such as: model correctly specified; independent residuals. The latter is violated in this data set due to consecutive measurements in time. This issue is explored in the linked paper, using mixed models for non-independent data (covered in course BI5302). The paper also uses GAMs for avoiding the discretization of continuous variables, and accounting for the cyclicity of the preditors (estimates at each end should match, e.g. 31st Dec-1st Jan, or 23:59 - 00:00)
## 
## # Of note is the low proportion of deviance explained by the model,
## # despite its complexity (75 parameters):
## (PA20.MAM.stepAIC$null.deviance - PA20.MAM.stepAIC$deviance) / PA20.MAM.stepAIC$null.deviance
## # 10%. This is quite normal with Bernoulli data.
## 


## ----Appendix, eval=FALSE, echo=TRUE, results=FALSE, collapse=FALSE----------------------------------------------
## fulldat<- read.delim("./data/FineScale_Dataset_GAMM_OFB2019.txt")
## 
## str(fulldat)
## 
## dat<- fulldat[fulldat$site == "Sutors", c("presence", "year", "julianday", "tideangle_deg", "mh")]
## 
## dat$mon<- as.numeric(cut(dat$julianday, seq(1, 370, by= 30.5)))
## 
## dat$tideangle_deg<- round(dat$tideangle_deg)
## # count number of data per year/month combination and represent as mosaicplot
## plot(table(dat$year, dat$mon))
## 
## # remove 2016
## dat<- dat[dat$year != 2016, ]
## 
## # Bin year into two periods (May+June vs rest of year)
## dat$Per2<- cut(dat$julianday, breaks= c(-1, 120, 180, 400),
## 			labels= c("RestOfYear", "MayJun", "RestOfYear"))
## dat$Per2<- factor(dat$Per2, levels= c("RestOfYear", "MayJun"))
## # (making "RestOfYear" the reference level)
## 
## # check this is working as intended:
## plot(as.numeric(dat$Per2) ~ dat$julianday)
## 
## # Bin year into 4 periods:
## # 3 periods of 20 days from early May to end of June vs rest of the year
## dat$Per4<- cut(dat$julianday, breaks= c(0, 120, 140, 160, 180, 400),
## 			labels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3", "RestOfYear"))
## dat$Per4<- factor(dat$Per4, levels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3"))
## # (reordering levels)
## 
## # check this is working as intended:
## plot(as.numeric(dat$Per4) ~ dat$julianday)
## 
## # Bin time of day into 6 4h periods (first centered on midnight)
## dat$Time6<- cut(dat$mh, breaks= c(-1, seq(2, 22, by= 4), 24),
## 			labels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2", "MNight"))
## dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2")) # reordering chronologically
## 
## # check this is working as intended:
## table(dat$Time6, dat$mh)
## 
## # Bin tide angle into 4 quadrants with peaks in middle of respective bin
## dat$Tide4<- cut(dat$tideangle_deg, breaks= c(-1, 45, 135, 225, 315, 360),
## 			labels= c(1:4, 1))
## 
## # check this is working as intended:
## plot(as.numeric(dat$Tide4) ~ dat$tideangle_deg)
## 
## # unless you desperately want to test the performance of your computer,
## # play safe and reduce the size of the data set from 50000 to 5000:
## set.seed(74) # makes the random sampling reproducible
## # This means you will get the same random sample as the solutions to
## # the exercises and the same results.
## dat<- dat[sample(1:nrow(dat), size= 5000), ] # random subset or rows
## 
## write.csv(dat, "dolphin.csv")

