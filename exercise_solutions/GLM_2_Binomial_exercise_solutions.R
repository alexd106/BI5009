## ----Q2, eval=TRUE, echo=TRUE, results=TRUE, collapse=FALSE------------------------------------------------------

dat<- read.csv("./data/dolphin.csv", stringsAsFactors= T)

# re-ordering factor levels for convenience:
dat$Per2<- factor(dat$Per2, levels= c("RestOfYear", "MayJun"))
# (making "RestOfYear" the reference level)
dat$Per4<- factor(dat$Per4, levels= c("RestOfYear", "MayJun1", "MayJun2", "MayJun3"))
dat$Time6<- factor(dat$Time6, levels= c("MNight", "AM1", "AM2", "MDay", "PM1", "PM2")) # reordering chronologically

str(dat)




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

