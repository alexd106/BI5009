## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------
sp<- read.table(file= "./data/species.txt", header= TRUE)
str(sp)

sp$logSp<- log(sp$Species)
sp$pH<- factor(sp$pH, levels= c("low", "mid", "high"))

VOI<- c("logSp", "Biomass", "pH")
pairs(sp[, VOI])
# Biomass tends to increase with pH, which could generate
# some collinearity between these explanatory variables in a model.
# but still plenty of variation in Biomass within each pH, 
# so hopefully this won't be an issue.

coplot(logSp ~ Biomass | pH, data= sp)
# the relationships look clean and well distinct between treatments, 
# supporting the idea of an interaction.
# looking promising.


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------
sp.glm1<- glm(Species ~ Biomass, family= poisson, data= sp)


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------
summary(sp.glm1)

# Model description:
# Species_i ~ Poisson(mu_i)
# log(mu_i) = 3.18 - 0.064*Biomass_i



## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------
# On the link scale:
3.18 - 0.064*5 # 2.86
# On the response scale (species count):
exp(3.18 - 0.064*5) # 17.46


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------------
sp.glm2<- glm(Species ~ Biomass * pH, family= poisson, data= sp)


## ----Q7-9, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------
summary(sp.glm2)
anova(sp.glm2, test= "Chisq")

# anova is useful for getting an idea of the relative contributions
# of each term, but beware that order does matter for all but the last term!

# drop1 would provide the same answer in this case, since there are
# no other interactions involved (and interactions are always last). 
# In general this would be the method of choice if our purpose is 
# to perform model simplification.

drop1(sp.glm2, test= "Chisq")

# the ANOVA table provides a global test across pH levels. 
# However the significance of the interactions is also unambiguous
# from the summary table, in this case, although the null hypothesis
# is different (coefficient different from zero in the latter 
# vs. significant proportion of variation explained in the former).



## ----Q10, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-----------------------------------------------------
library(car)
vif(sp.glm2)

# GVIF is high for all the terms.
# This is expected though, because all terms are part of an interaction. 
# Any change in one coefficient will have a strong impact on the others
# This might be exacerbated by the pre-existing correlation between 
# the predictors. But all components are very clearly needed here.



## ----Q11, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------
## par(mfrow= c(2, 2))
## plot(sp.glm2)
## 
## # nothing particularly bad...
## # the variance of the standardized residuals ("Pearson") tends to decrease
## # slightly, when it should be constant (underdispersion).
## # a few observations poorly predicted (18, 20) and slight
## # overestimation of the smaller values (left end of top-left graph),
## # just to be picky.
## # There is no expectation that the residuals should be normally distributed
## # (thus QQplot not particularly useful)
## 
## # we can produce more residual plots if desired:
## # extract various residuals and plot
## res.p<- resid(sp.glm2, type= "pearson")
## res.d<- resid(sp.glm2, type= "deviance")
## 
## fit<- fitted(sp.glm2) # on the response scale
## 
## plot(res.p, res.d); abline(0,1) # deviance against Std Pearson residuals
## # quite similar in this model (no overdispersion, moderate underdispersion)
## plot(fit, res.d, ylab= "Deviance residuals", xlab= "Fitted values (response scale)")
## abline(h= 0, col= 2, lty= 2)
## 
## # residuals against the predictors:
## plot(sp$pH, res.p, ylab= "Std Pearson residuals", xlab= "pH")
## abline(h= 0, col= 2, lty= 2)
## # low pH plots are more variable
## 
## plot(sp$Biomass, res.p, ylab= "Std Pearson residuals", xlab= "Biomass")
## abline(h= 0, col= 2, lty= 2)
## # Species richness over-predicted for highest and lowest Biomass plots
## # Few cases: could be by chance?
## 


## ----Q12, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8----------------------------------
## par(mfrow= c(1, 1))
## # create a sequence of increasing Biomass
## Biomass.seq<- seq(from= min(sp$Biomass), to= max(sp$Biomass), l= 25)
## # predict for a range of biomass values and a low pH
## MyData1<- data.frame(Biomass= Biomass.seq, pH= "low")
## # predict for a range of biomass values and a mid pH
## MyData2<- data.frame(Biomass= Biomass.seq, pH= "mid")
## # predict for a range of biomass values and a high pH
## MyData3<- data.frame(Biomass= Biomass.seq, pH= "high")
## 
## P.low<- predict(sp.glm2, newdata= MyData1, type= "response")
## P.mid<- predict(sp.glm2, newdata= MyData2, type= "response")
## P.high<- predict(sp.glm2, newdata= MyData3, type= "response")
## 
## plot(sp$Biomass, sp$Species, col= sp$pH, xlab= "Biomass", ylab= "N species")
## # when pH is converted to numeric,
## # you get 1 for "low", 2 for "med" and 3 for "high"
## # color 1 means black in R
## # color 2 means red in R
## # color 3 means green in R
## 
## lines(MyData1$Biomass, P.low, lty= 1, col= 1)
## lines(MyData2$Biomass, P.mid, lty= 1, col= 2)
## lines(MyData3$Biomass, P.high, lty= 1, col= 3)
## 
## legend("topright",
##  legend= c("high", "mid", "low"),
##  col= c(3:1),
##  lty= c(1, 1, 1),
##  lwd= c(1, 1, 1))


## ----Q13, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8----------------------------------
## P.low<- predict(sp.glm2, newdata= MyData1, type= "link")
## P.mid<- predict(sp.glm2, newdata= MyData2, type= "link")
## P.high<- predict(sp.glm2, newdata= MyData3, type= "link")
## 
## plot(sp$Biomass, sp$Species, col= sp$pH, xlab= "Biomass", ylab= "Number of species")
## 
## lines(MyData1$Biomass, P.low, lty= 1, col= 1)
## lines(MyData2$Biomass, P.mid, lty= 1, col= 2)
## lines(MyData3$Biomass, P.high, lty= 1, col= 3)
## # lines are straight and appear in the wrong place!
## # Because they are on the link scale (observations are on the response scale)
## 
## # back-transform the predictions to make them on the response scale
## # (note the 'exp')
## lines(MyData1$Biomass, exp(P.low ), lty= 1, col= 1)
## lines(MyData2$Biomass, exp(P.mid ), lty= 1, col= 2)
## lines(MyData3$Biomass, exp(P.high), lty= 1, col= 3)
## # Better?
## 
## legend("topright",
##  legend= c("high", "mid", "low"),
##  col= c(3:1),
##  lty= c(1, 1, 1),
##  lwd= c(1, 1, 1))


## ----Q14, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8----------------------------------
## P.low<- predict(sp.glm2, newdata= MyData1, type= "link", se.fit= T)
## P.mid<- predict(sp.glm2, newdata= MyData2, type= "link", se.fit= T)
## P.high<- predict(sp.glm2, newdata= MyData3, type= "link", se.fit= T)
## 
## plot(sp$Biomass, sp$Species, col= sp$pH, xlab= "Biomass", ylab= "Number of species")
## 
## # back-transform the predictions to make them on the response scale
## # (note the 'exp')
## lines(MyData1$Biomass, exp(P.low$fit ), lty= 1, col= 1)
## lines(MyData2$Biomass, exp(P.mid$fit ), lty= 1, col= 2)
## lines(MyData3$Biomass, exp(P.high$fit), lty= 1, col= 3)
## 
## # same for the lower bound of the 95% CI
## lines(MyData1$Biomass, exp(P.low$fit  - 1.96*P.low$se.fit ), lty= 3, col= 1)
## lines(MyData2$Biomass, exp(P.mid$fit  - 1.96*P.mid$se.fit ), lty= 3, col= 2)
## lines(MyData3$Biomass, exp(P.high$fit - 1.96*P.high$se.fit), lty= 3, col= 3)
## 
## # now the upper bound of the 95% CI
## lines(MyData1$Biomass, exp(P.low$fit  + 1.96*P.low$se.fit ), lty= 3, col= 1)
## lines(MyData2$Biomass, exp(P.mid$fit  + 1.96*P.mid$se.fit ), lty= 3, col= 2)
## lines(MyData3$Biomass, exp(P.high$fit + 1.96*P.high$se.fit), lty= 3, col= 3)
## 
## legend("topright",
##  legend= c("high", "mid", "low"),
##  col= c(3:1),
##  lty= c(1, 1, 1),
##  lwd= c(1, 1, 1))


## ----Q15, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------------------
## # The data show a very steep decline in species richness towards zero,
## # as biomass increases.
## 
## # The log-link allows to transform the straight lines (on the link scale)
## # into curves (on the response scale), while ensuring that predictions
## # are always positive. But the log link doesn't allow enough flexibility
## # for the decline to happen as fast as it does in the data
## # while remaining positive for any value of biomass.
## 
## # The model (a straight line for each pH) is too simple to capture this
## # level of detail in the data, and hence is biased.
## 
## # However it does a pretty good job overall, and can be trusted to
## # make robust inference about the general trends.
## 
## # Note that the interaction is not obvious on the response scale,
## # as the curves look quite "parallel". It is indeed one of the
## # peculiarities of transformations (like the log), that parallel lines
## # on one scale will not be so on the other scale.
## # Caution is therefore required in interpreting effects from the
## # summary of the model directly, and plotting on the response scale
## # is critical before drawing any conclusion!
## 

