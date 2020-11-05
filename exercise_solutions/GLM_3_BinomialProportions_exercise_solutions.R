## ----Q1, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
Mites<- read.delim("./data/DrugsMites.txt", 
                   stringsAsFactors = TRUE)
str(Mites)

Mites$fToxic<- factor(Mites$Toxic)
str(Mites)


## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
## # Outliers Y
## boxplot(Mites$Proportion) # not very useful for this data set
## dotchart(Mites$Proportion)
## 
## # Outliers X
## dotchart(Mites$Concentration)
## 
## # Correlations between predictors
## plot(jitter(Mites$Concentration), jitter(Mites$Toxic))
## 
## # Toxics 2 and 3 have not been assayed at the highest concentrations
## # but have had more trials at the lower concentrations
## 
## # Relationships
## boxplot(Proportion ~ Toxic, data= Mites)
## with(Mites, plot(x= Concentration, y= Proportion))
## 
## # Since lots of proportions have the same values, we can also add
## # a bit of noise to the coordinates in the previous plot, to limit overlap:
## with(Mites, plot(x= jitter(Concentration), y= jitter(Proportion)))
## 
## coplot(jitter(Proportion) ~ jitter(Concentration) | fToxic, data= Mites)
## 
## # looks like researchers stopped increasing concentration of a chemical when they had reached ~ 100 % mortality in assays
## 


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
Mites$Living_mites<- Mites$Total - Mites$Dead_mites


## ----Q4, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
M1<- glm(cbind(Living_mites, Dead_mites) ~ Concentration + fToxic + Concentration : fToxic, family= binomial, data= Mites)


## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
summary(M1)

## Mathematical model description:

# Prop_surviving ~ Binomial(Ntot, P) 
# log(P / (1-P)) = 
#         1.43 - 3.80*Concentration 
#         + 0.0085*fToxic2 + 0.014*fToxic3 + 0.27*fToxic4 
#         - 6.18*Concentration:fToxic2
#         - 1.90*Concentration*fToxic3 
#         + 1.48*Concentration*fToxic4

## "biological" hypotheses:

# (Intercept):     the mean (on the logit scale) for Toxic 1 when its
# concentration is 0 (null hypothesis not very useful, however
# the value is quite interesting, when back-transformed on the proportion
# scale: this is the proportion that survive the experiment in the absence
# of toxic chemical. Shouldn't differ between chemicals, presumably?)

# Concentration: slope of Concentration, or change in survival per unit
# increase in Toxic 1 concentration (on logit link), or change in log-odds
# of survival (on proportion scale)

# fToxic2:       difference between intercepts of Toxic 2 and Toxic 1
# fToxic3:       difference between intercepts of Toxic 3 and Toxic 1
# fToxic4:       difference between intercepts of Toxic 4 and Toxic 1

# Concentration:fToxic2  difference between Conc. slopes for Toxic 2 and
# Toxic 1. Negative means a more lethal effect than Toxic 1

# Concentration:fToxic3  difference between Conc. slopes for Toxic 2 and Toxic 1
# Concentration:fToxic4  difference between Conc. slopes for Toxic 2 and Toxic 1


## ----Q6, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------------
# Yes, because this is a binomial variable with more than 1 trial per observation
# Residual deviance: 117.67  on 107  degrees of freedom
# -> negligible overdispersion



## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
## # we don't need to perform model selection here: given that the assay is
## # to compare the effect of different chemicals, the interaction alone is the
## # focus of the experiment. If it is not significant, we would have our answer
## # and the simplified model wouldn't have much use.
## 
## drop1(M1, test= "Chi")
## 
## # interaction is significant: we can reject the null hypothesis that there is
## # no difference between  chemicals.
## 


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE, fig.height=8-----------------
## library(car)
## vif(M1)
## 
## # extremely high VIF values: are they a problem?
## # Not particularly:
## # The cause probably lies with the choice of the researchers to stop increasing
## # the concentration when 100% mortality was reached (or close to), leading to
## # small sample size for high concentrations in the most potent chemicals.
## # Nevertheless their approach was sufficient to get decent estimates of the
## # curves for each treatment, with sufficient precision still to get significant
## # differences. Adding more assays at higher concentrations (which presumably
## # would have yield close to 100% mortality as well), for the sake of getting a
## # more balanced design would make little difference to the estimates.
## 
## par(mfrow= c(3, 2))
## plot(M1)
## 
## # a few complementary plots for info:
## 
## # Pearson residuals
## E1<- resid(M1, type= "pearson")
## F1<- fitted(M1)
## plot(x= F1, y= E1,
##         xlab= "Predicted values (logit scale)", ylab= "Pearson residuals")
## abline(h= 0, col= grey(0.5), lty= 2)
## 
## # The Pearson residuals are quite different from the the raw or deviance residuals in this example,
## # because when predicted values are small (near p=0), the variance of the binomial distribution is very
## # small. Pearson residuals being the raw residuals (difference between observed and predicted) divided
## # by the sqrt of the variance (a very small value), the Pearson residuals can thus be very large even
## # if the raw residual isn't.
## 
## #Independence
## E1<- resid(M1, type= "pearson")
## plot(x= jitter(Mites$Concentration), y= jitter(E1),
##         xlab= "Concentration", ylab= "Pearson residuals")
## abline(h= 0, col= grey(0.5), lty= 2)
## 
## # Some trends in the residuals against concentration, suggesting that a straight
## # line on the logit scale is not a perfect fit for these data. The model remains
## # good enough for its purpose of evidencing differences in the toxicity between
## # chemicals
## 


## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE-------------------------------
## MyData1<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "1")
## MyData2<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "2")
## MyData3<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "3")
## MyData4<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "4")
## 
## P1<- predict(M1, newdata= MyData1, type= "response")
## P2<- predict(M1, newdata= MyData2, type= "response")
## P3<- predict(M1, newdata= MyData3, type= "response")
## P4<- predict(M1, newdata= MyData4, type= "response")
## 
## par(mfrow= c(1, 1)) # restore default 1x1 window
## # to plot the proportion surviving, use 1 - proportion that died:
## plot(x= jitter(Mites$Concentration), y= jitter(1 - Mites$Proportion), pch= 16, col= Mites$Toxic)
## lines(MyData1$Concentration, P1, col= 1, lty= 1)
## lines(MyData2$Concentration, P2, col= 2, lty= 1)
## lines(MyData3$Concentration, P3, col= 3, lty= 1)
## lines(MyData4$Concentration, P4, col= 4, lty= 1)
## 
## legend("topright",
##             legend= c("1", "2", "3", "4"),
## 			lty= 1,
## 			col= c(1, 2, 3, 4), bty= "n")
## 
## # All chemicals have a significant negative effect on the proportion of mites surviving, as their concentration increases (to ascertain this you could relevel in hindsight the least powerful chemical as the reference level, which will test whether its slope is significantly different from zero).
## # The Chem with the highest toxicity at lower concentrations is fToxic 2, followed by fToxic 3, 1 and 4 is the least potent.
## 
## Mites$fToxic_4AsRef<- relevel(Mites$fToxic, "4")
## M2<- glm(cbind(Living_mites, Dead_mites) ~ Concentration + fToxic_4AsRef + Concentration : fToxic_4AsRef, family= binomial, data= Mites)
## summary(M2)
## 
## # Concentration                 -2.3183     0.4026  -5.758 8.52e-09 ***
## # shows that the negative dose-effect relationship (slope for concentration) is significant even for Toxic 4
## 
## # Concentration:fToxic_4AsRef1  -1.4828     0.9630  -1.540 0.123626
## # Concentration:fToxic_4AsRef2  -7.6592     2.1524  -3.558 0.000373 ***
## # Concentration:fToxic_4AsRef3  -3.3826     1.1189  -3.023 0.002502 **
## # these coefficients and their p-values show that only the dose-effect relationships of Toxics 2 and 3 are significantly different from the dose-effect relationship of Toxic 4
## 


## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE------------------------------
## # Sketch model fit with 95% confidence bands for Toxic 1
## MyData1<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "1")
## MyData2<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "2")
## MyData3<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "3")
## MyData4<- data.frame(Concentration= seq(0, 2.16, length= 50), fToxic= "4")
## 
## P1<- predict(M1, newdata= MyData1, type= "link", se= TRUE)
## P2<- predict(M1, newdata= MyData2, type= "link", se= TRUE)
## P3<- predict(M1, newdata= MyData3, type= "link", se= TRUE)
## P4<- predict(M1, newdata= MyData4, type= "link", se= TRUE)
## 
## par(mfrow= c(1, 1)) # restore default 1x1 window
## plot(x= jitter(Mites$Concentration, 10), y= jitter(1-Mites$Proportion, 2), pch= 16, col= Mites$Toxic)
## 
## G1<- exp(P1$fit) / (1 + exp(P1$fit))
## G1.UP<- exp(P1$fit + 1.96 * P1$se.fit ) / (1 + exp(P1$fit + 1.96 * P1$se.fit))
## G1.LO<- exp(P1$fit - 1.96 * P1$se.fit) / (1 + exp(P1$fit - 1.96 * P1$se.fit))
## 
## lines(MyData1$Concentration, G1, col= 1, lty= 1)
## lines(MyData1$Concentration, G1.UP, col= 1, lty= 2)
## lines(MyData1$Concentration, G1.LO, col= 1, lty= 2)
## 
## # repeat the example above for all 'Toxic' levels (gets a bit busy on the graph)
## 
## legend("topright",
##             legend= c("1", "2", "3", "4"),
## 			lty= 1,
## 			col= c(1, 2, 3, 4), bty= "n")
## 
## # Note how the confidence intervals are and should be asymmetrical on the response scale
## # (they are symmetrical on the link scale), and the logit link effectively
## # prevents them from going below 0 or above 1
## 

