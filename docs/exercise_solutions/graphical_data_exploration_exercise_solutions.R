## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE, 
                   stringsAsFactors = TRUE)
str(loyn)

# 67 observations and 8 variables (from str())

summary(loyn)

# no NAs reported for any variable, so nothing to deal with on that front

# GRAZE is coded as numeric (i.e. 1,2,3,4,5)

# this matters. Left as a number, R would treat grazing as a continuous variable and fit
# a single slope through it, forcing the effect to change by the same amount from level
# 1 to 2 as from level 4 to 5. As a factor, each level gets its own mean and the data
# decide the shape.

# create a new factor variable FGRAZE which is a factor of GRAZE
loyn$FGRAZE <- factor(loyn$GRAZE)


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
table(loyn$FGRAZE)

# or use xtabs function
xtabs(~ FGRAZE, data = loyn)

# 13, 11, 17, 13 and 13 patches. Reasonably balanced, and every level has enough patches
# to estimate a mean. Level 2 has the fewest, so its mean will be the least precisely
# estimated of the five.


## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
# mean abundance of birds for each level of FGRAZE
tapply(loyn$ABUND, loyn$FGRAZE, mean, na.rm = TRUE)

# variance in the abundance of birds for each level of FGRAZE
tapply(loyn$ABUND, loyn$FGRAZE, var, na.rm = TRUE)

# OR use the summary function
tapply(loyn$ABUND, loyn$FGRAZE, summary, na.rm = TRUE)

# means: 28.6, 19.4, 20.2, 19.0 and 6.3 birds. Note this isn't a steady decline across
# the grazing gradient. Levels 2, 3 and 4 are much of a muchness and the drop is
# concentrated at the most heavily grazed level.

# variances: 32.6, 73.1, 89.4, 50.6 and 23.1. The largest is about four times the
# smallest, which is right at the usual rule of thumb but not alarming. A linear model
# assumes one common variance, so make a note to look for this again in the residual
# plots rather than doing anything about it now.


## ----Q7a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# first split the plotting device into 2 rows and 3 columns
par(mfrow = c(2,3))

# now produce the plots - the response variable first, then each of the continuous
# explanatory variables
dotchart(loyn$ABUND, main = "Bird abundance")
dotchart(loyn$AREA, main = "Area")
dotchart(loyn$DIST, main = "Distance")
dotchart(loyn$LDIST, main = "Distance to larger patch")
dotchart(loyn$YR.ISOL, main = "Year of isolation")
dotchart(loyn$ALT, main = "Altitude")

# AREA has two patches far larger than everything else (973 and 1771 ha against a median
# of 7). DIST and LDIST are both strongly right skewed, LDIST especially, running from
# 26 m out to 4426 m. ABUND, YR.ISOL and ALT all look fine.

# these are unusual values, not wrong ones. A 1771 ha patch is a perfectly real forest
# patch. Nothing here is a reason to delete an observation.


## ----Q7b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# A fancier version of a dotplot - just for fun!
Z <- cbind(loyn$ABUND, loyn$AREA, loyn$DIST,
           loyn$LDIST,loyn$YR.ISOL,loyn$ALT,
           loyn$GRAZE)

colnames(Z) <- c("Abundance", "Area","Distance",
                 "larger dist","year of isolation",
                 "Altitude", "Grazing")
                 
library(lattice)
dotplot(as.matrix(Z),
      groups=FALSE,
      strip = strip.custom(bg = 'white',
            par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  =0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------------------------------------
# There appears to be two unusually large forest patches compared to the rest Also one
# potentially large distance in DIST, and LDIST is skewed in the same way (it ranges
# from 26 to 4426) One option would be to log10 transform AREA, DIST and LDIST log base
# 10 transform variables

loyn$LOGAREA <- log10(loyn$AREA)
loyn$LOGDIST <- log10(loyn$DIST)
loyn$LOGLDIST <- log10(loyn$LDIST)

# check the dataframe
str(loyn)

# first split the plotting device into 1 row and 3 columns
par(mfrow = c(1,3))

# now plot the transformed variables
dotchart(loyn$LOGAREA, main = "LOG Area")
dotchart(loyn$LOGDIST, main = "LOG Distance")
dotchart(loyn$LOGLDIST, main = "LOG Distance to larger patch")

# all three now look much more even, with the extreme values pulled back towards the
# rest. That is all the transformation is doing here.

# one consequence to keep in mind: LOGAREA now runs from -1 to 3.25, and a 1 unit change
# on that scale is a tenfold change in area. Anything we estimate from LOGAREA later
# will be in those units, not in hectares.


## ----Q9a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# Vanilla pairs plot

pairs(loyn[,c("LOGAREA", "LOGDIST", "LOGLDIST",
               "YR.ISOL", "ALT", "GRAZE")])

# or first create a new dataframe and then use this data frame with the pairs function

explan_vars <- loyn[,c("LOGAREA", "LOGDIST", "LOGLDIST",
               "YR.ISOL", "ALT", "GRAZE")]
pairs(explan_vars)


## ----Q9b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE----------------------------------------------------------------------------------------------------------------
# And with correlations in the upper panel

# first need to define the panel.cor function
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
    op <- par(usr = c(0, 1, 0, 1))   # set new plotting region, keep the old
    on.exit(par(op))                 # and restore the old one on exit
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

# then use the panel.cor function when we use pairs
pairs(loyn[,c("LOGAREA","LOGDIST", "LOGLDIST",
               "YR.ISOL","ALT","GRAZE")],
      upper.panel = panel.cor)

# the largest are LOGDIST with LOGLDIST (0.59), YR.ISOL with GRAZE (-0.56) and LOGAREA
# with GRAZE (-0.48). Collinearity only really starts to cause trouble above about 0.7,
# so none of these need anything done about them.

# the pairings make sense: a patch with a near neighbour usually has a near larger
# neighbour too, and the patches isolated longest ago tend to be the more heavily grazed
# ones.

# careful reading the upper panel: panel.cor plots the ABSOLUTE correlation, so the
# number never tells you the sign. Look at the scatterplot below the diagonal for that.


## ----Q10a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------------------------
pairs(loyn[,c("ABUND","LOGAREA","LOGDIST", "LOGLDIST",
    	"YR.ISOL","ALT","GRAZE")],
      	upper.panel = panel.cor)

# LOGAREA has by far the strongest relationship with ABUND (r = 0.77), then GRAZE (r =
# -0.63). YR.ISOL (0.44) and ALT (0.32) are weaker and the two distance variables are
# close to nothing (both 0.13).

# don't read these one at a time as though they were separate effects. GRAZE and YR.ISOL
# are themselves correlated (-0.56), so some of what looks like an effect of one may
# belong to the other. Sorting that out is the model's job, not the pairs plot's.


## ----Q10b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------------------------
plot(loyn$LOGAREA, loyn$ABUND, xlab = "log area", ylab = "bird abundance")

# abundance rises fairly steadily with log10 area and the scatter looks about even along
# the x axis. A straight line on this scale is a sensible thing to fit.


## ----Q11a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------------------------
# Interaction between LOGAREA and FGRAZE? Do the slopes look similar or different?
coplot(ABUND ~ LOGAREA | FGRAZE, data = loyn)

# each panel is one grazing level, shown by the highlighted block in the strip along the
# top.

# the slopes are clearly not all the same. Abundance rises with area in every panel, but
# much more steeply in some than others, which is exactly what an interaction between
# LOGAREA and FGRAZE looks like.


## ----Q11b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE---------------------------------------------------------------------------------------------------------------
# Fancier version of the above plot with a line of best fit included just for fun
coplot(ABUND ~ LOGAREA | FGRAZE,
      data = loyn,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

# with the lines drawn in, the fitted slopes are roughly 4.1, 8.4, 13.2, 17.8 and 6.1
# birds per unit log10 area for grazing levels 1 to 5.

# don't over-read them. Each line rests on 11 to 17 patches, and the levels cover very
# different ranges of area: level 1 runs from 2 up to 1771 ha, while levels 3, 4 and 5
# all stop below 50 ha. Some of the difference between panels is the different stretch
# of x axis each line is drawn over.

# this is exploration, not the analysis. We are deciding what to put in the model, not
# deciding what the answer is.

