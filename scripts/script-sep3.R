# attach data
data("infert")﻿

# with command
with(infert, table(education, induced))﻿

# xtabs command
xtabs(~education + induced, data= infert) 

#better from 3rd party graphics library/CrossTable
library(models)
with(infert, CrossTable(education, induced))﻿

# prop.table command
prop.table(addmargins(xtabs(~education + induced, data=infert)))﻿

# ftable command
three.table=  xtabs(~education + induced + spontaneous, data = infert)
ftable(three.table)

#plotting categorical data
require(datasets)
data(chickwts)

# visualizing categorical data r - plot example
plot(chickwts$feed)

# example - Barplot in R
x <- table(chickwts$feed)
barplot(x)

# example - barplot in R
barplot(x[order(x, decreasing = TRUE)])

library(vcd)
data(HairEyeColor)
mosaic(HairEyeColor, shade = TRUE)

# data set for chi square test (chi square in R example)
data(ChickWeight)

# chisq r - 2 by 2 table chi square test in r
chisq.test(ChickWeight$Diet, ChickWeight$weight)
chisq.test(ChickWeight$Time, ChickWeight$weight)

#evolving all of this to confusion matrix
library(caret)

###################
## 2 class example

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
               c(
                 rep(lvs, times = c(54, 32)),
                 rep(lvs, times = c(27, 231))),
               levels = rev(lvs))

xtab <- table(pred, truth)

confusionMatrix(xtab)
confusionMatrix(pred, truth)
confusionMatrix(xtab, prevalence = 0.25)


###################
## 3 class example

confusionMatrix(iris$Species, sample(iris$Species))

newPrior <- c(.05, .8, .15)
names(newPrior) <- levels(iris$Species)

confusionMatrix(iris$Species, sample(iris$Species))

