library(psych)
library(FactoMineR)

auto <- read.csv("~/Auto Accident Personal Injury Claims.csv")
auto$CLAIM_AMOUNT <-as.numeric(gsub("[//$,]", "", auto$CLAIM_AMOUNT))
auto$PAID_AMOUNT <-as.numeric(gsub("[//$,]", "", auto$PAID_AMOUNT))

#Original file had empty rows between all observations. It has since been cleaned, making the next two steps irrelevant.
#toDelete <- seq(2, nrow(auto), 2)
#auto <- auto[ toDelete ,]
auto <- na.omit(auto)

describe(auto)
summary(auto)
table(auto$CLAIM_SUSPICION_SCORE)

#Testing Ridit scoring. This code creates Ridit score for IND_01, levels 1, 2, and 5. Scores are already in the data frame
myData <- auto[1:502, 6:25]
rid=data.frame("RIDIT" = cbind("i1"= 2*(0 + 0.5*table(myData$IND_01)[1]/502)-1,
                               "i2"=2*(table(myData$IND_01)[1]/502 + 0.5*table(myData$IND_01)[2]/502)-1,
                               "i5"=2*(table(myData$IND_01)[1]/502+table(myData$IND_01)[2]/502+table(myData$IND_01)[3]/502+table(myData$IND_01)[4]/502+0.5*table(myData$IND_01)[5]/502)-1))
rid



#PRIDIT model
myRidit = auto[1:502, 26:45]
myRiditPCA = princomp(myRidit)
summary(myRiditPCA)

screeplot(myRiditPCA, type = "lines")

#viz for PRIDIT
res.pca = PCA(auto[,26:45],scale.unit = TRUE, ncp = 5, graph = T)

#viz for PRDIT ellipses
auto$CS2 <- as.factor(auto$CLAIM_SUSPICION_SCORE)
myData2 <- auto[,c(26:45, 46)] #prof used 6:25 for IND instead
res.pca3=PCA(myData2[1:21], scale.unit = TRUE, ncp = 5, quali.sup = 21, graph = T)
plotellipses(res.pca3, 21)
plotellipses(res.pca3, 21, label = "ind.sup")

#13th variable explains >70% variance. Try modeling from here.