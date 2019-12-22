#https://cran.r-project.org/web/packages/twang/vignettes/mnps.pdf

library("Matching")
data("lalonde")
attach(lalonde)


m1 <- glm(treat ~ age + educ + black + hisp + married +
            nodegr + re74 + re75, family = binomial, data = lalonde)

pm1 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M = 1, replace = TRUE)
summary(pm1)
pm2 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M = 1, replace = FALSE)
summary(pm2)

mb <- MatchBalance(treat ~ age + educ + black + hisp + married +
                     nodegr + re74 + re75,
                   match.out = pm1, nboots = 5000, data = lalonde)

par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2,0.2,0), tcl=-0.2)
qqplot(lalonde$age[pm1$index.control], lalonde$age[pm1$index.treated],
       xlim=c(20,50), ylim=c(20,50),
       xlab="Control Observations", ylab="Treatment Observations", main="age")
abline(coef = c(0, 1), lty=2)
qqplot(lalonde$re74[pm1$index.control], lalonde$re74[pm1$index.treated],
       xlim=c(0,35000), ylim=c(0,35000),
       xlab="Control Observations", ylab="Treatment Observations", main="re74")
abline(coef = c(0, 1), lty=2)

install.packages("rgenoud")
library(rgenoud)

X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75)
set.seed(1)
genout <- GenMatch(Tr=treat, X=m1$fitted, BalanceMatrix=X, estimand="ATT")
mout <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout)
summary(mout)
mb1 <- MatchBalance(treat ~ age + educ + black + hisp + married +
                      nodegr + re74 + re75,
                    match.out = mout, nboots = 5000, data = lalonde)

par(mfrow=c(1,2), mar=c(3,3,3,1), mgp=c(2,0.2,0), tcl=-0.2)
qqplot(lalonde$age[mout$index.control], lalonde$age[mout$index.treated],
       xlim=c(20,50), ylim=c(20,50),
       xlab="Control Observations", ylab="Treatment Observations", main="age")
abline(coef = c(0, 1), lty=2)
qqplot(lalonde$re74[mout$index.control], lalonde$re74[mout$index.treated],
       xlim=c(0,35000), ylim=c(0,35000),
       xlab="Control Observations", ylab="Treatment Observations", main="re74")
abline(coef = c(0, 1), lty=2)

install.packages("rbounds")
library(rbounds)
psens(x = mout, Gamma = 2, GammaInc = 0.1)
hlsens(x = mout, Gamma = 2, GammaInc = 0.1)

exactMatch <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", exact=TRUE)
summary(exactMatch)
nearMatch <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT")
summary(nearMatch)
nearMatch3 <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M=3)
summary(exactMatch3)
nearMatch3NoReplace <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", M=3, replace=FALSE)
summary(exactMatch3NoReplace)

csMatch <- Match(Y = re78, Tr = treat, X = m1$fitted, estimand = "ATT", CommonSupport=TRUE)

summary(mout)
mout2 <- Match(Y=re78, Tr=treat, X=m1$fitted, estimand="ATT", Weight.matrix=genout, CommonSupport=TRUE)
summary(mout2)


detach(lalonde)

install.packages(c("MatchIt", "optmatch"))
library(MatchIt)
data(lalonde)
# 1 vs 1 Nearest Neighborhood Matching
set.seed(1)
mNearest1v1 <- matchit(treat ~ age + educ + black + hispan + married +
                         nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=1)
# 1 vs 2 Nearest Neighborhood Matching
mNearest1v2 <- matchit(treat ~ age + educ + black + hispan + married +
                         nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=2)
# 1 vs 1 Nearest Neighborhood Matching with replacemnet
mNearestReplace <- matchit(treat ~ age + educ + black + hispan + married +
                             nodegree + re74 + re75, data = lalonde, method = "nearest", ratio=1,
                           replace = TRUE)
# Mahalanobis Distance Matching
mMahalanobis <- matchit(treat ~ age + educ + black + hispan + married +
                          nodegree + re74 + re75, data = lalonde, method = "nearest",
                        distance = "mahalanobis")
# Optimal Matching
mOptimal <- matchit(treat ~ age + educ + black + hispan + married +
                      nodegree + re74 + re75, data = lalonde, method = "optimal", ratio=2)


print(mNearest1v1)
plot(mNearest1v1, type="QQ")
plot(mNearest1v1, type="jitter")
plot(mNearest1v1, type="hist")

sNearest <- summary(mNearest1v1, standardize = TRUE)
print(sNearest)
plot(sNearest)

mData <- match.data(mNearest1v1,  group = "all")
mDataTreated <- match.data(mNearest1v1, group = "treat")
mDataControl <- match.data(mNearest1v1,  group = "control")

install.packages("Zelig")
library(Zelig)
z.out <- zelig(re78 ~ treat + age + educ + black + hispan + nodegree +
                 married + re74 + re75, data = mData, model = "ls")
xTreated <- setx(z.out, treat = 1)
xControl <- setx(z.out, treat = 0)
ATE <- sim(z.out, x = xControl, x1 = xTreated)
summary(ATE)

library(rbounds)
YTreated <- mData$re78[mData$treat==1]
YControl <- mData$re78[mData$treat==0]
psens(x = YControl, y=YTreated, Gamma = 2, GammaInc = 0.1)
hlsens(x = YControl, y=YTreated, Gamma = 2, GammaInc = 0.1)
