##### Book: Computational Actuarial Science with R - Edited by Arthur Charpentier

## Chapter 14: General Insurance Pricing

## Serges Letsudem Wonanke
####################################################################################
####################################################################################

## 14.2.1 Annualized Claims Frequency

require(CASdatasets)

##  French Motor Third-Part Liability datasets

data(freMTPLfreq)     # CONTRACTS, contains contract and client information
data(freMTPLsev)      # CLAIMS, contains claims information

df.contract<-freMTPLfreq
df.claims<-freMTPLsev
str(df.contract) # we describe the size of the dataset as well as its variables.

# We cut continuous variables to define factor because this simplifies interpretation, 
# stabilizes GLM estimates, and better captures nonlinear risk patterns in insurance data.

df.contract$DriverAge<-cut(df.contract$DriverAge,c(17,22,26,42,74,Inf))
df.contract$CarAge<-cut(df.contract$CarAge,c(0,1,4,15,Inf),include.lowest=T)
df.contract$Density<-cut(df.contract$Density,c(0,40,200,500,4500,Inf),include.lowest=T)

# We compute the average annualized frequency and its empirical variance

vec.int.vY<-df.contract$ClaimNb   # returns the vector of claims with discrete value (0,1,2,3,4)
int.agg.claims <- aggregate(df.contract$ClaimNb, # we aggregate the claims for each discrete value
                        by = list(ClaimNb = df.contract$ClaimNb),
                        FUN = length)

colnames(int.agg.claims)[2] <- "Count"
int.agg.claims  # returns the number of claims for each discrete value 

vec.num.vE<-df.contract$Exposure  # returns the numeric vector of Exposure with value in (0-2)
boxplot(df.contract$Exposure,
        main="Boxplot of Exposure",
        col="grey")

num.m<- sum(vec.int.vY)/sum(vec.num.vE) # average annualized frequency
num.v<- sum((vec.int.vY- num.m*vec.num.vE)^2)/sum(vec.num.vE) # emp. variance of the average annualized frequency
cat("average =",num.m," variance =",num.v,"phi =",num.v/num.m,"\n")

# The condition on phi to be 1 is important for Poisson assumption to be valid (variance = mean)
# If phi > 1, the empirical variance is larger than the mean. This means: The Poisson model underestimates variability.
# A Negative Binomial or quasi-Poisson model is more realistic. This happens often with insurance data because:
# Some policyholders have more claims than average, there is unobserved heterogeneity, Exposure varies, Random effects exist

# Sometime one may compute phi by category (e.g., Region) to check whether the poisson assumption is valid for each
# category / subgroup (Region, Age group, Vehicle type, Urban vs.rural).

vec.factor.region.vX<-as.factor(df.contract$Region)
for(i in 1:length(levels(vec.factor.region.vX))){
  region.name <- levels(vec.factor.region.vX)[i]
  vec.num.vE.i<-vec.num.vE[vec.factor.region.vX==region.name] # returns the vector of Exposure for the region i
  vec.int.vY.i<-vec.int.vY[vec.factor.region.vX==region.name] # returns the vector of Counts of claims for the region i
  num.m.i<- sum(vec.int.vY.i)/sum(vec.num.vE.i) # average annualized frequency for the region i
  num.v.i<- sum((vec.int.vY.i- num.m.i*vec.num.vE.i)^2)/sum(vec.num.vE.i) # emp. variance of the average annualized frequency for the region i
  cat("Region:", region.name, "average =",num.m.i," variance =",num.v.i,"phi =",num.v.i/num.m.i,"\n")
}
# For Île-de-France (resp. Nord-Pas-de-Calais), the estimated dispersion parameter phi ≈ 1.11 (resp. phi ≈ 1.12) indicates mild 
# but non-negligible overdispersion relative to the Poisson assumption (phi = 1). While a Poisson GLM can still be used for 
# point estimation, a quasi-Poisson or Negative Binomial model is recommended for accurate standard errors and inference for
# these two cases.

## 14.2.2 Poisson Regression

vec.int.Y<-df.contract$ClaimNb   # returns the vector of Counts of claims with discrete value (0, 1,2,3,4)
vec.num.E<-df.contract$Exposure  # returns the numeric vector of Exposure with value in (0–2)
vec.num.w <- vec.num.E/sum(vec.num.E)  # return the vector of the weights w_i

(num.lambda<- sum(vec.num.w*(vec.int.Y/vec.num.E))) # return the estimate of Annualized Claims Frequency, lambda
(num.lambda<-weighted.mean(vec.int.Y/vec.num.E,vec.num.w)) # compute lambda directly with the function in R

# The difference between the annualized Claims Frequency in 14.2.1 and lambda in 14.2.2 is due to the fact that
# the authors used another dataset named CONSTRACTS instead of CONTRACTS.

# Given the the estimated annual claim frequency parameter lambda, we can now use the the Poisson model to predicts
# probabilities for 0,1,2,3 and 4 claims (in %)

dpois(0:4, num.lambda)*100 # in the Book probabilities for 0,1,2 and 3

# The result matches real insurance portfolios: Most policyholders never claim, a few claim once, very few claim twice

formula<- ClaimNb~Gas+DriverAge+Density+offset(log(vec.num.E))
funct.link<-poisson(link="log")

model.pois<-glm(formula,family=funct.link,data=df.contract)
summary(model.pois)

# The intercept estimate = −1.86471. This is the baseline annual frequency ist given by exp(-1.86471)≈ 0.155. This
# means about 0.155 claims/year for the reference profile. Regular gas vehicles have ~19% lower frequency than 
# the reference fuel type. Frequency decreases strongly with age. Claim frequency almost doubles in very dense areas
# Statistical significance: All covariates: p < 5.02e−12, Effects are highly credible actuarially

# design matrix and response
matrix.num.X <-model.matrix(model.pois)
vec.num.Y <- model.response(model.frame(model.pois))
# fitted mean mu (vector)
vec.num.mu <- fitted(model.pois)
# Score (gradient) at MLE
vec.num.score <- t(matrix.num.X) %*% (vec.num.Y - vec.num.mu)         # p x 1 vector
vec.num.score 
# The score equations are satisfied for all rating factors, meaning that at the estimated parameters,
# observed and expected claims are fully balanced within each risk class.
# Observed Hessian
##############################################################################################################
# matrix.num.W <- diag(as.numeric(vec.num.mu))
# matrix.num.H <- - t(matrix.num.X) %*% matrix.num.W %*% matrix.num.X           # p x p matrix (negative definite)
# Fehler: cannot allocate vector of size 1271.9 Gb -> diag(as.numeric(vec.num.mu)) is a n × n dense matrix
# and n = 413,169 is too large. Alternativ, instead of WX = W%*%X we compute WX = X*mu
##############################################################################################################
matrix.num.H<- - t(matrix.num.X) %*% (matrix.num.X * as.numeric(vec.num.mu))
matrix.num.Info<- -matrix.num.H
matrix.num.Info
vcov_manual <- solve(matrix.num.Info)       # (X' W X)^{-1}
SE_manual    <- sqrt(diag(vcov_manual))     # standard errors are the square roots of its diagonal
data.frame(Estimate = coef(model.pois),
           SE_manual = SE_manual,
           SE_summary = summary(model.pois)$coefficients[, "Std. Error"])
# check differences
max(abs(SE_manual - summary(model.pois)$coefficients[, "Std. Error"]))

# looking at the diagonal of matrix.num.Info, one can say that, GasRegular, age 42–74 and high-density zones have 
# high credibility; very old drivers have low credibility.

# In the following sections, we will discuss the interpretation of this regression, on categorical
# variables (one or two) and on continuous variables (one or two).

## 14.2.3 Ratemaking with One Categorical Variable
##
vec.factor.gas.X1<-df.contract$Gas
name.vec.factor.gas.X1<-levels(vec.factor.gas.X1)
#  The number of claims per gas type is
tapply(vec.int.vY, vec.factor.gas.X1, sum)
# and the annualized claim frequency is
tapply(vec.int.vY, vec.factor.gas.X1, sum)/tapply(vec.num.vE, vec.factor.gas.X1, sum)

# Now we take a look at the Poisson regression without the (Intercept) variable. This is done
# by Removing the intercept to makes each coefficient a group-specific frequency levels, therefore
# Each level of X1 (fuel type) gets its own parameter,there is no baseline category.

df.contract.gas <- data.frame(vec.int.vY,vec.num.vE,vec.factor.gas.X1)

formula.0<- vec.int.vY~0+vec.factor.gas.X1+offset(log(vec.num.E))
funct.link<-poisson(link="log")

model.pois.gas.0<-glm(formula.0,family=funct.link,data=df.contract.gas)
summary(model.pois.gas.0)
exp(coefficients(model.pois.gas.0)) # we view the observed annualized frequencies, per gas type.

# In sum, this model is equivalent to a pure premiums table grouped by fuel type. The beta for Diesel which is equal
# to the log(frequency of Diesel) is given by beta_Diesel= −2.59462 therefore  the frequency of Diesel is given
# by frequency_Diesel= exp(−2.59462)≈ 0.0747. This means that the estimated claim frequency for Diesel is about 
# 7.47% per year (i.e., 0.0747 claims per exposure unit).






