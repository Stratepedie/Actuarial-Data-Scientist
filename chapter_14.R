
####################################################################################
#################### Chapter 14: General Insurance Pricing #########################
####################################################################################

# The goal is to propose a premium that an insurance company should charge a
# client, for a yearly contract, based on a series of characteristics of the: 
# driver, such as the age or the region, or 
# car, such as the power, the make, or the type of gas.

## 14.1 Introduction and Motivation

## 14.1.1 Collective Model in General Insurance

# most common formula are given by:

# premium = (1 + alpha) * expected_loss or π = (1 + α) × E[S],  with α ≥ 0.

# α (alpha) = loading factor / markup / safety loading
# α = 0  means pure premium (theoretical break-even, usually leads to ruin)
# α > 0  means includes margin for profit + uncertainty + expenses + ruin protection

# Pure premium in a Homogeneous Context:
#   π = E[S] = E[N] × E[Y] , ((Annual) claims frequency × Average cost of individual claims)
# where
#   N  = number of claims in one year (claim frequency)
#   Y  = individual claim amount (average severity)
#   S  = total annual loss = sum_{i=1}^N Y_i

## 14.1.2 Pure Premium in a Heterogenous Context:

# With observable Z (low/high risk):
#   Charging the same premium for everyone creates adverse selection:
#   Low risks switch to competitors, high risks stay, company collapses.

# risk-differentiated pure premium: π(z) = E[S | Z=z] = E[N | Z=z] × E[Y | Z=z]

# In practice: Z is not directly observable, but we observe covariates X (age, car type, etc.)

# Predictive premium becomes: π(x) = E[S | X=x] = E[N | X=x] × E[Y | X=x]

# Chapter goal: Build predictive models for pure premium components in heterogeneous portfolios

# Target quantities to predict (given policyholder covariates x):

# 1. Expected claim frequency (annualized): lambda(x) = E[N | X = x]    # number of claims per year

# 2. Expected claim severity: mu(x) = E[Y | X = x]    # average cost per claim

# Then the individualized pure premium is: pi(x) = lambda(x) * mu(x) = E[S | X = x]

# Typical models used:
#   - Frequency (N): Poisson / NB / Zero-inflated models (glm, glm.nb, gamlss, ...)
#   - Severity (Y):  Gamma / LogNormal / Inverse Gaussian (glm(..., family = Gamma(link="log")), ...)

## 14.1.3 Dataset

#The CONTRACTS dataset has French motor insurance data (ID, claims count NB, exposure, 
#car power/age/brand, driver age, fuel, region, density); CLAIMS links by ID with claim costs (INDEMNITY).

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
int.agg.claims <- aggregate(vec.int.vY, 
                        by = list(ClaimNb = vec.int.vY),
                        FUN = length)  # we aggregate the claims for each discrete value

colnames(int.agg.claims)[2] <- "Count"
int.agg.claims  # returns the number of claims for each discrete value 

vec.num.vE<-df.contract$Exposure  # returns the numeric vector of Exposure with value in (0-2)
boxplot(vec.num.vE,
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
# the authors used another dataset named CON(S)TRACTS instead of CONTRACTS as from the beginning.

# Given the the estimated annual claim frequency parameter lambda, we can now use the the Poisson model to predicts
# probabilities for 0,1,2,3 and 4 claims (in %)

dpois(0:4, num.lambda)*100 # in the Book only the probabilities for 0,1,2 and 3 are given

# The result matches real insurance portfolios for this short periode of time 2 years: 
# Most policyholders never claim, a few claim once, very few claim twice

# The following shows how to estimate the true annual claim frequency lambda_i (varying by covariates for individualized pricing) 
# using a Poisson GLM on observed claims Y_i, by adding log(exposure) as an offset (fixed coefficient = 1)

formula<- ClaimNb~Gas+DriverAge+Density+offset(log(vec.num.E))
funct.link<-poisson(link="log")
                                                                                                                                                                            
model.pois<-glm(formula,family=funct.link,data=df.contract)
summary(model.pois)

# Baseline (reference profile = Diesel): Intercept only, lambda_ref = exp(Intercept) = exp(-1.86471) approx. 0.1549 claims per year
# means about 15.5 claims per 100 policy in a years. GasRegular (vs. reference = Diesel) Coefficient beta_GasRegular = -0.20598
# Multiplicative factor = exp(beta_GasRegular) = exp(-0.20598) approx. 0.8138. This imply that Regular gas vehicles have approx.81.4% 
# of the frequency of diesel vehicles: Reduction of approx. 18.6% (computed as 1 - exp(beta_GasRegular) approx. 0.186).
# Driver age groups (vs. reference category, usually the youngest: (17,22]). beta_Age(22,26]= -0.61606, exp(betaAge(22,26]) approx. 0.540, 
# this correspond to the relative reduction of 1-exp(betaAge(22,26])= 1-0.540 approx. 46% lower. Strong protective effect of age: frequency 
# drops sharply after approx. 22 and stays low. Population density (vs. reference = lowest category [0,40)). All positive coefficients imply 
# that higher density = higher frequency (risk-increasing). beta_Density([40,200))= 0.18473, exp(beta_Density([40,200))) approx. 1.203
# the relative increase = exp(0.18473) -1 approx. +20.3%. Claim frequency nearly doubles in the densest urban areas vs. rural/low-density

# example: full prediction for a high-risk profile: Young driver (17-22], Diesel, very dense area (>4500 inhab/km²)
# eta = Intercept + 0*(age) + 0*(gas) + 0.63717*(density) = -1.86471 + 0 + 0 + 0.63717 approx. -1.2275
# lambda= = exp(-1.2275) approx. 0.293 claims/year approx. 29.3 claims per 100 policy-years

# Dispersion parameter = 1: The model assumes no overdispersion (variance = mean, as required for a pure Poisson distribution)
# Deviance drop = 105613 − 103986 = 1627, The covariates explain 1627 units of deviance. Adding these risk factors 
# (GAS, age groups, density) genuinely improves our ability to separate high-risk from low-risk drivers for fair pricing.
# AIC = 135263 Lower AIC = better model fit (penalizing complexity). we come to this later to compare against alternative models
# Fisher Scoring iterations = 6, Convergence was fast and stable, this means no numerical issues with the IRLS algorithm.
# IRLS stands for Iteratively Reweighted Least Squares.

# ---------------------------------------------------------------------
# design matrix and response
#----------------------------------------------------------------------

# In Poisson GLM (log-link + offset(log(exposure))), the MLE estimator beta_cap satisfies the score equations = 0 at convergence.
# The score vector (gradient of the log-likelihood) is:
#   score(beta) = X' (Y - mu)   where mu = exp(X*beta + offset) = fitted values
# At the MLE (beta_cap), we should have: score(beta_cap) ≈ 0    (within numerical precision)
# This implies that, globally and within each covariate pattern / risk class, 
# the observed total claims (sum Y) ≈ expected total claims (sum mu).

# In other words: the model is balanced. It does not systematically over- or under-predict the number of claims
# in any combination of rating factors (GAS, AGEDRIVER, DENSITY, etc.). This balance property is a direct
# consequence of the first-order conditions of MLE and is very important for actuarial credibility
# and fairness in a priori ratemaking.

# ---------------------------------------------------------------------
# Code to verify this property empirically
#----------------------------------------------------------------------

# Design matrix (without intercept column if you want, but usually include it)
matrix.num.X <- model.matrix(model.pois)           # n × p matrix

# Observed response vector (number of claims Y_i)
vec.num.Y <- model.response(model.frame(model.pois))  # n × 1

# Fitted values mu_i = exp(X*beta_cap + log(E_i))
vec.num.mu <- fitted(model.pois)                   # n × 1

# Score vector at MLE (should be ≈ 0 for each component)
vec.num.score <- crossprod(matrix.num.X, vec.num.Y - vec.num.mu)   # p × 1

# Print it (very small values = good convergence)
print(vec.num.score)
round(vec.num.score, 8)   # easier to read

# Quick check: are all components numerically zero?
all(abs(vec.num.score) < 1e-6)    # should return TRUE

# ---------------------------------------------------------------------
# Observed (Expected) Information / Hessian computation
# ---------------------------------------------------------------------

# In Poisson GLM, the observed Hessian H(beta_cap) = - X' W X   (negative second derivative of log-likelihood)
#   implies W = diag(mu_i)   since for Poisson, variance = mu_i and 
#   the second derivative of the log-likelihood with respect to beta = -mu_i X X'
#
# At MLE, the Fisher Information matrix I(beta_cap) ≈ -H(beta_cap) = X' W X
#   -> Its inverse I^{-1} gives the asymptotic covariance matrix of beta_cap
#   -> Diagonal elements -> variances -> standard errors = sqrt(variance)
#
# Goal here: Verify that we can recover the same standard errors as glm() summary()
#            -> proves numerical understanding of GLM inference
#            -> useful for custom models, bootstrap alternatives, or large-scale implementations
#
# Problem: n = 413,169 observations -> diag(mu) would be n×n dense matrix ≈ 1,272 GB (!) -> impossible
# Solution: Avoid building full diag(W) -> compute X' W X = X' (mu .* X) = t(X) %*% (X * mu)
#           -> memory-efficient (only uses X and mu vectors)

# Efficient computation of -Hessian = Information matrix
matrix.num.H    <- - crossprod(matrix.num.X, matrix.num.X * as.numeric(vec.num.mu))   # p×p
matrix.num.Info <- - matrix.num.H                                                     # = X' W X ≈ Fisher Information
matrix.num.Info
# Asymptotic covariance matrix = inverse of Information
vcov_manual <- solve(matrix.num.Info)

# Manual standard errors
SE_manual <- sqrt(diag(vcov_manual))

# Compare with glm() built-in standard errors
comparison <- data.frame(
  Estimate   = coef(model.pois),
  SE_manual  = SE_manual,
  SE_glm     = summary(model.pois)$coefficients[, "Std. Error"]
)

print(comparison)
max_diff <- max(abs(SE_manual - summary(model.pois)$coefficients[, "Std. Error"]))
cat("Maximum absolute difference between manual and glm SE:", max_diff, "\n")
# -> Should be extremely small (e.g. < 1e-10) if computation is correct

# ---------------------------------------------------------------------
# Interpretation of standard errors / credibility insight
# ---------------------------------------------------------------------

# Smaller standard error => more precise estimate => higher "credibility" for that coefficient
#   (i.e. narrower confidence interval, more reliable effect size)
#
# looking at the diagonal of matrix.num.Info, one can say that, GasRegular, age 42–74 and high-density zones have 
# high credibility; very old drivers have low credibility.


## 14.2.3 Ratemaking with One Categorical Variable

# In the following sections, we will discuss the interpretation of this regression, on categorical
# variables (one or two) and on continuous variables (one or two).

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






