barplot(dpois(0:10,3),space=0,width=1,names=0:10)
pois.lik=function(mu,k){exp(-mu)*mu^k/factorial(k)}
curve(pois.lik(x,3),0,10)
curve(dpois(3,x),0,10)
observed=rpois(10,3)
barplot(table(observed))
pois.lik(3,observed) #Likelihood of each observation

pois.lik2=function(mu,k){prod(exp(-mu)*mu^k/factorial(k))} #Fixed for multiple observations
Pois.lik2=Vectorize(pois.lik2,"mu") #Vectorized for graphing
curve(Pois.lik2(x,observed),0,10,ylab="Likelihood",xlab="Value of Mu")
curve(log(Pois.lik2(x,observed)),0,10,ylab="Likelihood",xlab="Value of Mu")

df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
glm(data=df,admit~gre,family=binomial())

# Everybody uses the crab data
crab=read.table("~/dsci3701/Lectures/Week 7/crab.txt")
crab=read.table("https://personal.psu.edu/abs12/stat504/online/07_poisson/graphics/crab.txt")
colnames(crab)=c("obs","color","spine","width","weight","n.sat")
crab=crab[,-1] #Drop the observation column
pairs(crab)
#Obs: Observation Number
#Color: 1: light medium
#       2: medium
#       3: dark medium
#       4: dark
#Spine Condition
#       1: Both good
#       2: One worn or broken
#       3: Both worn or broken
#Carapace Width (in cm)
#Weight in kilograms
#Number of Satellites (aka-- male's grouped around female hoping to mate)
#Data from Jane Brockmann, Zoology Department University of Floriday from Etcholog 102: 1-21, 1996
with(crab,{plot(table(color,spine))}) #Obvious relation... suggests age to me
#As color gets darker spine conditions gets worse
with(crab,plot(table(color,cut(width,5)))) #That's not consistent with age.. I would have thought
# carapace width was associated with age... but small 
with(crab,boxplot(width~color))
# That highlights that it's the inverse of what I would have expected
pairs(crab)
# width and weight are, reasonably correlated
with(crab,plot(table(color,cut(n.sat,5))))
with(crab,(boxplot(n.sat~color)))
# Seems to showcase Preference for lighter colors (with outliers)
with(crab,(boxplot(n.sat~spine)))
model=glm(data=crab,n.sat~width,family=poisson())

plot(crab$width,jitter(crab$n.sat,0.2),cex=3,pch=20,col=rgb(0,0,0,1/4))
points(pch=".",cex=5,col="blue",crab$width,fitted.values(model))
approx=split(crab$n.sat,cut(crab$width,7))
#A key thing to look for in Poisson regression is "overdispersion"
# The idea is that in a Poisson regression the conditional distribution of Y shoudl be Poisson... this
# this means the variance should equal the mean.  Let's bin n.sat by width and see what we get:

sapply(approx,mean)
#(21,22.8] (22.8,24.6] (24.6,26.4] (26.4,28.1] (28.1,29.9] (29.9,31.7] (31.7,33.5] 
#1.000000    1.312500    2.711864    3.146341    4.666667    4.857143    4.500000 
sapply(approx,var)
#(21,22.8] (22.8,24.6] (24.6,26.4] (26.4,28.1] (28.1,29.9] (29.9,31.7] (31.7,33.5] 
#3.000000    5.963710    9.587960    6.478049   16.076923    7.142857   12.500000 

#Ooof.. that doesn't look good.  

#Quick negative binomial:
# Number of failures before the r'th success
#P(X=k) = (k+r-1 choose k) p^r(1-p)^k
barplot(dnbinom(0:30,5,0.3),width=1,space=0,names=0:30)
#X~NB(r,p) [r is number of successes, prob of success per trial is p]
#In the language of the variable names size is r and prob is p
# E(X) = p/(1-p)* r [The odds times the number of successes]
# VAR(X) = 1/(1-p) E(X) [note 0 < p < 1 so VAR(X) > E(X)]

# THe GLM ALSO
# Also uses log link
# model is ALSO log(mu) = b0 + b1 X1 + ...
# BUT the log likelihood formula is different so the fit is different.
library(MASS)
model.nb=glm.nb(data=crab,n.sat~width)
# There are SEVERAL parameterizations of negative binomial Many regression techniques use a different 
# parameterization than the one I provided.  For example, R also allows Nbinom(mu,size) where mu
# is the expected value and var(X) = mu + alpha mu^2 (and alpha = 1/size)
# Prob = size/(size+mu) under the alternate mu 

# The key thing here is that the estimator has to estimate TWO parameters.  One is called the DISPERSION
# Parameter because it influences the change inv ariance
df$rank.minus=df$rank-1

glm(data=df,rank.minus~gre,family=poisson())

plot(crab$width,jitter(crab$n.sat,0.2),cex=3,pch=20,col=rgb(0,0,0,1/4))
points(pch=".",cex=5,col="blue",crab$width,fitted.values(model))
points(pch=".",cex=5,col="green",crab$width,fitted.values(model.nb))
approx=split(crab$n.sat,cut(crab$width,7))
lowers=str_extract(names(approx),"\\([^,]+[,]")
lowers=as.numeric(substring(lowers,2,nchar(lowers)-1))

uppers=str_extract(names(approx),"[,][^\\]]+\\]")
uppers=as.numeric(substring(uppers,2,nchar(uppers)-1))

mids=(lowers+uppers)/2

mid.mus=sapply(mids,function(m){exp(sum(coef(model.nb)*c(1,m)))})
mid.vars=mid.mus+mid.mus^2/model.nb$theta
#Compare
mid.vars
sapply(approx,var)
sapply(approx,length)
# Much better on the low-end.  And cetainly a better fit on some of the bigger

# Back to logistic

crab$hasS = as.numeric(crab$n.sat>0)
model.logistic=glm(data=crab,hasS~width,family=binomial())
summary(model.logistic)
fv=fitted.values(model.logistic)
ov=crab$hasS
pv=as.numeric(fv>=0.5)
table(pv,ov)
## Wednesday
