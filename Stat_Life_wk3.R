babies <- read.table("babies.txt", header=TRUE)
library(tidyverse)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

#Q1
set.seed(1, sample.kind="Rounding")
N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
str(dat.s)
tval <- t.test(dat.s, dat.ns)$stat
tval
#or 
X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.s - X.ns)/sd.diff
tval <- abs(tval) #crit values always in abs()

#Q2
pval <- 2 * pnorm(abs(tval), lower.tail = F) #pnorm() computes the prob of getting a t-stat larger than given t-stat, since lower.tail = F
                                     #this can also be rewritten as 1 - pnorm(), or getting the complementary of t < tstat
                                     #we multiply by 2 since we compute right and left tails
#or
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval))) #pnorm(abs(tval)) ;computes the area left of abs(tval)
                                               #this is the prob of values less than or equal to tval: 2.120904
                                               #pnorm(-abs(tval));computes the area left of -abs(tval)
                                               #this is the prob of values less than or equal to tval: -2.120904
                                               #the diff betw the two is the middle area of the distr,
                                               #this is the prob that other tvals would be > -2.120904 and < 2.120904
                                               #or prob of -2.120904 < tvals < 2.120904
                                               #this prob is subtracted to 1, to get the prob of other tvals being > 2.120904 and < -2.120904
                                               #or prob of -2.120904 > tvals > 2.120904
                                               #Recall that, 1 - p is the complementary of p
                                               #these probs are the tail ends of the normal distr


#Q3
2*pnorm(-abs(tval)) #Because standard norm distr is symmetrical, this is also another way of computing the probability that a t-value 
                    #under the null could have a larger absolute value than tval 
                    #-abs(tval) gets the negative of absolute value of tval
                    #in this case, getting abs value, then taking the neg may be redundant since given tval is already neg (-2.120904)
                    #However, if we do not get abs value, for instance, -(-2.120904) , tval would be +
                    #pnorm(2.120904) would then compute area <= 2.120904 and be multiplied by 2, which would yield a prob > 1 , which would not make sense
                    #by taking the neg of abs(tval), this ensures that pnorm() would compute
                    #the left tail ONLY and this would be multiplied by 2 since right tail would have same pvalue because of symmetry

#Q4
SE <- sqrt(var(dat.s)/N + var(dat.ns)/N)
z.99 <- qnorm(0.99 + (1-0.99)/2) #zscore for 99% conf level, 0.99 + tail ends
ME <- z.99 * SE                  #ME = margin of error = zscore * SE 
ME
#or 

N <- 25
qnorm(0.995)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N ) #0.995 = 0.99 +((1-0.99)/2)
                                                      #multiplier is SE

#conf interval
obsdiff <- mean(dat.s) - mean(dat.ns)
interval_range <- c(obsdiff - ME, obsdiff + ME )

#Conf Interval Exercises
library(tidyverse)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

pop.diff <- mean(bwt.nonsmoke)-mean(bwt.smoke)

#Q1
set.seed(1, sample.kind="Rounding")
N <- 25
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
obs.diff <- mean(dat.ns) - mean(dat.s)
SE <- sqrt(var(dat.ns)/N + var(dat.s)/N)
tstat <- qt(1- 0.01/2, df = 2*N-2)
ME <- tstat*SE
ME

#or
N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
##note that if you define dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both answers

#Q2
#N and thus the degrees of freedom is large enough to make the normal and t-distributions very similar.

#Q3
#NOT True
#From the original data alone, you can tell whether you have made a Type I error.

#Q4
set.seed(1, sample.kind="Rounding")
N <- 5
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)
t.test(dat.s,dat.ns)

#or
N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value
##Note that if you defone dat.s before dat.ns, you get a different answer
##due to sampling randomness
##tolerance is set to accept both

#Power Calculation Exercises
library(tidyverse)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke) #population mean diff
popsd(bwt.nonsmoke) #population standard deviation
popsd(bwt.smoke)

pop.diff <- mean(bwt.nonsmoke)-mean(bwt.smoke)

#Q1
N <- 5
B <- 10000
alpha.val <- 0.05
set.seed(1, sample.kind="Rounding")
reject <- function(N, alpha = alpha.val){ #reject is a function that takes input N, and default alpha value of 0.01
  dat.s <- sample(bwt.smoke,N)     #when called: 1. reject(), makes obj dat.s which stores a sample from bwt.smoke of sample size N, the reject() input  
  dat.ns <- sample(bwt.nonsmoke,N) #2. reject(), makes obj dat.ns which stores a sample from bwt.nonsmoke of sample size N, the reject() input 
  pval <- t.test(dat.s,dat.ns)$p.value# 3. it performs a t.test of the objs dat.s and dat.ns, which are samples of size N and stores the result to obj pval
  pval < alpha                     #4. lastly, it evaluates if pval is less than alpha, in this case, alpha was assigned the value of 0.01
}                                  #Therefore, reject func either returns TRUE or FALSE
reject() #calls the reject func, since it takes the value of obj N at default, it does not need any input
         #however, one can change N, the sampl size before calling the func
         #ex. reject(50, alpha = 0.10)
          

#Q2
rejections <- replicate(B, reject(N)) #replicate B times, in this case 10000, the function reject() 
                                      #it stores the 10000 values of either TRUEs or FALSEs in obj rejections 
mean(rejections)                      #counts the number of TRUEs and divides by B or 10000; proportion of TRUEs
                                      #Prop of times wer reject Null at alpha 0.05; this is power

#or
N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)

#Q3
alpha.val <- 0.05
Ns <- c(30,60,90,120)   #vector of values 30,60,90,120. These are to be used as sample sizes
power <- sapply(Ns,function(Ns){ #sapply applies a function(to be specified) which takes each element of the vector Ns, which are 30,60,90,120 as input
  rejections <- replicate(B, reject(Ns))# replicates B times the reject(), and stores the values in obj rejections
  mean(rejections)                     #counts all TRUEs in obs rejections and divides by B; 
  })                                   #the obj power has 4 numeric elements, each element is a prop of rejections for each element in Ns
plot(Ns, power, type="b") #plots Ns to x-axis, power to y-axis, type = b makes points and lines, see ?plot
Ns[ which.min( abs( power - .8) ) ] #takes all 4 elements of Ns substracts each of them by 0.8; convert abs diff 
                                    #returns the element in Ns which has the minimum (lowest) abs diff when subtracted by 0.8
#or
Ns=c(30,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] #takes a vector slice; subsetting the values of Ns
                                  #isolates the minimum element(s) in Ns which gives y value of at least 0.8

#Q4
alpha.val <- 0.01 #change reject func to alpha = 0.01
N <- c(30,60,90,120)
power <- sapply(Ns,function(Ns){
  rejections <- replicate(B, reject(Ns, alpha = alpha.val))
  mean(rejections)
})
plot(Ns, power, type="b")
Ns[ which.min( abs( power - .8) ) ] 


#or 
Ns=c(30,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 


#Monte Carlo Exercises

#Q1
set.seed(1, sample.kind = "Rounding")
N <- 5
sample <- rnorm(N)
t <- sqrt(N)*mean(sample)/sd(sample)
t

#Q2
set.seed(1, sample.kind = "Rounding")
N <- 5
B <- 1000
tstats <- replicate(B, {
  sample <- rnorm(N)
  sqrt(N)*mean(sample)/sd(sample)
  } )
mean(tstats > 2)



#Q3
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

#Q4
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = T)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

#Q5
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined

#Q6
set.seed(1, sample.kind = "Rounding")
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)
#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same.

#Q7
library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*median(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 

#Permutation Exercises

#controllibrary(tidyverse)
#controllibrary(rafalib)
#controlbabies <- read.table("babies.txt", header=TRUE)
#controlbwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
#controlbwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
#controlN <- 10
#controlset.seed(1, sample.kind = "Rounding")
#controlnonsmokers <- sample(bwt.nonsmoke , N)
#controlsmokers <- sample(bwt.smoke , N)
#controlobs.diff <- mean(smokers) - mean(nonsmokers)

library(tidyverse)
library(rafalib)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N <- 10
set.seed(1, sample.kind = "Rounding")
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs.diff <- mean(smokers) - mean(nonsmokers)

dat <- sample(c(smokers,nonsmokers))
smokersstar <- dat[1:N]
nonsmokersstar <- dat[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar)

#controldat <- c(smokers,nonsmokers)
#controlshuffle <- sample( dat )
#controlsmokersstar <- shuffle[1:N]
#controlnonsmokersstar <- shuffle[(N+1):(2*N)]
#controlmean(smokersstar)-mean(nonsmokersstar)

#Q1
set.seed(1, sample.kind = "Rounding")
N <- 10
avg.diff <- replicate(1000, {
  dat <- sample(c(bwt.smoke,bwt.nonsmoke))
  smokersstar <- dat[1:N]
  nonsmokersstar <- dat[(N+1):(2*N)]
  return(mean(smokersstar) - mean(nonsmokersstar))
})
hist(avg.diff)
abline(v=obs.diff, col="red", lwd=2)
(sum(abs(avg.diff) > abs(obs.diff)) + 1) / (length(avg.diff) + 1)
#or
et.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

#Q2
library(tidyverse)
library(rafalib)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
N <- 10
set.seed(1, sample.kind = "Rounding")
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs.diff <- median(smokers) - median(nonsmokers)

set.seed(1, sample.kind = "Rounding")
avg.diff <- replicate(1000, {
  dat <- sample(c(smokers,nonsmokers))
  smokersstar <- dat[1:N]
  nonsmokersstar <- dat[(N+1):(2*N)]
  median(smokersstar) - median(nonsmokersstar)
})
hist(avg.diff)
abline(v=obs.diff, col="red", lwd=2)
(sum(abs(avg.diff) >= abs(obs.diff)) + 1) / (length(avg.diff) + 1)

#Assoc Test
#Q1
d <-  read.csv("assoctest.csv")
tab <- table(d)
chisq.test(tab)

#Q2
fisher.test(tab) 


