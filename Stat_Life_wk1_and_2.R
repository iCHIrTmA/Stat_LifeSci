dat <- read.csv("femaleMiceWeights.csv")
library(dplyr)
controls <- dat %>% 
  filter(Diet == "chow")
controls <- dat %>% 
  filter(Diet == "chow") %>% 
  select(Bodyweight) %>% 
  unlist(controls)
# From R 3.0.0 onwards mean(<data.frame>) is defunct
# Alternatives: lapply(<data.frame>, mean, na.rm = TRUE)
#               sapply(<data.frame>, mean, na.rm = TRUE)
#               colMeans(<data.frame>, na.rm = TRUE)
#               summary(<data.frame>)
sapply(results, mean, na.rm = TRUE)

#dplyr_exercises
#Q1
msleep <- read.csv("msleep_ggplot2.csv") 
class(msleep)

#Q2
primates <- msleep %>% 
  filter(order == "Primates") 
nrow(primates)

#Q3
class(primates)

#Q4
primates <- msleep %>% 
  filter(order == "Primates") %>% 
  select(sleep_total)
class(primates)

#Q5
sapply(primates, mean)

#Q6
primates <- msleep %>% 
  filter(order == "Primates") %>% 
  select(sleep_total) %>% 
  summarize(sleep_total = mean(sleep_total))

#Intro Random Variables

treatment <- dat %>% 
  filter(Diet == "hf") %>% 
  select(Bodyweight) %>% 
  unlist

sapply(treatment, mean, na.rm = TRUE)  - sapply(controls, mean, na.rm = TRUE)

pop_fmice <- read.csv("femaleControlsPopulation.csv")

mean(sample(pop_fmice, 12))

#Random Variable Exercises

sapply(pop_fmice, mean, na.rm = TRUE) 
#23.89338 

set.seed(1, sample.kind="Rounding")
pop_fmice <- unlist(pop_fmice)
samp_fmice <- sample(pop_fmice, 5)
abs(sapply(as.data.frame(pop_fmice), mean, na.rm = TRUE) - mean(samp_fmice))
#0.2706222 

set.seed(5, sample.kind="Rounding")
samp_fmice <- sample(pop_fmice, 5) #takes a new set of sample, values differ from previous sample 
abs(sapply(as.data.frame(pop_fmice), mean, na.rm = TRUE) - mean(samp_fmice))
#1.433378 

#Intro Null Distributions
obs <- sapply(treatment, mean, na.rm = TRUE)  - sapply(controls, mean, na.rm = TRUE)
pop_control_fmice <- read.csv("femaleControlsPopulation.csv")

control <- sample(unlist(pop_control_fmice), 12)
treatment <- sample(unlist(pop_control_fmice), 12)

mean(treatment) - mean(control) #Get diff values each time ranging from - to + because of random sampling


n <- 10000
nulls <- vector("numeric", n)
for(i in 1:n){
    control <- sample(unlist(pop_control_fmice), 12)
    treatment <- sample(unlist(pop_control_fmice), 12) 
    nulls[i] <- mean(treatment) - mean(control)
    }

max(nulls)

hist(nulls)

mean(nulls > obs)
or
sum(nulls > obs) / n
#0.0129

mean(abs(nulls) > obs)
#0.026

#Null Dist Exercises

#Ques1
set.seed(1)
n <- 1000
x <- read.csv('femaleControlsPopulation.csv')
x <- unlist(x)
averages5 <- vector("numeric",n) #This creates empty vector names averages5, storing numeric data, length n 
for(i in 1:n){            #from 1 to n, make i value, since n is 1000, process would be repeated 1000 times, and 1000 i values are created
  X <- sample(unlist(x),5) #get sample of size 5 from x
  averages5[i] <- mean(X) #get mean of the sample and store it to object averages5
}
averages5 <- unlist(averages5)
hist(averages5)
mean( (abs( averages5 - mean(x) )) > 1) #innermost: substracts from the 1000 sample means in averages5 the mean of population 
                                        #gets absolute value of the difference between 1000 sample means and population mean
                                        #outermost: to get proportion we use mean func, 
                                        #THis gives the proportion of absolute differences that are 1 gram away, hence abs(averages5 - mean(x)) > 1 
#0.498

#Probability Distributions Exercises


#Q1
library(gapminder)
x <- gapminder %>% filter(year == "1952") %>% select(lifeExp)
x <- unlist(x)
hist(x)
mean(x <= 40) #0.2887324


#Q2
mean(x <= 60) - mean(x <= 40)
#0.4647887

#sapply() on a custom function
prop = function(q) { # makes func prop which takes argument q
    mean(x <= q) }   # the func prop compares if x values are less than or equal to q
                     # this returns to either TRUE or FALSE
                     # mean func counts all TRUE and divides by total number of observation thus giving a prop
#building a range to apply func prop
qs = seq(from=min(x), to=max(x), length=20) # creates object qs which contains sequence of values
                                            # ranging from the minimum to max value of x with length 20
                                            # since there are 142 observations in x, it gets 20 quantiles

props = sapply(qs, prop) # apply to all columns of qs func prop
                         # instead of taking q as argument, func prop takes
                         # ALL 20 values in the obj qs
                         # props stores all 20 props

plot(qs, props)
#We could also write in one line
props = sapply(qs, function(q) mean(x <= q)) # we created the func prop, inside sapply itself
                                            # rather than creating it separately and calling it inside sapply
#pre buit-in func in R
plot(ecdf(x)) # this gives same result

#Normal Distribution Exercises

#Ques1
x <- read.csv('femaleControlsPopulation.csv')
x <- unlist(x)
set.seed(1, sample.kind="Rounding")
n <- 1000
xbar <- vector("numeric", n)
for(i in 1:n){
  samp1 <- sample(x, 5)
  xbar[i] <- mean(samp1)
}
set.seed(1, sample.kind="Rounding")
hist(xbar)
n <- 1000
xbar <- vector("numeric", n)
for(i in 1:n){
  samp1 <- sample(x, 50)
  xbar[i] <- mean(samp1)
  }
hist(xbar)

#Ques2
n <- 1000
xbar <- vector("numeric", n)
for(i in 1:n){
  samp1 <- sample(x, 50)
  xbar[i] <- mean(samp1)
}
hist(xbar)

mean(xbar <= 25) - mean(xbar <= 23) #this takes proportion of TRUEs where xbar is less than or equal to 25
                                    #and subtracted by the proportion of TRUEs where xbar is less than or equal to 23
                                    #difference is the area between 25 and 23

#Ques3
pnorm(25, mean = 23.9, sd = 0.43) - pnorm(23, mean = 23.9, sd = 0.43)


#Population, Samples, and Estimates Exercises
#Ques1
dat <- read.csv("mice_pheno.csv")
dat <- na.omit( dat )
library(tidyverse)
X <- dat %>% filter(Diet == "chow", Sex == "M") %>% select(Bodyweight) %>% unlist
mean(X)

#Ques2
install.packages("rafalib")
library(rafalib)
popsd(X)

#Ques3
set.seed(1, sample.kind="Rounding")
X <- unlist(X)
n <- 1
xbar <- vector("numeric", n)
for(i in 1:n){
  samp <- sample(X, 25)
  xbar[i] <- mean(samp)
  }
xbar

#Ques4
Y <- dat %>% filter(Diet == "hf", Sex == "M") %>% select(Bodyweight) %>% unlist
mean(Y)

#Ques5
 popsd(Y)

#Ques6
 set.seed(1, sample.kind="Rounding")
ybar <- sample(Y, 25)
  mean(ybar)
  
#Ques7
set.seed(1, sample.kind="Rounding")
X <- unlist(X)
n <- 1
xbar <- vector("numeric", n)
for(i in 1:n){
    samp <- sample(X, 25)
    xbar[i] <- mean(samp)
}

set.seed(1, sample.kind="Rounding")
Y <- unlist(Y)
ybar <- vector("numeric", n)
for(i in 1:n){
    samp <- sample(Y, 25)
    ybar[i] <- mean(samp)
  }
a <- ybar - xbar
b <- mean(unlist(Y)) - mean(unlist(X))
a - b 

#Ques8
X <- dat %>% filter(Diet == "chow", Sex == "F") %>% select(Bodyweight)
Y <- dat %>% filter(Diet == "hf", Sex == "F") %>% select(Bodyweight)
set.seed(1, sample.kind="Rounding")
X <- unlist(X)
n <- 1
xbar <- vector("numeric", n)
for(i in 1:n){
  samp <- sample(X, 25)
  xbar[i] <- mean(samp)
}

set.seed(1, sample.kind="Rounding")
Y <- unlist(Y)
ybar <- vector("numeric", n)
for(i in 1:n){
  samp <- sample(Y, 25)#Y is a set of values
  ybar[i] <- mean(samp)
}
a <- ybar - xbar
b <- mean(unlist(Y)) - mean(unlist(X))
a - b 

#Ques9

popsd(X)
popsd(Y)



#Week 2 Central Limit Theorem Exercises 

#Ques1
pnorm(1) - pnorm(-1) #without specifying mean and sd, pnorm uses its default values: mean = 0, sd = 1
                     #this is the standard normal curve
                     #pnorm(1) computes the prop of values below 1 sd from mean
                     #pnorm(-1) computes the prop of values below -1 sd from mean
                     #the diff is the prop of values betw sd 1 and sd -1
#Ques2
pnorm(2)-pnorm(-2)


#Ques3
pnorm(3)-pnorm(-3)


#Ques4
y <- dat %>% filter(Sex == "M", Diet == "chow") %>% select(Bodyweight)
ybar <- mean(unlist(y))
y_popsd <- popsd(unlist(y))
z_score <- (y - ybar)/y_popsd #the obj z_score contains the z scores of the 223 observations in obj y 
mean(abs(z_score) <= 1) #innermost: abs() converts all values in z_score into their absolute value,
                        #outermost: mean() checks the logical statement inside the()
                        #in this case, it checks each indiv element of z_score if it is <= 1
                        #it returns TRUE if they are <= 1, otherwise it returns FALSE
                        #it counts all TRUE and divides it with total number of values in z_score
# or                    #the resulting value is the prop of TRUE statements
sum(abs(z_score) <= 1)/nrow(z_score)

#Check:
mean(abs(z_score) <= 1) == sum(abs(z_score) <= 1)/nrow(z_score)

#Q5
mean(abs(z_score) <= 2)

#Q6
mean(abs(z_score) <= 3)

#Q7
qqnorm(unlist(z_score), type = "l") #normal qqnorm(z_score) does not work
abline(0,1)


#The mouse weights are well approximated by the normal distribution, 
#although the larger values (right tail) are larger than predicted by the normal. 
#This is consistent with the differences seen between question 3 and 6.

#Q8
#This just happens to be how nature behaves in this particular case. 
#Perhaps the result of many biological factors averaging out.

#Q9
set.seed(1, sample.kind="Rounding")
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
ybar <- replicate(10000, mean( sample(y, 25))) #innermost: takes a sample in obj y of sample size 25
                                               #then mean() gets the mean of the 25 randomly selected obs 
                                               #replicate() repeat the above process 10000 times
                                               #all 10000 sample means are stored in obj ybar
mypar(1,2)
hist(ybar)
qqnorm(ybar)
qqline(ybar)

mean(ybar) #takes mean of elements in ybar, this is the mean of all 10000 sample means

#Q10
popsd(ybar) #use popsd() instead of sd() because this is a sampling distr, a theoretical distr
            #we use popsd because sd() would divide squared sum of all deviations by n - 1, accounting for the -1 degress of freeedom
            #while popsd() divides squared sum of all deviations by n

#or
mu <- mean(ybar);
sqrt(sum((ybar - mu)^2)/length(ybar))

#Check:
sqrt(sum((ybar - mu)^2)/length(ybar)) == popsd(ybar)


#CLT and t-distribution in Practice Exercises
dat <- read.csv("femaleMiceWeights.csv")
#Q1
set.seed(1, sample.kind="Rounding")
n <- 100      #roll 100 dices
sides <- 6  
p <- 1/sides  #prob of success is 1/6; (i.e, there is 1/6 prob landing any number from 1 to 6)
zs <- replicate(10000,{    #replicate process 10000 times
  x <- sample(1:sides,n,replace=T)#get a sample of range 1 to 6, of size 100 w/ replacement
                                  # replace = T is essential because sampl size is greater than selection range
                                  # 100 > 6, this allows "recycling", if you pick 1, it would be returned and you could pick 1 again
                                  # obj x contains all results of the 100 dice rolls,
                                  # i.e. x is a sample of 100 dice rolls where each roll has a 1/6 chance of getting a number from 1 to 6
  (mean(x==6) - p) / sqrt(p*(1-p)/n)#Dividend:
                                    #mean(x==6) is the observed prop of 6 in the current sample, this may or may not be exactly 1/6 
                                    #p is the true prop or mu which is 1/6
                                    #mean(x==6) - p is the difference between the obs prop and true prop, 
                                    #Having a true prop of 1/6 does not mean one would observe exactly 1/6 prop of success at all times
                                    #In this context, the prob of landing a "6" when one rolls a 6-sided fair dice is 1/6
                                    #However, when rolling 100 dices one cannot expect that (1/6 * 100) or 16 out of the 100  dice rolls would show a "6"
  
                                    #Divisor: sqrt(p*(1-p)/n  = binom distr: standard error or sd of sampling distr of mean
                                    # if normal distr: s/sqrt(n), where s = samp sd and n= sampl size
                                    
                                    #the code mean(x==6) - p) / sqrt(p*(1-p)/n computes the z stat for a binom distr
})                                  #obj zs is a vector containing all 10000 zstats since the process is repeated 10000
qqnorm(zs)
abline(0,1)   
mean(abs(zs) > 2)                   #this gets the proportion of  all zs values that are greater than 2
                                    #since we converted all z stats to abs(), we also implicitly converted sd -2 and sd 2 to absolute values
                                    #this formula gives us the area right of sd 2, plus the area left of sd -2 in the normal distr with mean=0
                                    #this is combined area of the right tail and left tail
                                    
#Q2
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

#Q3
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

xbar <- mean(X) #mean of control group

#Q4

#Q5 
#mean of standard normal distr is 0

#Q6
s <- sd(X) #when pop sd is not known,
           #standard dev of sample is the estimate of pop sd 


#Q7
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )#Innermost: 2/sd(X) * sqrt(12)
                                    # 2 is the obs diff; 25.81 - 23.81
                                    #zstat for sample mean 2 grams larger or smaller than estimated pop mean (i.e 25.81 or 21.81 ), when popsd is not known
                                    #zstat = sqrt(n)(xbar-mu)/sd(X)
                                    #      = (sqrt(12)*(25.81333-23.81333))/sd(X)
                                    #in multiplication order does not matter
                                    #pnorm gets area <= zstat; 1 - pnorm gets > zstat
                                    #1 - pnorm() == pnorm(x, lower.tail=F)
                                    #multiply by 2 since we need to get 2 tails (xbar>2 sd and xbar<2 sd)

#Q8
SE <- sqrt(var(Y)/12 + var(X)/12) 


#Q9
xbar <- mean(X) #mean of control group (chow)
ybar <- mean(Y) #mean of treatment group (hf)
obs <- xbar - ybar #obs diff between mean control and mean treatment, in null, assumed 0
tstat <- obs/SE
tstat <- abs(tstat) #always take abs() of crit values, zstat and tstat 

#Q10
#Normal mean = 0, sd = 1

#Q11
Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)#take zstat of tstat
2*( 1-pnorm(Z))                                         #pvalue of 2 tails of zstat

#Q12
t.test(Y,X)$p.value

#Q13
#t-distr accounts for variability of SE















