#Midterm Key

#Please use this key to grade your midterm. Please annotate any changes you make to your document. Try to see why you might have gotten something wrong. If you dont know, come see me soon.
#Because Liam is the worst TA in the history of Canvas, Lets say this is due next Friday, November 10th. Feel free to work with others on this.

#Name:

##################################################################

#1. Below are field measurements of egg mass of treefrogs (We will assume a normal distribution).
egg.mass<-c(3.1, 4.6, 3.9, 3.4, 3.6, 3.7, 3.8, 4.6, 4.7, 4.3, 2.6, 6, 4.7, 3.4, 3.1, 4.9, 3.8, 5.3, 4.4, 5.9, 3.6, 5.1, 5.2, 3.6, 5.3, 2.9, 4.7, 4.7, 3.8, 6.5, 5.1, 6.3, 5.1, 2.7, 4.4, 4.3, 3.7, 5.5, 3.2, 5.6, 4, 2.3, 5.2, 4.4, 4.8, 3.6, 3.7, 4.2, 5.3, 5.3, 5.6, 4.1, 4.1, 5, 4.1, 5, 3.3, 5.9, 5.7)


#A.  Based upon our parameter estimates for a normal distribution, what is the equal tailed 95% interval for any sample drawn from this population (i.e., that is the expected range of 95% of our sample)?

mu <- mean(egg.mass)
sd <- sd(egg.mass)
mu+1.96 * sd #6.328
mu-1.96 * sd #2.509
#B. What is the equal tailed 95% interval for OUR sample?

quantile(egg.mass, c(0.025, 0.975))

#2.645 6.165

#C. If we were to sample this population tens of thousands of times, what is the expected standard deviation of the means?

se <- sd/sqrt(length(egg.mass)) # 0.1269

# the expected standard deviation of the means is the standard error

#D. Calculate the 95% confidence interval
mu + 1.96 * se # 4.667
mu - 1.96 * se # 4.17

#E. Based on our parameter estimates, what is the probability that a sampled egg from the population has an egg mass equal to or less than 3.3

pnorm(3.3, mu, sd) # 0.1255

##################################################################

#2.  You've sampled tunicates (sessile marine chordates) from m^2 quadrates in Oregon (the data is below).  Based on these data, answer the following questions.

tunicates<-c(13,9,7,4,5,9,5,4,5,6,8,8,6,9,9,5,8,5,6,7,14,9,3,13,9,5,8,4,5,3,6,9,3,2,10,6,8,8,6,9,12,4,19,7)

#A  What is an appropriate distribution to use to model this data?

#poisson

#B  What is/are the parameter estimate(s) for this distribution?

lambda <- mean(tunicates) # 7.273

#C What is the probability that your next quadrate will have 3 or fewer tunicates?

ppois(3, lambda) # 0.0686

#D What is the probability that your next quadrate will have 9 or greater tunicates?

ppois(8, lambda, lower.tail=FALSE) # 0.307

#E Based on our parameter estimates, what is the probability that a quadrat has between 4 and 5 tunicates?
ppois(3,lambda,lower.tail = F)-ppois(5,lambda,lower.tail = FALSE)

#0.1986295

#######################################################

#3. Suppose 0.5% of all students seeking treatment at a school infirmary are eventually diagnosed as having mononucleosis.  Of those who do have mono, 90% complain of a sore throat.  But 30% of those who do not have mono also claim to have sore throats.  If a student comes to the infirmary and says he or she has a sore throat, what is the probability the student has mono?


#To say in a bays way, what is the probability of mono given a sore throat?

ProbOfMono<-0.005
ProbOfNotMono<-1-ProbOfMono
ProbOfModel<-0.9 # if you have mono, what is the probability you test positive (have a sore throat?)
ProbOfData<- ProbOfModel*ProbOfMono + 0.3*ProbOfNotMono #All the ways to get a positive test(sore throat)

(ProbOfModel*ProbOfMono)/ProbOfData #answer 1.5%


######################################################
#4.  Here are some data on fish mouth gape and gill raker length. Based on this data, answer the following questions.

gape.cm<-c(100.3, 97.2, 100.8, 105, 102, 101.1, 100, 105.6, 100.8, 102.5, 100.2, 105.5, 103.5, 98.6, 99.3, 98.9, 102.1, 97.7, 98.5, 99.4, 100.3, 100.1, 100.8, 101.2, 95.5, 99.3, 96.5, 97.8, 101.5, 100.4, 101.1, 95.6, 99.1, 95.5, 104.8, 102.4, 98, 101, 101.3, 103.4, 98.6, 98, 99.5, 96.7, 99.5, 100.4, 100.3, 96.2, 98.4)

gill.raker.length.mm<-c(250.7, 230, 257.6, 241.2, 247.7, 252, 251.5, 261.5, 242.7, 245.4, 265.9, 256.1, 238.3, 256.9, 236.2, 244.3, 247.2, 235.7, 253.8, 247.8, 242.4, 251.7, 251.3, 245.5, 237.5, 249.6, 228.8, 241.8, 253.5, 244.1, 242.3, 240.5, 251.6, 241, 253.4, 248.9, 232.5, 250.8, 252.4, 245.9, 238.8, 241.9, 225.1, 232.1, 239.9, 236.1, 253.8, 223.4, 233.2)

#A  What is the pearson-product moment correlation for these data?

cor(gape.cm, gill.raker.length.mm) # 0.526

#B  What is the 99% confidence interval around this correlation?

cor.test(gape.cm, gill.raker.length.mm, conf.level = 0.99)$conf.int[c(1,2)]
#  0.2024, 0.7464

#C  What units are correlation in?

sds/unitless
#D  What is the covariance between these two?

cov(gill.raker.length.mm, gape.cm)
#E What units are covariance in?

#cm*mm 

##################################################################
#5. Below are data on total leaf lesions due to viral infection and average nectar volume of flowers (microliters) of deadly nightshade (Atropa belladona). We are interested in knowing whether the number of leaf lesions can predict nectar volume.

#A. Make a pretty figure showing the data, your results, and the 95% confidence interval around the fitted values. Use these data to address the following questions.

nectar.ul <- c(132.6,125.66,133.99,129.78,124.59,131.93,131.49,131.95,130.88,129.6,126.09,132.59,133.5,130.5,133.35,130.46,131.84,136.92,129.24,130.81,127.19,127.05,125.32,127.71,127.44,130.94,125.5,125.3,132.04,132.65,129.04,126.43,122.96,126.15,125.35,127.01,133.2,136.29,135.59,130.46,125.07,132.54,130.3,124.04,131.43,128.21,132.81,131.45,132.25,130.21,125.4,129.43,126.75,131.9,130.34,132.96,127.61,130.39,130.19,129.27,133.51,127.37,131.83,128.48,132.39,131.06,136.65,130.95,134.02,130.87,130.6,130.72,128.86,129.84,128.69,130.5,126.97,129.58,129.43,131.76,128.49,135.49,126.94,130.11,127.82,132.24,122.45,131.49,130.26,131.61,127.09,133.37,128.22,127.17,126.86,128.83,131.12,131.71,130.56,130.21)

lesions <- c(51,17,25,28,29,22,33,50,38,17,38,15,5,29,44,22,46,28,29,32,17,20,42,46,19,27,33,16,44,45,16,49,40,30,21,25,28,43,25,27,36,31,8,19,33,34,30,33,45,34,19,22,34,23,36,19,33,18,34,28,57,7,30,13,36,37,32,31,31,35,45,29,42,47,30,27,13,41,21,36,8,39,22,20,42,33,30,34,23,42,22,27,35,25,31,16,22,30,37,23)


#B. Use an anova to test the null hypothesis that the slope is different from zero.

mod <-lm(nectar.ul ~ lesions)
anova(mod)
mod

# the null hypothesis is that the slope is equal to zero

#C. Theory predicts that nectar volume should INCREASE as the number of lesions increases. Test this hypothesis.
summary(mod)
tstat <- 0.05456/0.0279
pt(tstat, df=length(lesions)-2, lower.tail=FALSE)
## 0.026

#D.  Based on your parameter estimates, what is the 95% confidence interval around the fitted values of a plant with 62 lesions? 

pred <- data.frame(lesions = 62)
conf62 <- predict(mod, int="confidence", newdata=pred)


predict(mod, pred, interval="confidence")

#131.6569 129.7794 133.5344

##################################################################



#Given the data y below, what is the probability AND log likelihood that the data are drawn from the following distributions?

y<-c(1.2,3.4,4.4,5.2)

#A. A normal distribution with a mean of 2 and standard deviation of 3.
sum(dnorm(y,2,3,log = TRUE))

# -9.103537

#B. A normal distribution with a mean of 3 and a standard deviation of 1.
sum(dnorm(y,3,1,log = TRUE))

 #-8.775754

#What is the probability and log likelihood of the data based on the maximum likelihood estimate of the mean and standard deviation?
sum(dnorm(y,mean(y),sd(y),log = TRUE))

#-7.370755
