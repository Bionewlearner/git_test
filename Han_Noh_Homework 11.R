#Return to eebbiostatistics@gmail.com by 10:10am, Friday April 27.
#   on the subject line of the email put    YOURLASTNAME_EXERCISE
#   Save the .R file as "First name_Last name_Homework11"

#Name:Han Noh


#1) Measuring the survival of butterflies is your new favorite thing. You want to determine if introduced parasitic wasps will influence survival rate. To do this, you place 10 caterpillars on each of 100 host plants, each plant in its own cage. In 50 of the cages you introduce a parasitic wasp. You are looking to address the hypothesis: parasitoid presence has an effect on butterfly survival.  Load in the R object butterflysurvival.R that has the following data: the number alive, the number dead.
load("C:/Users/hnoh2/Downloads/butterflysurvival.R") #load the R
attach(Dat)
library(MASS)


#A. Build a glm to test the hypothesis that treatment has an effect on survivorship.
#hypothesis: parasitoid presence has an effect on butterfly survival

#treat is like 0 and 1

modelx<-glm(cbind(live,dead)~treat,family = "binomial")
summary(modelx)

#B. How many times more likely is a butterfly to die in the presence of the parasitoid?

#possion distribution
#The coefficient of numeracy is: 0.380, so that a one unit change in numeracy produces approximately a 0.385 unit change in log scale.
exp(0.380) #For every year increase, the odds of being dead is increased by 1.46 times

