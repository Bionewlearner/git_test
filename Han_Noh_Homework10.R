#Return to eebbiostatistics@gmail.com by 10:10am, Friday April 20.
#   on the subject line of the email put    YOURLASTNAME_EXERCISE
#   Save the .R file as "First name_Last name_Homework10"

#Name:

#Download and read in the file "AlgalDataHW10.csv" and use it to adress the following:

#1. While diving off Pupukea Beach you notice an algae that you have never seen before. You wonder if the parrotfish you see at other beaches are able to keep down this algal invader. You want to test the question: Do parrotfish have an effect on invasive Macroalgae cover? You suspect there might be an effect of water depth as well. You acquire permits to work at 5 different beaches and enough cages for 160 plots. Your assignment is to analyze the data with the appropriate statistical model and address the research question above. Be sure to annotate your code, show all of your work, and write a short paragraph summarizing your 'findings' (like 3 sentences).

#Question:Do parrotfish have an effect on invasive Macroalgae cover?
#Random Effect:Water depth 
#5 different beaches and 160 different plots


Algaeproject<- read.csv("AlgalDataHW10.csv")
attach(Algaeproject)
library(lmertest)
package(lme4)
Model <- lmer(Invasive.Algae.Mass~Location+(1|Treatment),data=Algaeproject) #Making a new model for treatment(cage)
anova(Model) #Ran anova test on model

step(lm(formula = lm(Invasive.Algae.Mass~Treatment+Location+Water.Depth+Treatment:Location,data=Algaeproject))) #Applied step func.
library(car)
BigMod <- lmer(Invasive.Algae.Mass~Treatment+Water.Depth+Location+(1+Water.Depth|Location),data=Algaeproject) #Made whole model
Bigmod1<- lmer(Invasive.Algae.Mass~Treatment+Water.Depth+Location+(1|Water.Depth),data=Algaeproject)
Bigmod2<- lmer(Invasive.Algae.Mass~Treatment+Location+Water.Depth+(1|Water.Depth),data=Algaeproject)
Bigwholemode <-lmer(Invasive.Algae.Mass~Location+(1|Water.Depth/Treatment))


Bigmode <-lmer(Invasive.Algae.Mass~Invasive.Algae.Mass+(1|Water.Depth/Treatment))

anova(BigMod) #Ran anova on whole model
anova(Bigmod1)
summary(Bigmod1)
summary(BigMod)
summary(Bigmod2)
#Random effect Summary(Bigmod2)
#Water.Depth (Intercept)  0.02985 0.1728 
#Residual:Residual 28.70 5.35 
#Due to big residual and small size of random effect, there we shouldn't consider the random effect.
aov.1<-aov(Invasive.Algae.Mass~Location)
Anova(Bigmod)
summary(aov.1)

interaction.plot(Treatment,Water.Depth,Invasive.Algae.Mass) #Looked into interactions

unloadNamespace("lmerTest") #Cleared lmerTest

anova(BigMod) #ran anova
Anova(BigMod) #ran Anova
#There is a signficiant interaction between Treatment:WaterDepth: one for deep water and one for shallow. 

Invasive.Algae.Mass.deep<-Invasive.Algae.Mass[Water.Depth=="deep"]
Treatment.deep<-Treatment[Water.Depth=="deep"]
Water.depth.deep<-Water.Depth[Water.Depth=="deep"]
Location.deep<-Location[Water.Depth=="deep"] 
#creating Deep depth model

Invasive.Algae.Mass.shallow<-Invasive.Algae.Mass[Water.Depth=="shallow"]
Treatment.shallow<-Treatment[Water.Depth=="shallow"]
Water.depth.shallow<-Water.Depth[Water.Depth=="shallow"]
Location.shallow<-Location[Water.Depth=="shallow"]
#Creating Shallow depth model

Original_ShalLow_Mod <- lmer(Invasive.Algae.Mass.shallow~Treatment.shallow+(1+Treatment.shallow|Location.shallow))
Shallow_Mod <- lmer(Invasive.Algae.Mass.shallow~Treatment.shallow+(1+Treatment.shallow|Location.shallow))
anova(Original_ShalLow_Mod,Shallow_Mod)


summary(Shallow_Mod)

#Finding
#There is also water depth effect
#parrotfish do have an effect on invasive Macroalgae cover. an effect of water depth was not really signficantly effect on Invasive marcroalgae cover. According to our data, parrot fish do have an effect in deep areas for both caged and non caged. 

Deep_Mod <- lmer(Invasive.Algae.Mass.deep~Treatment.deep+(1+Treatment.deep|Location.deep))
anova(Deep_Mod)
summary(Deep_Mod)
plot(Deep_Mod)