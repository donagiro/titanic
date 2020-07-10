# INTIAL KILA PROJECT: JAKE AND DON
# Titanic: a fun decryption of who got to die!!
# import data from source for analysis "if data is in text format remember to declare (header=True)"
titanic.raw <- read.csv("C:/users/Don Krieg/Documents/R/titanic/cleaned_tit/train1.csv")

# view data to ensure you have the right content loaded
View(titanic.raw)

# remove unnecessary columns or vectors from data frame
titanic.raw <- titanic.raw[,1:14]
View(titanic.raw)

#check for incomplete cases
length(which(!complete.cases(titanic.raw)))
head(titanic.raw)

# we run the str() function to check out table/data_frame structure i.e.
# what are the data types of each column? are they factors, integers, strings, arrays etc  
str(titanic.raw)

# after getting an outlook of the structure, we now convert the necessary columns to factors as needed
# we can always revert them or change them to different data classes as needed
titanic.raw$Survived <- as.factor(titanic.raw$Survived)
titanic.raw$Pclass <- as.factor(titanic.raw$Pclass)
titanic.raw$SibSp <- as.factor(titanic.raw$SibSp)
titanic.raw$Parch <- as.factor(titanic.raw$Parch)

# xtabs is use for pulling two columns showing us there stats on a two way table
# one thing xtabs are useful for is they help show us that the various columns being analyzed have 
# a good representation e.g at least 30 samples per column or whatever you deem a useful no. of samples
# the first xtab(two way table) compares the gender(sex) against the survival data
xtabs(~Survived + Sex, data = titanic.raw)

# the second xtab(two way table) compares the survival data to passenger class
# ideal you would do this for all the variables you want to input in your model, but it's not necessary tbh
xtabs(~Survived + Pclass, data = titanic.raw)

# first i try the Generalized Linear Model(glm) given its popularity working with two way decision analysis
# in this project we predict if someone lived or died
# we will try different models in the future
titanic.model = glm(Survived ~ Sex, data = titanic.raw, family = binomial())
summary(titanic.model)

####################################################################################################
### this is what you get when you run the summary function above::: summary(titanic.model)###########
### we explain the output for our draft code before fine tuning anything ###########################
### explanations are enclosed in double curly brackets "{{}}"|| THEY WON'T BE PART OF YOUR OUTPUT ##
####################################################################################################

######{{ "first line shows us the orgininal glm() function we called/ran" }}
# Call:
# glm(formula = Survived ~ Sex, family = binomial(), data = titanic.raw) 
#___________________________________________________________________________________________________ 
######{{ "this is a summary of the Deviance residuals":: they are close to being centered on zero and roughly symetrical }}
# Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
# -1.6462  -0.6471  -0.6471   0.7725   1.8256  
#___________________________________________________________________________________________________
#  Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)   1.0566     0.1290   8.191 2.58e-16 ***
#  Sexmale      -2.5137     0.1672 -15.036  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#  (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 1186.7  on 890  degrees of freedom
# Residual deviance:  917.8  on 889  degrees of freedom
#___________________________________________________________________________________________________
######{{ "AKAIKE INFORMATION CRITERION(AIC), in this context, this represents the residual deviance " }}
######{{ "adjusted for the number of Parameters in the model." }}
######{{ "the AIC can be used to compare one model to another, the lower the AIC, the better the model." }}
# AIC: 921.8
#___________________________________________________________________________________________________
# Number of Fisher Scoring iterations: 4
####################################################################################################

#we create data frame that contains the probabilities of Surviving along with the status of whether person survive or not
predicted.titanic <- data.frame(probability.of.survive=titanic.model$fitted.values, Survived=titanic.raw$Survived)

#Sort our table predicting the probability of survival
predicted.titanic <- predicted.titanic[order(predicted.titanic$probability.of.survive, decreasing = FALSE),]

#we add a new column to the data.frame that has the rank of each sample, from low probability to high probability
predicted.titanic$rank <- 1:nrow(predicted.titanic)
View(predicted.titanic)

#load ggplot library to draw fancy graphs of our prediction probabilities
library(ggplot2)

#load the cowplot library so that ggplot has nice looking defaults. 
library(cowplot)

predicted.titanic$probability.of.survive <- as.numeric(predicted.titanic$probability.of.survive)
predicted.titanic$rank <- as.integer(predicted.titanic$rank)
str(predicted.titanic)

predict.df <- as.data.frame(predicted.titanic)

#ggplot2() has multiple inbuilt types of graphing function you can use we will use geom_point() 
#to draw the data, try determine best way to present data to your audience
ggplot(data = predicted.titanic , aes(x=rank, y=probability.of.survive)) +
  geom_point(aes(color=Survived), alpha=1, shape=4, stroke=2) +
  xlab("index")
  ylab("Predicted probability of surviving")

#save our predicted probabilites graph as a pdf to share with your audience 
ggsave("Titanic_Survival_Probablilities.pdf")
