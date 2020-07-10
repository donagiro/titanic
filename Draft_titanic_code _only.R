# INTIAL KILA PROJECT: JAKE AND DON
titanic.raw <- read.csv("C:/users/Don Krieg/Documents/R/titanic/cleaned_tit/train1.csv")

View(titanic.raw)

titanic.raw <- titanic.raw[,1:14]
View(titanic.raw)

length(which(!complete.cases(titanic.raw)))
head(titanic.raw)

str(titanic.raw)

titanic.raw$Survived <- as.factor(titanic.raw$Survived)
titanic.raw$Pclass <- as.factor(titanic.raw$Pclass)
titanic.raw$SibSp <- as.factor(titanic.raw$SibSp)
titanic.raw$Parch <- as.factor(titanic.raw$Parch)

xtabs(~Survived + Pclass, data = titanic.raw)

titanic.model = glm(Survived ~ Sex, data = titanic.raw, family = binomial())
summary(titanic.model)

predicted.titanic <- data.frame(probability.of.survive=titanic.model$fitted.values, Survived=titanic.raw$Survived)

predicted.titanic <- predicted.titanic[order(predicted.titanic$probability.of.survive, decreasing = FALSE),]

predicted.titanic$rank <- 1:nrow(predicted.titanic)
View(predicted.titanic)

library(ggplot2)

library(cowplot)

predicted.titanic$probability.of.survive <- as.numeric(predicted.titanic$probability.of.survive)
predicted.titanic$rank <- as.integer(predicted.titanic$rank)
str(predicted.titanic)

predict.df <- as.data.frame(predicted.titanic)

ggplot(data = predicted.titanic , aes(x=rank, y=probability.of.survive)) +
  geom_point(aes(color=Survived), alpha=1, shape=4, stroke=2) +
  xlab("index")
  ylab("Predicted probability of surviving")

ggsave("Titanic_Survival_Probablilities.pdf")
