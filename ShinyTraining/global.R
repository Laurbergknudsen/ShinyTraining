library(shinydashboard)
library(shiny)
library(shinythemes)
library(tidyverse)
library(titanic)

train <- titanic::titanic_train
test <- left_join(titanic::titanic_test,titanic::titanic_gender_model, by = c("PassengerId" = "PassengerId"))

train <- train %>% filter(!is.na(Age) , !is.na(Sex), !is.na(Embarked)) %>% 
  mutate(Survived = factor(ifelse(Survived == 1,"Yes", "No")),
         Embarked = ifelse(Embarked == "S", "Southampton",ifelse(Embarked == "C", "Cherbourg","Queenstown")))
test <- test %>% filter(!is.na(Age), !is.na(Sex) , !is.na(Embarked)) %>% 
  mutate(Survived = factor(ifelse(Survived == 1,"Yes", "No")),
         Embarked = ifelse(Embarked == "S", "Southampton",ifelse(Embarked == "C", "Cherbourg","Queenstown")))
total <-  rbind(train,test)


mdl = glm(Survived~Age+Sex+Embarked, data = train, family = "binomial")

test$mdl = predict(mdl,test, type = "response")

test$mdl = factor(ifelse(test$mdl > 0.5,"Yes","No"))


survival_rate <- total %>% 
  group_by(Survived) %>% 
  summarise(antal = n(), .groups = "drop") %>% 
  mutate(andel = 100*antal/sum(antal))







sex_choices <-unique(total$Sex)

embark <- total$Embarked %>% unique()


