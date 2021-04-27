library(shiny);library(shinydashboard);library(shinythemes);library(tidyverse)

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







sex_choices <- total$Sex %>% unique()

embark <- total$Embarked %>% unique()

ui <- dashboardPage(
    dashboardHeader(title = "Titanic"),
    dashboardSidebar(
    collapsed = FALSE,  
    sidebarMenu(
        id="dk",
        menuItem("Titanic survival info", 
                 tabName = "survival",
                 icon = icon("chart-bar")),
        menuItem("Would you survive", 
                 tabName = "test",
                 icon = icon("ship"))
    )
    ),
    


    dashboardBody(
        fluidRow(
            valueBoxOutput(width = 6, outputId = "rate"),
            valueBoxOutput(width = 6, outputId = "age"),
            
        ),
        tabItems(
            tabItem(
                tabName =  "survival",

                fluidRow(
                    tabBox(title = "", id = "age_gender", width = 12,
                           tabPanel("",
                                    fluidRow(column(width = 4, box(title = "Choose gender", status = "primary", width = NULL,
                                                                   solidHeader = TRUE, 
                                                                   selectizeInput("sex_", "Gender:",
                                                                               c("both",sex_choices)),
                                                                   checkboxInput(inputId = "percentages", label = "Show as percentages", value = FALSE),
                                                                   submitButton(text = "Make changes")
                                    ))),
                                    fluidRow(
                                        column(width = 12, box(title = "Survivors by gender", status = "primary", width = NULL, plotOutput(outputId = "sex_graph")))
                                    ))
                           
                )
                ),
                
            ),
            
            tabItem(
                tabName =  "test",
                
                fluidRow(
                    tabBox(title = "", id = "surivval", width = 12,
                           tabPanel("Your info",
                                    fluidRow(column(width = 8, box(title = "Your gender", status = "primary", width = 8,
                                                                   solidHeader = TRUE, 
                                                                   selectizeInput("gender_", "",
                                                                                  c(sex_choices)),
                                    
                                        numericInput(
                                            inputId = "age_",
                                            label = "",
                                            value = 50,
                                            min = 1,
                                            max = 200,
                                            step = 1
                                        ),
                                    
                                        selectizeInput("embark_", "",
                                                       c(embark)),
                                        submitButton(text = "Check your chances of survival")
                                    )),
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    ),
                                    fluidRow(
                                        column(width = 12, box(title = "Will you survive", status = "primary", width = NULL, h1(textOutput(outputId = "survival_chance")))),
                                        column(width = 12, h5("*Note model is not validated and output is generated from a very simple binomial GLM"))
                                    ))
                           
                    )
                ),
                
            )
        )
    )
)
