library(shinydashboard)
library(shiny)
library(shinythemes)
library(tidyverse)
library(titanic)



ui <- shinydashboard::dashboardPage(
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
                           tabPanel("",
                                    fluidRow(column(width = 8, box(title = "Your info", status = "primary", width = 8,
                                                                   solidHeader = TRUE, 
                                                                   selectizeInput("gender_", "Gender:",
                                                                                  c(sex_choices)),
                                    
                                        numericInput(
                                            inputId = "age_",
                                            label = "Age:",
                                            value = 50,
                                            min = 1,
                                            max = 200,
                                            step = 1
                                        ),
                                    
                                        selectizeInput("embark_", "Port of embarkation:",
                                                       c(embark)),
                                        submitButton(text = "Check your chances of survival")
                                    )),
                                    
                                    
                                    
                                    
                                    
                                    
                                    
                                    ),
                                    fluidRow(
                                        column(width = 12, box(title = "", status = "primary", width = NULL, h1(textOutput(outputId = "survival_chance")))),
                                        column(width = 12, h5("*Note model is not validated and output is generated from a very simple binomial GLM"))
                                    ))
                           
                    )
                ),
                
            )
        )
    )
)
