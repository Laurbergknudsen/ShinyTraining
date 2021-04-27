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


server <- function(input, output) {

    output$rate <-  renderValueBox({
        last_row = tail(survival_rate,1)[3]
        valueBox(value = str_c(round(last_row,1),"%"), 
                 subtitle = paste0("Overall survival rate"), 
                 icon = icon("swimmer"),
                 color = "blue")
    })
    
    output$age <- renderValueBox({
        survival_age <- total %>% filter(Survived == "Yes") %>% 
            summarise(gns_alder = round(mean(Age, na.rm=T),0)) 
      
        valueBox(value = paste0(survival_age$gns_alder, " Years"), 
                 subtitle = paste0("Mean age of survivors"), 
                 icon = icon("hourglass-half"),
                 color = "blue")
                
    })
    
    
    output$sex_graph <- renderPlot({
        colors = c("#FF1654","#70c1b3")

        
        
        if(input$percentages) {
        if (input$sex_ == "both"){
        sg <-  total %>% 
        ggplot() + geom_bar(aes(x = Survived, y = (..count..)/sum(..count..), fill = Sex), stat = "count") +
            scale_fill_manual(values = colors) + ylab("Percentages")} else {
        sg <-  total %>% 
        filter(Sex == input$sex_) %>% 
        ggplot() + geom_bar(aes(x = Survived, y = 100*(..count..)/sum(..count..)), 
                            fill = ifelse(input$sex_ == "male","#70c1b3", "#FF1654"), stat = "count") +
            ylab("Percentage") +
            xlab(str_c("Survival status ", input$sex_))
            }
        
        } else {
            if (input$sex_ == "both"){
                sg <-  total %>% 
                    ggplot() + geom_bar(aes(x = Survived, fill = Sex), stat = "count") +
                    scale_fill_manual(values = colors)} else {
                        sg <-  total %>% 
                            filter(Sex == input$sex_) %>% 
                            ggplot() + geom_bar(aes(x = Survived), 
                                                fill = ifelse(input$sex_ == "male","#70c1b3", "#FF1654"), stat = "count") +
                            xlab(str_c("Survival status ", input$sex_))
                    }
            
        }
                
        
            print(sg)
    })
    
    
    output$survival_chance <- renderText({
        info <- data.frame(Age = input$age_,
                           Sex = input$gender_,
                           Embarked = input$embark_)
        
        chance <- predict(mdl,info, type = "response")
        
        table = table(test$Survived,test$mdl)
        
        print(paste0("You have a ", round(100*chance,2), "% chance of surviving*"))
        
        
    })
    

    
}




