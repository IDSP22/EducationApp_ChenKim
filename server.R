### Load packages
require(pacman)
pacman::p_load(shiny,
               shinyFeedback,
               shinyWidgets,
               shinyalert,
               tidyverse,
               ggplot2,
               magrittr,
               shinyjs,
               gt,
               gtsummary, 
               waiter,
               sortable, 
               ggthemr, 
               caret, 
               boot)

# Define server logic 
shinyServer(function(input, output) {

    ## Definition quiz
    output$definition_quiz_res <- renderText({
        req(input$definition_quiz_submit)
        res <- isolate(
            if(input$definition_quiz1=="Pizza" &
                           input$definition_quiz2 =="Getting sick (nausea and diarrhea)"){
               "<span style=\"color:#0b5b67\"> Correct answers! 
               Click on Next to go to the next page</span>"
                }else if(input$definition_quiz1=="Pizza" &
                         input$definition_quiz2 !="Getting sick (nausea and diarrhea)"){
                    "<span style=\"color:#C34129\"> Uh oh, you only got the Question 1 correct. Try again.</span>"
                }else if(input$definition_quiz1!="Pizza" &
                         input$definition_quiz2 =="Getting sick (nausea and diarrhea)"){
                    "<span style=\"color:#C34129\"> Uh oh, you only got the Question 2 correct. Try again.</span>"
                }else{
                    "<span style=\"color:#C34129\"> Uh oh, wrong answers. Try again.</span>"
                }
               )
        return(res)
    })
    
    ## Interpret quiz
    output$interpret_quiz_res <- renderText({
        req(input$interpret_quiz_submit)
        ifelse(isolate(input$interpret_quiz=="No"),
               "<span style=\"color:#0b5b67\"> Correct answer! 
               Click on Next to go to the next page</span>", 
               "<span style=\"color:#C34129\"> Wrong answer. 
               Confidence interval spanning across null indicates inconclusive evidence</span>")
        
    })
    
    #### From here to the end is the step controls ####
    # intro to definition
    observeEvent(input$`start`, {
        req(input$`start`)
        updateTabsetPanel(inputId = "steps", selected = "definition")
    })
    
    # definition to calculate
    observeEvent(input$`1_2`, {
        req(input$`1_2`)
        updateTabsetPanel(inputId = "steps", selected = "calculate")
    })
    
    # definition to intro
    observeEvent(input$`1_0`, {
        req(input$`1_0`)
        updateTabsetPanel(inputId = "steps", selected = "intro")
    })
    
    # calculate to when_used
    observeEvent(input$`2_3`, {
        req(input$`2_3`)
        updateTabsetPanel(inputId = "steps", selected = "when_used")
    })
    
    # calculate to definition
    observeEvent(input$`2_1`, {
        req(input$`2_1`)
        updateTabsetPanel(inputId = "steps", selected = "definition")
    })
    
    # when_used to true_vs_sample
    observeEvent(input$`3_4`, {
        req(input$`3_4`)
        updateTabsetPanel(inputId = "steps", selected = "true_vs_sample")
    })
    
    # when used to calculate
    observeEvent(input$`3_2`, {
        req(input$`3_2`)
        updateTabsetPanel(inputId = "steps", selected = "calculate")
    })
    
    # true_vs_sample to conf_int
    observeEvent(input$`4_5`, {
        req(input$`4_5`)
        updateTabsetPanel(inputId = "steps", selected = "conf_int")
    })
    
    # true_vs_sample to when_used
    observeEvent(input$`4_3`, {
        req(input$`4_3`)
        updateTabsetPanel(inputId = "steps", selected = "when_used")
    })
    
    # conf_int to interpret
    observeEvent(input$`5_6`, {
        req(input$`5_6`)
        updateTabsetPanel(inputId = "steps", selected = "interpret")
    })
    
    # conf_int to true_vs_sample
    observeEvent(input$`5_4`, {
        req(input$`5_4`)
        updateTabsetPanel(inputId = "steps", selected = "true_vs_sample")
    })
    
    # interpret to try_yours
    observeEvent(input$`6_7`, {
        req(input$`6_7`)
        updateTabsetPanel(inputId = "steps", selected = "try_yours")
    })
    
    # interpret to conf_int
    observeEvent(input$`6_5`, {
        req(input$`6_5`)
        updateTabsetPanel(inputId = "steps", selected = "conf_int")
    })
})
