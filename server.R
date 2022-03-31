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
               boot,
               DT)

# Define server logic 
shinyServer(function(input, output) {

    ### initialize blank data frame
    rv <- reactiveValues(
        calc_tab = {
            temp= data.frame(got_sick = c(0,0), didnt_get_sick = c(0,0))
            rownames(temp) <- c("ate_pizza", "didnt_eat_pizza")
            temp},
        when_used_tab = {
            temp= data.frame(persistent_suicidal_behavior = c(0,0), no_persistent_suicidal_behavior = c(0,0))
            rownames(temp) <- c("depression_at_baseline", "no_depression_at_baseline")
            temp
        }
    )
    
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
    
    ## Calculate quiz
    output$calculate_table <- renderDT({
        DT::datatable(rv$calc_tab %>% make_pretty(), 
                      editable = T,
                      options = list(
                          dom = 't',
                          ordering = F
                      ))    
    })
    
    observeEvent(input$calculate_table_cell_edit,{
        info = input$calculate_table_cell_edit
        i = as.numeric(info$row)
        j = as.numeric(info$col)
        val = as.numeric(info$value)
        rv$calc_tab[i,j]<-val
        
    })
    
    output$calculate_quiz1_res <- renderText({
        req(input$calculate_quiz1_submit)
        isolate({
            if(sum(as.matrix(rv$calc_tab)==
                   matrix(c(8,19,4,27),ncol=2,nrow=2,byrow=T))==4){
                "<span style=\"color:#0b5b67\"> Correct answer!</span>"
            }else{
                "<span style=\"color:#C34129\"> Wrong answer. Try again!</span>"
            }
        })
    })
    
    output$calculate_quiz2_res <- renderText({
        req(input$calculate_quiz2_submit)
        isolate({
            if(input$calculate_quiz2_1==8 &
               input$calculate_quiz2_2==19){
                "<span style=\"color:#0b5b67\"> Correct answer!</span>"
            }else{
                "<span style=\"color:#C34129\"> Wrong answer. Try again!</span>"
            }
        })
    })
    
    output$calculate_quiz3_res <- renderText({
        req(input$calculate_quiz3_submit)
        isolate({
            if(input$calculate_quiz3_1==4 &
               input$calculate_quiz3_2==27){
                "<span style=\"color:#0b5b67\"> Correct answer!</span>"
            }else{
                "<span style=\"color:#C34129\"> Wrong answer. Try again!</span>"
            }
        })
    })
    
    output$calculate_quiz4_res <- renderText({
        req(input$calculate_quiz4_submit)
        isolate({
            if(input$calculate_quiz4=="(8*27)/(4*19)"){
                "<span style=\"color:#0b5b67\"> Correct answer!</span>"
            }else{
                "<span style=\"color:#C34129\"> Wrong answer. Try again!</span>"
            }
        })
    })
    
    ## when_used quiz (cohort study)
    output$when_used_table <- renderDT({
        DT::datatable(rv$when_used_tab %>% make_pretty(), 
                      editable = T,
                      options = list(
                          dom = 't',
                          ordering = F
                      ))    
    })
    
    observeEvent(input$when_used_table_cell_edit,{
        info = input$when_used_table_cell_edit
        i = as.numeric(info$row)
        j = as.numeric(info$col)
        val = as.numeric(info$value)
        rv$when_used_tab[i,j]<-val
        
    })
    
    output$when_used_quiz1_res <- renderText({
        req(input$when_used_quiz1_submit)
        isolate({
            if(sum(as.matrix(rv$when_used_tab)==
                   matrix(c(45,86,32,100),ncol=2,nrow=2,byrow=T))==4){
                "<span style=\"color:#0b5b67\"> Correct answer!</span>"
            }else{
                "<span style=\"color:#C34129\"> Wrong answer. Try again!</span>"
            }
        })
    })
    
    output$when_used_quiz2_res <- renderText({
        req(input$when_used_quiz2_submit)
        isolate({
            if(input$when_used_quiz2=="1.63"){
                "<span style=\"color:#0b5b67\"> Correct answer!</span>"
            }else{
                "<span style=\"color:#C34129\"> Wrong answer. Try again!</span>"
            }
        })
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
    observeEvent(input$start, {
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
