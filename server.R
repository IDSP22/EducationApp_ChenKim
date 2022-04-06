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
    
    ## Define sample OR
    
    #Updating Sample Size
    dN1 <- reactive({
      as.integer(input$nSamp1)
    })
    
    dN2 <- reactive({
      as.integer(input$nSamp2)
    })
    
    dN3 <- reactive({
      as.integer(input$nSamp3)
    })
    
    dN4 <- reactive({
      as.integer(input$nSamp4)
    })
    
    CI <- reactive({
      as.integer(input$ci)
    })
    
    sampORtab <- reactive({
      n1 <- dN1()
      n2 <- dN2()
      sampORtab <- data.frame(rbind(c(n1, 0.395*n1), c(n2, 0.644*n2)))
      colnames(sampORtab) <- c("Got sick", "Did not get sick")
      rownames(sampORtab) <- c("Ate Pizza", "Did not eat Pizza")
      return(sampORtab)
    })
    
    sampORCItab <- reactive({
      n3 <- dN3()
      n4 <- dN4()
      sampORCItab <- data.frame(rbind(c(n3, 0.395*n3), c(n4, 0.644*n4)))
      colnames(sampORCItab) <- c("Got sick", "Did not get sick")
      rownames(sampORCItab) <- c("Ate Pizza", "Did not eat Pizza")
      return(sampORCItab)
    })
    
    ## Define try yours
    
    ### Use your own data part
    # load_data <- reactive({
    #   dat <- read_csv(input$load_file$datapath)
    #   return(dat)
    #   
    # })
    # 
    # outcomevar <- function(data, x){
    #   vars <- names(data)
    #   y <- vars[!vars %in% x]}
    # 
    # exposurevar <- reactive({
    #   outcomevar(load_data(), input$X)
    # })
    # 
    # ### Forest Plot
    # 
    # makeDatatabletoList <- function(mytable){
    #   mylist = c()
    #   rnum <- nrow(mytable)
    #   for (i in 1:rnum){
    #     mylist[[i]] = matrix(as.numeric(mytable[i,]),nrow=2,byrow=TRUE)
    #   }
    #   mylist
    # }
    # 
    # makeTable <- function(mylist, referencerow=2)
    # {
    #   require("rmeta")
    #   numstrata <- length(mylist)
    #   # make an array "ntrt" of the number of people in the exposed group, in each stratum
    #   # make an array "nctrl" of the number of people in the unexposed group, in each stratum
    #   # make an array "ptrt" of the number of people in the exposed group that have the disease,
    #   # in each stratum
    #   # make an array "pctrl" of the number of people in the unexposed group that have the disease,
    #   # in each stratum
    #   ntrt <- vector()
    #   nctrl <- vector()
    #   ptrt <- vector()
    #   pctrl <- vector()
    #   if (referencerow == 1) { nonreferencerow <- 2 }
    #   else                   { nonreferencerow <- 1 }
    #   for (i in 1:numstrata)
    #   {
    #     mymatrix <- mylist[[i]]
    #     DiseaseUnexposed <- mymatrix[referencerow,1]
    #     ControlUnexposed <- mymatrix[referencerow,2]
    #     totUnexposed <- DiseaseUnexposed + ControlUnexposed
    #     nctrl[i] <- totUnexposed
    #     pctrl[i] <- DiseaseUnexposed
    #     DiseaseExposed <- mymatrix[nonreferencerow,1]
    #     ControlExposed <- mymatrix[nonreferencerow,2]
    #     totExposed <- DiseaseExposed + ControlExposed
    #     ntrt[i] <- totExposed
    #     ptrt[i] <- DiseaseExposed
    #   }
    #   names <- as.character(seq(1,numstrata))
    #   myMH <- meta.MH(ntrt, nctrl, ptrt, pctrl, conf.level=0.95, names=names,statistic="OR")
    #   
    #   
    #   tabletext<-cbind(c("","Study",myMH$names,NA,"Summary"),
    #                    c("Treatment","(effective)",ptrt,NA,NA),
    #                    c("Treatment","(non-effective)",pctrl, NA,NA),
    #                    c("Control","(effective)",(ntrt-ptrt),NA,NA),
    #                    c("Control","(non-effective)",(nctrl-pctrl), NA,NA),
    #                    c("","OR",format((exp(myMH$logOR)),digits=3),NA,format((exp(myMH$logMH)),digits=3)))
    # }
    # 
    # 
    # makeForestPlot <- function(mylist, referencerow=2)
    # {
    #   require("rmeta")
    #   numstrata <- length(mylist)
    #   # make an array "ntrt" of the number of people in the exposed group, in each stratum
    #   # make an array "nctrl" of the number of people in the unexposed group, in each stratum
    #   # make an array "ptrt" of the number of people in the exposed group that have the disease,
    #   # in each stratum
    #   # make an array "pctrl" of the number of people in the unexposed group that have the disease,
    #   # in each stratum
    #   ntrt <- vector()
    #   nctrl <- vector()
    #   ptrt <- vector()
    #   pctrl <- vector()
    #   if (referencerow == 1) { nonreferencerow <- 2 }
    #   else                   { nonreferencerow <- 1 }
    #   for (i in 1:numstrata)
    #   {
    #     mymatrix <- mylist[[i]]
    #     DiseaseUnexposed <- mymatrix[referencerow,1]
    #     ControlUnexposed <- mymatrix[referencerow,2]
    #     totUnexposed <- DiseaseUnexposed + ControlUnexposed
    #     nctrl[i] <- totUnexposed
    #     pctrl[i] <- DiseaseUnexposed
    #     DiseaseExposed <- mymatrix[nonreferencerow,1]
    #     ControlExposed <- mymatrix[nonreferencerow,2]
    #     totExposed <- DiseaseExposed + ControlExposed
    #     ntrt[i] <- totExposed
    #     ptrt[i] <- DiseaseExposed
    #   }
    #   names <- as.character(seq(1,numstrata))
    #   myMH <- meta.MH(ntrt, nctrl, ptrt, pctrl, conf.level=0.95, names=names,statistic="OR")
    #   
    # }
    
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
    
    
    ## True OR
    tureORtab <- data.frame(rbind(c(630,249),c(236, 152)))
    colnames(tureORtab) <- c("Got sick", "Did not get sick")
    rownames(tureORtab) <- c("Ate Pizza", "Did not eat Pizza")

    output$TrueOR <- renderTable(tureORtab, rownames = TRUE)
    
    ## Sample OR
    output$SampleOR <- renderTable(sampORtab(), rownames = TRUE)
    
    ## True OR CI
    tureORCItab <- data.frame(rbind(c(630,249),c(236, 152)))
    colnames(tureORCItab) <- c("Got sick", "Did not get sick")
    rownames(tureORCItab) <- c("Ate Pizza", "Did not eat Pizza")
    
    output$TrueORCI <- renderTable(tureORCItab, rownames = TRUE)
    
    ## Sample OR
    output$SampleORCI <- renderTable(sampORCItab(), rownames = TRUE)
    
    
    
    
    ## Interpret quiz
    output$interpret_quiz_res <- renderText({
        req(input$interpret_quiz_submit)
        ifelse(isolate(input$interpret_quiz=="No"),
               "<span style=\"color:#0b5b67\"> Correct answer! 
               Click on Next to go to the next page</span>", 
               "<span style=\"color:#C34129\"> Wrong answer. 
               Confidence interval spanning across null indicates inconclusive evidence</span>")
        
    })
    
    ## Try yours forest plot
    outputlist <- 
    output$try_yours_table <- renderPlot(makeTable(gvc_list))
    
    
    
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
