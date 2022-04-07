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
               DT, 
               rmeta)

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
        },
        or_tab = {
            temp= data.frame(got_sick = c(0,0), didnt_get_sick = c(0,0))
            rownames(temp) <- c("ate_pizza", "didnt_eat_pizza")
            temp
        },
        or_ci_tab = {
            temp= data.frame(got_sick = c(0,0), didnt_get_sick = c(0,0))
            rownames(temp) <- c("ate_pizza", "didnt_eat_pizza")
            temp
        },
        dat = readRDS(here::here("data", "data.rds")),
        define_quiz_complete = F,
        when_used_quiz_complete = c(F,F),
        calculate_quiz_complete = c(F,F,F,F),
        interpret_quiz_complete = F,
        your_dat = NA,
        your_expo=""
    )

    ## Definition quiz
    output$definition_quiz_res <- renderText({
        req(input$definition_quiz_submit)
        res <- isolate({

            shiny::validate(need(!is.null(input$definition_quiz1) & !is.null(input$definition_quiz2),
                                 "Choose answer for both questions!")
            ) 
            
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
               })
        return(res)
    })

   observeEvent(input$definition_quiz_submit,{
       shiny::validate(need(!is.null(input$definition_quiz1) & !is.null(input$definition_quiz2),
                            "Choose answer for both questions!")
       ) 
        if(input$definition_quiz1=="Pizza" &
           input$definition_quiz2 =="Getting sick (nausea and diarrhea)"){
            rv$define_quiz_complete <-T
        }
    })
    
    ## Define try yours
    
    ### Use your own data part
    load_data <- reactive({
      dat <- read_csv(input$load_file$datapath)
      return(dat)

    })
     
    ### Forest Plot

    makeTable <- function(dat)
    {
      require("rmeta")
      n.trt <- dat[,2]
      n.ctrl <- dat[,3]
      col.trt <- dat[,4] 
      col.ctrl <- dat[,5]
      
      names <- names(dat)
      myMH <- meta.MH(n.trt, n.ctrl, col.trt, col.ctrl, conf.level=0.95, names=Name,statistic="OR", data = dat, na.action = na.omit,
                      subset=c(13,6,5,3,7,12,4,11,1,8,10,2))


      tabletext <- cbind(c("","Study",myMH$names,NA,"Summary"),
                       c("Treatment","(effective)",dat$col.trt[c(13,6,5,3,7,12,4,11,1,8,10,2)],NA,NA),
                       c("Treatment","(non-effective)",dat$col.ctrl[c(13,6,5,3,7,12,4,11,1,8,10,2)], NA,NA),
                       c("Control","(effective)",(dat$n.trt-dat$col.trt)[c(13,6,5,3,7,12,4,11,1,8,10,2)],NA,NA),
                       c("Control","(non-effective)",(dat$n.ctrl-dat$col.ctrl)[c(13,6,5,3,7,12,4,11,1,8,10,2)], NA,NA),
                       c("","OR",format((exp(myMH$logOR)),digits=3),NA,format((exp(myMH$logMH)),digits=3)))
    }


    makeForestPlot <- function(dat)
    {
      require("rmeta")
      
      myMH <- meta.MH(n.trt, n.ctrl, col.trt, col.ctrl, conf.level=0.95, names=Name,statistic="OR", data = dat, na.action = na.omit,
                      subset=c(13,6,5,3,7,12,4,11,1,8,10,2))
      
      metap <- metaplot(myMH$logOR, myMH$selogOR, nn=myMH$selogOR^-2, myMH$names,
               summn=myMH$logMH, sumse=myMH$selogMH, sumnn=myMH$selogMH^-2,
               logeffect=T, colors=meta.colors(box="#34186f",lines="blue", zero ="red",
                                               summary="#ff864c", text="black"))
    }
    
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
    
    observeEvent(input$calculate_quiz1_submit,{
        isolate({
            if(sum(as.matrix(rv$calc_tab)==
                   matrix(c(8,19,4,27),ncol=2,nrow=2,byrow=T))==4){
                rv$calculate_quiz_complete[1] <-T
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
    
    observeEvent(input$calculate_quiz2_submit,{
        isolate({
            shiny::validate(need(!is.null(input$calculate_quiz2_1) &
                                     !is.null(input$calculate_quiz2_2),
                                 "Fill in both numbers!")
            ) 
            if(input$calculate_quiz2_1==8 &
               input$calculate_quiz2_2==19){
                rv$calculate_quiz_complete[2] <-T
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
    
    observeEvent(input$calculate_quiz3_submit,{
        isolate({
            shiny::validate(need(!is.null(input$calculate_quiz3_1) &
                                     !is.null(input$calculate_quiz3_2),
                                 "Fill in both numbers!")
            ) 
            if(input$calculate_quiz3_1==4 &
               input$calculate_quiz3_2==27){
                rv$calculate_quiz_complete[3] <-T
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

    observeEvent(input$calculate_quiz4_submit,{
        isolate({
            shiny::validate(need(!is.null(input$calculate_quiz4),
                                 "Choose your answer!")
            ) 
            if(input$calculate_quiz4=="(8*27)/(4*19)"){
                rv$calculate_quiz_complete[4] <-T
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
    
    observeEvent(input$when_used_quiz1_submit,{
        isolate({
            if(sum(as.matrix(rv$when_used_tab)==
                   matrix(c(45,86,32,100),ncol=2,nrow=2,byrow=T))==4){
                rv$when_used_quiz_complete[1] <-T
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
    
    observeEvent(input$when_used_quiz2_submit,{
        isolate({
            shiny::validate(need(!is.null(input$when_used_quiz2),
                                 "Choose answer for the question!")
            ) 
            if(input$when_used_quiz2=="1.63"){
                rv$when_used_quiz_complete[2] <-T
            }
        })
    })
    
    ## True OR
    tureORtab <- reactive({
        temp <- data.frame(rbind(table(rv$dat)[,1],table(rv$dat)[,2]))
        colnames(temp) <- c("Got sick", "Did not get sick")
        rownames(temp) <- c("Ate Pizza", "Did not eat Pizza")
        return(temp)
        }) # I changed this to read data from the embedded data frame
   

    output$TrueOR <- renderTable(tureORtab(), rownames = TRUE, width = "96%")
    
    ## Define sample OR
    
    #Updating Sample Size
    dN1 <- reactive({
        as.integer(input$nSamp1)
    })
    
    dN2 <- reactive({
        as.integer(input$nSamp2)
    })
    
    output$SampleOR <- renderTable({
        input$sample_or_submit
        isolate({
        n1 <- dN1()
        n2 <- dN2()
        # I changed this to do a random sampling from the embedded dataset and also save the table to populate the text below
        rv$or_tab <- rbind(
            sample(rv$dat %>% filter(pizza=="Ate pizza") %>% select(sick) %>% unlist(), size = n1, replace = F) %>% table(), 
            sample(rv$dat %>% filter(pizza=="Did not eat pizza") %>% select(sick) %>% unlist(), size = n2, replace = F) %>% table()
        )
        format_tab(rv$or_tab)
        })
    }, rownames=T, width = "96%")
    
    output$SampleOR_text <- renderUI({
        rv$or_tab
        withMathJax(
        sprintf("$$\\text{Sample OR =} \\frac{%i * %i}{%i * %i} = %.2f$$",
                rv$or_tab[1,1], rv$or_tab[2,2], rv$or_tab[1,2], rv$or_tab[2,1],
                rv$or_tab[1,1]*rv$or_tab[2,2]/(rv$or_tab[1,2]*rv$or_tab[2,1]))
        )
    })

    ## True OR CI
    tureORCItab <- reactive({
        temp <- data.frame(rbind(table(rv$dat)[,1],table(rv$dat)[,2]))
        colnames(temp) <- c("Got sick", "Did not get sick")
        rownames(temp) <- c("Ate Pizza", "Did not eat Pizza")
        return(temp)
        }) # I changed this to read data from the embedded data frame
    
    
    output$TrueORCI <- renderTable(tureORCItab(), rownames = TRUE, width = "96%")
    
    ## Sample OR CI
    dN3 <- reactive({
        as.integer(input$nSamp3)
    })
    
    dN4 <- reactive({
        as.integer(input$nSamp4)
    })
    
    CI <- reactive({
        as.integer(input$ci)
    })
    
    output$SampleORCI <- renderTable({
        input$sample_or_ci_submit
        isolate({
        n3 <- dN3()
        n4 <- dN4()
        # I changed this to do a random sampling from the embedded dataset and also save the table to populate the text below
        rv$or_ci_tab  <- rbind(
            sample(rv$dat %>% filter(pizza=="Ate pizza") %>% select(sick) %>% unlist(), size = n3, replace = F) %>% table(), 
            sample(rv$dat %>% filter(pizza=="Did not eat pizza") %>% select(sick) %>% unlist(), size = n4, replace = F) %>% table()
        )
        format_tab(rv$or_ci_tab)
        })
    }, rownames=T, width = "96%")
    
    output$sample_or_ci_text <- renderUI({
        input$sample_or_ci_submit
        isolate({
        rv$or_ci_tab
        or <- rv$or_ci_tab[1,1]*rv$or_ci_tab[2,2]/(rv$or_ci_tab[1,2]*rv$or_ci_tab[2,1])
        val_sqrt <- sqrt(1/rv$or_ci_tab[1,1] + 1/rv$or_ci_tab[1,2] + 1/rv$or_ci_tab[2,1] + 1/rv$or_ci_tab[2,2])
        zval <- abs(qnorm((1-input$ci)/2))
        withMathJax(
            sprintf("$$\\text{Sample OR =} \\frac{%i * %i}{%i * %i} = %.2f
                    \\\\
                    \\text{Upper CI =}\\exp[ln(%.2f) + %.2f * \\sqrt{\\frac{1}{%i} + \\frac{1}{%i} + \\frac{1}{%i} + \\frac{1}{%i})}] = %.2f
                    \\\\
                    \\text{Lower CI =}\\exp[ln(%.2f) - %.2f * \\sqrt{\\frac{1}{%i} + \\frac{1}{%i} + \\frac{1}{%i} + \\frac{1}{%i})}] = %.2f
                    $$",
                    rv$or_ci_tab[1,1], rv$or_ci_tab[2,2], rv$or_ci_tab[1,2], rv$or_ci_tab[2,1],
                    or, 
                    or, zval,
                    rv$or_ci_tab[1,1], rv$or_ci_tab[1,2], rv$or_ci_tab[2,1], rv$or_ci_tab[2,2],
                    exp(log(or) + zval*val_sqrt),
                    or, zval,
                    rv$or_ci_tab[1,1], rv$or_ci_tab[1,2], rv$or_ci_tab[2,1], rv$or_ci_tab[2,2],
                    exp(log(or) - zval*val_sqrt)
                    )
        )
        })
    })
    
    
    ## Interpret quiz
    output$interpret_quiz_res <- renderText({
        req(input$interpret_quiz_submit)
        isolate({
        shiny::validate(need(!is.null(input$interpret_quiz),
                             "Choose the answer!")
        ) 
        ifelse(input$interpret_quiz=="No",
               "<span style=\"color:#0b5b67\"> Correct answer! 
               Click on Next to go to the next page</span>", 
               "<span style=\"color:#C34129\"> Wrong answer. 
               Confidence interval spanning across null indicates inconclusive evidence</span>")
        
        })
    })
    
    observeEvent(input$interpret_quiz_submit,{
        isolate({
        shiny::validate(need(!is.null(input$interpret_quiz),
                             "Choose the answer!")
        ) 
        if(input$interpret_quiz=="No"){
            rv$interpret_quiz_complete <-T
        }
        })
    })
    
    # Try yours table preview
    observe({
        req(input$load_file)
        is.csv <-
            input$load_file$type == "text/csv" # check whether file is of .csv format
        feedbackDanger(inputId = "load_file",
                       show = !is.csv,
                       text = "File has to be in .csv format")
        req(is.csv) # read the file to the reactive object only when has correct format
        rv$your_dat <- rio::import(input$load_file$datapath) %>% na.omit()
    })
    
    output$custom_tab <- renderTable({
        req(rv$your_dat)
        head(rv$your_dat, n = 5)
    })
    
    # populate outcome and exposure options
    observeEvent(rv$your_dat, {
        req(rv$your_dat)
        rv$your_expo <- colnames(rv$your_dat)[unlist(lapply(rv$your_dat, is_dichotomous))]
        if(length(rv$your_expo)<=1){
           shinyalert(
               "Use new data",
                "Dataset does not contain enough number of dichotomous variables. Upload new data.",
               type = "warning"
            )
        }else{
        updateSelectInput(inputId = "exposure",
                          choices = rv$your_expo
        )
        }
    })
    
    observeEvent(input$exposure, {
        req(input$exposure)
        options <- colnames(rv$your_dat)[unlist(lapply(rv$your_dat, is_dichotomous))]

        updateSelectInput(inputId = "outcome",
                          choices = options[which(options!=input$exposure)]
        )
    })
    
    ## Try yours outputs
    output$your_table <- renderPrint({
        table(Exposure = rv$your_dat[,input$exposure], 
              Outcome = rv$your_dat[,input$outcome])
        })
    
    output$your_or <- renderUI({
        res <- table(Exposure = rv$your_dat[,input$exposure], 
                     Outcome = rv$your_dat[,input$outcome])
        or <- res[1,1]*res[2,2]/(res[1,2]*res[2,1])
        val_sqrt <- sqrt(1/res[1,1] + 1/res[1,2] + 1/res[2,1] + 1/res[2,2])

        withMathJax(
            sprintf("$$\\text{OR =} %.2f \\text{ (95%% CI =} %.2f - %.2f)$$",
                    or,
                    log(or)-1.96*val_sqrt,
                    log(or)+1.96*val_sqrt
        ))
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
        if(rv$define_quiz_complete){
        updateTabsetPanel(inputId = "steps", selected = "calculate")
        }
        else{
            shinyalert(
                inputId = "warning_define",
                title = "Wait!", 
            text = "Finish the quiz before you move to the next page!",
            type = "warning",
            confirmButtonText = "OK"
            )
        }
    })
    
    # definition to intro
    observeEvent(input$`1_0`, {
        req(input$`1_0`)
        updateTabsetPanel(inputId = "steps", selected = "intro")
    })
    
    # calculate to when_used
    observeEvent(input$`2_3`, {
        req(input$`2_3`)
        if(sum(rv$calculate_quiz_complete) ==4){
            updateTabsetPanel(inputId = "steps", selected = "when_used")
        }
        else{
            shinyalert(
                inputId = "warning_calculate",
                title = "Wait!", 
                text = "Finish the quiz before you move to the next page!",
                type = "warning",
                confirmButtonText = "OK"
            )
        }
    })
    
    # calculate to definition
    observeEvent(input$`2_1`, {
        req(input$`2_1`)
        
        updateTabsetPanel(inputId = "steps", selected = "definition")
    })
    
    # when_used to true_vs_sample
    observeEvent(input$`3_4`, {
        req(input$`3_4`)
        if(sum(rv$when_used_quiz_complete)==2){
            updateTabsetPanel(inputId = "steps", selected = "true_vs_sample")
        }
        else{
            shinyalert(
                inputId = "warning_when_used",
                title = "Wait!", 
                text = "Finish the quiz before you move to the next page!",
                type = "warning",
                confirmButtonText = "OK"
            )
        }
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
        if(rv$interpret_quiz_complete){
            updateTabsetPanel(inputId = "steps", selected = "try_yours")
        }
        else{
            shinyalert(
                inputId = "warning_when_used",
                title = "Wait!", 
                text = "Finish the quiz before you move to the next page!",
                type = "warning",
                confirmButtonText = "OK"
            )
        }
    })
    
    # interpret to conf_int
    observeEvent(input$`6_5`, {
        req(input$`6_5`)
        updateTabsetPanel(inputId = "steps", selected = "conf_int")
    })
})
