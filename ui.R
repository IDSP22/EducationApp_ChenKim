### Load packages
require(pacman)
pacman::p_load(
    shiny,
    shinyFeedback,
    shinyWidgets,
    shinyalert,
    tidyverse,
    ggplot2,
    magrittr,
    shinyjs,
    gt,
    waiter,
    bslib,
    sass,
    curl,
    sortable,
    ggthemr,
    markdown,
    knitr,
    DT,
    rmeta
)

source(here::here("helper.R"))

### Define UIs for each step separately

## Brief intro page
intro <- tabPanel(
    value = "intro",
    title = "Welcome!",
    
    # Markdown text chunk
    includeMarkdown(here::here('markdowns/intro.md')),
    
    # Start button
    div(
        style = "display:inline-block; float:right;",
        actionButton("start", "Start",
                     style = "background-color: #0b5b67; border-color: transparent")
    )
)

## Definition of OR
definition <- tabPanel(
    value = "definition",
    title = "Definition of Odds Ratio (OR)",
    
    # Markdown text chunk
    includeMarkdown(here::here('markdowns/definition.md')),

    # Quiz for the definition
    fluidRow(column(12,
        radioButtons(
            "definition_quiz1",
            label = HTML("<b>Question 1</b>. What is the potential <u>exposure</u> of interest?"),
            choices = c("Nausea", "Diarrhea", "Pizza"),
            selected = character(0),
            inline = T
        ),
        radioButtons(
            "definition_quiz2",
            label = HTML("<b>Question 2</b>. What is the <u>outcome</u> of interest?"),
            choices = c("Jason Momoa", "Getting sick (nausea and diarrhea)", "Consuming Pizza"),
            selected = character(0),
            inline = T
        )
    )
    ),
    
    ## result text
    fluidRow(
        column(12,
               htmlOutput("definition_quiz_res")
        ),
        align = "center"
    ),
    
    br(),
    
    ## submit button
    fluidRow(
        column(12,
               actionButton("definition_quiz_submit",
                            "Submit",
                            style = "background-color: #353842; border-color: transparent")
        ),
        align = "center"),
    
    br(),
    
    # Action button to move to the next step
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("1_0", "Previous",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("1_2", "Next",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "right"
    ))
)

## Calculating OR
calculate <- tabPanel(
    value = "calculate",
    title = "Calculating Odds Ratio (OR)",
    
    # Markdown text chunk
    withMathJax(includeMarkdown(here::here('markdowns/calculate.md'))),
    

    # Quiz to calculate odds ratio
    
    ## Quiz 1. Table to fill in
    fluidRow(
        column(12,
               HTML("<b>Quiz 1.</b> Fill in the table below with correct values. (Double click the cell to change the value)"))
    ),
    
    fluidRow(
        column(8,
            DTOutput(outputId="calculate_table",
                     width = "96%"),
            style = "align: right"
        ),
        column(4,
               actionButton("calculate_quiz1_submit",
                            "Check Answer",
                            style = "background-color: #353842; border-color: transparent;"),
               style = "margin-top: 80px; align: left"
        )
    ),
    fluidRow(
        column(8,
               htmlOutput("calculate_quiz1_res")
               ),
        align="center"
    ),
    br(),
    ## Quiz 2.
    fluidRow(
        column(12,
               HTML("<b>Quiz 2.</b> What is the odd of getting sick among those who <u>ate</u> pizza?"))
    ),
    fluidRow(
        column(2,
                numericInput("calculate_quiz2_1", "",value=NULL),
               style = "align:center"
               ),
        column(1,
                h3("/"),
               style = "align: center"
               ),
        column(2,
                numericInput("calculate_quiz2_2", "", value=NULL),
               style = "align:left"
            ),
        column(3,
               
        ),
        column(4,
               actionButton("calculate_quiz2_submit",
                            "Check Answer",
                            style = "background-color: #353842; border-color: transparent;"),
               style = "margin-top: 25px; align: left"
        )
    ),
    fluidRow(
        column(5,
               htmlOutput("calculate_quiz2_res")
        ),
        align="center"
    ),
    br(),
    
    ## Quiz 3.
    fluidRow(
        column(12,
               HTML("<b>Quiz 3.</b> What is the odd of getting sick among those who <u>did not eat</u> pizza?"))
    ),
    fluidRow(
        column(2,
               numericInput("calculate_quiz3_1", "",value=NULL),
               style = "align:center"
        ),
        column(1,
               h3("/"),
               style = "align: center"
        ),
        column(2,
               numericInput("calculate_quiz3_2", "", value=NULL),
               style = "align:left"
        ),
        column(3,
               
        ),
        column(4,
               actionButton("calculate_quiz3_submit",
                            "Check Answer",
                            style = "background-color: #353842; border-color: transparent;"),
               style = "margin-top: 25px; align: left"
        )
    ),
    fluidRow(
        column(5,
               htmlOutput("calculate_quiz3_res")
        ),
        align="center"
    ),
    br(),
    
    ## Quiz 4.
    fluidRow(
        column(12,
               HTML("<b>Quiz 4.</b> What is the <u>odds ratio</u> of getting sick among those who ate pizza 
                    vs. those who did not eat pizza??"))
    ),
    
    fluidRow(
        column(8,
               radioButtons("calculate_quiz4",
                            "",
                            choices = c(
                                "(8*27)/(4*19)",
                                "(8*19)/(4*27)",
                                "(8*4)/(19*27)"
                            ),
                            selected = character(0))
               ),
        column(4,
               actionButton("calculate_quiz4_submit",
                            "Check Answer",
                            style = "background-color: #353842; border-color: transparent;"),
               style = "margin-top: 70px; align: left"
        )
    ),
    fluidRow(
        column(8,
               htmlOutput("calculate_quiz4_res")
        ),
        align="center"
    ),
    br(),
    
    # Action button to move to the next step
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("2_1", "Previous",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("2_3", "Next",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "right"
    ))
)


## When is it used
when_used <- tabPanel(
    value = "when_used",
    title = "When Is It Used?",
    
    # Markdown text chunk
    includeMarkdown(here::here('markdowns/when_used.md')),
    
    # Quiz for the cohort study example
    ## Quiz 1. Table to fill in
    fluidRow(
        column(12,
               HTML("<b>Quiz 1.</b> Fill in the table below with correct values. (Double click the cell to change the value)"))
    ),
    
    fluidRow(
        column(8,
               DTOutput(outputId="when_used_table",
                        width = "100%"),
               style = "align: right"
        ),
        column(4,
               actionButton("when_used_quiz1_submit",
                            "Check Answer",
                            style = "background-color: #353842; border-color: transparent;"),
               style = "margin-top: 200px; align: left"
        )
    ),
    fluidRow(
        column(8,
               htmlOutput("when_used_quiz1_res")
        ),
        align="center"
    ),
    br(),
    ## Quiz 2.
    fluidRow(
        column(12,
               HTML("<b>Quiz 2.</b> What is the odds ratio of <u>exhibiting persistent suicidal behavior</u>
               given the baseline depression in comparison to no base line depression?"))
    ),
    
    fluidRow(
        column(8,
               radioButtons("when_used_quiz2",
                            "",
                            choices = c(
                                "0.61",
                                "1.63",
                                "0.17",
                                "5.97"
                            ),
                            selected = character(0))
        ),
        column(4,
               actionButton("when_used_quiz2_submit",
                            "Check Answer",
                            style = "background-color: #353842; border-color: transparent;"),
               style = "margin-top: 75px; align: left"
        )
    ),
    fluidRow(
        column(8,
               htmlOutput("when_used_quiz2_res")
        ),
        align="center"
    ),
    br(),
    
    # Action button to move to the next step
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("3_2", "Previous",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("3_4", "Next",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "right"
    ))
    
)

## True vs. sample OR
true_vs_sample <- tabPanel(
    value = "true_vs_sample",
    title = "True vs. Sample Odds Ratio (OR)",
    
    # Markdown text chunk
    includeMarkdown(here::here('markdowns/true_vs_sample.md')),
    
    ###### UI for the quiz/example goes here ######
    h3(strong("True Population:")),

    fluidRow(column(
      12,
      # True Odds Ratio Tables
      tableOutput("TrueOR")
    )),
    
    (p(("Odds ratio (θ) :    \\(\\frac{630*152}{239*236}= 1.63\\)"),
       style = "white-space: pre-wrap"
    )),
    
    h3(strong("Sample Population:")),

    fluidRow(column(
      6,
      # Slider Input
      
      sliderInput(
        "nSamp1",
        "Sample Size for People Ate Pizza",
        min = 30,
        max = 879,
        value = 50,
        step = 5
      )
      ),
      column(6,
      sliderInput(
        "nSamp2",
        "Sample Size for People Did Not Eat Pizza",
        min = 30,
        max = 388,
        value = 50,
        step = 5
      ))),
      
     # submit button
    fluidRow(
        column(12,
               actionButton("sample_or_submit",
                            "Submit",
                            style = "background-color: #353842; border-color: transparent")
        ),
        align = "center"),
    
      # Sample Odds Ratio Tables
    br(),
      fluidRow(column(12,
      tableOutput("SampleOR")
    )),
    
    # sample OR calculation
    uiOutput("SampleOR_text"),

        # Action button to move to the next step
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("4_3", "Previous",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("4_5", "Next",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "right"
    ))
)

## Confidence interval
conf_int <- tabPanel(
    value = "conf_int",
    title = "Confidence Interval (CI)",
    
    # Markdown text chunk
    withMathJax(includeMarkdown(here::here('markdowns/conf_int.md'))),
    
    ###### UI for the quiz goes here ######
    h3(strong("True Population:")),

    fluidRow(column(
      12,
      # True Odds Ratio Tables
      tableOutput("TrueORCI")
    )),
    
    (p(("Odds ratio (θ) :    \\(\\frac{630*152}{239*236}= 1.63\\)"),
       style = "white-space: pre-wrap"
    )),
    
    h3(strong("Sample Population:")),

    fluidRow(column(
      6,
      # Slider Input
      
      sliderInput(
        "nSamp3",
        "Sample Size for People Ate Pizza",
        min = 30,
        max = 879,
        value = 50,
        step = 5
      )
      ),
      column(6,
      sliderInput(
        "nSamp4",
        "Sample Size for People Did Not Eat Pizza",
        min = 30,
        max = 388,
        value = 50,
        step = 5
      ))
      ),
    fluidRow(
        column(6,
      sliderInput(
        "ci",
        "Confidence Interval",
        min = 0.1,
        max = 0.99,
        value = 0.95,
        step = 0.01
      )
        ),
      column(6,
        actionButton("sample_or_ci_submit",
                          "Submit",
                          style = "background-color: #353842; border-color: transparent"),
        style = "margin-top: 25px", 
        align = "center")
             
),
             # Sample Odds Ratio Tables

     fluidRow(
         column(12,
      # Sample Odds Ratio Tables
      tableOutput("SampleORCI")
         )
     ),

    # text output of sample OR and CI
    uiOutput("sample_or_ci_text"),

    
    # Action button to move to the next step
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("5_4", "Previous",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("5_6", "Next",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "right"
    ))
)

interpret <- tabPanel(
    value = "interpret",
    title = "Interpretation",
    
    # Markdown text chunk
    includeMarkdown(here::here('markdowns/interpret.md')),
    
    # UI for quiz
    ## question
    fluidRow(column(12,
                    radioButtons("interpret_quiz",
                                 label = "",
                                 choices = c("Yes", "No"),
                                 selected = character(0),
                                 inline = T,
                                 choiceValues = c(T, F))
        ),
        align = "center"
    ),
    
    ## result text
    fluidRow(
        column(12,
            htmlOutput("interpret_quiz_res")
            ),
        align = "center"
    ),
    
    br(),
    
    ## submit button
    fluidRow(
        column(12,
               actionButton("interpret_quiz_submit",
                            "Submit",
                            style = "background-color: #353842; border-color: transparent")
        ),
        align = "center"),
    
    br(),
    
    # Action button to move to the next step
    fluidRow(column(
        12,
        # Button to move to the previous/next step
        div(
            style = "display:inline-block; ",
            actionButton("6_5", "Previous",
                         style = "background-color: #EFb758; border-color: transparent"),
            actionButton("6_7", "Next",
                         style = "background-color: #0b5b67; border-color: transparent"),
            style = "display:center-align;"
        ),
        align = "right"
    ))
)

try_yours <- tabPanel(
    value = "try_yours",
    title = "Use Your Own Data",
    
    # Markdown text chunk
    includeMarkdown(here::here('markdowns/try_yours.md')),

    ###### UI for the BYOD app goes here ######
    fileInput(
      inputId = 'load_file',
      label = 'Select a .csv file:',
      accept = ".csv"
    ),
    
    # Shows the first 5 rows of the uploaded dataset when successful
    HTML("<b>Data preview (first 5 rows):</b><br>"),
    
    div(style = 'overflow-y:scroll;',
        tableOutput("custom_tab")),
    
    selectInput("exposure",
                "Choose the exposure variable",
                choices = NULL),
    selectInput("outcome",
                "Choose the outcome variable",
                choices = NULL),
      
      fluidRow(column(
        12,
        # meta analysis table
        verbatimTextOutput("your_table"),
        uiOutput("your_or")
      ))
        
)
    


### Compile each step to one tab sets
tab_steps <- navlistPanel(id = "steps",
                          intro,
                          definition,
                          calculate,
                          when_used,
                          true_vs_sample,
                          conf_int,
                          interpret,
                          try_yours
                         )


### Define UI for application
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Odds Ratio"),
        h4("Guided learning application"),
        
        HTML("<p>Yuyu (Ruby) Chen (yc4178@nyu.edu) & Sooyoung Kim (sk9076@nyu.edu)</p>"),
        
        # Change theme
        theme = theme_ks,
        
        tags$head(tags$style(
            # Set the color for validate() messages and tab color
            HTML(
                "
              .shiny-output-error-validation {
                color: #C34129;
              }
              .well {
                background-color: #F0E6D7;
                border-color: transparent;
                font-weight: bold;
              }
              "
            )
            
        )),
        
        
        # Set to use shinyjs
        shinyjs::useShinyjs(),
        shinyFeedback::useShinyFeedback(),
        
        # Sidebar with step-by-step instructions
        tab_steps
    )
)