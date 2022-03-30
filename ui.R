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
    knitr
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
    
    ###### UI for the quiz goes here ######
    
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
    
    ###### UI for the quiz/example goes here ######
    
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