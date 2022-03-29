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

### Define custom theme
theme_ks <- bslib::bs_theme(
    version = ,
    bg = "#FfFfFf",
    fg = "#183a5a",
    # dark navy (title, base font and stuff)
    primary = "#183a5a",
    # tab font color and hover block color etc
    secondary = "#353842",
    # (for messages that don't need to stand out)
    success = "#0b5b67",
    # dark green
    info = "#3e83a8",
    # ligher dark blue (text that are informative not critical)
    warning = "#EFb758",
    # mustard-ish yellow
    danger = "#C34129",
    # blood orange
    base_font = font_google("Source Sans Pro"),
    code_font = font_google("Fira Mono"),
    heading_font = font_google("Oswald"),
    font_scale = 0.9
)

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

    ###### UI for the quiz goes here ######
    
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
        
        # Sidebar with step-by-step instructions
        tab_steps
    )
)