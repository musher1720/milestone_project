################################################################################
# app.R
#
# This is a Shiny web application showcasing Another Predictive Text
# Model (APTM™).
#
# https://musher1720.shinyapps.io/aptm/
#
# Requires:
#    predictNext.R
#    tokenizer.R
#    www/dts_pruned_8.rda (training ngrams, pruned to count > 8)
#    www/style.css

library(shiny)
library(stringr)

source("predictNext.R")

# Load training Ngrams
load("www/dts_pruned_8.rda")

################################################################################
# Define UI for application
ui <- fluidPage(
    
    theme="style.css",
    
    # Application title
    titlePanel(HTML("<h1>Another Predictive Text Model </h1>"),
               windowTitle="APTM&Trade"),
    
    # Text instructions
    HTML(paste0(
        "Enter text in the input box below, as though you were using a mobile ",
        "messaging app, and APTM&trade; will attempt to predict your next ",
        "word. If the last character entered is a space, APTM&trade; will know ",
        "the previous word is complete. If the last character entered is not ",
        "a space, APTM&trade; will assume you are still typing the next word, ",
        "and will offer predictions that start with the last word or word ",
        "fragment you have typed.<br><hr>"
    )),
    
    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            width=5,
            
            wellPanel(
                textInput(
                    "X",
                    "Enter text below:"
                )
            ),
            
            wellPanel(
                HTML("<b>Prediction:</b>"),
                
                htmlOutput("next_word")
            ),
            
            # Built with Shiny by RStudio
            h5("Built with",
               a(href="https://shiny.rstudio.com",
                 img(src="http://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                     height="30px")),
               "by",
               a(href="https://www.rstudio.com",
                 img(src="http://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png",
                     height="30px")),
               "."
            )
        ),
            
        # Show best prediction and table of top five predictions
        mainPanel(
            width=7,
            
            tableOutput("predictions")
        )
    ),
    
    HTML("<hr>"),
    
    HTML(paste0(
        "The APTM&trade; app uses a 5-gram language model to predict the next ",
        "word from user input, ranking the predictions using a method called ",
        "\"Stupid Backoff\", as described in ",
        "<a href=\"http://www.aclweb.org/anthology/D07-1090.pdf\">'Large ",
        "Language Models in Machine Translation'</a> by T. Brants et al, in ",
        "EMNLP/CoNLL 2007.<p><br>"
    )),
    
    HTML(paste0(
        "APTM&trade; was implemented as the capstone project for the Johns ",
        "Hopkins University Coursera ",
        "<a href=\"https://www.coursera.org/specializations/jhu-data-science\">",
        "Data Science Specialization</a>.<p><br>",
        "The R code underlying this app can be found on ",
        "<a href=\"https://github.com/musher1720/milestone_project\">GitHub</a>.<br>"
        
    ))
)

################################################################################
# Define server logic
server <- function(input, output) {
    
    # Reactive expression to get predictions
    predictions <- reactive({
        req(input$X)
        
        out <- predictNext(input$X, dts)
        names(out) <- c("Score", "Prediction")
        
       return(out[, c("Prediction", "Score")])
    })
    
    input_display <- reactive({
        req(input$X)
        
        # If the input ends with a space, the last word is complete
        # If it doesn't end with a space, the last word may not be complete
        if (str_sub(input$X, start= -1) == " ") {
            out <- input$X
        } else {
            if (str_count(input$X, "\\S+") == 1) {
                out <- ""
            } else {
                # Split beginning fragment of next word off of input
                out <-paste(word(input$X, 1:(str_count(input$X, "\\S+") - 1)),
                            collapse=" ")
            }
        }
        
        return(out)
    })
    
    # Output the top prediction appended to the input
    output$next_word <- renderUI((
        HTML(paste0(
            input_display(),
            " <i>",
            predictions()$Prediction[1], 
            "</i>"
        ))
    ))
    
    # Output the top five predictions with their SBO score
    output$predictions <- renderTable(
        {predictions()},
        digits=4
    )
}

# Run the application 
shinyApp(ui = ui, server = server)