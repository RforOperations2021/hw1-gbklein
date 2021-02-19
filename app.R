
# Summary:
# App can create a bar chart showing the birth state of all people living in a given state,
# where y axis is # of people living in the state chosen by user,
# and x axis is state of birth (includes all 50 states + Puerto Rico).
# Next app can create a bar chart showing a given metric across all 50 states,
# where y axis is the metric chosen by user (e.g. % of residents born in-state or % of residents born out-of-state),
# and x axis is all 50 states + Puerto Rico.
# User can download associated data tables as well as a summary list for the second bar chart. 

library(shiny)
library(DT)
library(ggplot2)
library(stringr)
library(dplyr)
library(shinythemes)
library(tools)
library(colourpicker)


#Load file
states <- read.csv("states_placeofbirth.csv")

#Remove Underscores in Column and Row Names
names(states) <- gsub(x = names(states), pattern = "_", replacement = " ") 

#Convert column with list of states into row names
row.names(states) <- states[,1]

# Define UI for application that plots features of states-----------------------
ui <- fluidPage(
    
    #Theme selection
    theme = shinytheme("slate"),
    
    # Application title --------------------------------------------------------
    titlePanel("State-to-State Migration"),
    
    # Sidebar layout with a input and output definitions -----------------------
    sidebarLayout(
        
        # Inputs: Select variables to plot -------------------------------------
        sidebarPanel(
            
            # Select state of residence for y axis of first bar chart - Residents by Birth State ----
            selectInput(inputId = "current_residence", 
                        label = "Show birth states for residents of:",
                        choices = row.names(states),
                        selected = states[1,1]),
            
            # Show data table for first bar chart-------------------------------
            checkboxInput(inputId = "show_data1",
                          label = "Show data table for birth states of chosen state",
                          value = TRUE),
            
            #Select metric for y axis variable of second bar chart - Metric ----
            selectInput(inputId = "metric",
                        label = "OR compare all states by:",
                        choices = colnames(states[2:8]),
                        selected = colnames(states[2:2])),
            
            #Show data table for second bar chart-------------------------------
            checkboxInput(inputId = "show_data2",
                          label = "Show data table for all states by chosen metric",
                          value = TRUE),
            
            #Show summary for second bar chart ---------------------------------
            checkboxInput(inputID = "show_summarydata2",
                          label = "Show summary table of chosen metric",
                          value = TRUE),
            
            # Enter text for plot title -----------------------------------------
            textInput(inputId = "plot_title", 
                      label = "Plot title", 
                      placeholder = "Enter your plot title, e.g. Where Residents of Alabama were Born"),
            
            #Select color for bar charts ---------------------------------------
            colourInput(inputID = "col",
                        label = "Select bar chart color",
                        value = "red"),
            
              # Horizontal line for visual separation --------------------------
            hr(),
            
        ),
        
        # Output: --------------------------------------------------------------
        mainPanel(
            
            # Show first barchart - Residents by Birth State--------------------
            plotOutput(outputId = "barchart_birthstate"),
            br(),        # a bit of visual separation
            
            # Show second barchart - Metric-------------------------------------
            plotOutput(outputId = "barchart_metric"),
            br(),        #a bit of visual separation
            
            # Show first data table - Residents by Birth State------------------
            DT::dataTableOutput(outputId = "table_birthstate"),
            
            #Show second data table - Metric -----------------------------------
            DT:: dataTableOutput(outputId = "table_metric"),
            br(),        #a bit of visual separation
            
            #Show summary ------------------------------------------------------
            textOutput(outputID = "summary_metric"),
            br(),        #a bit of visual separation
        
            #Download button ---------------------------------------------------
            downloadButton(outputId = "downloadbutton",
                label = "Download",
                class = NULL)
        )
    )
)


# Define server function required to create the barplot ------------------------
server <- function(input, output, session) {
    
    # Create a subset of data filtering for selected State of Current Residence ------
    states_subset <- reactive({
        req(input$current_residence) # ensure availablity of state value before proceeding
        filter(states, input$current_residence)
    })
    
    #Create a subset of data filtering for selected Metric----------------------
    metric_subset <- reactive({
        req(input$metric) # ensure availability of metric value before proceeding
        filter(states,input$metric)
    })
    
    # Convert plot_title toTitleCase -------------------------------------------
    pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
    # Create State of Current Residence barplot the plotOutput function expects --
    output$barchart_birthstate <- renderPlot({
        barplot(data = states_subset,
                x = colnames(states_subset[9:59]),
                y = input$current_residence,
                xlab ="Born in",
                ylab = "Currently living in ",
                title = pretty_plot_title(),
                col = input$col)
    })
    
    # Create the Metric barplot the second plotOutput function expect-----------
    output$barchart_metric <- renderPlot({
        barplot(data = metric_subset,
                x = metric_subset[,1],
                y = input$metric,
                xlab = "State",
                ylab = "Chosen Metric",
                title = pretty_plot_title,
                col = input$col)
    })
    
    
    # Print first data table if checked ----------------------------------------
    output$table_birthstate <- DT::renderDataTable(
        if(input$show_data1){
            DT::datatable(data = states_subset, 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
    })
    
    #Print second data table if checked ----------------------------------------
    output$table_metric <- DT::renderDataTable(
        if(input$show_data2){
            DT::datatable(data = metric_subset,
                          options = list(pagelength=10),
                          rownames = FALSE)
    })
    

    #Print summary of metric if checked ----------------------------------------
    output$summary_metric <- renderText(
        if(input$show_summarydata2){
            paste(min(metric_subset),
                  max(metric_subset),
                  mean(metric_subset))
        }
    )
}

#Download data if checked ------------------------------------------------------
output$downloadbutton <- downloadHandler(
    filename = function(){
        paste("states_placeofbirth.csv")
    },
    content = function(file) {
        write.csv("states_placeofbirth.csv", file)
    }
)

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)