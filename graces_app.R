library(shiny)
library(DT)
library(stringr)
library(dplyr)
library(shinythemes)
library(tools)

#Directions:
  #Create three (3) different kinds of plots/figures
  #Use DT to create one (1) data table
  #Include at least two (2) different types of inputs
  #One (1) functioning downloadButton()
  #Inputs must use reactivity in a logical manner with all outputs displayed to users

#Objective 1:
  #Create a visualization showing the birth state of all people living in a given state,
  #where y axis is # of people living in the state chosen by user,
  #and x axis is state of birth (includes all 50 states).

#Objective 2: 
  #Create a visualization showing a given metric across all 50 states,
  #where y axis is the metric chosen by user (e.g. % of residents born in-state or % of residents born out-of-state),
  #and x axis is all 50 states.


#Load file
states <- read.csv(file="C:\\Users\\Grace\\Documents\\GitHub\\hw1-gbklein\\states_pob.csv")

#Remove Underscores in Column and Row Names
column_names_spaces = str_replace_all(colnames(states),"_"," ")
row_names_spaces = str_replace_all(rownames(states),"_"," ")



# Define UI for application that plots features of states-----------
ui <- fluidPage(
  
    #Theme selection
    theme = shinytheme("slate"),
    
    # Application title -----------------------------------------------
    titlePanel("State-to-State Migration"),
    
    # Sidebar layout with a input and output definitions --------------
    sidebarLayout(
        
        # Inputs: Select variables to plot ------------------------------
        sidebarPanel(
            
            # Select first plot input - State of Current Residence--------
            selectInput(inputId = "y", 
                        label = "Current State of Residence:",
                        choices = row_names_spaces,
                        selected = row_names_spaces[1]),
                        
            #Select second plot input - Metric----------------------------
            selectInput(inputId = "metric",
                        label = "Metric",
                        choices = column_names_spaces[2:8],
                        selected = column_names_spaces[2]),

            # Show first data table - State of Current Residence----------
            checkboxInput(inputId = "show_data1",
                          label = "Show data table",
                          value = TRUE),
          
            #Show second data table - Metric------------------------------
            checkboxInput(inputID = "show_data2",
                          label - "Show data table",
                          value = TRUE),
            
            # Enter text for plot title -----------------------------------------
            textInput(inputId = "plot_title", 
                      label = "Plot title", 
                      placeholder = "Enter plot title e.g. Where Residents of Alabama were Born"),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
        ),
        
        # Output: -------------------------------------------------------
        mainPanel(
            
            # Show first barchart --------------------------------------------
            plotOutput(outputId = "barchartstatebystate"),
            br(),        # a little bit of visual separation
            
            # Show second barchart
            plotOutput(outputId = "barchartmetric"),
            br(),        #a little bit of visual separation
           
            # Show data table ---------------------------------------------
            DT::dataTableOutput(outputId = "table_statebystate"),
            
            #Show second table --------------------------------------------
            DT:: dataTableOUtput(outputID = "table_metric"),
            
            #Download button -------------------------------------
            downloadButton(
                outputId = "downloadbutton",
                label = "Download",
                class = NULL,
                icon = shiny::icon("download"))
                
            )
        )
    )


# Define server function required to create the barplot ---------
server <- function(input, output, session) {
    
  # Create a subset of data filtering for selected State of Current Residence ------
  states_subset <- reactive({
    req(input$y) # ensure availablity of value before proceeding
    filter(states, input$y)
  })
  
  #Create a subset of data filtering for selected Metric---------------
  metric_subset <- reactive({
    req(input$metric) # ensure availability of value before proceeding
    filter(states,input$metric)
  })

    # Convert plot_title toTitleCase ----------------------------------
    pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
    # Create StatebyState barplot object the plotOutput function is expecting --
    output$barchartstatebystate <- renderPlot({
        barplot(data = states_subset,
                x= column_names_spaces[9:59],
                y= input$y,
                xlab="Born in",
                ylab = ("Currently living in "),
                title = pretty_plot_title(),
                col = "blue")
            
        })
    # Create the Metric barplot the 2nd plotOutput function is expecting
    output$barchartmetric <- renderPlot({
      barplot(data = metric_subset,
              x= row_names_spaces,
              y= input$metric,
              xlab= "State",
              ylab= input$metric,
              title= pretty_plot_title,
              col = "green")
    })
    
  
    # Print first data table if checked -------------------------------------
    output$table_statebystate <- DT::renderDataTable(
        if(input$show_data1){
            DT::datatable(data = states_subset, 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        })
    
    #Print second data table if checked ------------------------------------
    output$table_metric <- DT::renderDataTable(
      if(input$show_data2){
        DT::datatable(data = metric_subset,
                      options = list(pagelength=10),
                      rownames = FALSE)
      }
    )
    
}
    
    #Download data if checked ----------------------------------------
    output$downloadbutton <- downloadHandler(
      filename = function(){
        paste('states_pob.csv')
      },
      content = function(file) {
        write.csv(data, file)
      }
    )

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
