# Define UI for data upload app ----
options(shiny.maxRequestSize = 900000*1024^2)

ui <- fluidPage( theme = shinytheme("yeti"), # slate
                 # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
                 
                 # App title ----
                 titlePanel("Predictive Passenger Arrivals Models"),
                 
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                   # Sidebar panel for inputs ----
                   sidebarPanel(
                     # Input: Select a file ----
                     fileInput("file1", "Choose CSV File",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"),
                               width = "80%"),
                     # Horizontal line ----
                     tags$hr(),
                     # https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput
                     # I() indicates it is raw JavaScript code that should be evaluated, instead
                     # of a normal character string
                     selectizeInput(
                       'station', 'Select Station', choices = c("CEN", "MOK", "PRE", "TSW", "WAC", "CAB", "TIH", "TIS"),
                       options = list(
                         placeholder = 'Please select a station',
                         onInitialize = I('function() { this.setValue(""); }')
                       ), width="80%"
                     ),
                     
                     # selectInput("station", "Select the station", choices = c("CEN", "MOK", "PRE", "TSW", "WAC", "CAB", "TIH", "TIS"), 
                     #             selected=FALSE, width="50%", multiple=FALSE, selectize = FALSE),
                     tags$hr(),
                     actionButton("pri", "Print the data", width="80%"),
                     tags$hr(),
                     actionButton("pre", "Preprocess the data", width="80%"),
                     # Horizontal line ----
                     tags$hr(),
                     actionButton("pred", "Make predictions", width="80%"),  
                     tags$hr(),
                     # Add a download button
                     downloadButton(outputId = "download_data", label = "Download predictions"),
                     downloadButton(outputId = "download_hist", label = "Download hist_avg"),
                     downloadButton(outputId = "download_y", label = "Download true demand")
                     
                     
                     
                   ),
                   
                   # Main panel for displaying outputs ----
                   mainPanel(
                     
                     plotOutput("myPlot"),
                     
                     tableOutput("myTable"),
                     
                     verbatimTextOutput("sum")
                     
                     
                   )
                   
                 )
)
