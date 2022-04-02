#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This application allows to evaluate the normality of a variable of a data frame.
# The data frame must be loaded on the first tab "Upload data". After that,
# a variable must be selected from the frame in the "Select column" tab.
# The following tabs help to decide whether the variable is normally
# distributed.

# the application is dependent on the following packages:
if(!require(moments)){
    install.packages("moments")
    library(moments)
}
if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
}

# Define UI for application
ui <- navbarPage(
    
    # Application title
    "Normality evaluation",

    # Application is subset in tabs
    # tab #1: load data
    tabPanel("1) Upload data",
             fileInput("file",
                       "Select file for upload"),
             
             checkboxInput("header", "Enable if the data has a header", TRUE),
             
             radioButtons("sep", "What is the separator?",
                          choices = c(Comma = ",",
                                      Semicolon = ";",
                                      Tab = "\t"),
                          selected = ","),
             
             radioButtons("quote", "What is the quoting character?",
                          choices = c(None = "",
                                      "Double Quote" = '"',
                                      "Single Quote" = "'"),
                          selected = '"'),
             ),
    
    # tab #2: Choose column
    tabPanel("2) Select column",
             conditionalPanel(
                 condition = is.null("input.file"),
                 textOutput("placeholder"),tags$hr(),),
             
             actionButton("choice", "Please first click here to import column names"),
             
             tags$hr(),
             
             selectInput("columns", "Select a column to evaluate normality in further tabs", choices = NULL),
             
             tableOutput("contents"),
             
             tableOutput("Table_selected.col")),
    
    # tab #3: illustrate data
    tabPanel("3) Plot column",
             sidebarPanel(
                selectInput("trafo", "Data transformation (for this and all further tabs):",  
                            c("None" = "none",
                              "log10(x)" = "log",
                              "log10(1+x)" = "log2",
                              "sqrt(x)" = "sqrt",
                              "asin(sqrt(x)) {range x from 0 to 1}" = "arcsin"
                            ), 
                            multiple = FALSE),
                
                radioButtons("figure", "Choose figure", c("Histogram", "Boxplot")),
                
                conditionalPanel(
                    condition = 'input.figure == "Histogram"',
                    numericInput(inputId="nbins",
                                 label="Number of bins",
                                 value=15))
                ),
             
             mainPanel(
                 conditionalPanel(
                     condition = is.null("input.file"),
                     textOutput("placeholder2")),
                 
                 conditionalPanel(
                     condition = "input.columns == ''",
                     textOutput("placeholder6")),
                 
                 plotOutput("plot", width="500px", height = "500px"))),
    
    # tab #4: QQ plot
    tabPanel("4) QQ Plot", 
             conditionalPanel(
                 condition = is.null("input.file"),
                 textOutput("placeholder3")),
             
             conditionalPanel(
                 condition = "input.columns == ''",
                 textOutput("placeholder7")),
             
             plotOutput("qqplot", width="500px", height = "500px")),
    
    # tab #5: Skewness and Kurtosis
    tabPanel("5) Skewness and Kurtosis", 
             conditionalPanel(
                 condition = is.null("input.file"),
                 textOutput("placeholder4")),
             
             conditionalPanel(
                 condition = "input.columns == ''",
                 textOutput("placeholder8")),
             
             htmlOutput("text")),
    
    # tab #6: Normality test
    tabPanel("6) Shapiro-wilk test", 
             conditionalPanel(
                 condition = is.null("input.file"),
                 textOutput("placeholder5")),
             
             conditionalPanel(
                 condition = "input.columns == ''",
                 textOutput("placeholder9")),
             
             htmlOutput("test"))
)

# Define server logic
server <- function(input, output, session) {
    
    # required for loading data from file
    data <- reactive({
        req(input$file)
        
        userdata <- read.csv(input$file$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
        
        return(userdata)
    })
    
    # required for drawing figure
    output$plot <- renderPlot({
        c <- column()
        c <- subset(c, select = input$columns) 
        if(input$trafo == "none") {}
        else if(input$trafo == "log") {c <- log10(c)}
        else if(input$trafo == "log2") {c <- log10(1+c)}
        else if(input$trafo == "arcsin") {c <- asin(sqrt(c))}
        else if(input$trafo == "sqrt") {c <- sqrt(c)}
        if(input$figure == "Histogram") { hist(c[[1]], main="", input$nbins, xlab=input$columns)}
        if(input$figure == "Boxplot") { boxplot(c[[1]], main="", xlab=input$columns)
        }
    })
    
    # required for drawing QQ Plot
    output$qqplot <- renderPlot({
        c <- column()
        c <- subset(c, select = input$columns)
        if(input$trafo == "none") {}
        else if(input$trafo == "log") {c <- log10(c)}
        else if(input$trafo == "log2") {c <- log10(1+c)}
        else if(input$trafo == "arcsin") {c <- asin(sqrt(c))}
        else if(input$trafo == "sqrt") {c <- sqrt(c)}
        qqnorm(c[[1]])
        qqline(c[[1]], col="blue", lwd=2)
    })
    
    # required for table
    output$contents <- renderTable({
        req(data())
        data()
    })
    
    # requred for choosing column
    column <- eventReactive(input$choice, {
        req(input$file)
        
        userdata <- read.csv(input$file$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
        
        vars <- names(userdata)

        updateSelectInput(session, "columns", "Select a column to evaluate normality in further tabs", choices = vars)
        
        userdata
    })
    
    output$Table_selected.col <- renderTable({
        c <- column()
        c <- subset(c, select = input$columns) 
        return(NULL)
    })
    
    # required for Skewness and Kurtosis
    output$text <- renderText({
        c <- column()
        c <- subset(c, select = input$columns) 
        if(input$trafo == "none") {}
        else if(input$trafo == "log") {c <- log10(c)}
        else if(input$trafo == "log2") {c <- log10(1+c)}
        else if(input$trafo == "arcsin") {c <- asin(sqrt(c))}
        else if(input$trafo == "sqrt") {c <- sqrt(c)}
        HTML("Skewness: ",skewness(c[[1]]), 
               "<br> Kurtosis: ", kurtosis(c[[1]]))
    })
    
    # required for normality test
    output$test <- renderText({
        c <- column()
        c <- subset(c, select = input$columns) 
        if(input$trafo == "none") {}
        else if(input$trafo == "log") {c <- log10(c)}
        else if(input$trafo == "log2") {c <- log10(1+c)}
        else if(input$trafo == "arcsin") {c <- asin(sqrt(c))}
        else if(input$trafo == "sqrt") {c <- sqrt(c)}
        s <- shapiro.test(c[[1]])
        W <- gsub("W = c\\(", "", s[[1]])
        HTML("W = ", W, "<br> p = ", s[[2]])
    })
    
    # remind user to upload data
    output$placeholder <- output$placeholder2 <- output$placeholder3 <- output$placeholder4 <- output$placeholder5 <-renderText({
        ifelse(is.null(input$file), yes = "Please upload data in tab 1", no = "") })
    
    # remind user to choose column
    output$placeholder6 <- output$placeholder7 <- output$placeholder8 <- output$placeholder9 <-renderText({
        ifelse(input$columns == "", yes = "Please select a column in tab 2", no = "") })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
