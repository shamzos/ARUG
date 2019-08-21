## app.R ##
#Libraries# 
library(shinydashboard)
library(shinyjs)
library(data.table)
library(DT)
library(plotly)
library(tsoutliers)
library(TSA)
library(lmtest)
library(astsa)
library(imputeTS)
library(adamethods)
library(autoencoder)
###

#User Interface
ui <- dashboardPage(
  dashboardHeader(title = "SAM dashboard"),
  dashboardSidebar(
    #Creat sidebar Menus.
    sidebarMenu(
      menuItem("Data", tabName = "Data", icon = icon("dashboard")),
      menuItem("Cleaning", tabName = "outlier", icon = icon("broom")),
      menuItem("Filling The Gaps", tabName = "nas", icon = icon("clipboard-check"))
      
    )#SidebarMenu
    
  ),#DashboardSidebar 
  #DashBoard Core
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems( # Total Items  Grouped items
      tabItem(tabName = "Data",  
              fluidRow(
                useShinyjs(),
                box(title = "File Input",
                    
                    checkboxInput("header", "Header", TRUE),
                    
                    selectInput("sep", "Separator:",
                                c("Comma" = ",",
                                  "Semicolon" = ";",
                                  "Tab" = "\t")
                    ),
                    
                    numericInput("skip", "Skip Rows:", 0, min = 0),
                    
                    numericInput("cn", "Fetch Columns names from row number:", 1, min = 1),
                    textInput("nanstring","NA charachter:",value = "?"),
                    
                    
                    fileInput("file1", "Choose CSV File",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    )
                ),
                box(title = "Plot Options",
                    radioButtons("plt", "Plot type:",
                                 c("Scatter Plot" = "markers",
                                   "Line Plot"   = "line")
                    ),
                    uiOutput("Xaxis"),
                    uiOutput("Yaxis"),
                    actionButton("goButton", "Go!")
                )
              ),
              
              fluidRow(
                box(title = "Plot",
                    width = NULL,
                    plotlyOutput("plot_intro"))
                
              ),
              fluidRow(
                
                tabBox(
                  title = "Data",
                  width = NULL,  # enforce width to match parent container
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  
                  tabPanel("Raw Data", DT::dataTableOutput("contents")),
                  tabPanel("Data Summary",verbatimTextOutput("sum"))
                )
              )
      ),#TabItem
  #### -------------------------------------------- Part II Outliers
      tabItem(tabName = "outlier",
              fluidRow(
                useShinyjs(),
                       box(title = "Outlier Parameters",
                           selectInput("algo_out", "Algorithmes:",
                                       c("ARIMA Based" = "arima",
                                         "KNN" = "knn",
                                         "Autoencoder" = "ANN")),
                           checkboxGroupInput("ArimaP", "please Select one of theme:",
                                              choices = c("additive outlier" = "AO",
                                                          "intervention outlier"="IO"),
                                              selected = "AO"),
                           
                           sliderInput("outlierper", "Outlier % to select", 
                                       min = 85, max = 99, value = 90, step= 1),
                           uiOutput("mcolumns"),
                           
                           actionButton("ABotton", "Apply")
                           ),
                box(title = "Plot Options",
                    radioButtons("plt_out", "Plot type:",
                                 c("Scatter Plot" = "markers",
                                   "Line Plot"   = "line")
                                 ),
                    uiOutput("Xaxis_out"),
                    uiOutput("Yaxis_out"),
                    actionButton("goButton2", "Go!")
                    )
                ),
              fluidRow(
                
                tabBox(
                  title = "Data",
                  width = NULL,  # enforce width to match parent container
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset2",
                  
                  tabPanel("Data", DT::dataTableOutput("Cleaned"))
                ),fluidRow(
                  box(title = "Plot",
                      width = NULL,
                      plotlyOutput("plot_out"))
                  
                )
                
              )
              
      ),#tabName
  ### -----------------------------------------   Part III Fill The NA's
      tabItem(tabName = "nas",
              fluidRow(
                useShinyjs(),
                box(title = "Fill the NA's value",
                    selectInput("algo_na", "Algorithmes:",
                                c("Interpolation",
                                  "kalman" = "na.kalman")),
                    
                    uiOutput("mcolumns_nas"),
                    
                    
                    actionButton("ABotton2", "Apply"),
                    downloadButton("downloadData", "Download")
                )
                #box(title = "Save Data into a File",
                #    uiOutput("mcolumns_csv"),
                #    downloadButton("downloadData", "Download")
                #    textInput("name","File's name"),
                #    actionButton("ABotton3", "Save")
                #)
                
              ),#fluidRow
              fluidRow(
                
                tabBox(
                  title = "Data",
                  width = NULL,  # enforce width to match parent container
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset3",
                  
                  tabPanel("Data", DT::dataTableOutput("filled"))
                ))
      )#tabName

      
    )
  )#DashBoardBody
)



# R server 
options(shiny.maxRequestSize=1024*1024^2) # to increase file size to read.

server <- function(input, output) {
  # creat observation of an event in order to enable or disable an numeric input
  observeEvent(input$header, {
    if(input$header == F){
      disable("cn")
    } else {
      enable("cn")
    }
  })
  # creat reactibe datatable
  d <- reactive({
    inFile <- input$file1
    
    #check existence of the file path
    if (is.null(inFile))
      return(NULL)
    # read the file
    g <- fread(inFile$datapath,
               skip = input$skip,
               header = input$header,
               sep =input$sep,
               stringsAsFactors = F ,
               na.strings = input$nanstring)
    # assign header in case of box header checked
    if(input$header){
      colnames(g) <- as.character(fread(inFile$datapath,stringsAsFactors = F,header = FALSE )[input$cn,])
    }
    g
  })
  #render the dataframe
  output$contents <- DT::renderDataTable(
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    d(),
    options = list(scrollX = TRUE)
  )
  
  # creat summary
  output$sum <- renderPrint({
    summary(d())
    })
  
  #creat a render UI to select X and Y axis from dataframe columns
  output$Xaxis <- renderUI({
    selectInput("Xaxis", "Select your X axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  output$Yaxis <- renderUI({
    selectInput("Yaxis", "Select your Y axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  # creat X and Y axis labels.
  xx <- reactive({
    list(
      title = input$Xaxis,
      showticklabels = FALSE
    )})
  
  yx <-  reactive({
    list(
      title = input$Yaxis
    )})
  
  
  # render the plot
  output$plot_intro <- renderPlotly({
    ploting()
    
  })
  
  
  
  # create the plot from all input previously
  ploting <- eventReactive(input$goButton, {
    data <- d()
    plot_ly(data = data, x = ~get(input$Xaxis), y = ~get(input$Yaxis), type = "scatter",mode = input$plt) %>%
      layout(xaxis = xx(), yaxis = yx())
  })
  
###### --------------------------------------- Part II
  # observe event to disable or enable arima parameters input
  observeEvent(input$algo_out, {
    if(input$algo_out == "arima"){
      enable("ArimaP")
    } else {
      disable("ArimaP")
    }
  })

  #enable and disable ANN
  observeEvent(input$algo_out, {
    if(input$algo_out %in% c("ANN","knn")  ){
      enable("outlierper")
      
    } else {
      disable("outlierper")
    
    }
  })
  

  #creat names list that has only numerics
  numeric_list <- reactive({
    names(unlist(lapply(d(), is.numeric)))
    })
  
 
  #selection from the list above
  output$mcolumns <- renderUI({
    selectInput("mcolumns", "Select Columns to clean Outliers",
                choices =numeric_list() ,multiple = TRUE)})
  #creat D_partII list with d as NULL
  D_partII <- reactiveValues(d=NULL)
  
  #observe event to apply algorithm once selected
  observeEvent(input$ABotton,{
    
    A <- as.data.frame(d())
    if(input$algo_out=="arima" ){
      for(i in input$mcolumns){
        s <- ts(A[,i]) # creat Time Series Object
        S_tso <- tso(s, 
                     discard.method="bottom-up", # to handle NA's
                     types = input$ArimaP,
                     maxit.iloop = 10, #Choice TA OA
                     maxit.oloop = 10
                     )
        ind <- S_tso$outliers$ind
        k  <- S_tso$yadj
        A[ind,i] <- k[ind]
      }
    }else if(input$algo_out=="knn"){
      for(i in input$mcolumns){
        x <- c(1:length(A[,i]))
        G <- data.frame(x,A[,i])
        colnames(G) <- c("x","y")
        k = as.integer(sqrt(length(x)))
        top_n = nrow(G) - nrow(G)*input$outlierper/100
        ind <- do_knno(G, k=k,top_n = top_n)
        A[ind,i] <- NA}
      }else{
        for( i in input$mcolumns ){
          
        nl=3
        unit.type = "tanh"
        Nx.patch=10
        Ny.patch=10
        N.input = Nx.patch*Ny.patch 
        N.hidden = 5*5
        lambda = 0.0002
        beta=6
        rho = 0.01
        epsilon <- 0.001
        max.iterations = 2000
        
        x <- c(1:length(A[,i]))
        G <- data.frame(x,A[,i])
        colnames(G) <- c("x","y")
        outliercount <- nrow(G) - nrow(G)*input$outlierper/100
        traind = as.matrix(G)
        
        autoencoder.object <- autoencode(X.train=traind,
                                         nl=nl,
                                         N.hidden=N.hidden,
                                         unit.type=unit.type,
                                         lambda=lambda,
                                         beta=beta,
                                         rho=rho,
                                         epsilon=epsilon,
                                         optim.method="BFGS",
                                         max.iterations=max.iterations,
                                         rescale.flag=TRUE,
                                         rescaling.offset=0.001)
        
        scores2 <- predict(autoencoder.object,X.input = traind)
        rajmse<-function(x_hat,x) rowMeans((x_hat-x)^2)
        score3 <- rajmse(G, scores2$X.output)
        d <- as.data.frame(score3)
        distance <- d[,1]
        temp <- cbind(G,distance)
        temp$cluster <- 1
        outlier <- order(temp$distance, decreasing=T)[1:outliercount]
        temp$outlier <- FALSE
        temp$outlier[outlier[1:outliercount]] <- TRUE 
        A[temp$outlier,i] <- NA
        }
        }
      
    
    D_partII$d <- A
  })
  
  #Render the last data from Outlier Detection algorithms
  output$Cleaned <- DT::renderDataTable(
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    D_partII$d,
    options = list(scrollX = TRUE)
  )
  
  # render Axis
  output$Xaxis_out <- renderUI({
    selectInput("Xaxis_out", "Select your X axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  output$Yaxis_out <- renderUI({
    selectInput("Yaxis_out", "Select your Y axis Variable",
                choices = colnames(d()), selected = d()[1])})
  
  
  #Axis Labes
  xx_out <- reactive({
    list(
      title = input$Xaxis_out,
      showticklabels = FALSE
    )})
  
  yx_out <-  reactive({
    list(
      title = input$Yaxis_out
    )})
  
  
  
  # creat plot with event reactive
  ploting_out <- eventReactive(input$goButton2, {
    data <- d()
    data2 <- D_partII$d
    
    plotlout <- plot_ly(data = data, x = ~get(input$Xaxis_out), y = ~get(input$Yaxis_out), type = "scatter",mode = input$plt_out) %>%
      layout(xaxis = xx_out(), yaxis = yx_out())
    if(input$plt_out=="line"){
      plotlout <- add_lines(plotlout, y = data2[,input$Yaxis_out] ,type = "scatter",mode = input$plt_out,name="Correction")
    }else{
      plotlout<- add_markers(plotlout, y = data2[,input$Yaxis_out] ,type = "scatter",mode = input$plt_out,name="Correction")
    }
    
    plotlout
  })
  #Render plot
  output$plot_out <- renderPlotly({
    ploting_out()
    
  })
  
  ###### --------------------------------------- Part III
  #Columns Selections
  output$mcolumns_nas <- renderUI({
    selectInput("mcolumns_nas", "Select Columns to fill the NA's",
                choices =numeric_list() ,multiple = TRUE)})
  
  #columns selected to save in csv Files
  output$mcolumns_csv <- renderUI({
    selectInput("mcolumns_csv", "Select Columns:",
                choices =numeric_list() ,multiple = TRUE)})
  
  
  
  D_partIII <- reactiveValues(d=NULL,f=NULL)
  
   observeEvent(input$ABotton2,{
    if(is.null(D_partII$d)){
      A <- d()
    }else{
      A <- D_partII$d
    }
    
    
   
    for(i in input$mcolumns_nas ){
      if( input$algo_na =="Interpolation"){
        A[,i] <- na.interpolation(A[,i])
      }else{
        A[,i] <- na.kalman(A[,i])
      }
      
    }
     D_partIII$d <- A
  })
  
  output$filled <- DT::renderDataTable(
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    D_partIII$d,
    options = list(scrollX = TRUE)
  )
  #Write the files base on the inputs above
  
  observeEvent(input$ABotton2,{
    if(is.null(D_partIII$d)){
      A <- D_partII$d
    }else{
      A <- D_partIII$d
    }
    
    D_partIII$f <- A
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(D_partIII$f, file,row.names = FALSE)
    }
  )
  
  
  
  
}

shinyApp(ui, server)