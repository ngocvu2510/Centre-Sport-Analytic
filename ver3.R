library(DT)
library(tidyverse)
library(ggplot2)
library(shiny)
#library(DT)


# Define UI
ui <- shinyUI(fluidPage(
  
  titlePanel("Example Volley App"),
  
  sidebarLayout(
    fileInput('target_upload', 'Choose file to upload',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                '.csv'
              ))
    #radioButtons("skill" , "Skill: " , choices = c("Serve","Reception","Dig","Set"
    #                                              ,"Attack","Block","Free Ball")),
    #DT::dataTableOutput("sample_table")
    ,
    mainPanel(
       
      
      #tableOutput("Table"),
      plotOutput("plot2",
      width = "75%",
      height = "300px",
      click = NULL,
      dblclick = NULL,
      hover = NULL,
      brush = NULL,
      inline = FALSE),
      uiOutput("Match"),
      uiOutput("Teams"),
      radioButtons("skill" , "Skill: " , choices = c("Serve","Reception","Dig","Set"
                                                     ,"Attack","Block","Free Ball")
                   , inline = TRUE),
      checkboxGroupInput("previous" , "Previous Hit Quality:" ,
                         choices = c("All" , "0","1","2","3","4","5")
                   , inline = TRUE),
      #uiOutput("ComboSelection")
      uiOutput('ComboSelection'),
      actionButton("Apply", "Apply Settings")
    )
  )
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = ',')
    skill <- input$skill
    if (skill != "Set"){
      key <- substr(skill, 1,1)}
    else{
      key <- "E"
    }
    Previous <- input$previous
    if("All" %in% Previous)
      Previous <- c('1','2','3','4','5','0')
      #df <- filter(df, PreviousHit %in% Previous)
      
    
    #df <- filter(df, Skill == key)
    
    #combosvec <- input$combo
    
    #if ((skill == "Set")|(skill == "Attack"))
    #df <- filter(df, Combination.Settercall %in% combosvec)
    
    return(df)})

    
  filterdata <- reactive({
    
    #reactive({
    inFile <- input$target_upload
  
    if (is.null(inFile))
      return(NULL)
    df <- df_products_upload()
    
    skill <- input$skill
    if (skill != "Set"){
      key <- substr(skill, 1,1)}
    else{
      key <- "E"
    }
    Previous <- input$previous
    if("All" %in% Previous)
      Previous <- c('1','2','3','4','5','0')
    combosvec <- input$combo
    games <- input$match
    team <- input$team
  
#      combosvec <- unique(df$Combination.Settercall)
    df <- filter(df, Skill == key)
    df <- filter(df, team==Team.Name)
    if("All" %in% Previous)
      Previous <- c('1','2','3','4','5','0')
    df <- filter(df, PreviousHit %in% Previous)
    
    if ((skill == "Set")|(skill == "Attack"))
      if("All" %in% combosvec)
        combosvec <- unique(df$Combination.Settercall)
      df <- filter(df, Combination.Settercall %in% combosvec)
      if (is.null(input$match))
        return(df)
      df <- filter(df, Match %in% games)
      return(df)
    })
  
    
    
    
  

  
  
 
  
  output$plot2 <- 
    renderPlot({
    if(is.null(df<-filterdata())) 
      return(NULL)

 
    df <- filterdata()
    if (is.null(df))
      return(NULL)
    plot <- ggplot(data = df, aes(x=Result)) +
                       
      stat_count(aes(y = 100*(..count..)/sum(..count..))) +
      ylab("Percent")
    return(plot)})
    
     

    
  
    
  output$ComboSelection <-renderUI({
    df <- df_products_upload()
    if(is.null(df))
      return(NULL)
    skill <- input$skill
    if (skill != "Set"){
      key <- substr(skill, 1,1)}
    else{
      key <- "E"
    }
    df <- filter(df, Skill == key)
    X <- unique(df$Combination.Settercall)
    boxes <- c("All",X)
    
    checkboxGroupInput("combo" , "Attack Combination/Setter Calls:"
                       , choices = (boxes)
                       , inline = TRUE
                       )}
    
    )
  output$Match <-renderUI ({
    matchdf <- df_products_upload()
    if(is.null(matchdf))
      return(NULL)
   # matches <- unique(c(matchdf$Match, df$Date))
    
   # boxnames <- unique(matches)
    boxvalues <- unique(matchdf$Match)
    
    selectInput('match','Match:',choice=boxvalues,multiple=TRUE)
    })
    
  
  output$Teams <-renderUI({
    df <- df_products_upload()
    if (is.null(df))
      return(NULL)
    
    matches <- input$match
    #df <- filter(df, matches)
    
    boxes <- unique(df$Team.Name)
    radioButtons("team","Team:",choices=(boxes),inline=TRUE)
  })
})


  


shinyApp(ui = ui, server = server)
