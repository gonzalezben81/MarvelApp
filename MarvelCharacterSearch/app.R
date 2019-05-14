#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(rmarkdown)
library(stringr)
library(shinycssloaders)
source("MarvelFunctions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # setBackgroundImage(src = "https://cdn.vox-cdn.com/thumbor/ONmWPwzm8uxZ-fW-T-Hbhf6VV4w=/0x0:3840x2160/1200x800/filters:focal(1674x298:2288x912)/cdn.vox-cdn.com/uploads/chorus_image/image/61237571/MSM_Screen_PS4Pro_E32018_00003_1528814671.0.jpg"),
  # Application title
  titlePanel(title = tags$h1("Marvel Character Searcher",style = "font-family: Comic Sans MS;
                             color: black;"),windowTitle = "Marvel Character Searcher"),
  # includeCSS("styles.css"),  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the name of the Marvel Comics Superhero you want to search for and click",br(),
               "Get Bio Info:"),
      textInput(inputId = "character_name",label = "Character Name:",value = "spider-man",width = "250px"),
      hr(),
      actionButton(inputId = "get_data",label = "Get Bio Info:"),
      numericInput(inputId = "numeric",label = "Number:",value = 1,min = 1,step = 1),
      hr(),
      downloadButton('downloadReport'),
      hr(),
      wellPanel(tags$b("Marvel Superhero/Vilian Image:"),id = "myid1",
                wellPanel(uiOutput(outputId = "image")%>% withSpinner(type = 4,color = "blue"))
                # imageOutput(outputId = "image", width = "100%", height = "400px", inline = FALSE)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      wellPanel(id = "myid2",fluidRow(column(3,textInput(inputId = "character_name_two",label = "Character Name:"))),
                fluidRow(column(3,textAreaInput(inputId = "bio",label = "Character Bio:",height = "250px")),
                         column(3,textInput(inputId = "num_ava",label = "# Comics Available:"),
                                  textInput(inputId = "series_ava",label = "Appeared in # Series:")))),
      wellPanel(fluidRow(column(4,uiOutput(outputId = "events_image")%>% withSpinner(type = 4,color = "blue")),
                column(4,uiOutput(outputId = "events_image_two")),
                column(4,uiOutput(outputId = "events_image_three"))))
      # wellPanel(tableOutput('movie_table'))
      
    )
  )
  )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  observeEvent(input$get_data,{
    info<- get_character_info(input$character_name)
    
    updateTextInput(session = session,inputId = "character_name_two",label = "Character Name:",value = info$data.results.name)
    updateTextAreaInput(session = session,inputId = "bio",label = "Character Bio:",value = info$data.results.description)
    updateTextAreaInput(session = session,inputId = "num_ava",label = "# Comics Available:",value = info$data.results.comics.available)
    updateTextAreaInput(session = session,inputId = "series_ava",label = "Appeared in # Series:",value = info$data.results.series.available)
  })
  
  
  
  
  observeEvent(input$get_data,{
    marvel_hash_params()
    info<- get_character_info(input$character_name)
    print(info)
    return(info)
    
  })
  poster<- eventReactive(input$get_data,{
    
    info<- get_character_info(input$character_name)
    thumbnail <- paste(info$data.results.thumbnail.path,".",info$data.results.thumbnail.extension,sep = "")
    print(thumbnail)
    return(thumbnail)
    
  })
  
  
  numerics<- reactive({ num<- input$numeric
           print(num)
           return(num)
  })
  
  events<- eventReactive(input$get_data,{
    # params <- marvel_hash_params()
    
    info<- get_character_info(input$character_name)
    
    res <- httr::GET(paste0("http://gateway.marvel.com/v1/public/characters/",info$data.results.id,"/events"),query=marvel_hash_params())
    
    res<- content(x = res,as = "text")
    
    infos <- fromJSON(res,flatten = TRUE)
    
    infos <- as.data.frame(infos)
    
    infos <- infos[numerics(),]
    infos <- as.data.frame(infos)
    print(infos)
    thumbnails <- paste(infos$data.results.thumbnail.path,".",infos$data.results.thumbnail.extension,sep = "")
    print("Hello")
    print(thumbnails)
    return(thumbnails)
    
    

  })
  
  bio_info <- eventReactive(input$get_data,{
    
    info<- get_character_info(input$character_name)
    bio_info<- info$data.results.description
    return(bio_info)
    
  })
  
  comics_available <- eventReactive(input$get_data,{
    info<- get_character_info(input$character_name)
    number_available<- info$data.results.comics.available
    return(number_available)
    
    
  })
  
  # mov_table<- eventReactive(input$get_data,{
  #   
  #   movie<- GET(paste0("http://www.omdbapi.com/?t='",input$movie_name,"'&y=",input$year,"&apikey=672de4d"))
  #   info<- content(x = movie,as = "text")
  #   info <- fromJSON(info, flatten = TRUE)
  #   info <- info$Ratings
  #   info <- as.data.frame(info)
  #   colnames(info) <- c("Site:","Score:")
  #   print(info)
  #   return(info)
  #   
  # })
  
  
  # output$movie_table <- renderTable(mov_table())
  # 
  output$image <- renderUI({
    tags$img(src=poster(), width="400px",height="400px")
    
    
  })
  
  output$events_image <- renderUI({
    tags$img(src=events(), width="400px",height="400px")
    
  })
  
  output$events_image_two <- renderUI({
    tags$img(src=events(), width="400px",height="400px")
    
  })
  
  output$events_image_three <- renderUI({
    tags$img(src=events(), width="400px",height="400px")
    
  })
  
  ###Marvel Trading Card 
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$character,s= poster(),l=bio_info(),set_author = input$character_name)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

