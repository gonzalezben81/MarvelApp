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
      helpText("Enter the name of the Marvel Comics Superhero you want to search for and click"),
               hr(),
               includeHTML("include.html"),
      textInput(inputId = "character_name",label = "Character Name:",value = "spider-man",width = "250px"),
      hr(),
      actionButton(inputId = "get_data",label = "Get Bio Info:"),
      # numericInput(inputId = "numeric",label = "Number:",value = 1,min = 1,step = 1),
      hr(),
      downloadButton('downloadReport'),
      hr(),
      wellPanel(tags$b("Marvel Superhero/Villain Image:"),id = "myid1",
                wellPanel(uiOutput(outputId = "image")%>% withSpinner(type = 4,color = "blue")),
                wellPanel(textAreaInput(inputId = "bio",label = "Character Bio:",height = "250px")),
                wellPanel(fluidRow(textInput(inputId = "num_ava",label = "# Comics Available:"),
                          textInput(inputId = "series_ava",label = "Appeared in # Series:")))
      )
    ),
    

    mainPanel(
      wellPanel(fluidRow(column(6,textInput(inputId = "title_one",label = "Title:",width = "250px"),
                                uiOutput(outputId = "events_image"),
                                textAreaInput(inputId = "descrip_one",label = "Description",width = "400px",height = "150px")),
                         column(6,textInput(inputId = "title_two",label = "Title:",width = "250px"),
                                uiOutput(outputId = "events_image_two"),
                                textAreaInput(inputId = "descrip_two",label = "Description",width = "400px",height = "150px")))),
      wellPanel(fluidRow(column(6,textInput(inputId = "title_three",label = "Title:",width = "250px"),
                                uiOutput(outputId = "events_image_three"),
                                textAreaInput(inputId = "descrip_three",label = "Description",width = "400px",height = "150px")),
                         column(6,textInput(inputId = "title_four",label = "Title:",width = "250px"),
                                uiOutput(outputId = "events_image_four"),
                                textAreaInput(inputId = "descrip_four",label = "Description",width = "400px",height = "150px")))),
      wellPanel(fluidRow(column(6,textInput(inputId = "title_five",label = "Title:",width = "250px"),
                                uiOutput(outputId = "events_image_five"),
                                textAreaInput(inputId = "descrip_five",label = "Description",width = "400px",height = "150px")),
                        column(6,textInput(inputId = "title_six",label = "Title:",width = "250px"),
                                uiOutput(outputId = "events_image_six"),
                                textAreaInput(inputId = "descrip_six",label = "Description",width = "400px",height = "150px"))))

    )
  )
  )

##Marvel Server Logic
server <- function(input, output,session) {
  
  ##Updates the data to present to the end user
  observeEvent(input$get_data,{
    info<- get_character_info(input$character_name)
    
    updateTextInput(session = session,inputId = "character_name_two",label = "Character Name:",value = info$data.results.name)
    updateTextAreaInput(session = session,inputId = "bio",label = "Character Bio:",value = info$data.results.description)
    updateTextAreaInput(session = session,inputId = "num_ava",label = "# Comics Available:",value = info$data.results.comics.available)
    updateTextAreaInput(session = session,inputId = "series_ava",label = "Appeared in # Series:",value = info$data.results.series.available)
    updateTextInput(session = session,inputId = "title_one",label = "Title:",value = events()$title_one)
    updateTextAreaInput(session = session,inputId = "descrip_one",label = "Description:",value = events()$descrip_one)
    updateTextInput(session = session,inputId = "title_two",label = "Title:",value = events()$title_two)
    updateTextAreaInput(session = session,inputId = "descrip_two",label = "Description:",value = events()$descrip_two)
    updateTextInput(session = session,inputId = "title_three",label = "Title:",value = events()$title_three)
    updateTextAreaInput(session = session,inputId = "descrip_three",label = "Description:",value = events()$descrip_three)
    updateTextInput(session = session,inputId = "title_four",label = "Title:",value = events()$title_four)
    updateTextAreaInput(session = session,inputId = "descrip_four",label = "Description:",value = events()$descrip_four)
    updateTextInput(session = session,inputId = "title_five",label = "Title:",value = events()$title_five)
    updateTextAreaInput(session = session,inputId = "descrip_five",label = "Description:",value = events()$descrip_five)
    updateTextInput(session = session,inputId = "title_six",label = "Title:",value = events()$title_six)
    updateTextAreaInput(session = session,inputId = "descrip_six",label = "Description:",value = events()$descrip_six)
  })
  
  
  
  ##Gets the character info data
  observeEvent(input$get_data,{
    marvel_hash_params()
    info<- get_character_info(input$character_name)
    print(info)
    return(info)
    
  })
  
  ##Gets the Marvel character poster either hero or villain
  poster<- eventReactive(input$get_data,{
    
    info<- get_character_info(input$character_name)
    thumbnail <- paste(info$data.results.thumbnail.path,".",info$data.results.thumbnail.extension,sep = "")
    print(thumbnail)
    return(thumbnail)
    
  })
  
  
  # numerics<- reactive({ num<- input$numeric
  #          print(num)
  #          return(num)
  # })
  
  ##Gets the series event data for the superhero or villain
  events<- eventReactive(input$get_data,{
    # params <- marvel_hash_params()
    
    info<- get_character_info(input$character_name)
    
    res <- httr::GET(paste0("http://gateway.marvel.com/v1/public/characters/",info$data.results.id,"/events"),query=marvel_hash_params())
    
    res<- content(x = res,as = "text")
    
    infos <- fromJSON(res,flatten = TRUE)
    
    infos_data <- as.data.frame(infos)
    
    infos <- infos_data[1,]
    infos_two <- infos_data[2,]
    infos_three <- infos_data[3,]
    infos_four <- infos_data[4,]
    infos_five <- infos_data[5,]
    infos_six <- infos_data[6,]
    
    
    infos <- as.data.frame(infos)
    infos_two <- as.data.frame(infos_two)
    infos_three <- as.data.frame(infos_three)
    infos_four <- as.data.frame(infos_four)
    infos_five <- as.data.frame(infos_five)
    infos_six <- as.data.frame(infos_six)
    

    thumbnails_one <- paste(infos$data.results.thumbnail.path,".",infos$data.results.thumbnail.extension,sep = "")
    thumbnails_two <- paste(infos_two$data.results.thumbnail.path,".",infos_two$data.results.thumbnail.extension,sep = "")
    thumbnails_three <- paste(infos_three$data.results.thumbnail.path,".",infos_three$data.results.thumbnail.extension,sep = "")
    thumbnails_four <- paste(infos_four$data.results.thumbnail.path,".",infos_four$data.results.thumbnail.extension,sep = "")
    thumbnails_five <- paste(infos_five$data.results.thumbnail.path,".",infos_five$data.results.thumbnail.extension,sep = "")
    thumbnails_six <- paste(infos_six$data.results.thumbnail.path,".",infos_six$data.results.thumbnail.extension,sep = "")
    
    title_one <- infos$data.results.title
    title_two <- infos_two$data.results.title
    title_three <- infos_three$data.results.title
    title_four <- infos_four$data.results.title
    title_five <- infos_five$data.results.title
    title_six <- infos_six$data.results.title
    
    descrip_one <- infos$data.results.description
    descrip_two <- infos_two$data.results.description
    descrip_three <- infos_three$data.results.description
    descrip_four <- infos_four$data.results.description
    descrip_five <- infos_five$data.results.description
    descrip_six <- infos_six$data.results.description
    
    pics_list <- list("one" = thumbnails_one,"two"=thumbnails_two,"three"=thumbnails_three,"four"=thumbnails_four,
                      "five"=thumbnails_five,"six"=thumbnails_six,"title_one"=title_one,"title_two"=title_two,
                      "title_three"=title_three,"title_four"=title_four,"title_five"=title_five,"title_six"=title_six,
                      "descrip_one"=descrip_one,"descrip_two"=descrip_two,"descrip_three"=descrip_three,"descrip_four"=descrip_four,
                      "descrip_five"=descrip_five,"descrip_six"=descrip_six)
    print(pics_list)
    return(pics_list)
    
    

  })
  
  ##Displays the bio info of the hero or villain
  bio_info <- eventReactive(input$get_data,{
    
    info<- get_character_info(input$character_name)
    bio_info<- info$data.results.description
    return(bio_info)
    
  })
  
  ##Returns the number of comics available for the hero or villain
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
  
  ##Outputs the images for the series the hero or villain is a part of
  output$image <- renderUI({
    tags$img(src=poster(), width="400px",height="400px")
    
    
  })
  
  output$events_image <- renderUI({
    tags$img(src=events()$one, width="400px",height="400px")

  })

  output$events_image_two <- renderUI({
    tags$img(src=events()[[2]], width="400px",height="400px")

  })

  output$events_image_three <- renderUI({
    tags$img(src=events()[[3]], width="400px",height="400px")

  })
  
  
  output$events_image_four <- renderUI({
    tags$img(src=events()[[4]], width="400px",height="400px")
    
  })
  
  output$events_image_five <- renderUI({
    tags$img(src=events()[[5]], width="400px",height="400px")
    
  })
  
  output$events_image_six <- renderUI({
    tags$img(src=events()[[6]], width="400px",height="400px")
    
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
      
      # Set up parameters to pass to the Rmd document: Character name, image, bio, and events list
      params <- list(n = input$character,s= poster(),l=bio_info(),set_author = input$character_name,comic_events=events())
      
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

