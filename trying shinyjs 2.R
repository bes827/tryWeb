

library(shiny)
library(shinyjs)
library(glue)
library(tidyverse)
library(lubridate)
library(httr)

# Define UI for displaying current time ----
ui <- fluidPage(
  useShinyjs(),
  textInput("zipp", label = h3("Your ZIP code:"), value = ""),
  actionButton("act", "Submit"),
  hr(),
  htmlOutput("inc"),
  h2(textOutput("localtime")),
  h2(textOutput("tillfagr")),
  br(),
  br(),
  h2(textOutput("target1")),
  h2(textOutput("till1")), 
  uiOutput('my_audio1')
 
)



server <- function(input, output, session) {
  
  
  #functions: 
  get_time <- function(time = now()) {
    time %>%
      str_split(" ") %>%
      map_chr(2) %>%
      lubridate::hms()
  }
  
  #upload the zipcode db:
  zipdb = readRDS(file = "www/zipl.rds")
  
  #Define the variables:
  
  nzip <- eventReactive(input$act, {
    req(input$zipp)
    input$zipp
  })
  
  
  timezo <- eventReactive(input$act, {
    req(input$zipp)
    
    zipdb %>% 
      filter(zipdb$zipcode %in% nzip()) %>% 
      select("timezone") %>% 
      as.character("timezone")
  })  
  
  
  output$timezo <- renderText({
    
    paste("reactive timezone", timezo())
  })
  
  
  
  county <- eventReactive(input$act, {
    req(input$zipp)
    
    zipdb %>% 
      filter(zipdb$zipcode %in% nzip()) %>% 
      select("county") %>% 
      as.character("county")
  })  
  
  
  state <- eventReactive(input$act, {
    req(input$zipp)
    
    zipdb %>% 
      filter(zipdb$zipcode %in% nzip()) %>% 
      select("state") %>% 
      as.character("state")
  })  
  
  #Extract 
  #1)fagr time 
  fgr2 <- eventReactive(input$act, {
    req(input$zipp)
    r <- GET("http://www.islamicfinder.us/index.php/api/prayer_times", 
             query = list(zipcode = nzip() , country = "US", method="2"))
    xx = content(r)
    xx = xx$results
    xx = xx$Fajr
    xx = gsub(" %am%", ":00", xx) 
    lubridate::hms(xx)
  })
  
  
  
  #2)Local time (updates every second):
  ltime <- reactive({
    req(input$zipp)
    invalidateLater(1000, session)
    
    get_time(lubridate::with_tz(now(), tzone = paste("US/",timezo(), sep="")))
  }) 
  

  #Times till::
  #time til fagr: 
  tillfagr <- reactive({
    req(input$zipp)
    invalidateLater(1000, session)
    
    x=as.numeric(fgr2()-ltime())
    x1=ifelse(x >=0, x,86400+x)
    x2=seconds_to_period(x1)
    x2
    
  }) 
  
 
  
  
  tillduaa <- reactive({
    req(input$zipp)
    invalidateLater(1000, session)
    
    x=as.numeric(fgr2()-ltime())
    x1=ifelse(x >=0, x,86400+x)
    x2=x1-(5*60)
    x3=seconds_to_period(x2)
    x3
  }) 
  
  
  tillquran <- reactive({
    req(input$zipp)
    invalidateLater(1000, session)
    
    x=as.numeric(fgr2()-ltime())
    x1=ifelse(x >=0, x,86400+x)
    x2=x1-(20*60)
    x3=seconds_to_period(x2)
    x3
  })
  
  
  
  observe({
    req(input$act)
 
    
    htmop = glue('<p><strong>Zip code:</strong>: {nzip()}&nbsp;</p>
<p><strong>State: </strong>{state()}&nbsp;</p>
<p><strong>County: </strong>{county()}</p>
<p><strong>Time zone: </strong>{timezo()}&nbsp;</p>
<p><strong>Fagr:</strong> {fgr2()}</p>
<hr />
<p><strong>Local time:</strong> {ltime()}</p>
<p><strong>Time till fagr (azan)</strong>: {tillfagr()}</p>
<p><strong>Time till Quran(-20)</strong>: {tillquran()}</p>
<p><strong>Time till Duaa(-5)</strong>: {tillduaa()}</p>
<hr />')
    
    
    output$inc<-renderUI({
      HTML (htmop)
    })
    
  })
  
 
  #collect(cache) fixed time points (after pressing submit):
  #
  #
  
  ltime.fix <- reactive({
    req(input$act)
 
    
    get_time(lubridate::with_tz(now(), tzone = paste("US/",timezo(), sep="")))
  }) 
  
  
  
  adan.fix <- reactive({
    req(input$act)
    x=as.numeric(fgr2()-ltime.fix())
    x1=ifelse(x >=0, x,86400+x)
    x2=seconds_to_period(x1)
    x2
    
  })

 
  tillfagr.fix <- reactive({
    req(input$act)
    x=as.numeric(fgr2()-ltime.fix())
    x1=ifelse(x >=0, x,86400+x)
    x2=seconds_to_period(x1)
    x2
    
  }) 
  
  
  
  
  tillduaa.fix <- reactive({
    req(input$act)
    x=as.numeric(fgr2()-ltime.fix())
    x1=ifelse(x >=0, x,86400+x)
    x2=x1-(5*60)
    x3=seconds_to_period(x2)
    x3
  }) 
  
  
  tillquran.fix <- reactive({
    req(input$act)
    x=as.numeric(fgr2()-ltime.fix())
    x1=ifelse(x >=0, x,86400+x)
    x2=x1-(20*60)
    x3=seconds_to_period(x2)
    x3
  })
  
  
  
  
  
  
  ########
  
  #extract local time: 
  output$localtime <- renderText({
    invalidateLater(1000, session)
    glue('Local time:{ltime()}')
  })
  
  
  output$tillfagr <- renderText({
    invalidateLater(1000, session)
    glue('Till fagr: {fgr2()-ltime()}')
  })
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", format(Sys.time(), "%H:%M:%S"))
  })
 
  t1 <- isolate({
 
    Sys.time() + 80
    
  })
  
  
  
  
  output$target1 <- renderText({
 
    glue("Fixed 
         till fagr time: {adan.fix()}  
         till duaa: {tillduaa.fix()}  
         till quran: {tillquran.fix()}
         
         ")
  })
 
  
  output$till1 <- renderText({
 
    invalidateLater(1000, session)
    
    paste("Seconds till target:", round (t1-Sys.time()))
  })
 
  
  sys1 = isolate({
    Sys.time()
  })
  
  observe({
    print(adan.fix())
 
    print(sys1)
    print(t1)
    print(t1-sys1)
    print(as.numeric(t1-sys1, units = "secs")*1000)
  })
  
  
  delay_by <- (as.numeric(t1-sys1, units = "secs")-60)*1000
 
 
  
  observe({
 
     #req(input$play) 
    
    delay (delay_by, 
     
  output$my_audio1 <-renderUI({
               tags$audio(src = "https://github.com/bes828/files/blob/main/fgr2.mp3?raw=true", 
                          type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
    })
  )
 
    delay (delay_by, 
           showModal(modalDialog(
             title = "",
             "Audio should play now"))  
    )
  })
  
  
  observe({
    
    #req(input$play) 
    
    delay (delay_by+20000, 
           
           output$my_audio1 <-renderUI({
             tags$audio(src = "https://www.soundhelix.com/examples/mp3/SoundHelix-Song-7.mp3", 
                        type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
           })
    )
  })
   
 
}

# Create Shiny app ----
shinyApp(ui, server)

