library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(googleVis)
library(plyr)

data_events <- read.csv("athlete_events.csv")
df.physical <-data_events %>% select(Height,Weight,Sex,Year,Sport) %>% filter(!is.na(Height),!is.na(Weight))
df.physical <-as.data.frame(df.physical)
iso <- read.csv("noc_regions.csv", header=T)
## Prepare data

#Change header name and merge by NOC 
athletesWiso <- merge(data_events, iso, "NOC")
medal <- athletesWiso[complete.cases(athletesWiso[ , c("Medal")]), ]
summer <- subset(medal, Season == "Summer")


#Change medal name to numeric value in 3 columns (probably a much tidyer way to do this exists)
summer$Gold <- ifelse(summer$Medal == "Gold", 1, 0)
summer$Silver <- ifelse(summer$Medal == "Silver", 1, 0)
summer$Bronze <- ifelse(summer$Medal == "Bronze", 1, 0)
#Summarize data by Edition(Year) and country, adding medal values and including a TOTAL column
mapsummary <- ddply(summer,.(Year, region), summarize,
                    Gold=sum(Gold), 
                    Silver=sum(Medal=Silver), 
                    Bronze=sum(medal=Bronze),
                    total=sum(c(Gold,Silver,Bronze)),
                    combined=paste("Gold:",Gold, " ", "Silver:", Silver, " ", "Bronze:", Bronze))


ui<- navbarPage("Visualizing Olympics",
                tabPanel("Distribution of Height/Weight for Each Sport", 
                         titlePanel("Distribution of Height/Weight for Each Sport"),
                         sidebarLayout(
                           sidebarPanel(
                             helpText("Create a scatter plot of height/weight for every sport in the Olympics."),
                             
                             htmlOutput("sport_selector"),
                             htmlOutput("year_selector"),
                             htmlOutput("sex_selector")
                             
                           )
                           ,
                           mainPanel(
                             h2("Scatter Plot of Height VS Weight"),
                             plotlyOutput("olympicphysical"),
                             h3("Summary of Height"),
                             verbatimTextOutput("summary"),
                             h3("Summary of Weight"),
                             verbatimTextOutput("summary2")
                           )
                         )
                         
                         ),
                tabPanel("Wold map of Olympic medals by Year",
                         headerPanel(h1("Wold map of Olympic medals by Year")),
                         # Sidebar with a slider input Olympic Year 
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             sliderInput("Year", "Select Year:", 
                                         min = 1896, max = 2008, value = 1896, step = 4,
                                         sep="",  ticks=FALSE),
                             br(),
                             br(),
                             img(src="https://upload.wikimedia.org/wikipedia/en/thumb/b/b1/Olympic_Rings.svg/1280px-Olympic_Rings.svg.png",height = 100, width = 200),
                             
                             br(),
                             br(),
                             br(),
                             br(),
                             h3("Olympics cancelled due to World Wars"),
                             p("There is no data for 1916, 1940 and 1944")
                           ),
                           # Show world map for selected year
                           mainPanel(
                             h3(textOutput("year")),
                             h5("Hover mouse over each country for the breakdown of medals won"),
                             htmlOutput("TT")
                           )
                         )
                )
)

server <- shinyServer(function(input,output){
  output$sport_selector = renderUI({
    selectInput(inputId="sport", label= "Sport:", choices=as.character(unique(df.physical$Sport))
    )})
  output$year_selector = renderUI({
    selectInput(inputId = "year", label="Year:", choices=sort(unique(df.physical$Year)))
  })
  output$sex_selector = renderUI({
    selectInput(inputId = "Sex",label="Sex:", choices=unique(df.physical$Sex))
  })
  output$olympicphysical = renderPlotly({
    data_available=df.physical[df.physical$Sport == input$sport, ]
    data_available1=data_available[data_available$Year == input$year, ]
    data_available2=data_available1[data_available1$Sex == input$Sex, ]
    if (dim(data_available2)[1]>0){
      
      plot_ly(data = data_available2, x = data_available2$Height, y = data_available2$Weight, type = "scatter",
              marker = list(size = 10,
                            color = 'rgba(255, 182, 193, .9)',
                            line = list(color = 'rgba(152, 0, 0, .8)',
                                        width = 2)))
    }
  })
  output$summary = renderPrint({
    data_available=df.physical[df.physical$Sport == input$sport, ]
    data_available1=data_available[data_available$Year == input$year, ]
    data_available2=data_available1[data_available1$Sex == input$Sex, ]
    if (dim(data_available2)[1]>0){
      summary(data_available2$Height)
    }
  })
  output$summary2 = renderPrint({
    data_available=df.physical[df.physical$Sport == input$sport, ]
    data_available1=data_available[data_available$Year == input$year, ]
    data_available2=data_available1[data_available1$Sex == input$Sex, ]
    if (dim(data_available2)[1]>0){
      summary(data_available2$Weight)
    }
  })
  
  #Receive slider input for year  
  myYear <- reactive({
    
    input$Year
    
  })
  
  output$year <- renderText({
    paste("Olympic Medals for each competing country in ", myYear())
  })
  
  ## Define output     
  output$TT <- renderGvis({
    
    #subset data for selected year        
    mapdata <- mapsummary[mapsummary$Year==myYear(),]
    
    #GoogleVis maps    
    gvisGeoChart(mapdata, locationvar = "region",
                 colorvar = "total",
                 hovervar = "combined", 
                 options = list(width=1000,height=800,
                                colorAxis="{colors:['#1831C3', '#F70802', '#F7E802']}"))
  })
  
})
shinyApp(ui = ui, server = server)


