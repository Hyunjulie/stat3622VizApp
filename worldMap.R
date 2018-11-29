## packages required ##

install.packages("googleVis")
install.packages("plyr")
install.packages(shiny)

suppressPackageStartupMessages(library(googleVis))
library(plyr)
library(shiny)


## Get data

#Read in medals data set

athletes <- read.csv("athlete_events.csv", header=T)
#Read in ISO codes to link Olympic country codes (NOC) with country name
iso <- read.csv("noc_regions.csv", header=T)

head(athletes)
head(iso)

## Prepare data

#Change header name and merge by NOC 
athletesWiso <- merge(athletes, iso, "NOC")

head(athletesWiso)

medal <- athletesWiso[ complete.cases(athletesWiso[ , c("Medal")]), ]

head(medal)

summer <- subset(medal, Season == "Summer")
head(summer)


#Change medal name to numeric value in 3 columns (probably a much tidyer way to do this exists)
summer$Gold <- ifelse(summer$Medal == "Gold", 1, 0)
summer$Silver <- ifelse(summer$Medal == "Silver", 1, 0)
summer$Bronze <- ifelse(summer$Medal == "Bronze", 1, 0)

head(summer)

#Summarize data by Edition(Year) and country, adding medal values and including a TOTAL column
mapsummary <- ddply(summer,.(Year, region), summarize,
                    Gold=sum(Gold), 
                    Silver=sum(Medal=Silver), 
                    Bronze=sum(medal=Bronze),
                    total=sum(c(Gold,Silver,Bronze)),
                    combined=paste("Gold:",Gold, " ", "Silver:", Silver, " ", "Bronze:", Bronze))


shinyApp(
  # Define UI for application that draws a world map
  shinyUI(fluidPage(
    
    # Application title
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
  )),
  
  shinyServer(function(input, output) {
    
    
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
  
  
  
)

