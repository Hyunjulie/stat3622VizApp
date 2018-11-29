
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

data_events <- read.csv("athlete_events.csv")
df.physical <-data_events %>% select(Height,Weight,Sex,Year,Sport) %>% filter(!is.na(Height),!is.na(Weight))
df.physical <-as.data.frame(df.physical)
ui<- fluidPage(
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
)
server=shinyServer(function(input,output){
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
})
shinyApp(ui = ui, server = server)


