#install.packages("shiny")
#install.packages("highcharter")
#install.packages("dplyr")
#install.packages("viridis")
#install.packages("countrycode")


library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(viridis)
library(countrycode)

data_events <- read.csv("athlete_events.csv")
data_team <- read.csv("noc_regions.csv")
df <- data_events%>%filter(!is.na(Age),Season=='Summer', Year<2013)%>%group_by(Sex,Age,Year)%>%summarize(pop=n())
df$Sex <- ifelse(df$Sex=="F","Female","Male")

#Change female athlete numbers to negative to put it into a chart
df <- df%>%
  mutate(athletes = pop*ifelse(Sex == "Female", -1, 1))

series <- df %>% 
  group_by(Sex, Age)%>%
  do(data = list(sequence = .$athletes)) %>% 
  ungroup() %>% 
  group_by(Sex) %>% 
  do(data = .$data) %>%
  mutate(name = Sex)%>%
  list_parse()

maxpop <- max(abs(df$athletes))

xaxis <- list(categories = sort(unique(df$Age)),
              reversed = FALSE, tickInterval = 3,
              labels = list(step= 3))
yrs <-  sort(unique(df$Year))


#-----------Winter Season Data----- 

df1 <- data_events%>%filter(!is.na(Age),Season=='Winter')%>%group_by(Sex,Age,Year)%>%summarize(pop=n())
df1$Sex <- ifelse(df1$Sex=="F","Female","Male")
df1 <- df1%>%
  mutate(athletes = pop*ifelse(Sex == "Female", -1, 1))

series1 <- df1 %>% 
  group_by(Sex, Age)%>%
  do(data = list(sequence = .$athletes)) %>% 
  ungroup() %>% 
  group_by(Sex) %>% 
  do(data = .$data) %>%
  mutate(name = Sex)%>%
  list_parse()

maxpop1 <- max(abs(df1$athletes))

xaxis1 <- list(categories = sort(unique(df1$Age)),
              reversed = FALSE, tickInterval = 3,
              labels = list(step= 3))
yrs1 <-  sort(unique(df1$Year))

#---- Map of Winners Data ----- 

team <- data_events %>%left_join(data_team, by=c('NOC'='NOC'))%>%group_by(region) %>% summarise(mTotal = n())

count_medal<- data_events %>%left_join(data_team, by=c('NOC'='NOC')) %>% filter(!is.na(Medal))%>%group_by(region) %>% summarise(Medals = n())

team <- team%>%left_join(count_medal, by =c('region'='region'))
team$mTotal <- NULL
team[is.na(team)]<- 0

team$iso3 <- countrycode(team$region,  "country.name","iso3c")

data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(seq(0,.01,by=0.0025),seq(0.06,0.16,by=.05),seq(.31,1,by=.15)),
                        c = substring(viridis(13 , option = "C"), 0, 13)) %>%  list_parse2()



# UI ----------------------------------------------------------------
ui <- fluidPage(
  # App title -------------------------------------------------------
  titlePanel("Visualizing The Olympics"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs --------------------------------------------------------    
    sidebarPanel(
      selectInput("theme", 
                  label = "Theme",
                  choices = c("FiveThirtyEight" = "fivethirtyeight", 
                              "Chalk" = "chalk",
                              "Dark Unica" = "darkunica", 
                              "Economist" = "economist",
                              "Gridlight" = "gridlight", 
                              "Handdrawn" = "handdrawn", 
                              "Sandsignika" = "sandsignika"))
    ),
    
    # Output --------------------------------------------------------    
    mainPanel(
      tabsetPanel(
        tabPanel("Summer Olympic Games", highchartOutput("hcontainer", height = "500px")),
        tabPanel("Winter Olympic Games", highchartOutput("hcontainer2", height = "500px")),
        tabPanel("Medal Winners by Country", highchartOutput("hcontainer3", height = "500px"))
      )
    )
  )
)

# Define server logic required to draw highcharter ----
server = function(input, output) {

  # Highchart -------------------------------------------------------
  output$hcontainer <- renderHighchart({
    
    hc <- highchart() %>%
      hc_add_series_list(series) %>%
      hc_chart(type="bar") %>%
      hc_motion(enabled=TRUE, labels=yrs, series= c(0,1), autoplay=FALSE, updateInterval=4) %>%
      hc_plotOptions(
        series = list(stacking = "normal"),
        bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
      ) %>% 
      hc_tooltip(shared = TRUE) %>%
      hc_yAxis(min = -750, max = 750) %>%
      hc_xAxis(
        xaxis,
        rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0)))%>%      
      hc_title(text = "Athletes by Gender and Age",
               style = list(fontWeight = "bold")) %>% 
      hc_add_theme(hc_theme_538()) %>%
      hc_subtitle(text = paste("Summer Olympics 1896 to 2012")) %>%
      hc_tooltip(shared = FALSE, 
                 formatter = JS("function () { return '<b>' + this.series.name + ', Age ' + this.point.category + '</b><br/>' + 'athletes: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}"))
    # Determine theme and apply to highchart ------------------------
    if (input$theme != "FiveThirtyEight") {
      theme <- switch(input$theme,
                      chalk = hc_theme_chalk(),
                      darkunica = hc_theme_darkunica(),
                      fivethirtyeight = hc_theme_538(),
                      gridlight = hc_theme_gridlight(),
                      handdrawn = hc_theme_handdrawn(),
                      economist = hc_theme_economist(),
                      sandsignika = hc_theme_sandsignika()
      )
      hc <- hc %>% 
        hc_add_theme(theme)
      }
      
      hc
    })

  
  output$hcontainer2 <- renderHighchart({
    
    hc2 <- highchart() %>%
      hc_add_series_list(series1) %>%
      hc_chart(type="bar") %>%
      hc_motion(enabled=TRUE, labels=yrs1, series= c(0,1), autoplay=FALSE, updateInterval=4) %>%
      hc_plotOptions(
        series = list(stacking = "normal"),
        bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
      ) %>% 
      hc_tooltip(shared = TRUE) %>%
      hc_yAxis(min = -300, max = 300) %>%
      hc_xAxis(
        xaxis1,
        rlist::list.merge(xaxis1, list(opposite = TRUE, linkedTo = 0)))%>%      
      hc_title(text = "Athletes by Gender and Age",
               style = list(fontWeight = "bold")) %>% 
      hc_add_theme(hc_theme_538()) %>%
      hc_subtitle(text = paste("Winter Olympics 1924 to 2010")) %>%
      hc_tooltip(shared = FALSE, formatter = JS("function () { return '<b>' + this.series.name + ', Age ' + this.point.category + '</b><br/>' + 'athletes: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}"))

        # Determine theme and apply to highchart ------------------------
    if (input$theme != "FiveThirtyEight") {
      theme <- switch(input$theme,
                      chalk = hc_theme_chalk(),
                      darkunica = hc_theme_darkunica(),
                      fivethirtyeight = hc_theme_538(),
                      gridlight = hc_theme_gridlight(),
                      handdrawn = hc_theme_handdrawn(),
                      economist = hc_theme_economist(),
                      sandsignika = hc_theme_sandsignika()
      )
      hc2 <- hc2 %>% 
        hc_add_theme(theme)
    }
    hc2
    })

  output$hcontainer3 <-renderHighchart({
    hc3 <- highchart() %>%
      hc_add_series_map(worldgeojson, team, value = "Medals", joinBy = "iso3", name="Country: Medals") %>%
      hc_colorAxis(stops = dshmstops) %>%
      hc_legend(enabled = TRUE, align = "right", verticalAlign = "bottom") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_title(text = "Number of Medal Winning Athletes by Country") %>%
      hc_subtitle(text = "Summer Olympics from 1896 to 2012") %>%
      hc_add_theme(hc_theme_sandsignika())
    if (input$theme != "FiveThirtyEight") {
      theme <- switch(input$theme,
                      chalk = hc_theme_chalk(),
                      darkunica = hc_theme_darkunica(),
                      fivethirtyeight = hc_theme_538(),
                      gridlight = hc_theme_gridlight(),
                      handdrawn = hc_theme_handdrawn(),
                      economist = hc_theme_economist(),
                      sandsignika = hc_theme_sandsignika()
      )
      hc3 <- hc3 %>% 
        hc_add_theme(theme)
    }
    
    hc3
  })
}

shinyApp(ui, server)
