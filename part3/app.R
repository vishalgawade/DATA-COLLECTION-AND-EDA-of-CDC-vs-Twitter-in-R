library("shiny")
# install.packages("twitteR")
# Installing
# install.packages("readr")
# install.packages("sp")
# install.packages("maps")
# install.packages("maptools")
# Loading
library("readr")
library(stringr)
library(usmap)
# install.packages("plyr")
library(plyr)
library(ggplot2)
library(twitteR)

library("ggmap")
library("maptools")
library(maps)
library(sp)
library(maptools)
library(ggmap)
register_google(key = "AIzaSyBcudY08pJqUuQUuESkxY0nCL3MiQ0tQzI") 
#get state names from state abrrevation
getStateName <- function(abbrv){
  ab    <- tolower(c("AL",
                     "AK", "AZ", "KS", "UT", "CO", "CT",
                     "DE", "FL", "GA", "HI", "ID", "IL",
                     "IN", "IA", "AR", "KY", "LA", "ME",
                     "MD", "MA", "MI", "MN", "MS", "MO",
                     "MT", "NE", "NV", "NH", "NJ", "NM",
                     "NY", "NC", "ND", "OH", "OK", "OR",
                     "PA", "RI", "SC", "SD", "TN", "TX",
                     "CA", "VT", "VA", "WA", "WV", "WI",
                     "WY", "DC"))
  st    <- c("Alabama",
             "Alaska", "Arizona", "Kansas",
             "Utah", "Colorado", "Connecticut",
             "Delaware", "Florida", "Georgia",
             "Hawaii", "Idaho", "Illinois",
             "Indiana", "Iowa", "Arkansas",
             "Kentucky", "Louisiana", "Maine",
             "Maryland", "Massachusetts", "Michigan",
             "Minnesota", "Mississippi", "Missouri",
             "Montana", "Nebraska", "Nevada",
             "New Hampshire", "New Jersey", "New Mexico",
             "New York", "North Carolina", "North Dakota",
             "Ohio", "Oklahoma", "Oregon",
             "Pennsylvania", "Rhode Island", "South Carolina",
             "South Dakota", "Tennessee", "Texas",
             "California", "Vermont", "Virginia",
             "Washington", "West Virginia", "Wisconsin",
             "Wyoming", "District of Columbia")
  st[match(tolower(abbrv), ab)]
}

#get last n characters from string
substrFromEnd <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
data <- read.csv("statesList.csv", sep = ",")
# head(gsub('[[:digit:]]+', '', data$address))
data$address=str_remove_all(data$address," ");
data$address=str_remove_all(gsub('[[:digit:]]+', '', data$address),",usa");
data$address=substrFromEnd(data$address,2);
testPoints <- data.frame(x = tolower(getStateName(data$address)), y = data$lat)
print(testPoints$x[!is.na(testPoints$x)][3:40]);
# data <- read.csv("statesList.csv", sep = ",")
# testPoints <- data.frame(x = data$lon, y = data$lat)
# raw<-latlong2state(testPoints)
# states <- raw[!is.na(raw)]
states <- testPoints$x[!is.na(testPoints$x)]
# head(states) 
tweet_count <- as.data.frame(table(states))
us <- map_data("state")                           
tweet_count=data.frame(tweet_count)
tweet_count$state<-tweet_count$states
#ggtitle("2018-19 Influenza Season Week 7 ending Feb 16, 2019")+
twitter<-plot_usmap(data = tweet_count, values = "Freq", lines = "black") + 
  scale_fill_continuous(
    low = "green", high = "red", name = "ILI Activity Level", label = scales::comma
  ) + 
  # labs(title = "2018-19 Influenza Season Week 4 ending Jan 26, 2019") + 
  theme(legend.position = "right")
# us <- map_data("state")
# StateDatabyWeekforMap_2018-19week40-9
statedata <- read.csv("StateDataforMap_2018-19week4.csv")
statedata$activityval <- as.numeric(gsub("Level ","",statedata$ACTIVITY.LEVEL))
statedata$state <- tolower(statedata$STATENAME)
#ggtitle("2018-19 Influenza Season Week 7 ending Feb 16, 2019")+
cdc<-plot_usmap(data = statedata, values = "activityval", lines = "black") + 
  scale_fill_continuous(
    low = "green", high = "red", name = "ILI Activity Level", label = scales::comma
  ) + 
  # labs(title = "2018-19 Influenza Season Week 4 ending Jan 26, 2019") + 
  theme(legend.position = "right")
dataSneeze <- read.csv("statesList_Sneezing_keyword.csv", sep = ",")
dataSneeze$address=str_remove_all(dataSneeze$address," ");
dataSneeze$address=str_remove_all(gsub('[[:digit:]]+', '', dataSneeze$address),",usa");
dataSneeze$address=substrFromEnd(dataSneeze$address,2);
testPoints <- data.frame(x = tolower(getStateName(dataSneeze$address)), y = dataSneeze$lat)
# data <- read.csv("statesList.csv", sep = ",")
# testPoints <- data.frame(x = data$lon, y = data$lat)
# raw<-latlong2state(testPoints)
# states <- raw[!is.na(raw)]
statesSneeze <- testPoints$x[!is.na(testPoints$x)]
tweet_countSneeze <- as.data.frame(table(statesSneeze))
tweet_count=data.frame(tweet_countSneeze)
tweet_countSneeze$state<-tweet_countSneeze$states
#ggtitle("2018-19 Influenza Season Week 7 ending Feb 16, 2019")+
twitterSneeze<-plot_usmap(data = tweet_countSneeze, values = "Freq", lines = "black") + 
  scale_fill_continuous(
    low = "green", high = "red", name = "ILI Activity Level", label = scales::comma
  ) + 
  # labs(title = "2018-19 Influenza Season Week 4 ending Jan 26, 2019") + 
  theme(legend.position = "right")
# us <- map_data("state")
dataFlu <- read.csv("statesList_flu_keyword.csv", sep = ",")
dataFlu$address=str_remove_all(dataFlu$address," ");
dataFlu$address=str_remove_all(gsub('[[:digit:]]+', '', dataFlu$address),",usa");
dataFlu$address=substrFromEnd(dataFlu$address,2);
testPoints <- data.frame(x = tolower(getStateName(dataFlu$address)), y = dataFlu$lat)
# data <- read.csv("statesList.csv", sep = ",")
# testPoints <- data.frame(x = data$lon, y = data$lat)
# raw<-latlong2state(testPoints)
# states <- raw[!is.na(raw)]
statesFlu <- testPoints$x[!is.na(testPoints$x)]
tweet_countFlu <- as.data.frame(table(statesFlu))
tweet_count=data.frame(tweet_countFlu)
tweet_countFlu$state<-tweet_countFlu$states
twitterFlu<-plot_usmap(data = tweet_countFlu, values = "Freq", lines = "black") + 
  scale_fill_continuous(
    low = "green", high = "red", name = "ILI Activity Level", label = scales::comma
  ) + 
  # labs(title = "2018-19 Influenza Season Week 4 ending Jan 26, 2019") + 
  theme(legend.position = "right")
#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

ui<-fluidPage(
  titlePanel("DATA COLLECTION AND EXPLORATORY DATA ANALYSIS"),
  sidebarLayout(
    sidebarPanel(h3("Choose a Heat Map"), position="left",
                 helpText("Observe different heat maps of CDC and Twitter for influenza like illness."),
                 selectInput("dataset", label = h5("Select Map"), 
                             choices = list("CDC"=1, "Twitter"=2, "CDC vs Twitter"=3, "CDC vs Twitter(Flu)"=4, "CDC vs Twitter(Sneeze)"=5), selected = 1)),
    mainPanel(plotOutput("Plot"), 
              position="right")
  )
)
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  output$Plot <- renderPlot({
    if(input$dataset == 1) { cdc}
    else if(input$dataset == 2){twitter}
    else if(input$dataset == 3){multiplot(cdc, twitter, cols=2)}
    else if(input$dataset == 4){multiplot(cdc, twitterFlu, cols=2)}
    else{multiplot(cdc, twitterSneeze, cols=2)}
  })
}
shinyApp(ui, server)