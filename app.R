if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
library(shinythemes)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(geojsonio)
library(dplyr)
library(maps)
library(ggplot2)
library(googleCharts)
library(rsconnect)


# import and processing data
#setwd("/Users/myfiles/Desktop/Group12 Final Project")
worldcountry = geojson_read("50m.geojson", what = "sp")
dta = read.csv("dta_processed_update2.csv")
dta$Region <- as.factor(dta$Region)

#define functions 
fun1<-function(indicator1,time){
  dta_selected<- dta %>%filter(year == time)
  indicator <- aggregate(dta_selected%>%select(indicator1),
                         by=list(dta_selected$Region), 
                         FUN=mean) 
  head(dta_selected$indicators)
  colnames(indicator) <- c("Region", "mean_indicator")
  indicator <- indicator[order(indicator$mean_indicator,decreasing = F), ]
  indicator$Region <- factor(indicator$Region, levels = indicator$Region)  # to retain the order in plot.
  ranking_plot<-ggplot(indicator, aes(x=Region, y=mean_indicator)) + 
    geom_bar(stat="identity", width=.5, fill="tomato3") + 
    labs(title="Ordered Bar Chart",
         y = indicator1) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  ranking_plot+ coord_flip()}


# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Global Health Data Visualization</a>'), id="nav",
             windowTitle = "Global Health Data Visualization",
    tabPanel("Global mapper", #tab object name is Global mapper
           div(class="outer",
               tags$head(includeCSS("styles.css")),
               leafletOutput("mymap", width="100%", height="100%"),
               
               #panel for plot
               absolutePanel(id = "controls", class = "panel panel-default", 
                             top = 75, left = 55, width = 400, fixed=TRUE,
                             draggable = TRUE, height = 600,
                             
                             span(tags$i(h5("Data source: worldbank.org")), style="color:#045a8d"),
                             h3("The ranking of different regions", align = "right"),
                             h4(textOutput("clean_date_reactive"), align = "right"),
                             plotOutput("bar_plot", height="200px", width="100%"),  #output barplot
                             selectInput(inputId = "indicators",  
                                         label = h5("Please select one health indicator"),
                                         choices = c( "Health.expenditure.per.capita.US." ,       
                                                      "Hospital.beds.per.1000.people."  ,         
                                                      "Incidence.of.Malaria.per.1000.population.",
                                                      "Number.of.nurses.per.1000.people." ,   
                                                      "Number.of.physicians.per.1000.people."  ,  
                                                       "Population.growth.annual.percent." ,       
                                                       "Prevalence.of.HIV.percent."   
                                                     ), 
                                         selected = "Hospital.beds.per.1000.people."),
                             sliderTextInput("plot_date",
                                             label = h5("Select mapping year"),
                                             choice = 2000:2017,
                                             selected = 2015,
                                             grid = FALSE,
                                             animate=animationOptions(interval = 1000, loop = FALSE))

                              ))),
    
    tabPanel("Interactive Bubble Chart",
             
             # This line loads the Google Charts JS library
             googleChartsInit(),
             
             # Use the Google webfont "Source Sans Pro"
             tags$link(
               href=paste0("http://fonts.googleapis.com/css?",
                           "family=Source+Sans+Pro:300,600,300italic"),
               rel="stylesheet", type="text/css"),
             tags$style(type="text/css",
                        "body {font-family: 'Source Sans Pro'}"
             ),
             googleBubbleChart("chart",
                               width="100%", height = "475px",
                               options = list(
                                 fontName = "Source Sans Pro",
                                 fontSize = 13,
                                 # Set axis labels and ranges
                                 hAxis = list(
                                   title = "Health expenditure or Selected medical resouces situation",
                                   viewWindow = NULL
                                 ),
                                 vAxis = list(
                                   title = "Incidence of the selected disease",
                                   viewWindow = NULL
                                 ),
                                 # The default padding is a little too spaced out
                                 chartArea = list(
                                   top = 50, left = 75,
                                   height = "75%", width = "75%"
                                 ),
                                 # Allow pan/zoom
                                 explorer = list(),
                                 # Set bubble visual props
                                 bubble = list(
                                   opacity = 0.4, stroke = "none",
                                   # Hide bubble label
                                   textStyle = list(
                                     color = "none"
                                   )
                                 ),
                                 # Set fonts
                                 titleTextStyle = list(
                                   fontSize = 16
                                 ),
                                 tooltip = list(
                                   textStyle = list(
                                     fontSize = 12
                                   )
                                 )
                               )
             ),
             # y-axis
             selectInput("diseases", "Diseases:",   
                         choices = c("Incidence.of.Malaria.per.1000.population.", "Prevalence.of.HIV.percent."),
                         selected = c("Incidence.of.Malaria.per.1000.population."),
                         multiple = FALSE),
             selectInput("MedRes", "Medical Resources:",
                         choices = c("Health.expenditure.per.capita.US.", "Hospital.beds.per.1000.people.", 
                                     "Number.of.nurses.per.1000.people.", "Number.of.physicians.per.1000.people."),
                         selected = c("Health.expenditure.per.capita.US."),
                         multiple = FALSE),
             #time
             fluidRow(
               shiny::column(4, offset = 4,
                             sliderInput("year", "Year",
                                         min = min(dta$year), max = max(dta$year), step = 1,
                                         value = min(dta$year), animate = TRUE)))

             
             
             
             )
    
    ))
  

# Define server logic required to draw a histogram


server <- function(input, output) {
  
###############tab1#################   
  
  #load the base map
  output$mymap <- renderLeaflet({ 
    #create basemap
    domain1<-input$indicators
    maximum<-max(dta%>%select(domain1))
    bins = c(0,maximum/1000,maximum*(50/1000),maximum*(100/1000),maximum*(500/1000),maximum,Inf)  #draw bins 
    #bins = c(0,10,50,100,500,1000,Inf)
    cv_pal <- colorBin("Oranges", domain = dta%>%select(domain1), bins = bins)
    plot_map <- worldcountry
    basemap = leaflet(plot_map) %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      addLegend("bottomright", pal = cv_pal, values = ~dta%>%select(domain1),
                title = domain1) 
    basemap
  })
  
  #display information over the map
  #create reactive_db and reactive_polygons
  formatted_date = reactive({
    input$plot_date
  })

  reactive_polygons = reactive({
    worldcountry
  })
  reactive_db = reactive({  # reactive_db is a dataset
    dta %>% 
      filter(year == formatted_date()) 
      })
  
  observeEvent(c(input$plot_date,input$indicators),
               { domain3<-input$indicators
                 maximum<-max(dta%>%select(domain3))
                 bins = c(0,maximum/1000,maximum*(50/1000),maximum*(100/1000),maximum*(500/1000),maximum,Inf)  #draw bins 
                 #bins = c(0,10,50,100,500,1000,Inf)
                 cv_pal <- colorBin("Oranges", domain = dta%>%select(domain3), bins = bins)
                 if(domain3 == "Health.expenditure.per.capita.US."){
                   z<-reactive_db()$Health.expenditure.per.capita.US.
                 }else if(domain3 == "Hospital.beds.per.1000.people." ){
                   z<-reactive_db()$Hospital.beds.per.1000.people.
                 }else if(domain3 ==   "Incidence.of.Malaria.per.1000.population."){
                   z<-reactive_db()$Incidence.of.Malaria.per.1000.population.
                 }else if(domain3 == "Number.of.nurses.per.1000.people."){
                   z<-reactive_db()$Number.of.nurses.per.1000.people.
                 }else if(domain3 == "Number.of.physicians.per.1000.people."){
                   z<-reactive_db()$Number.of.physicians.per.1000.people.
                 }else if(domain3 == "Population.growth.annual.percent."){
                   z<-reactive_db()$Population.growth.annual.percent.
                 }else{
                   z<-reactive_db()$Prevalence.of.HIV.percent.}
                 leafletProxy("mymap") %>% 
                   clearMarkers() %>%
                   clearShapes() %>%
                   addPolygons(data = reactive_polygons(), 
                               stroke = FALSE, 
                               smoothFactor = 0.1, 
                               fillOpacity = 0.15,
                               fillColor = ~ cv_pal(z))#, fillColor = ~cv_pal(reactive_db()$expenditure
               })

  output$bar_plot <- renderPlot({ 
    fun1(input$indicators, input$plot_date)
  })  
  
################tab2##################  
  #defaultColors
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  
  #series 
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(dta$Region)
  )
  
  #yearData
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- dta %>%
      filter(year == input$year) %>%
      select(Country, input$MedRes, input$diseases, Region, Population.growth.annual.percent.) %>%
      arrange(Region)
  })
  
  #output
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Health expenditure or medical resources vs. diseases",
          input$year),
        series = series
      )
    )
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::deployApp()

