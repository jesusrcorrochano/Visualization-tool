#Load libraries
library(shiny)
library(leaflet.minicharts)
library(leaflet)
library(ggplot2)
library(dplyr)

#Load data
source_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(source_dir)
data <- read.csv('terrorism_data.csv')

#Prepare data for the visualizations
data$nkill[is.na(data$nkill)] <- 0

data$claimed[is.na(data$claimed)] <- 0

df <- data

b <-df %>% 
  group_by(country_txt,attacktype1_txt,iyear) %>% 
  summarise(n = n(),nkills = sum(nkill))

daten <- data.frame(na.omit(df))
b<- data.frame(na.omit(b))

#### UI ####
ui<- fluidPage(
  #Title 
  titlePanel("Terrorism Dataset Analytics"),
  
  #First the map, two display options cummulative, exact location
  #Also we can select a range of years (date)
  radioButtons("display_mode", 
               h4("Select Display mode"), 
               choices = list("Cummulative" = 1, 
                              "Location" =0)),
  sliderInput(inputId = "date", "Date:", min = 1970, max = 2017, value = c(1970, 2017)),
  leafletOutput(outputId = "distPlot", width = "700px", 
                height = "400px"),
  
  #Second some plots to analyze the data per country. First select a country,
  #then a range of years, and also the data we are interesting in number of kills,
  #or number of attacks. Finally you can select one binary feature to plot a pie chart
  sidebarLayout(
    
    sidebarPanel(
      selectInput("region", h4("Country"), choices 
                  =unique(daten$country_txt),selected = "Afghanistan"),
      sliderInput("year", 
                  label = "Years:",
                  min = 1970, max = 2017, value = c(1970, 2017)),
      radioButtons("display_mode_plot", 
                   h4("Select mode"), 
                   choices = list("Kills" = 1, 
                                  "Attacks" =0),
                   selected = 1),
      radioButtons("display_mode_pie", 
                   h4("Pie chart"),
                   choices = list("Claimed" = 2,
                                  "Suicide" = 1,
                                  "Success" =0),
                   selected = 1),
      width = 3  
    ),
    
    #Main Panel: Three different plot using the selections made before
    mainPanel (fluidRow(
      splitLayout(cellWidths = c("45%", "55%"),plotOutput(outputId = "plot", width = "300px", 
                                                          height = "300px"),
                  plotOutput(outputId = "plot1", width = "400px",height = "300px"))
    ),
    plotOutput(outputId = "plot2", width = "400px",height = "300px")
  )
  ) 
)

#### SERVER ####
server <- function(input, output){
  
  #Map
  data_reg2 <-reactive({ filter(daten, between (iyear, input$date[1], input$date[2]))})
  output$distPlot <- renderLeaflet({
    data_region <- data_reg2()
    basemap= leaflet()  %>%
      setView(lng = 0, lat = 30, zoom = 1.5)%>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE))
    if(input$display_mode == 1) {
      data_region_cum <- data_region %>% 
        group_by(country_txt) %>% 
        summarise(cumulative_n = n(),
                  latitude = mean(latitude),
                  longitude = mean(longitude))
      basemap %>%
        addCircles(data = data_region_cum, lat = ~ latitude, lng = ~ longitude,
                   weight = 1, radius = ~cumulative_n*40, 
                   popup = ~as.character(country_txt), 
                   label = ~as.character(paste0("attacks : ", sep = " ", cumulative_n )),
                   color = 'red', fillOpacity = 0.8)
    }else{
      basemap %>%
        addCircles(data = data_region, lat = ~ latitude, lng = ~ longitude,
                   weight = 1, radius = 20, popup = ~as.character(country_txt),
                   color = 'red', fillOpacity = 0.8)
      
      
    }
  })
  
  #Plots
  data_reg <-reactive({ filter(daten, daten$country_txt == input$region &
                                 between (iyear, input$year[1], input$year[2]))})
  #Plot 1: Bar chart: Year/Number kills( or Number attacks)
  output$plot <- renderPlot({
    data_region <- data_reg()
    if (input$display_mode_plot == 1){
      data_region <- data_region %>% 
        group_by(iyear) %>% 
        summarise(nkills = sum(nkill))
      aes = aes(x=iyear, y=nkills)
      title.y = 'Count of kills'
      title = 'Kills by Year'
    }
    else {
      data_region <- data_region %>% 
        group_by(iyear) %>% 
        summarise(n = n())
      aes = aes(x=iyear, y=n)
      title.y = 'Count of attacks'
      title = 'Attacks by Year'
    }
    ggplot(data = data_region,aes) + 
      geom_bar(position = "dodge", stat = "identity", fill="red") + ylab(title.y) + 
      xlab("Year") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle(title)
  })
  
  data_reg1 <-reactive({ filter(b, b$country_txt == input$region &
                                 between (iyear, input$year[1], input$year[2]))})
  #Plot 2: Bar chart: Type of attack / Number kills(or Number attacks)
  output$plot1 <- renderPlot({
    data_region <- data_reg1()
    data_region <- data_region %>% 
      group_by(attacktype1_txt) %>% 
      summarise(nkills = sum(nkills),
                n = sum(n))
    if (input$display_mode_plot == 1){
      aes = aes(x=nkills, y=attacktype1_txt)
      title.x = 'Num of kills'
      title = 'Kills by type of attack'
    }
    else {
      aes = aes(x=n, y=attacktype1_txt)
      title.x = 'Num of attacks'
      title = 'Attacks by type'
    }
    ggplot(data = data_region, 
           aes) + 
      geom_bar(position = "dodge", stat = "identity", fill="red") + ylab("Type of Attack") + 
      xlab(title.x) + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold"),) + 
      ggtitle(title)
  })

  #Plot 3: Pie chart: Compare if the attacks are: Suicide or no suicide, 
  #success or no success and claimed or no claimed 
  output$plot2 <- renderPlot({
    data_region <- data_reg()

    if (input$display_mode_pie == 1){
      data_region <- data_region %>% 
        group_by(suicide) %>% 
        summarise(n = n())
      pie(data_region$n,labels = c('no suicide','suicide'),border='white',col = c('red','black'))
    } else if (input$display_mode_pie == 0)  {
      data_region <- data_region %>% 
        group_by(success) %>% 
        summarise(n = n())
      pie(data_region$n,labels = c('no success','success'),border='white',col = c('red','black'))
    } else {
      data_region <- data_region %>% 
        group_by(claimed) %>% 
        summarise(n = n())
      pie(data_region$n,labels = c('no claimed','claimed'),border='white',col = c('red','black'))
    }
  })
  
  
} 

#Execute the App
shinyApp(ui = ui, server = server)
