
library(shiny)
library(ggmap) #For interaction and extraction with Google API maps
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library (readr)
library(RColorBrewer) #has a set of colors for print and graphics
library(DT)
library(broom) #for tidy function
library(stringr)
library(ggthemes) #additional color themes
library(shinycssloaders) #to add spinner to plot during processing of updates

server <- function(input, output, session) {
  #uses the continuous coloring from Color Brewer for leaflet map
  greens = colorNumeric("Greens", domain = NULL)
  
  #imports the data file and creates an identifier for each geocode
  Internet_Use <- read_rds("internet_use.rds") 
  
  
  #generates the average internet use for all countries                
  world_Avg <- Internet_Use %>%
    group_by(Years) %>%
    summarize(Internet_Users_per_100=mean(Internet_Users_per_100), GNI_per_cap=mean(GNI_per_cap, na.rm=TRUE),Percent_urban=mean(Percent_urban, na.rm=TRUE)) %>%
    mutate(Country="World") %>%
    dplyr::select(Country, Years, Internet_Users_per_100, GNI_per_cap, Percent_urban)
  
  #Create the dot plot
  internet_usage <- Internet_Use %>%
    group_by(Country) %>%
    subset(Year == max(Year)) %>%
    select(Country, Internet_Users_per_100)
  
  output$internet_use_plot <- renderPlot({ 
    internet_usage %>%
    ggplot(aes(x=reorder(Country, Internet_Users_per_100), Internet_Users_per_100)) + 
    geom_point(col="green", size=3) +   # Draw points
    geom_segment(aes(x=Country, 
                     xend=Country, 
                     y=min(0), 
                     yend=max(Internet_Users_per_100)), 
                 linetype="dashed", 
                 size=0.1) +   # Draw dashed lines
    labs(title="Dot Plot", 
         subtitle="Internet Usage by Country in 2015", 
         caption="source: UN",
         y = "Internet Usage per 100 inhabitants", x= "Country") +  
    coord_flip() 
   })
  
  #creates the data to be used for the leaflet map only
  Internet_map <- Internet_Use %>%
    filter(Year == 2015) %>%
    mutate(Internet_User_per_100 = as.character(round(Internet_Users_per_100, 2)))  %>%
    mutate(Size_user = (round(Internet_Users_per_100, 2))/10)
  
  
  
  #Instructions to user
  output$mytext <- renderText({
    paste("Click up to 10 Circle Markers on the map below to see more details for the selected countries")
  })
  
  #Have a plot that initializes with Canada
  output$Internet_Trend <- renderPlot({ 
    Internet_Use %>%
      filter(id == "106.34677156.130366") %>% 
      dplyr::select(Country, Years, Internet_Users_per_100) %>%
      rbind(world_Avg) %>%
      ggplot(aes(x=Years, y=Internet_Users_per_100, color=Country)) +
      geom_line() +
      geom_point() +
      labs(y = "Number of Internet Users per 100 inhabitants") +
      ggtitle("Internet Usage Trend against the World Average") 
  })
  
  #set a variable to capture the id associated with each marker from leaflet's layerid function
  
  
  p <- reactiveValues(Clicks=list())
  
 
  
  # produce the leaflet map with circle markers
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-3.435973, 55.378051, zoom = 2) %>%
      addCircleMarkers(lat=Internet_map$latitude, lng=Internet_map$longitude, radius= Internet_map$Size_user, color = greens(Internet_map$Size_user), popup=paste("Internet Users per 100 = ", Internet_map$Internet_User_per_100), layerId=Internet_map$id) %>%
      addLegend("bottomright", pal = greens, values = Internet_map$Size_user,
                title = "Users per 100",
                labFormat = labelFormat(suffix = "0"),
                opacity = 0.6)
  )
  
 
  #observe the marker click info and transfer it to the tables and scatterplot when it is changed.
  observeEvent(
    input$mymap_marker_click,
    {
    click <- input$mymap_marker_click
    p$Clicks <- c(p$Clicks, click$id)
    if ((length(p$Clicks) > 4)) {
       p$Clicks <- NULL
        output$toomany <- renderText({
          "Sorry, you have selected too many countries, please start again!"
        })
      } else {
        output$toomany <- renderText({ "" })
      }
    }) 
  
  observeEvent(
    input$button,
    {p$Clicks <- NULL})
  
 
  output$Internet_Trend <- renderPlot({ 
    Internet_Use %>% 
      filter(id %in% p$Clicks) %>% 
      dplyr::select(Country, Years, Internet_Users_per_100, GNI_per_cap, Percent_urban) %>%
      rbind(world_Avg) %>%
      ggplot(aes(x=Years, y=Internet_Users_per_100, color=Country)) +
      geom_line() +
      geom_point() +
      labs(y = "Number of Internet Users per 100 inhabitants") +
      ggtitle("Internet Usage Trend against the World Average") 
  })
    
  
  output$Internet_Factor <- renderPlot({
    Internet_Use %>%
      filter(id %in% p$Clicks) %>%
      dplyr::select(Country, Years, Internet_Users_per_100, GNI_per_cap, Percent_urban) %>%
      rbind(world_Avg) %>%
      mutate(Internet_Users_per_100 = as.character(round(Internet_Users_per_100, 2))) %>%
      filter(Years == 2015) %>%
      ggplot(aes_string( y = input$factor,  width = 0.3)) +
      geom_bar(aes(x = Country), fill= "green", stat = "identity") +
      labs(x = NULL) +
      ggtitle(paste(input$factor, ",2015")) +
      labs(y = input$factor, x= NULL) +
      theme(legend.position="none") +
      coord_flip() +
      geom_text(aes(x = Country, label=paste0("Usage: ", Internet_Users_per_100)),vjust="inward",angle=0,hjust="inward")

  })

  #Get median values for number of Internet Users and Gross National Income
  Internet_for_factor <- reactive({
   data_factor <- Internet_Use %>%
    dplyr::select(Year, Internet_Users_per_100, input$factor) %>%
    group_by(Year) %>%
    dplyr::summarise(Med_Internet_Users=median(Internet_Users_per_100), Med_factor=median(get(input$factor), na.rm = TRUE))
  })
  
   #Get estimate of slopes, intercepts for linear model
   facfit <- reactive({
    slopes <- tidy(lm(Med_Internet_Users ~ Med_factor, data=Internet_for_factor()))
    print(slopes)
             })
    
    print(facfit)
  
  #Capture p-value to pass to graph
   pfac <- reactive({
   pval <- toString(round(facfit[2,5], digits=5))
          })
   print(pfac)
   
  #Linear Regression
  output$Factor_Plot <- renderPlot({
    ggplot(Internet_for_factor(), aes(Med_factor, Med_Internet_Users)) +
    geom_point(shape=1) +
    geom_smooth(method=lm) +
    labs(
      subtitle="World Internet Usage as influenced by Selected Factor",
      y = "Median Number of Internet Users per 100 inhabitants per Year", x= paste("Median ", input$factor)) +
    theme(plot.subtitle = element_text(size = 15)) 
    #annotate("text", x=10, y=10, label = paste("p-value=", pval), parse=F)
    })
  
 
  
  #Create datatable with selected countries
  output$myTable <- DT::renderDataTable({
      return(
        filter(Internet_Use,id %in% p$Clicks) %>%
          mutate(Internet_Users_per_100=round(Internet_Users_per_100,2)) %>%
          mutate(Population=prettyNum(Tot_pop, big.mark=",")) %>%
          mutate(Percent_in_urban=round(Percent_urban,2)) %>%
          mutate(National_Income_Per_Cap=prettyNum(GNI_per_cap, big.mark=",")) %>%
          mutate(Percent_with_Electricity=round(Per_Access_Electricity,2)) %>%
          mutate(Median_Life_Exp=round(Median_Life_Exp,2)) %>%
          dplyr::select(Country, Years, Internet_Users_per_100, Population, Percent_in_urban, National_Income_Per_Cap, Percent_with_Electricity, Median_Life_Exp)
      )
   })
  
  
  # Downloadable csv of resulting dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mySave", ".csv", sep = "")
    },
    content = function(file) {
      filter(Internet_Use,id %in% p$Clicks) %>%
        mutate(Internet_Users_per_100=round(Internet_Users_per_100,2)) %>%
        mutate(Population=prettyNum(Tot_pop, big.mark=",")) %>%
        mutate(Percent_in_urban=round(Percent_urban,2)) %>%
        mutate(National_Income_Per_Cap=prettyNum(GNI_per_cap, big.mark=",")) %>%
        mutate(Percent_with_Electricity=round(Per_Access_Electricity,2)) %>%
        mutate(Median_Life_Exp=round(Median_Life_Exp,2)) %>%
        dplyr::select(Country, Years, Internet_Users_per_100, Population, Percent_in_urban, National_Income_Per_Cap, Percent_with_Electricity, Median_Life_Exp) %>%
        write.csv(file, row.names = FALSE)
    })
  
  
  
}


