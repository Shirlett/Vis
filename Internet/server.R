
library(shiny)
library(ggmap) #For interaction and extraction with Google API maps
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(readr)
library(RColorBrewer) #has a set of colors for print and graphics
library(DT)
library(broom) #for tidy function
library(stringr)
library(ggthemes) #additional color themes
library(shinycssloaders) #to add spinner to plot during processing of updates
library(tidyverse)
library(forcats) #used to manipulate factors for ordering, lumping etc
library(plotly)
library(scales)  


 
server <- function(input, output, session) {
  
  #uses the continuous coloring from Color Brewer for leaflet map
  greens = colorNumeric("Greens", domain = NULL)
  
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4)
  
  #imports the data file which contains an identifier for each geocode
  Internet_Use <- read_rds("internet_use.rds")
                  
    
  
  
  #generates the average internet use for all countries                
  world_Avg <- Internet_Use %>%
    group_by(Years) %>%
    summarize(Internet_Users=mean(Internet_Users), Life_Exp=mean(Life_Exp, na.rm=TRUE),
              with_Electricity=mean(with_Electricity, na.rm=TRUE), Unemployment=mean(Unemployment, na.rm=TRUE),
              Percent_urban=mean(Percent_urban, na.rm=TRUE), Schooling=mean(Schooling, na.rm=TRUE),
              National_Income=mean(National_Income, na.rm=TRUE)) %>%
    mutate(Country="0World_Avg") %>%
    dplyr::select(Country, Years, Internet_Users, Life_Exp, with_Electricity, Unemployment, Percent_urban, Schooling, National_Income)
  
  #Create the dot plot
  # internet_usage <- Internet_Use %>%
  #   group_by(Country) %>%
  #   subset(Year == max(Year)) %>%
  #   select(Country, Internet_Users) 
  
    
  Internet_for_usage <- reactive({
    internet_usage <- Internet_Use %>%
      subset(Year == max(Year)) %>%
      dplyr::filter(Internet_Users >= input$IUsage[1], Internet_Users <= input$IUsage[2]) %>%
      select(Country, Internet_Users) 
    print (internet_usage)
    
  })
   
  
  output$internet_use_plot <- renderPlotly({ 
    dotty_plot <- ggplot(Internet_for_usage(), aes(x=reorder(Country, Internet_Users), Internet_Users)) +
    geom_point(col="green", size=3) +   # Draw points
     geom_segment(aes(x=Country, 
                      xend=Country, 
                     y=min(0), 
                     yend=max(Internet_Users)), 
                  linetype="dashed", 
                  size=0.1) +   # Draw dashed lines
    labs(title="Dot Plot showing Internet Usage per 100 Inhabitants per Country, 2014", 
         subtitle="Internet Usage by Country in 2014", 
         caption="source: UN",
         y = "Internet Usage per 100 inhabitants", x= "Country") + 
    coord_flip()
    dotty_plot <- ggplotly(dotty_plot,  height = input$height, autosize=TRUE) 
    dotty_plot <- style(dotty_plot, hoverinfo = "x+y") 
  })
  
  
  output$ui_plot <- renderUI({
    plotlyOutput("internet_use_plot", width = "100%")
  })
  
  
  #creates the data to be used for the leaflet map only
   Internet_map <- Internet_Use %>%
     filter(Years == 2014) %>%
     mutate(Size_user = (round(Internet_Users, 2))/10) %>%
     mutate(Internet_Users = as.character(round(Internet_Users, 2)))  
     
  
  
  
  #Have a plot that initializes with Canada
  output$Internet_Trend <- renderPlot({
    Internet_Use %>%
      filter(id == "106.34677156.130366") %>%
      dplyr::select(Country, Years, Internet_Users, Life_Exp, with_Electricity, Unemployment, Percent_urban, Schooling, National_Income) %>%
      rbind(world_Avg) %>%
      ggplot(aes(x=Years, y=Internet_Users, color=Country)) +
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
      addCircleMarkers(lat=Internet_map$latitude, lng=Internet_map$longitude, radius= Internet_map$Size_user, color = greens(Internet_map$Size_user), popup=paste("Internet Users per 100 = ", Internet_map$Internet_Users), layerId=Internet_map$id) %>%
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
    if ((length(p$Clicks) > 6)) {
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
  
 #Create the line plot that shows the Internet Usage Trend Line
  output$Internet_Trend <- renderPlot({
    Internet_Use %>%
      filter(id %in% p$Clicks) %>%
      dplyr::select(Country, Years, Internet_Users, Life_Exp, with_Electricity, Unemployment, Percent_urban, Schooling, National_Income) %>%
      rbind(world_Avg) %>%
      ggplot(aes(x=Years, y=Internet_Users, color=Country)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette="Set2") +
      labs(y = "Number of Internet Users per 100 inhabitants") +
      ggtitle("Internet Usage Trend against the World Average") 
  })
  
 #create a parallel coordinates plot for all factors
  output$internet_par <- renderPlotly({
    Parallel <- Internet_Use %>%
    filter(id %in% p$Clicks) %>%
    dplyr::select(Country, Years, Internet_Users, Life_Exp, with_Electricity, Unemployment, Percent_urban, Schooling, National_Income) %>%
    rbind(world_Avg) %>%
    filter(Years==2014) %>%
    group_by(Country) %>%
    select(Country, Internet_Users, Percent_urban, with_Electricity, Schooling, Life_Exp, Unemployment, National_Income) %>%
    gather(Factor, measure,Internet_Users:National_Income) %>%
    ggplot(aes(x = Factor, y = measure, group = Country)) +
      geom_line(aes(color = Country),
                alpha = 1,
                lineend = 'round', linejoin = 'round', size=3) +
      scale_y_continuous( expand = c(0.05, 0)) +
      scale_size(breaks = 10, range = c(0, 100)) +
      scale_color_brewer(palette="Set2") +
      labs(title = "Internet Usage and Potential Factors, 2014") +
      geom_vline(xintercept = 1:7, linetype="dotted", 
                 color = "black", size=0.2, show.legend = FALSE) +
      theme_stata()
      Parallel <- ggplotly(Parallel) 
  })
  
  

  #creates the linear regression model table
  output$lm_model <- renderTable(
    Internet_lm <- Internet_Use %>%
      dplyr::select(Internet_Users, Life_Exp, National_Income, Percent_urban, Schooling, Unemployment, with_Electricity) %>%
      do(tidy(lm(Internet_Users ~ ., data=.))) %>%
      mutate(p.value=format(p.value, digits=4)) %>%
      rename(t.statistic=statistic)
  )
  

  #Get values for number of Internet Users and other factors
  Internet_for_factor <- reactive({
   data_factor <- Internet_Use %>%
    dplyr::select(Years, Internet_Users, input$factor) %>%
     mutate(Selected_factor=get(input$factor)) %>%
     drop_na(Selected_factor)
  })


  #Linear Regression Plot for the entire world of Internet Usage against Selected Factors
  output$Factor_Plot <- renderPlot({
    ggplot(Internet_for_factor(), aes(Selected_factor, Internet_Users)) +
    geom_point(shape=1) +
    geom_smooth(method=lm) +
    labs(
      subtitle="World Internet Usage vs Selected Factor,\n2008-14",
      y = "Number of Internet Users per 100 inhabitants per Year", x= paste("Selected Factor:", input$factor)) +
    theme(plot.subtitle = element_text(size = 15))
    })



  # Look at trend of selected factors for selected countries
  output$Internet_Factor <- renderPlot({
    Internet_Use %>%
      filter(id %in% p$Clicks) %>%
      dplyr::select(Country, Years, Internet_Users, Life_Exp, with_Electricity, Unemployment, Percent_urban, Schooling, National_Income) %>%
      rbind(world_Avg) %>%
      dplyr::select(Country, Years, input$factor) %>%
      ggplot(aes_string(y = input$factor,  width = 0.3)) +
      geom_line(aes(x = Years, color=Country)) +
      geom_point(aes(x = Years, color=Country)) +
      ggtitle(paste("Trends in", input$factor, "by Country")) +
      labs(y = input$factor, x="Years") +
      scale_color_brewer(palette="Set2") 
      
  })
  
  

  
  #Create datatable with selected countries
  output$myTable <- DT::renderDataTable({
      return(
        filter(Internet_Use,id %in% p$Clicks) %>%
          mutate(Internet_Users_per_100=round(Internet_Users,2)) %>%
          mutate(Population=prettyNum(Tot_pop, big.mark=",")) %>%
          mutate(Median_Life_Exp=round(Life_Exp,2)) %>%
          mutate(Per_Access_Electricity=round(with_Electricity,2)) %>%
          mutate(Per_Adult_Unemployment=round(Unemployment,2)) %>%
          mutate(Percent_urban=round(Percent_urban,2)) %>%
          mutate(School_Compl_Rate=round(Schooling,2)) %>%
          mutate(National_Income_per_cap_in_thousands=round(National_Income,2)) %>%
          dplyr::select(Country, Years, Internet_Users_per_100, Population, Median_Life_Exp, Per_Access_Electricity, Per_Adult_Unemployment, Percent_urban, School_Compl_Rate, National_Income_per_cap_in_thousands)
      )
   })


  # Downloadable csv of resulting dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mySave", ".csv", sep = "")
    },
    content = function(file) {
      filter(Internet_Use,id %in% p$Clicks) %>%
        mutate(Internet_Users_per_100=round(Internet_Users,2)) %>%
        mutate(Population=prettyNum(Tot_pop, big.mark=",")) %>%
        mutate(Median_Life_Exp=round(Life_Exp,2)) %>%
        mutate(Per_Access_Electricity=round(with_Electricity,2)) %>%
        mutate(Per_Adult_Unemployment=round(Unemployment,2)) %>%
        mutate(Percent_urban=round(Percent_urban,2)) %>%
        mutate(School_Compl_Rate=round(Schooling,2)) %>%
        mutate(National_Income_per_cap_in_thousands=round(National_Income,2)) %>%
        dplyr::select(Country, Years, Internet_Users_per_100, Population, Median_Life_Exp, Per_Access_Electricity, Per_Adult_Unemployment, Percent_urban, School_Compl_Rate, National_Income_per_cap_in_thousands) %>%
        write.csv(file, row.names = FALSE)
    })

  
  
}


