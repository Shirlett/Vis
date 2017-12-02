
library(shiny)
library(htmlwidgets)
library(shinycssloaders)
library(markdown)
library(leaflet)
library(shinythemes)



ui <- fluidPage(theme = shinytheme("simplex"),
  titlePanel(title=div(img(src="internet_world.jpg", height = 50, width = 100), "Internet Usage Across the World, 2015")),
  tabsetPanel(
    tabPanel("How to Use",
             includeMarkdown("Howto.Rmd")
             ), #closes first tabPanel
      tabPanel("PlotView",
               plotOutput("internet_use_plot", height = 2000)
               ), #closes second tabPanel
      tabPanel("Interactive MapView", 
  sidebarLayout(position="right", 
                
                sidebarPanel(withSpinner(plotOutput("Internet_Trend"))
                ,
                textOutput("toomany"),
                br(),
                actionButton("button", "Clear Current Selections")
                            ), #closes sidebarPanel
                
                mainPanel(   
                  textOutput("mytext"),
                  leafletOutput("mymap"),
                  p(em("Source:"),
                    a(href="http://data.un.org/Explorer.aspx?d=SDGs&f=series%3aSL_TLF_UEM", "UN dataset", target="_blank")),
                  br(),
                  br()
                        ) #closes mainPanel
                ), #closes sidebarLayout
                fluidRow(
                    column(4,
                      wellPanel(
                          selectInput("factor", "Potential Factors:", list("National Income per cap" = "GNI_per_cap", "Urban Density" = "Percent_urban"))
                               ) #closes wellPanel
                            ) #closes first column
                        ), #close fluidrow
                fluidRow(
                   column(4,
                      plotOutput("Factor_Plot")
                         ), #closes first column
                   column(4,
                          tableOutput("facfit")
                   ), #closes second column
                   column(4,
                      plotOutput("Internet_Factor")
                         ) #closes third column
                        ) #close fluidrow
            ), # close third tabpanel 
     
    tabPanel("Download Table",
           DT::dataTableOutput("myTable"),
           downloadButton("downloadData", "Download Resulting Table")
           ) #close fourth tabpanel
         ) #close tabset panel
) #close fluidpage

           



