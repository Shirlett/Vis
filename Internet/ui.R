
library(shiny)
library(htmlwidgets)
library(shinycssloaders)
library(markdown)
library(leaflet)
library(shinythemes)
library(plotly)



ui <- fluidPage(
theme = shinytheme("simplex"),
  titlePanel(title=div(img(src="internet_world.jpg", height = 50, width = 100), HTML("<strong>DiviVis: Exploring the Relationship between Internet Usage and Socio-economic Factors</strong>")),  windowTitle = "DiviVis"),
  tabsetPanel(
    tabPanel("How to Use",
             includeMarkdown("Howto.Rmd")
             ), #closes first tabPanel
      tabPanel("Overview",
               fluidRow(
                 column(4,
                    sliderInput('IUsage', 'Use Slider to Adjust Range of Internet Usage', 
                                    min = 0, max = 100, value = c(0,100))
                 ), #closes first column
                 column(4,
                        sliderInput("height", "Use Slider to Adjust Plot Height", min = 0, max = 2000, value = 2000)
                 ) #closes second column
               ), #close fluidrow
               uiOutput("ui_plot")
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
                  tags$h2(HTML("Internet Usage Across the World, 2014")),
                  tags$h4(HTML("<strong>Click up to 6 Circle Markers on the map below to see more details for the selected countries</strong>")), 
                  leafletOutput("mymap"),
                  p(em("Source:"),
                    a(href="http://data.un.org/Explorer.aspx?d=SDGs&f=series%3aSL_TLF_UEM", "UN dataset", target="_blank")),
                  tags$h4(tags$span(style="color:green","Scroll Down to Explore Potential Factors")),
                  br()
                        ) #closes mainPanel
                ), #closes sidebarLayout
                fluidRow(
                      column(9,
                      plotlyOutput("internet_par")
                         ), #closes first column
                      br(),
                      column(3,
                             includeMarkdown("Variables.Rmd")
                      ) #closes second column
                         ), #close fluidrow
                  br(),
                fluidRow(
                      column(10,
                        tags$h4(tags$span(style="color:green"," Linear Regression Analysis"))
                      ) #closes first column
                      ), #close fluidrow
                  fluidRow(
                   column(5,
                          tags$h5("Summary Statistics of Internet Usage with all Other Factors:"),
                          tableOutput("lm_model")
                    ), #closes first column
                  column(3,
                         wellPanel(
                           selectInput("factor", "Potential Factors:", list("Median Life Expectancy" = "Life_Exp", "National Income per Capita" = "National_Income", "Urban Density" = "Percent_urban", "Primary School Completion Rate" = "Schooling", "Percent Adult Unemployment" = "Unemployment", "Percent w/ Access to Electricity" = "with_Electricity"))
                         ), #closes wellPanel, 
                         plotOutput("Factor_Plot")
                       ),  #closes second column
                   column(4,
                      plotOutput("Internet_Factor")
                         ) #closes third column
                  ) #close fluidrow
            ), # close third tabpanel 
     
    tabPanel("Download Raw Data",
           DT::dataTableOutput("myTable"),
           downloadButton("downloadData", "Download Resulting Table")
           ) #close fourth tabpanel
         ) #close tabset panel
) #close fluidpage

           



