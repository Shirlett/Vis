## About the Project

The contents of this folder include the sourcecode and sourcefile used to create an R shiny application that supports the analysis of Internet Usage against various socio-economic factors, like Median life expectancy, unemployment and Gross National Income per Capita. The application called DiviVis contains information for over 200 countries across the globe as retrieved from the UN Data Retrieval System. It is intended for students, researchers, government employees, and telecommunications decision-makers who want to gain some understanding of factors leading to the digital divide between countries or regions.

The live application can be viewed at this location and includes instructions for use: https://shirlett.shinyapps.io/worldinternetusage/

## Major File Descriptions

ALL_Data.csv - This is the full dataset based on data merged from individual tables in the UN database. The UN database is itself fed by other member organizations, such as the Organisation for Economic Co-operation and Development (OECD), and the International Telecommunications Union (ITU).

ui.R - This file contains the code and lists the R libraries that controls the layout of the user interface of the web application

server.R - This file contains the code and lists the R libraries that manipulate the underlying data to prepare it for each graph and table in the application.

Howto.Rmd - This is a markdown file that contains the instructions and is embedded on the front tab of the application

Variables.Rmd - This is also a markdown file that contains a tabular list of all variables used in the graphs and their complete descriptions. The table appears on the third tab. 

