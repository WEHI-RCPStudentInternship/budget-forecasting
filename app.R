
source("src/main_ui.R")
source("src/main_server.R")
source("requirements/packages.R")

# 1. Installing packages

run_setup()


# 2. Load packages

library(shiny)        # Core framework for the web app
library(bslib)        # For modern Bootstrap themes and styling
library(DT)           # For interactive data tables
library(dplyr)        # For data manipulation (filter, select, mutate)
library(readxl)       # For reading Excel files
library(openxlsx)     # For creating and formatting Excel exports
library(tidyr)        # For data tidying (e.g., replace_na)
library(rmarkdown)    # For generating PDF reports
library(shinyjs)      # For JavaScript operations (hiding/disabling inputs, onclick events)
library(tinytex)      # Helper for compiling LaTeX to PDF
library(RColorBrewer) # For professional color palettes


# 3. Load UI

ui <- main_ui_layout()



# 4. Load Server
server <- function(input, output, session) {
  
  # Memory (to be filled)
  values <- reactiveValues()
  
  main_server_logic(input, output, session, values)
  
}



# 5. Run Shiny App
shinyApp(ui, server)



