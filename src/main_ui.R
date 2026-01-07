
# Contains all ui components from ui folder
# Divided into four sections (pages)
# 1. Dashboard (default)
# 2. Forecast
# 3. Funding 
# 4. Expense

source("src/ui/dashboard.R")
source("src/ui/forecast-page.R")
source("src/ui/funding-page.R")
source("src/ui/expense-page.R")

main_ui_layout <- function() {
  
  # Custom theme Bootstrap
  
  
  
  
  
  # Sidebar
  sidebar = sidebar(
    
    
    # Main Content Tabs 
    navset_pill_list(
      
      
      # Dashboard Tab
      
      
      # Forecast Tab
      
      
      # Funding Tab
      
      
      # Expense Tab
      
      
    )
    
    
    
  )
}