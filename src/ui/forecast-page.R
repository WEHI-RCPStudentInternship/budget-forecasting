

# UI for Forecast Page


forecast_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      "Forecast",
      class = "content-title",
      
      div(
        class = "info-containers",
        
        # Upload Spreadsheet Card
        
        card(
          div(
            p("Upload the Excel file", class = "card-title"),
            
            div(
              id = "upload-container",
              
              div(
                id = "left-upload",
                
                fileInput(
                  inputId = "spreadsheet_upload",
                  label = NULL,
                  buttonLabel = "Choose File",
                  placeholder = "No file chosen",
                  accept = c(".xlsx")
                )
              ),
              
              div(
                id = "right-download",
                actionButton("download_template", "↓ Download Template",
                                 class = "template-download-btn")
              )
            )
          )
        ),
        
        
        
        # Priority Card
        
        card(
          id = "set_priority_card",
          full_screen = TRUE,
          
          div(
            p("Set Priority", class = "card-title"),
            
            div(
              selectInput(
                "select_priority",
                label = NULL,
                choices = c("Manual Priority", "Column Priority")
              ),
              style = "margin-left: 10px; margin-top: 5px;"
            ),
            
            uiOutput("priority_card")
            
          )
        )
      ),
      
      actionButton("generate_forecast", "Generate Forecast", class = "generate_forecast_btn")
    )
    
  )

  
}


manual_priority_ui <- function() {
  
  div(
    DTOutput("sample_manual_table"),
    style = "padding: 16px; font-weight: 400; font-size: 16px;"
  )
}


column_priority_ui <- function() {
  
  div("TBC...")
  
  
  
}











