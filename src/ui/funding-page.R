

# UI for Funding Page

funding_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      id = "funding-container",
      
      div(
        class = "input-title-container",
        
        div(
          "Funding",
          class = "content-title"
        ),
        
        div(
          actionButton("add_funding", "+ Add Funding", class = "add_data_btn")
        )
        
      ),
      
      card(
        div(
          p("this is funding table")
        ),
        full_screen = TRUE,
        class = "info-containers"
      )
      
    )
  
  )
  
}