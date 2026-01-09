

# UI for Expense Page

expense_ui <- function() {
  
  div(
    class = "result-container",
    
    div(
      id = "expense-container",
      
      div(
        class = "input-title-container",
        
        div(
          "Expense",
          class = "content-title"
        ),
        
        div(
          actionButton("add_expense", "+ Add Expense", class = "add_data_btn")
        )
        
      ),
      
      card(
        div(
          dataTableOutput("sample_expense_table")
        ),
        full_screen = TRUE,
        class = "info-containers",
        style = "padding: 16px;"
      )
      
    ),
    
    actionButton("initial_download", "↓ Download Excel file", class = "initial-excel-download")
    
  )
  
}


add_expense_modal <- function() {
  
  div(
    div("this is free"),
  
    div("this is expense")
  )
}






