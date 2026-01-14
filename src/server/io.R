

# Handles input and output
# Uploads, downloads and output schema (excel and reports)


main_output <- function(input, output, session, values) {

  # --- HANDLER: Download Excel Template ---
  output$download_template <- downloadHandler(
    filename = function() "budget_template.xlsx",
    content = function(file) {
      saveWorkbook(create_budget_template_wb(), file, overwrite = TRUE)
    }
  )

  # --- HANDLER: Download the Excel file with current data ---
  output$initial_download <- downloadHandler(
    filename = function() "budget_data.xlsx",
    content = function(file) {
      input_excel_download(values)
    }
  )
}



# Helper functions:

# --- Function: Download current data as Excel file ---
input_excel_download <- function(values) {
  wb <- createWorkbook()
  
  addWorksheet(wb, "Expenses")
  
  temp <- values
  
  # Rename columns for output
  colnames(temp$expenses) <- c("Priority", "Item ID", "Expense Category", "Planned Amount", "Latest Payment Date", "Notes", "old_index")
  writeData(wb, "Expenses", temp$expenses |> select(-old_index), withFilter = TRUE)

  colnames(temp$funding) <- c("Source ID", "Name", "Allowed categories", "Valid From", "Valid To", "Amount", "Notes")
  addWorksheet(wb, "Funding")
  writeData(wb, "Funding", temp$funding, withFilter = TRUE)

  saveWorkbook(wb, file, overwrite = TRUE)
}

# --- Function: Create Excel template workbook ---
create_budget_template_wb <- function() {
  wb <- createWorkbook()
  addWorksheet(wb, "Expenses")
  writeData(
    wb,
    "Expenses",
    data.frame(
      `Priority`= integer(),
      `Item ID` = character(),
      `Expense Category` = character(),
      `Planned Amount` = numeric(),
      `Latest Payment Date` = character(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  addWorksheet(wb, "Funding")
  writeData(
    wb,
    "Funding",
    data.frame(
      `Source ID` = character(),
      `Name` = character(),
      `Allowed categories` = character(),
      `Valid From` = character(),
      `Valid To` = character(),
      `Amount` = numeric(),
      `Probability` = numeric(),
      `Notes` = character(),
      check.names = FALSE
    )
  )
  wb
}