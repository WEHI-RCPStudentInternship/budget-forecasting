

# Handles the sorting of expenses
library(dplyr)
library(rlang)

# Main sorting function
# This function sorts the expenses data based on user selection.
# 1. "manual"   - sorting according to user drag-and-drop order (handled by DT/Shiny)
# 2. "by_rules" - sorting according to column priorities and dynamic category order
main_sorting_expenses <- function(expenses_data, 
                                  mode = NULL,           # "manual" or "by_rules"
                                  # manual_order = NULL,     # Dataframe representing user's drag-and-drop order (for manual mode)
                                  ordering_rules = NULL) { # List representing user's sorting rules (for by_rules mode)

  # ---------------------------------------------------------
  # 1. Column Sorting
  # ---------------------------------------------------------
  if (mode == "by_rules") {
    
    if (is.null(ordering_rules)) {
      stop("By_rules mode selected but ordering_rules is NULL")
    }
    
    # A. Reflect the order of category blocks dragged by the user on the page
    # Convert `expense_category` to a factor; the order of `levels` represents the user's desired sequence
    
    if (!is.null(ordering_rules$category_order)) {
      expenses_data <- expenses_data %>%
        mutate(expense_category = factor(
          expense_category, 
          levels = ordering_rules$category_order
        ))
    }
    
    # B. Define a mapping function: determine the sorting expression based on the project type
    # Logic: if it's Categories, use the drag-and-drop order; if it's Date, use the earliest/latest direction
    
    get_sort_expression <- function(p_item, p_date_dir) {
      if (is.null(p_item) || p_item == "None") return(NULL)
      
      if (p_item == "Categories") {
        # Sort directly according to the factor order set by mutate
        return(expr(expense_category))
      } 
      
      if (p_item == "Payment Date") {
        # Determine ascending or descending order based on the direction parameter
        if (!is.null(p_date_dir) && p_date_dir == "latest_payment_date") {
          return(expr(desc(latest_payment_date)))
        } else {
          return(expr(latest_payment_date))
        }
      }
      return(NULL)
    }
    
    # C. Construct a multi-level sorting list
    sort_list <- list()
    
    # 1st Priority
    p1_expr <- get_sort_expression(ordering_rules$p1_item, ordering_rules$p1_date_dir)
    if (!is.null(p1_expr)) sort_list[[length(sort_list) + 1]] <- p1_expr
    
    # 2nd Priority
    p2_expr <- get_sort_expression(ordering_rules$p2_item, ordering_rules$p2_date_dir)
    if (!is.null(p2_expr)) sort_list[[length(sort_list) + 1]] <- p2_expr
    
    # D. Tie-breaker
    # When payment date and categories are the same, this preserves the current relative order when rules are equal
    sort_list[[length(sort_list) + 1]] <- expr(priority)
    
    # E. Perform the sorting and rewrite the indices
    # Use !!! to unquote the expressions in the list for the arrange function
    expenses_sorted <- expenses_data %>%
      arrange(!!!sort_list) %>%
      mutate(priority = row_number())
    
    # return(expenses_sorted)
    
  } 
  
  # ---------------------------------------------------------
  # 2. Manual Sorting
  # ---------------------------------------------------------
  else if (mode == "manual") {
    expenses_sorted <- expenses_data %>%
      mutate(priority = row_number())
    return(expenses_sorted)
  }

  return(expenses_sorted)
}

# --- Manual Row Reordering ---
row_reorder <- function(newOrder, expenses, proxy, id_col) {
  new_idx <- match(newOrder, expenses[[id_col]])
  df <- expenses[new_idx, ] |> mutate(priority = seq_len(nrow(expenses)))
  replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)

  return(df) # Return updated dataframe
}

row_reorder_callback <- c(
  "table.on('row-reorder', function(e, details, edit) {",
  "  var ids = table.column(0).data().toArray();", # Get current id order
  "  var newOrder = [...ids];",
  "  for(var entry of details) {",
  "    newOrder[entry.newPosition] = ids[entry.oldPosition];",
  "  }",
  "  Shiny.setInputValue('newOrder', newOrder, {priority: 'event'});",
  "});"
)


