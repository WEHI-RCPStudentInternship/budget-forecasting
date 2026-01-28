

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(circlize)
library(chorddiag)


# Funding Source
funding <- data.frame(
  SourceID = c("FS001", "FS002", "FS003", "FS004", "FS005", "FS006", "FS007"),
  Categories = I(list(
    c("Salary"),
    c("Equipment"),
    c("Travel"),
    c("Salary"),
    c("Equipment"),
    c("Travel"),
    c("Equipment")
  )),
  ValidFrom = as.Date(c("2025-01-01", "2025-02-01", "2025-04-01", "2025-05-01", "2025-07-01", "2025-01-01", "2025-10-01")),
  ValidTo = as.Date(c("2025-03-31", "2025-06-30", "2025-09-30", "2025-12-31", "2025-12-31", "2025-12-31", "2025-12-31")),
  Amount = c(12000, 18000, 10000, 30000, 15000, 8000, 9000)
)


# Expense Data Frame
expense <- data.frame(
  ExpenseID = c("E001","E002","E003","E004","E005","E006","E007","E008","E009","E010"),
  Category = c("Salary","Equipment","Salary","Travel","Equipment","Salary","Travel","Equipment","Salary","Travel"),
  Amount = c(9000, 40000, 8000, 6000, 15000, 20000, 7000, 9000, 12000, 5000),
  Date = as.Date(c(
    "2025-01-15","2025-02-20","2025-03-10","2025-04-05","2025-05-15",
    "2025-06-10","2025-07-20","2025-08-30","2025-10-01","2025-11-15"
  ))
)


df_allocations <- data.frame(
  SourceID = c(
    "FS001","FS001","FS002","FS002","FS003","FS003",
    "FS004","FS004","FS004","FS005","FS005","FS006","FS006","FS007"
  ),
  ExpenseID = c(
    "E001","E003","E002","E005","E004","E007",
    "E003","E006","E009","E002","E008","E007","E010","E002"
  ),
  ExpenseCategory = c(
    "Salary","Salary","Equipment","Equipment","Travel","Travel",
    "Salary","Salary","Salary","Equipment","Equipment","Travel","Travel","Equipment"
  ),
  AllocatedAmount = c(
    9000,3000,3000,15000,6000,4000,
    5000,20000,5000,6000,9000,3000,5000,9000
  )
)

df_expenses_status <- data.frame(
  ExpenseID = c("E001","E002","E003","E004","E005","E006","E007","E008","E009","E010"),
  Category = c(
    "Salary","Equipment","Salary","Travel","Equipment",
    "Salary","Travel","Equipment","Salary","Travel"
  ),
  Amount = c(9000,40000,8000,6000,15000,20000,7000,9000,12000,5000),
  Date = as.Date(c(
    "2025-01-15","2025-02-20","2025-03-10","2025-04-05","2025-05-15",
    "2025-06-10","2025-07-20","2025-08-30","2025-10-01","2025-11-15"
  )),
  FilledAmount = c(9000,18000,8000,6000,15000,20000,7000,9000,5000,5000),
  IsFilled = c(TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE),
  Status = c(
    "Full","Partial","Full","Full","Full",
    "Full","Full","Full","Partial","Full"
  )
)

df_funds_summary <- data.frame(
  SourceID = c("FS001","FS002","FS003","FS004","FS005","FS006","FS007"),
  InitialAmount = c(12000,18000,10000,30000,15000,8000,9000),
  UsedAmount = c(12000,18000,10000,30000,15000,8000,9000),
  RemainingAmount = c(0,0,0,0,0,0,0)
)



allocation_with_funding_df <- df_allocations %>%
  left_join(
    funding %>%
      select(
        SourceID,
        ValidFrom,
        ValidTo
      ), by = "SourceID"
  )


full_allocation_df <- allocation_with_funding_df %>%
  left_join(
    df_expenses_status %>%
      select(
        ExpenseID,
        ExpenseAmount = Amount,
        ExpenseDate = Date
      ), by = "ExpenseID"
  )


# SHORTFALL PLOT


# Mock dataframe

"
Y axis: total shortfall amount
X axis: timeline in weeks or months

"

create_shortfall_bar <- function() {
  
  # This works if we ignore overdue payment
  #
  # date_ordered_allocation <- ordered_allocation[order(ordered_allocation$Date),]
  # 
  # date_ordered_allocation$shortfall <- date_ordered_allocation$Allocated - date_ordered_allocation$Amount
  # 
  # expense_shortfall <- date_ordered_allocation %>%
  #   filter(shortfall < 0) %>%
  #   mutate(StartMonth = floor_date(Date, "month"))
  # 
  # months <- seq(
  #   from = floor_date(min(date_ordered_allocation$Date), "month"),
  #   to = floor_date(max(date_ordered_allocation$Date), "month"),
  #   by = "1 month"
  # )
  # 
  # monthly_shortfall <- expense_shortfall %>%
  #   rowwise() %>%
  #   mutate(Month = list(months[months >= StartMonth])) %>%
  #   unnest(Month) %>%
  #   ungroup() %>%
  #   group_by(Month) %>%
  #   summarise(
  #     TotalShortfall = sum(shortfall),
  #     NumberOfShortfalls = n(),
  #     .groups = "drop"
  #   )
  # 
  # total_shortfalls <- tail(monthly_shortfall$NumberOfShortfalls, n = 1)
  # 
  
  # Prepping dataframe by setting all monthly baseline
  df <- full_allocation_df %>%
    mutate(
      ExpenseDateMonth = floor_date(ExpenseDate, "month"),
      ValidFromMonth = floor_date(ValidFrom, "month"),
      ValidToMonth = floor_date(ValidTo, "month"),
      Overdue = case_when(
        ExpenseDate >= ValidFrom & ExpenseDate <= ValidTo ~ "In Time",
        TRUE ~ "Overdue"
      )
    )
  
  
  # Dataframe including range of months involved in the allocation and prepping
  # for final shortfall dataframe
  months <- seq(
    from = min(df$ExpenseDateMonth),
    to = max(df$ValidToMonth),
    by = "1 month"
  )
  months_df <- tibble(Month = months)
  

  # Extracting distinct expenses 
  distinct_expenses <- df %>% 
    distinct(ExpenseID, ExpenseAmount, ExpenseDateMonth)

  
  # Cumulative allocation for each expense for each month
  funding_by_month <- df %>%
    rowwise() %>%
    mutate(Month = list(months[months >= ValidFromMonth])) %>%
    unnest(Month) %>%
    group_by(ExpenseID, Month) %>%
    summarise(
      CumulativeAllocated = sum(AllocatedAmount, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # Combining dataframe and recording shortfall timeline after
  # each expense latest payment date
  expense_month_grid <- distinct_expenses %>%
    crossing(months_df) %>%
    filter(Month >= ExpenseDateMonth)
  

  
  # Dataframe showing cumulative shortfalls for each expense across all months
  expenses_month_status <- expense_month_grid %>%
    left_join(funding_by_month, by = c("ExpenseID", "Month")) %>%
    mutate(
      CumulativeAllocated = replace_na(CumulativeAllocated, 0),
      Shortfall = CumulativeAllocated - ExpenseAmount,
      IsShort = Shortfall < 0,
      IsOverdue = IsShort & (Month > ExpenseDateMonth)
    )
  

  # Final monthly shortfall dataframe 
  monthly_shortfall <- expenses_month_status %>%
    group_by(Month) %>%
    summarise(
      TotalShortfall = sum(if_else(IsShort, Shortfall, 0), na.rm = TRUE),
      NumberOfShortfalls = n_distinct(ExpenseID[IsShort]),
      OverdueShortfall = sum(if_else(IsOverdue, Shortfall, 0), na.rm = TRUE),
      NumberOverdue = n_distinct(ExpenseID[IsOverdue]),
      .groups = "drop"
    ) %>%
    right_join(months_df, by = "Month") %>%
    mutate(
      TotalShortfall = replace_na(TotalShortfall, 0),
      NumberOfShortfalls = replace_na(NumberOfShortfalls, 0L),
      OverdueShortfall = replace_na(OverdueShortfall, 0),
      NumberOverdue = replace_na(NumberOverdue, 0L)
    ) %>%
    arrange(Month)
  
  #print(monthly_shortfall)
  
  
  shortfall_num <- expenses_month_status %>%
    filter(IsShort == TRUE & IsOverdue == TRUE)
  
  total_shortfalls <- length(unique(shortfall_num$ExpenseID))
  
  total_balance <- sum(funding$Amount)
  
  
  # Number of shortfalls bar graph (by month)
  shortfall_number_bar <- plot_ly(
    data = monthly_shortfall,
    x = ~Month,
    y = ~NumberOfShortfalls,
    type = "bar",
    showlegend = FALSE,
    hovertemplate = paste(
      "Month: %{x|%b %Y}<br>",
      "Number of Shortfalls: %{y}<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(showticklabels = FALSE)
    )
  
  
  # Total shortfall amount bar graph (by month)
  shortfall_amount_bar <- plot_ly(
    data = monthly_shortfall,
    x = ~Month,
    y = ~TotalShortfall,
    type = "bar",
    showlegend = FALSE,
    hovertemplate = paste(
      "Month: %{x|%b %Y}<br>",
      "Total Shortfall Amount: %{y}<extra></extra>"
    )
  ) %>%
    layout(
      xaxis = list(tickformat = "%b %Y")
    )
  
  
  p <- subplot(
    shortfall_number_bar,
    shortfall_amount_bar,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.5, 0.5)
  ) %>%
    layout(
      xaxis = list(
        title = "",
        tickformat = "%b %Y",
        tickmode = "linear",
        dtick = "M1",
        showticklabels = TRUE
      ),
      yaxis = list(domain = c(0.5, 1)),
      yaxis2 = list(domain = c(0, 0.5)),
      margin = list(t = 80, b = 60),
      annotations = list(
        list(
          text = "Number of Shortfalls",
          x = 0.5,
          y = 1.1,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 15)
        ),
        list(
          text = "Total Shortfall Amount",
          x = 0.5,
          y = -0.07,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
      )
    ) 

  
  p$x$source <- "A"
  p <- event_register(p, "plotly_click")
  
  list(
    total_balance = total_balance,
    shortfall_plot = p,
    total_shortfalls = total_shortfalls
  )
  
}


# CIRCOS PLOT

"
  Two sides: funding and expenses,
  
  feature activation: when user clicks on a bar in the bar graph,
  app should show the circos plot of allocation at that point in time

"


create_circos_plot <- function(month) {
  #allocation should be done within the month of the latest payment date
  # if funding has a big interval.
  
  #print(month)
  
  ordered_expenses <- expense[order(expense$ExpenseID),]
  #print(funding)
  
  sources_ids <- unique(funding$SourceID)
  expenses_ids <- unique(ordered_expenses$ExpenseID)
  sectors <- c(sources_ids, expenses_ids)
  #print(sectors)
  
  
  rows_until_month <- full_allocation_df %>%
    filter(ValidFrom < month)
  #print(rows_until_month)
  
  mat <- matrix(0, nrow = length(sectors), ncol = length(sectors))
  rownames(mat) <- sectors
  colnames(mat) <- sectors
  
  # Allocating to expenses
  for (i in 1:nrow(rows_until_month)) {
    mat[rows_until_month$SourceID[i], rows_until_month$ExpenseID[i]] <- rows_until_month$AllocatedAmount[i]
    mat[rows_until_month$ExpenseID[i], rows_until_month$SourceID[i]] <- rows_until_month$AllocatedAmount[i]
  }

  # Leftover expenses self-links at the current time (current month)
  cumulative_expense <- rows_until_month %>%
    group_by(ExpenseID) %>%
    summarise(
      ExpenseAmount = first(ExpenseAmount),
      CumulativeAllocation = sum(AllocatedAmount),
      LeftoverExpense = ExpenseAmount - CumulativeAllocation,
      .groups = "drop"
    )
  
  #print(cumulative_expense)
  
  for (i in 1:nrow(cumulative_expense)) {
    mat[cumulative_expense$ExpenseID[i], cumulative_expense$ExpenseID[i]] <- cumulative_expense$LeftoverExpense[i]
  }
  
  # Funding that hasn't been allocated  or isn't available yet
  unallocated_funding <- funding %>%
    anti_join(rows_until_month, by = "SourceID")
  
  
  # Leftover funding self-links
  if (nrow(unallocated_funding) > 0) {
    for (i in 1:nrow(unallocated_funding)) {
      mat[unallocated_funding$SourceID[i], unallocated_funding$SourceID[i]] <- unallocated_funding$Amount[i]
    }
  }
  #print(mat)
  
  
  funding_length <- length(sources_ids)
  expense_length <- length(expenses_ids)
  
  
  funding_colors <- rainbow(funding_length)
  expense_colors <- heat.colors(expense_length)
  sector_colors <- c(funding_colors, expense_colors)
  
  chorddiag(mat,
            groupColors = sector_colors,
            groupNames = sectors,
            groupThickness = 0.1,
            groupPadding = 5,
            groupnamePadding = 40,
            showTicks = TRUE,
            margin = 80,
            tooltipNames = sectors,
            tooltipUnit = "$",
            tooltipGroupConnector = " → ",
            chordedgeColor = "#B3B6B7")
  
}

