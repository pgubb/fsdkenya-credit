

# Global functions

# Loads in raw (original) data
get_raw_data_fromfile <- function(year, filename) {
  # Opening file, subsetting to adults over the age of 18 and dropping pre-existing financial health indicators
  if (year == 2021) {
    path <- paste(PROJDIR, "data", year, filename, sep = "/")
    data <- read_sav(path) %>% filter(A19 >= 18) %>% dplyr::select(-starts_with("mfhi")) %>% mutate(year = year, year_fct = as.character(year))
  }

  if (year == 2019) {
    path <- paste(PROJDIR, "data", year, filename, sep = "/")
    data <- read_sav(path) %>%
      filter(a13 >= 18) %>%
      dplyr::rename(
        e1q1 = e11,
        e1q2 = e12,
        e1q3 = e13,
        e1q4 = e14,
        e1q5 = e15,
        e1q6 = e16,
        e1q7 = e17,
        e1q8 = e18,
        e1q9 = e19,
        e1q10 = e110,
        e1o6 = e106
      ) %>% mutate_at(vars(starts_with("c2_")), ~ ifelse(is.na(.), 0, .)) %>%
      mutate(year = year,
             year_fct = as.character(year),
             hh_id = paste(a1, a8, a8_1, a8_2, sep = "_"),
             mem_id = paste(hh_id, a_line, sep = "_"))

  }

  return(data)

}

# Creates a codebook mapping variable names to labels
gen_varbook_vec <- function(data) {

  # Creating a named vector with all variable names and labels
  n <- ncol(data)
  labels <- as.character(map(seq(1,n), function(x) attr(data[[x]], "label")))
  names(labels) <- names(data)

  return(labels)
}

# Modifies the indicator reference list
modglobal_indicator_reflist <- function(data) {
  # Takes FinAccess dataset and modifies the global loan indicators reference vector to add specific reasons the loan was taken

  # Reasons for taking loan
  reasons_num <- attr(data$E1iiiA, "labels")
  reasons <- names(reasons_num)
  names(reasons) <- reasons_num
  reasons <- reasons[2:length(reasons)]
  add <- reasons
  names(add) <- paste0("loan_reason_det_", names(reasons))

  INDICATORS_REFLIST_LVL_LOAN <<- c(INDICATORS_REFLIST_LVL_LOAN, add)

}

# Returns the population weighted total for an indicator(s)
get_indicator_total <- function(data, indicators, groups) {

  data <- data %>%
    group_by(mem_id, lender_agg3_str) %>%
    filter(row_number() == 1) %>%
    ungroup()

  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  return(sum(results$total))

}

# Prepares borrower-level data
prep_borrower_data <- function(data) {

  year <- unique(data$year)

  if (year == 2021) {
    return(prep_borrower_data_2021(data))
    }
  if (year == 2019) {
    return(prep_borrower_data_2019(data))
  }

  }

# Prepares loan-level data
prep_loans_data <- function(data, borrower_data) {

  year <- unique(data$year)

  if (year == 2021) {
    return(prep_loans_data_2021(data, borrower_data))
  }
  if (year == 2019) {
    return(prep_loans_data_2019(data, borrower_data))
  }

  }

# Prepares
prep_respdebt_data <- function(borrower_data, loans_data, lendervar) {

  lendervar <- sym(lendervar)

  alladults <- borrower_data %>% select(mem_id) 
  characteristics <- borrower_data %>% select(year, year_fct, mem_id, psu, probweights, hh_urbrur, hh_wlth_group, resp_gender_fct, resp_income_w_pred, resp_income_quintile)

  balances <- loans_data %>%
    select(mem_id, !!lendervar, nloans_pastyear, loan_principal, loan_balance) %>%
    group_by(mem_id, !!lendervar) %>%
    summarize(
      nloans_pastyear = sum(nloans_pastyear, na.rm = TRUE),
      loan_principal = sum(loan_principal, na.rm = TRUE),
      debt_balance = sum(loan_balance, na.rm = TRUE)
    ) %>% ungroup()

  debt_by_source <- left_join(alladults, balances, by = c("mem_id"))  %>%
    complete(mem_id, !!lendervar) %>%
    filter(!is.na(!!lendervar)) %>% 
    mutate(
      nloans_pastyear = ifelse(is.na(nloans_pastyear), 0, nloans_pastyear),
      loan_principal = ifelse(is.na(loan_principal), 0, loan_principal),
      debt_balance = ifelse(is.na(debt_balance), 0, debt_balance),
    )

  debt_all_sources <- debt_by_source %>%
    group_by(mem_id) %>%
    summarize(
      nloans_past_year = sum(nloans_pastyear), 
      loan_principal = sum(loan_principal),
      debt_balance = sum(debt_balance)) %>%
    mutate(!!lendervar := "All sources")

  analysis_data <-
    bind_rows(debt_by_source, debt_all_sources) %>%
    left_join(characteristics, by = c("mem_id")) %>%
    mutate(
      fullsample = "All adults",
      nloans_pastyear = ifelse(is.na(nloans_pastyear), 0, nloans_pastyear),
      has_recent_loan = ifelse(loan_principal > 0, 1, 0),
      has_debt = ifelse(debt_balance > 0, 1, 0),
      debt_balance_income = debt_balance/ifelse(resp_income_w_pred == 0, 1, resp_income_w_pred),
      debt_high = ifelse(debt_balance_income > 0.5, 1, 0),
      # Conditional on having debt
      nloans_pastyear_cd = ifelse(has_recent_loan == 0, NA, nloans_pastyear), 
      loan_principal_cd = ifelse(has_recent_loan == 0, NA, loan_principal),
      debt_balance_cd = ifelse(has_debt == 0, NA, debt_balance),
      debt_balance_income_cd = ifelse(has_debt == 0, NA, debt_balance_income),
      debt_high_cd = ifelse(debt_balance_income_cd > 0.5, 1, 0)
    )

  return(analysis_data)

}
