

########################################
## Variable/Indicator labels: borrower-level
########################################


select <- dplyr::select

PROJDIR <- getwd()

# Global caption for all figures
CAPTION <- "Source: 2021 FinAccess household survey (Nationally representative sample of 20,909  adults ages 18 and above)."

INDICATORS_REFLIST_LVL_BORR <- c(

  "borrower_any_status" = "Currently borrows from any source",
  "borrower_credit_demand" =  "Currently borrows or attempted to borrow in past year from any source",
  "borrower_formalfi_status" = "Currently borrows from bank or non-bank financial institution (SACCO/MFI/Insurance)",
  "borrower_formaloth_status" = "Currently borrows from other formal insitution (government or hire-purchase provider)",
  "borrower_formalany_status" = "Currently borrows from any formal institution (financial or other)",
  "borrower_digital_status" = "Currently using mobile banking, app or fuliza loan",
  "borrower_informal_status" = "Currently borrows from social network, chama, private moneylender, employer or buyer",

  "borrower_me_g1" = "Digital loans only",
  "borrower_me_g2" = "Non-digital formal loans only",
  "borrower_me_g3" = "Informal loans only",
  "borrower_me_g4" = "Digital + other formal loans",
  "borrower_me_g5" = "Digital + informal loans",
  "borrower_me_g6" = "Other formal + informal loans",
  "borrower_me_g7" = "Digital + Other formal + Informal loans",
  "borrower_me_formal" = "Formal non-digital loans",
  "borrower_me_digital" = "Digital loans",
  "borrower_me_informal" = "Informal loans only",

  "mimp_reason_1" = "Convenience/ Easy to get loan (including less documentation)" ,
  "mimp_reason_2" = "Affordable fees/ Low repayments",
  "mimp_reason_3" = "Easy to use and make repayments",
  "mimp_reason_4" = "Features are suited to my needs (including need for privacy)" ,
  "mimp_reason_5" =  "Trust" ,
  "mimp_reason_6" = "Only option/ Had no choice" ,
  "mimp_reason_7" = "Building credit history/ Keeping option open for future borrowing" ,
  "mimp_reason_8" = "Other reason",

  "lender_agg0_1" = "Currently borrows from bank or non-bank financial institution (SACCO/MFI/Insurance)",
  "lender_agg0_2" = "Currently borrows from mobile banking, app or fuliza",
  "lender_agg0_3" = "Currently borrows from social network, chama, private moneylender, employer or buyer",

  "lender_agg1_1" = "Bank",
  "lender_agg1_2" = "Sacco/MFI",
  "lender_agg1_3" = "Digital",
  "lender_agg1_4" = "Chama",
  "lender_agg1_5" = "Social network",
  "lender_agg1_6" = "Shopkeeper",
  "lender_agg1_7" = "Government",
  "lender_agg1_8" = "Other",

  "lender_agg2_1" = "Bank: Personal, business & other",
  "lender_agg2_3" = "Sacco/MFI",
  "lender_agg2_4" = "Bank: Mobile",
  "lender_agg2_5" = "Non-bank digital: App, Fuliza",
  "lender_agg2_6" = "Chama",
  "lender_agg2_7" = "Social network",
  "lender_agg2_8" = "Shopkeeper",
  "lender_agg2_9" = "Government",
  "lender_agg2_10" = "Other",

  "lender_agg3_1" = "Bank: Personal or business",
  "lender_agg3_2" = "Bank: Overdraft or credit card",
  "lender_agg3_3" = "Sacco/MFI",
  "lender_agg3_4" = "Bank: Mobile",
  "lender_agg3_5" = "Fintech: App",
  "lender_agg3_6" = "Mobile money operator: Fuliza",
  "lender_agg3_7" = "Chama",
  "lender_agg3_8" = "Social network",
  "lender_agg3_9" = "Shopkeeper",
  "lender_agg3_10" = "Government",
  "lender_agg3_11" = "Employer or buyer",
  "lender_agg3_12" = "Hire purchase",
  "lender_agg3_13" = "Private moneylender",
  "lender_agg3_14" = "Other",

  "mimp_loan_agg3_1" = "Bank: Personal & other",
  "mimp_loan_agg3_2" = "Sacco/MFI",
  "mimp_loan_agg3_3" = "Bank: Mobile",
  "mimp_loan_agg3_4" = "Fintech: App",
  "mimp_loan_agg3_5" = "Mobile money operator: Fuliza",
  "mimp_loan_agg3_6" = "Chama",
  "mimp_loan_agg3_7" = "Social network",
  "mimp_loan_agg3_8" = "Shopkeeper",
  "mimp_loan_agg3_9" = "Employer or buyer",
  "mimp_loan_agg3_10" = "Government",
  "mimp_loan_agg3_11" = "Other",

  "debt_high" = "Outstanding debt > 50% of monthly income",
  "debt_repaystress" = "Debt repayment stress", # Borrowed, sold assets, reduced food expenditures
  "debt_default" = "Has defaulted on loan", # Didn't pay only
  "debt_delinquent" = "Delinquent loan payment", # Didn't pay, paid late, missed a payment, paid less
  "debt_stressany" = "Repayment stress or default",
  "debt_monthly_repayment" = "Monthly debt repayment",
  "debt_monthly_repayment_high" = "Monthly debt repayment > 50% monthly income",

  "debt_default_lender_agg0_any" = "All sources",
  "debt_default_lender_agg0_1" = "Formal",
  "debt_default_lender_agg0_2" = "Digital",
  "debt_default_lender_agg0_3" = "Informal",

  "debt_default_lender_agg1_any" = "All sources",
  "debt_default_lender_agg1_1" = "Bank",
  "debt_default_lender_agg1_2" = "Sacco/MFI",
  "debt_default_lender_agg1_3" = "Digital",
  "debt_default_lender_agg1_4" = "Chama",
  "debt_default_lender_agg1_5" = "Social network",
  "debt_default_lender_agg1_6" = "Shopkeeper",
  "debt_default_lender_agg1_7" = "Government",
  "debt_default_lender_agg1_8" = "Other",

  "debt_default_lender_agg2_any" = "All sources",
  "debt_default_lender_agg2_1" = "Bank: Personal, business & other",
  "debt_default_lender_agg2_2" = "Sacco/MFI",
  "debt_default_lender_agg2_3" = "Bank: Mobile",
  "debt_default_lender_agg2_4" = "Non-bank digital: App, Fuliza",
  "debt_default_lender_agg2_5" = "Chama",
  "debt_default_lender_agg2_6" = "Social network",
  "debt_default_lender_agg2_7" = "Shopkeeper",
  "debt_default_lender_agg2_8" = "Government",
  "debt_default_lender_agg2_9" = "Employer or buyer",
  "debt_default_lender_agg2_10" = "Other",

  "debt_default_lender_agg3_any" = "All sources",
  "debt_default_lender_agg3_1" = "Bank: Personal or business",
  "debt_default_lender_agg3_2" = "Bank: Overdraft or credit card",
  "debt_default_lender_agg3_3" = "Sacco/MFI",
  "debt_default_lender_agg3_4" = "Bank: Mobile",
  "debt_default_lender_agg3_5" = "Fintech: App",
  "debt_default_lender_agg3_6" = "Mobile money operator: Fuliza",
  "debt_default_lender_agg3_7" = "Chama",
  "debt_default_lender_agg3_8" = "Social network",
  "debt_default_lender_agg3_9" = "Shopkeeper",
  "debt_default_lender_agg3_10" = "Government",
  "debt_default_lender_agg3_11" = "Employer or buyer",
  "debt_default_lender_agg3_12" = "Hire purchase",
  "debt_default_lender_agg3_13" = "Private moneylender",

  "debt_default_reason_1" = "Did not plan well enough", # Including borrowed too much initially
  "debt_default_reason_2" = "Interest/repayment increased",
  "debt_default_reason_3" = "Did not understand loan terms, payment more than expected",
  "debt_default_reason_4" = "Income declined, business performed poorly",
  "debt_default_reason_5" = "All money used up for basic needs (eg food or utility bills)",
  "debt_default_reason_6" = "Had to pay off other loans",
  "debt_default_reason_7" = "Unexpected emergency expenditure",
  "debt_default_reason_8" = "Other"

)

########################################
## Variable/Indicator labels: loan-level
########################################

INDICATORS_REFLIST_LVL_LOAN <- c(
  "resp_income" = "Personal monthly income",
  "nloans_pastyear" = "Number of loans taken in past year",
  "nloans_outstanding" = "Number of loans outstanding",
  "loan_principal" = "Loan size (Ksh)",
  "loan_principal_income" = "Loan principal as a share of borrower income (%)",
  "loan_balance" = "Outstanding loan balance (Ksh)",
  "total_borrowed" = "Estimated total borrowed in past year (Ksh)",
  "repay_adverse" = "Borrowed, sold assets, reduced food spending or removed children from school to repay loan in past year",
  "repay_default_any" = "Defaulted at least once on any loan in past year",
  "repay_default_n" = "Number of times defaulted on loan in past year",
  "ind_nloans_pastyear_bylender" = "Number of loans taken in the past year (by lender)",
  "ind_totalborrowed_pastyear_bylender" = "Total borrowed in the past year (by lender)",
  "loan_principal_cat_1" =  "< 2,500",
  "loan_principal_cat_2" = "[2,500 - 10,000)",
  "loan_principal_cat_3" = "[10,000 - 50,000)",
  "loan_principal_cat_4" = "> 50,000",

  "has_recent_loan" = "Used a loan recently (% of adults)",
  "has_debt" = "Has outstanding debt (% of adults)",
  "debt_balance_cd" = "Outstanding debt among borrowers (Ksh) [Median]",
  "loan_principal_cd" = "Loan size among borrowers (Ksh) [Median]",
  "debt_balance_income_cd" = "Outstanding debt as a share of income (%) [Median]",

  "lender_agg0_1" = "Formal",
  "lender_agg0_2" = "Digital",
  "lender_agg0_3" = "Informal",

  "lender_agg1_1" = "Bank",
  "lender_agg1_2" = "Sacco/MFI",
  "lender_agg1_3" = "Digital",
  "lender_agg1_4" = "Chama",
  "lender_agg1_5" = "Social network",
  "lender_agg1_6" = "Shopkeeper",
  "lender_agg1_7" = "Government",
  "lender_agg1_8" = "Other",

  "lender_agg2_1" = "Bank: Personal, business & other",
  "lender_agg2_2" = "Sacco/MFI",
  "lender_agg2_3" = "Bank: Mobile",
  "lender_agg2_4" = "Non-bank digital: App, Fuliza",
  "lender_agg2_5" = "Chama",
  "lender_agg2_6" = "Social network",
  "lender_agg2_7" = "Shopkeeper",
  "lender_agg2_8" = "Government",
  "lender_agg2_9" = "Employer or buyer",
  "lender_agg2_10" = "Other",

  "lender_agg3_1" = "Bank: Personal or business",
  "lender_agg3_2" = "Bank: Overdraft or credit card",
  "lender_agg3_3" = "Sacco/MFI",
  "lender_agg3_4" = "Bank: Mobile",
  "lender_agg3_5" = "Fintech: App",
  "lender_agg3_6" = "Mobile money operator: Fuliza",
  "lender_agg3_7" = "Chama",
  "lender_agg3_8" = "Social network",
  "lender_agg3_9" = "Shopkeeper",
  "lender_agg3_10" = "Government",
  "lender_agg3_11" = "Employer or buyer",
  "lender_agg3_12" = "Hire purchase",
  "lender_agg3_13" = "Private moneylender",
  "lender_agg3_14" = "Other", # Loan to purchase land or a home (unspecified lender)

  "loan_reason_agg_1" = "Basic consumption",
  "loan_reason_agg_2" = "Education",
  "loan_reason_agg_3" = "Business/farm investment",
  "loan_reason_agg_4" = "Infrequent/large purchases",
  "loan_reason_agg_5" = "Emergencies",
  "loan_reason_agg_6" = "Debt repayments",
  "loan_reason_agg_7" = "Other"
)

# Color palettes for Figures -------------------------

pal_lender_agg0 <- c("#0868ac", "#8ED081", "#f03b20")
names(pal_lender_agg0) <- INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste("lender_agg0_", collapse = '|'))]

pal_lender_agg1 <- c("#0868ac", "#B3DEE2", "#8ED081", "#f03b20", "#fd8d3c", "#fecc5c",  "#E2AEDD", "grey80")
names(pal_lender_agg1) <- INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste("lender_agg1_", collapse = '|'))]

pal_lender_agg3 <- c("#0868ac", "#43a2ca", "#B3DEE2", "#E2AEDD", "#E27396", "#8ED081", "#bd0026", "#f03b20", "#fd8d3c", "#fecc5c", "#ffffb2", "grey80",  "grey80", "grey80")
names(pal_lender_agg3) <- INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste("lender_agg3_", collapse = '|'))]

pal_wealth <- c("#F25C54", "#F4845F", "#F7B267")
names(pal_wealth) <- c("Poorest 40%", "Middle 40%", "Richest 20%")

pal_gender <- c("All adults" = "black", "Men" = "#0496FF", "Women" = "#E36588")

pal_geo2 <- c("Rural" = "#8ED081", "Non-Nairobi Urban" = "#bd0026",  "Nairobi" = "black")

pal_inc_group <- c("#F7B267", "#F79D65", "#F4845F", "#F27059","#F25C54")
names(pal_inc_group) <- c("< KSh 2,500",  "KSh 2,500 - 5,000", "KSh 5,000 - 10,000", "KSh 10,000 - 15,000", "KSh 15,000+")

pal_inc_quintile <- c("#F7B267", "#F79D65", "#F4845F", "#F27059","#F25C54")
names(pal_inc_quintile) <- c("Poorest 20%", "Q2",  "Q3","Q4", "Richest 20%")


PALETTES <- list(
  "lender_agg0_str" = pal_lender_agg0,
  "lender_agg1_str" = pal_lender_agg1,
  "lender_agg3_str" = pal_lender_agg3,
  "hh_wlth_group" = pal_wealth,
  "resp_gender_fct" = pal_gender,
  "hh_geo2_fct" = pal_geo2,
  "resp_inc_group_fct" = pal_inc_group,
  "resp_inc_quintile" = pal_inc_quintile
)


# Factor levels ------------------------
levels_indicator <- c( "< 2,500", "[2,500 - 10,000)", "[10,000 - 50,000)", "[50,000 - 100,000)",  "> 100,000" )
levels_group_cat <- c("All adults", "Men", "Women", "Poorest 40%", "Middle 40%", "Richest 20%", "Poorest 20%", "Q2","Q3","Q4","Richest 20%", "< KSh 2,500",  "KSh 2,500 - 5,000", "KSh 5,000 - 10,000", "KSh 10,000 - 15,000", "KSh 15,000+")
