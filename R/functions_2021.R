

prep_borrower_data_2021 <- function(data) {

  # Function creates indicators from questionnaire items to prepare for descriptive analysis/visualization
  # Indicators fall into the following categories:
  # Financial health (mfhi_)
  # Exposure to shocks (shocks_)
  # Household level characteristics (hh_)
  # Respondent level characteristics (resp_)
  # Financial service usage indicators (fin_, fst_, borrower_, mimp_, access_)
  # Asset ownership (asset_)

  data <- data %>%
    mutate(

      hh_id = paste(ClusterNo, HHNo, sep = "_"),
      mem_id = paste(hh_id, A14vi, sep = "_"),
      fullsample = "All adults"

    ) %>%

    # Removing duplicate observations
    group_by(mem_id) %>%
    filter(row_number() == 1) %>%
    ungroup()

    # Goals

    goals_levels = c(
      "food" = "Putting food on the table",
      "edu" = "Educating myself or my family",
      "live"= "Developing my livelihood/career",
      "other" = "Other"
    )

    data <- data %>%
    mutate(

    # At this point in your life what is your most important goal:

    goals_group = case_when(
      B1A == 1 ~ "food",
      B1A == 2 ~ "educ",
      B1A %in% c(3, 11) ~ "live",
      B1A %in% c(4, 5, 6, 12) ~ "other"
    ),
    goals_group_fct = factor(goals_levels[goals_group], goals_levels, ordered = TRUE),

  ) %>% dummy_cols(select_columns = c('goals_group'))

  # Shocks

  shocks_levels <- c(
    "clm" = "Climate shock (including crop diseases/pests)",
    "ill" = "Illness or accident",
    "dthi" = "Death of main income earner",
    "dtho" = "Death of other family member",
    "thf" = "Theft or fire",
    "brth" = "Birth of child",
    "job" = "Job or income loss",
    "infl" = "Cost of living")

  shocks_levels2 <- c(
    "clm" = "Climate shock (including crop diseases/pests)",
    "ill" = "Illness or accident",
    "dth" = "Death of family member or relative",
    "thf" = "Theft or fire",
    "brth" = "Birth of child",
    "job" = "Job or income loss",
    "infl" = "Cost of living")

  coping_levels <- c(
    "borr_for" = "Borrowed from Bank/SACCO/MFI",
    "borr_for_d" = "Borrowed from mobile or app",
    "borr_inf" = "Borrowed from social network",
    "sav_for" = "Savings held at Bank/SACCO/MFI",
    "sav_for_d" ="Savingss held in mobile wallet" ,
    "sav_inf_s" = "Savings held socially",
    "sav_cash" = "Savings held in cash",
    "sell_ass" = "Sold assets",
    "trnsf_inf" = "Transfers from social network",
    "cut_exp" = "Cut back on expenses",
    "job_chng" = "Changed job",
    "oth" = "Other"
  )

  data <- data %>% mutate(

    # Has respondent experience any of the following adverse shocks in past year:
    shocks_illness = case_when(R2A__1 == 1 ~ 1, R2A__1 == 0 ~ 0, is.na(R2A__1) ~ NA_real_),
    shocks_climate = case_when(R2A__2 == 1 ~ 1, R2A__2 == 0 ~ 0, is.na(R2A__2) ~ NA_real_),
    shocks_pests = case_when(R2A__9 == 1 ~ 1, R2A__9 == 0 ~ 0, is.na(R2A__9) ~ NA_real_),
    shocks_deathinc = case_when(R2A__3 == 1 ~ 1, R2A__3 == 0 ~ 0, is.na(R2A__3) ~ NA_real_),
    shocks_deathoth = case_when(R2A__4 == 1 ~ 1, R2A__4 == 0 ~ 0, is.na(R2A__4) ~ NA_real_),
    shocks_asset = case_when(R2A__5 == 1 ~ 1, R2A__5 == 0 ~ 0, is.na(R2A__5) ~ NA_real_),
    shocks_income = case_when(R2A__7 == 1 ~ 1, R2A__7 == 0 ~ 0, is.na(R2A__7) ~ NA_real_),
    shocks_prices = case_when(R2A__8 == 1 ~ 1, R2A__8 == 0 ~ 0, is.na(R2A__8) ~ NA_real_),
    shocks_other = 0,

    #shocks_other = case_when(R2A__10 == 1 ~ 1, R2A__10 == 2 ~ 0, is.na(R2A__10) ~ NA_real_),
    shocks_N = shocks_illness + shocks_climate + shocks_pests + shocks_deathinc + shocks_deathoth + shocks_asset + shocks_income + shocks_prices + shocks_other,
    shocks_health = ifelse(shocks_illness == 1 | shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
    shocks_health = ifelse(is.na(shocks_illness) & is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_health),
    shocks_natural = ifelse(shocks_climate == 1 | shocks_pests == 1, 1, 0),
    shocks_natural = ifelse(is.na(shocks_climate) & is.na(shocks_pests), NA, shocks_natural),
    shocks_deathany = ifelse(shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
    shocks_deathany = ifelse(is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_deathany),
    shocks_any = ifelse(shocks_illness == 1 | shocks_climate == 1 | shocks_deathinc == 1 | shocks_deathoth == 1 | shocks_asset == 1 | shocks_income == 1 | shocks_prices == 1 | shocks_pests == 1 | shocks_other == 1, 1, 0),
    shocks_any = ifelse(is.na(shocks_illness) & is.na(shocks_climate) & is.na(shocks_deathinc) & is.na(shocks_deathoth) & is.na(shocks_asset) & is.na(shocks_income) & is.na(shocks_prices) & is.na(shocks_pests) & is.na(shocks_other), NA, shocks_any),

    # Is shock attributable to COVID:

    shocks_illness_covid = R2Ai__1,
    shocks_illness_covid = ifelse(shocks_illness == 0, NA, shocks_illness_covid),

    shocks_deathany_covid = ifelse(R2Ai__3 == 1 | R2Ai__4 == 1, 1, 0),
    shocks_deathany_covid = ifelse(shocks_deathany == 0, NA, shocks_deathany_covid),

    shocks_health_covid = ifelse(shocks_deathany_covid == 1 | shocks_illness_covid == 1, 1, 0),
    shocks_health_covid = ifelse(shocks_health == 0, NA, shocks_health_covid),

    shocks_income_covid = ifelse(R2Ai__7 == 1, 1, 0),
    shocks_income_covid = ifelse(shocks_income == 0, NA, shocks_income_covid),

    shocks_prices_covid = ifelse(R2Ai__8 == 1, 1, 0),
    shocks_prices_covid = ifelse(shocks_prices == 0, NA, shocks_prices_covid),

    shocks_natural_covid = ifelse(R2Ai__2 == 1 | R2Ai__9 == 1, 1, 0),
    shocks_natural_covid = ifelse(shocks_natural == 0, NA, shocks_natural_covid),

    # Main shock

    shocks_main_group = case_when(
      R2B == 1 ~ "ill",
      R2B %in% c(2, 9) ~ "clm",
      R2B == 3 ~ "dthi",
      R2B == 4 ~ "dtho",
      R2B == 5 ~ "thf",
      R2B == 6 ~ "brth",
      R2B == 7 ~ "job",
      R2B == 8 ~ "infl"
    ),
    shocks_main_group_fct = factor(shocks_levels[shocks_main_group], shocks_levels, ordered = TRUE),

    shocks_main_group2 = case_when(
      R2B == 1 ~ "ill",
      R2B %in% c(2, 9) ~ "clm",
      R2B %in% c(3, 4) ~ "dth",
      R2B == 5 ~ "thf",
      R2B == 6 ~ "brth",
      R2B == 7 ~ "job",
      R2B == 8 ~ "infl"
    ),
    shocks_main_group2_fct = factor(shocks_levels2[shocks_main_group2], shocks_levels2, ordered = TRUE),


    #
    shocks_maincop_group = case_when(

      R2Di %in% c(1,6) ~ "borr_for",
      R2Di %in% c(2,3, 10) ~ "borr_for_d",
      R2Di %in% c(4,5,7,8, 9, 21) ~ "borr_inf",
      R2Di %in% c(11) ~ "sav_for",
      R2Di %in% c(12, 13) ~ "sav_for_d",
      R2Di %in% c(14, 15) ~ "sav_inf_s",
      R2Di %in% c(16) ~ "sav_cash",
      R2Di %in% c(17, 18) ~ "sell_ass",
      R2Di %in% c(19) ~ "trnsf_inf",
      R2Di %in% c(20) ~ "cut_exp",
      R2Di %in% c(25) ~ "job_chng",
      R2Di %in% c(22, 23, 24, 26, 27) ~ "oth"

    ),

    shocks_maincop_group_fct = factor(coping_levels[shocks_maincop_group], coping_levels, ordered = TRUE)

  ) %>% dummy_cols(select_columns = c('shocks_main_group', "shocks_maincop_group", "shocks_main_group2"))


  # Creating socio-economic, demographic variables -----------------------

  # Family structure, age, gender

  ASAL_counties_FA <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 15, 16, 17, 23, 24, 25, 28, 30, 31, 33, 34, 19, 13, 12, 14, 44, 43, 32, 22)

  geo_levels <- c("rur_asal" = "Rural, ASAL county",
                  "urb_nbo" = "Urban, Nairobi county",
                  "urb_oth" = "Urban, other",
                  "rur_oth" = "Rural, other")

  geo2_levels <- c("rur" = "Rural",
                   "urb_nbo" = "Nairobi",
                   "urb_oth" = "Non-Nairobi Urban")

  geo3_levels <- c("rur_asal" = "Rural, ASAL country",
                   "rur_oth" = "Rural, other",
                   "urb" = "Urban")

  # Household type variables estimated from household roster (not implemented for 2021)

  hhtype_levels <- c("smh_nmr" = "Single member HH: Never married",
                     "smh_div" = "Single member HH: Divorced or widowed",
                     "smh_mar" = "Single member HH: Married (living alone)",
                     "mmh_wch" = "Multi member HH: With school-age children",
                     "mmh_nch" = "Multi member HH: Without school-age children")


  hhtype2_levels <- c("smh" = "Single member HH",
                      "mmh_nuc" = "Multi-member HH: Nuclear family",
                      "mmh_mnc" = "Multi-member HH: Married, no children",
                      "mmh_ext" = "Multi-member HH: Extended family",
                      "mmh_sip" = "Multi member HH: Single parents")

  data <- data %>%
    mutate(
      psu = ClusterNo,
      probweights = IndWeight,
      hh_urbrur = ifelse(A9 == 1, "Rural", "Urban"),
      hh_geo = case_when(
        hh_urbrur == "Rural" & (County %in% ASAL_counties_FA) ~ "rur_asal",
        County == 47 ~ "urb_nbo",
        hh_urbrur == "Urban" ~ "urb_oth",
        hh_urbrur == "Rural" ~ "rur_oth"
      ),
      hh_geo_2 = case_when(
        County == 47 ~ "urb_nbo",
        hh_urbrur == "Urban" ~ "urb_oth",
        hh_urbrur == "Rural" ~ "rur"
      ),
      hh_geo_3 = case_when(
        hh_urbrur == "Rural" & (County %in% ASAL_counties_FA) ~ "rur_asal",
        hh_urbrur == "Urban" ~ "urb",
        hh_urbrur == "Rural" ~ "rur_oth"
      ),
      hh_geo_fct = factor(geo_levels[hh_geo], levels = geo_levels, ordered = TRUE),
      hh_geo2_fct = factor(geo2_levels[hh_geo_2], levels = geo2_levels, ordered = TRUE),
      hh_geo3_fct = factor(geo2_levels[hh_geo_2], levels = geo3_levels, ordered = TRUE),
      hh_size_all = NHM,
      hh_size_all_c = hh_size_all - mean(hh_size_all),
      hh_urbrur = case_when(
        hh_urbrur == "Urban" ~ "urb",
        hh_urbrur == "Rural" ~ "rur"
      )
      #hh_size_children = a11,
      #hh_size_adults = a10 - a11,
      #hh_singlemember = ifelse(a10 == 1, 1, 0),
      #hh_multimember = ifelse(a10 > 1, 1, 0),
      #hh_children = ifelse(a11 > 0, 1, 0),
      #hh_head_female = ifelse(HeadofHousehold_sex == 2, 1, 0),
      #hh_head_sex_fct = ifelse(HeadofHousehold_sex == 2, "Female", "Male"),
      #hh_head_age = Headofhousehold_Age,
      #hh_type = ifelse(hh_singlemember == 1 & a17 == 1, "smh_nmr", NA),
      #hh_type = ifelse(hh_singlemember == 1 & a17 %in% c(2,3), "smh_div", hh_type),
      #hh_type = ifelse(hh_singlemember == 1 & a17 == 4, "smh_mar", hh_type),
      #hh_type = ifelse(hh_singlemember == 0 & hh_children == 1, "mmh_wch", hh_type),
      #hh_type = ifelse(hh_singlemember == 0 & hh_children == 0, "mmh_nch", hh_type),
      #hh_type_fct = factor(hhtype_levels[hh_type], levels = hhtype_levels),
      #hh_type_2_fct = factor(hhtype2_levels[hh_type_2], levels = hhtype2_levels),
    ) %>% dummy_cols(select_columns = c("hh_type", "hh_urbrur", "hh_geo", "hh_geo_3"))

  # Household Assets:

  data <- data %>%
    mutate(

      # habitable rooms per person
      rooms_per_person = U6/NHM,
      rooms_per_person = ifelse(is.na(rooms_per_person), mean(rooms_per_person, na.rm = TRUE), rooms_per_person),

      dwelling_type = U4,
      dwelling_type = ifelse(is.na(dwelling_type), round(mean(dwelling_type, na.rm = TRUE), 0), dwelling_type),

      toilet_comp = case_when(
        U14a %in% c(1, 2, 3, 9) ~ 3,
        U14a %in% c(4, 5, 6) ~ 2,
        U14a %in% c(7,8) ~ 1,
        is.na(U14a) ~ 1
      ),
      asset_flushtoilet = ifelse(toilet_comp == 3, 1, 0),

      # Piped into home or plot/yard or bottled
      water_comp = case_when(
        U13 %in% c(10, 11, 12) ~ 3,
        U13 %in% c(5, 7, 9, 13, 14, 15) ~ 2,
        U13 %in% c(1, 2, 3, 4, 6, 8) ~ 1,
        is.na(U13) ~ 1
      ),
      asset_pipedwater = ifelse(water_comp == 3, 1, 0),

      # Finished wall (brick, cement, stone/lime)
      wall_comp = case_when(
        U10 %in% c(11, 12, 13, 16, 17) ~ 3,
        U10 %in% c(5, 6, 7, 10, 14) ~ 2,
        U10 %in% c(1, 2, 3, 4, 8, 9, 15) ~ 1,
        is.na(U10) ~ 1
      ),
      asset_brickwall = ifelse(wall_comp == 3, 1, 0),

      # Elecricity main source of lighting:
      lighting_comp = case_when(
        U12 %in% c(1, 7) ~ 3,
        U12 %in% c(2, 3, 4, 5, 11, 12, 13, 8, 9) ~ 2,
        U12 %in% c(6, 10) ~ 1,
        is.na(U12) ~ 1
      ),
      asset_electricity =  ifelse(lighting_comp == 3, 1, 0),

      # Electricity, gas or solar cooking fuel

      cookingfuel_comp = case_when(
        U11 %in% c(1, 3, 7) ~ 3,
        U11 %in% c(2, 4, 6) ~ 2,
        U11 %in% c(5) ~ 1,
        is.na(U11) ~ 1
      ),
      asset_elcgascookfuel = ifelse(cookingfuel_comp == 3, 1, 0),

      asset_radio = ifelse(U16__1 == 1, 1, 0),
      asset_tv = ifelse(U16__2 == 1 | U16__3 == 1 | U16__4 == 1 | U16__5 == 1, 1, 0),
      asset_bicycle = ifelse(U16__9 == 1, 1, 0),
      asset_motorcycle = ifelse(U16__10 == 1, 1, 0),
      asset_car = ifelse(U16__11 == 1, 1, 0),
      asset_fridge = ifelse(U16__13 == 1, 1, 0)

    ) %>% dummy_cols(select_columns = c("toilet_comp", "water_comp", "lighting_comp", "wall_comp", "cookingfuel_comp", "dwelling_type"))

  # Estimating household-level wealth index
  pr.out <- prcomp(~ rooms_per_person + toilet_comp_1 + toilet_comp_2 + toilet_comp_3 +
                     water_comp_1 + water_comp_2 + water_comp_3 +
                     lighting_comp_1 + lighting_comp_2 + lighting_comp_3 +
                     cookingfuel_comp_1 + cookingfuel_comp_2 + cookingfuel_comp_3 +
                     wall_comp_1 + wall_comp_2 + wall_comp_3 +
                     dwelling_type_1 + dwelling_type_2 + dwelling_type_3 + dwelling_type_4, scale = TRUE, data = data)

  #hist(pr.out$x[, 1])

  data <- data %>% mutate(
    hh_wlth_index = -1*pr.out$x[, 1],
    hh_wlth_decile = cut(hh_wlth_index,
                         breaks=quantile(hh_wlth_index, probs=seq(0, 1, by=0.1), na.rm=TRUE),
                         labels=c("Poorest 10%","Q9","Q8","Q7","Q6", "Q5", "Q4", "Q3", "Q2", "Richest 10%"),
                         include.lowest=TRUE),
    hh_wlth_quintile = cut(hh_wlth_index,
                           breaks=quantile(hh_wlth_index, probs=seq(0,1, by=0.2), na.rm=TRUE),
                           labels=c("Poorest 20%","Q2","Q3","Q4","Richest 20%"),
                           include.lowest=TRUE),
    hh_wlth_group = case_when(
      hh_wlth_quintile %in% c("Poorest 20%","Q2") ~ "Poorest 40%",
      hh_wlth_quintile %in% c("Q3","Q4") ~ "Middle 40%",
      hh_wlth_quintile %in% c("Richest 20%") ~ "Richest 20%"
    ),
    hh_wlth_group = factor(hh_wlth_group, levels = c("Poorest 40%", "Middle 40%", "Richest 20%"), ordered = TRUE),
    hh_wlth_bottom40 = ifelse(hh_wlth_group == "Poorest 40%", 1, 0)
  )

  # Respondent-level socio-economic characteristics
  age_levels <- c("age_18_25" = "[18-25)",
                  "age_25_45" = "[25-45)",
                  "age_45_65" = "[45-65)",
                  "age_65" = "[65+]")

  age2_levels <- c("age_18_25" = "Youth\n[18-25)",
                   "age_25_65" = "Working age\n[25-65)",
                   "age_65" = "Retirement age\n[65+]")

  marstat_levels <- c("nmr" = "Never married",
                      "div" = "Separated, divorced or widowed",
                      "mar" = "Married")

  edu_levels <- c("none" = "No formal education",
                  "smpr" = "Some primary",
                  "cmpr" = "Primary complete",
                  "smsc" = "Some secondary",
                  "cmsc" = "Secondary complete",
                  "smtr" = "Some or complete tertiary")

  edu_levels2 <- c("none" = "No formal education",
                   "pri" = "Some or complete primary",
                   "sec" = "Some or complete secondary",
                   "trt" = "Some or complete tertiary")

  live_levels <- c("farm" = "Agriculture",
                   "empl" = "Employed",
                   "cwrk" = "Casual",
                   "owbs" = "Own business",
                   "trns" = "Transfers",
                   "othr" = "Other")

  live_levels2 <- c("farm" = "Farming",
                    "empl" = "Employment",
                    "cwrk" = "Casual work",
                    "owbs" = "Own business",
                    "trns" = "Transfers",
                    "inv" = "Investment",
                    "othr" = "Other")

  gender_levels <- c("men" = "Men", "wmn" = "Women")

  inc_levels <- c("inc_1" = "< KSh 2,500",
                  "inc_2" = "KSh 2,500 - 5,000",
                  "inc_3" = "KSh 5,000 - 10,000",
                  "inc_4" = "KSh 10,000 - 15,000",
                  "inc_5" = "KSh 15,000+")



  data <- data %>%
    mutate(

      # Did financial status improve, stay the same or worsen in past year?
      fin_status_impr = ifelse(B1G == 1, 1, 0),
      fin_status_impr = ifelse(B1G %in% c(98,99), NA, fin_status_impr),
      fin_status_worse = ifelse(B1G == 3, 1, 0),
      fin_status_worse = ifelse(B1G %in% c(98,99), NA, fin_status_worse),

      fin_status_fct = ifelse(fin_status_impr == 1, "Financial status: Improved", "Financial status: Worsened"),
      fin_status_fct = ifelse(is.na(fin_status_impr) & is.na(fin_status_worse), NA, fin_status_fct),

      resp_gender_group = ifelse(A18 == 1, "men", "wmn"),
      resp_gender_fct = factor(gender_levels[resp_gender_group], gender_levels, ordered = TRUE),

      resp_age_yrs = A19,

      resp_age_group = case_when(
        resp_age_yrs < 25 ~ "age_18_25",
        resp_age_yrs < 45 ~ "age_25_45",
        resp_age_yrs < 65 ~ "age_45_65",
        resp_age_yrs >= 65 ~ "age_65"
      ),
      resp_age_group_fct = factor(age_levels[resp_age_group], levels = age_levels, ordered = TRUE),

      resp_age_group2 = case_when(
        resp_age_yrs < 25 ~ "age_18_25",
        resp_age_yrs < 65 ~ "age_25_65",
        resp_age_yrs >= 65 ~ "age_65"
      ),
      resp_age_group2_fct = factor(age2_levels[resp_age_group2], levels = age2_levels, ordered = TRUE),

      resp_marstat = case_when(
        A22 == 1 ~ "nmr",
        A22 %in% c(2,3)  ~ "div",
        A22 == 4  ~ "mar"
      ),
      resp_marstat_fct = factor(marstat_levels[resp_marstat], levels = marstat_levels, ordered = TRUE),
      resp_edu_group = case_when(
        A21 == 1 ~ "none",
        A21 == 2 ~ "smpr",
        A21 == 3 ~ "cmpr",
        A21 == 4 ~ "smsc",
        A21 == 5 ~ "cmsc",
        A21 %in% c(6, 7, 8, 9) ~ "smtr"
      ),
      resp_edu_group_fct = factor(edu_levels[resp_edu_group], levels = edu_levels, ordered = TRUE),
      resp_edu_group2 = case_when(
        A21 == 1 ~ "none",
        A21 %in% c(2, 3) ~ "pri",
        A21 %in% c(4, 5) ~ "sec",
        A21 %in% c(6, 7, 8, 9) ~ "trt",
      ),
      resp_edu_group2_fct = factor(edu_levels2[resp_edu_group2], levels = edu_levels2, ordered = TRUE),

      # Main income source
      resp_live_group = case_when(
        B3B == 1 ~ "farm",
        B3B == 2 ~ "empl",
        B3B == 3 ~ "cwrk",
        B3B == 4 ~ "owbs",
        B3B %in% c(5, 8, 9) ~ "trns",
        B3B %in% c(6, 7, 10) ~ "othr"
      ),
      resp_live_group_fct = factor(live_levels[resp_live_group], levels = live_levels, ordered = TRUE),

      resp_live_group2 = case_when(
        B3B == 1 ~ "farm",
        B3B == 2 ~ "empl",
        B3B == 3 ~ "cwrk",
        B3B == 4 ~ "owbs",
        B3B %in% c(5, 8, 9) ~ "trns",
        B3B %in% c(6, 7) ~ "inv",
        B3B == 10 ~ "othr"
      ),
      resp_live_group2_fct = factor(live_levels2[resp_live_group2], levels = live_levels2, ordered = TRUE),

      # Income from any source
      resp_live_agri = ifelse(B3A__1 == 1, 1, 0),
      resp_live_emp =  ifelse(B3A__2 == 1, 1, 0),
      resp_live_cwrk =  ifelse(B3A__3 == 1, 1, 0),
      resp_live_owbs =  ifelse(B3A__4 == 1, 1, 0),
      resp_live_inv =  ifelse(B3A__6 == 1 | B3A__7 == 1, 1, 0),
      resp_live_trnsf = ifelse(B3A__5 == 1 | B3A__8 == 1 | B3A__9 == 1, 1, 0),
      resp_live_other = 0,

      resp_live_trnsf_sn = ifelse(B3A__9 == 1, 1, 0),
      resp_live_trnsf_in = ifelse(B3A__5 == 1 | B3A__8 == 1, 1, 0),

      # Secondary income source
      resp_live_agri_sec = ifelse(resp_live_agri == 1 & B3B != 1, 1, 0),
      resp_live_emp_sec = ifelse(resp_live_emp == 1 & B3B != 2, 1, 0),
      resp_live_cwrk_sec = ifelse(resp_live_cwrk == 1 & B3B != 3, 1, 0),
      resp_live_owbs_sec = ifelse(resp_live_owbs == 1 & B3B != 4, 1, 0),
      resp_live_trnsf_sec = ifelse(resp_live_trnsf == 1 & B3B %not_in% c(5, 8, 9), 1, 0),
      resp_live_inv_sec = ifelse(resp_live_inv == 1 & B3B %not_in% c(6, 7), 1, 0),
      resp_live_other_sec = ifelse(resp_live_other == 1 & B3B != 10, 1, 0),

      resp_live_agricon = ifelse(B3C == 1, 1, 0),

      resp_live_farm_any = ifelse(resp_live_agri == 1 | resp_live_agricon == 1, 1, 0),

      resp_live_informal_any = ifelse(resp_live_agri == 1 | resp_live_cwrk == 1 | resp_live_owbs == 1, 1, 0),
      resp_live_informal_main = ifelse(B3B %in% c(1,3,4), 1, 0),
      resp_live_informal_sec = ifelse(resp_live_agri_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1, 1, 0),

      resp_live_sec_any = ifelse(resp_live_agri_sec == 1 | resp_live_emp_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1 | resp_live_trnsf_sec == 1 | resp_live_other_sec == 1, 1, 0),

      resp_income = B3I,
      resp_income = ifelse(B3I %in% c(98,99), NA, resp_income),

      resp_incsource_N = resp_live_agri + resp_live_emp + resp_live_cwrk + resp_live_owbs + resp_live_inv + resp_live_trnsf + resp_live_other,

      resp_live_trnsf_sn_only = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 0, 1, 0),
      resp_live_trnsf_in_only = ifelse(resp_live_trnsf_sn == 0 & resp_live_trnsf_in == 1, 1, 0),
      resp_live_trnsf_both = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 1, 1, 0),

      # resp_ppi_pred_npl = probability_pov_npl,
      # resp_ppi_decile_npl = cut(probability_pov_npl,
      #                           breaks=quantile(probability_pov_npl, probs=seq(0,1, by=0.1), na.rm=TRUE),
      #                           labels=c("Richest 10%","Q9","Q8","Q7","Q6", "Q5", "Q4", "Q3", "Q2", "Poorest 10%"),
      #                           include.lowest=TRUE),
      # resp_ppi_group_npl = ifelse(resp_ppi_decile_npl %in% c("Q4", "Q3", "Q2", "Poorest 10%"), "Bottom 40%",
      #                             ifelse(resp_ppi_decile_npl %in% c("Q8","Q7","Q6", "Q5"), "Middle 40%", "Top 20%"))

      # Has social support
      resp_socialfinsupport = ifelse(B1B1 == 1, 1, 0),

      # Has disability
      resp_disability = ifelse(A25 == 1, 1, 0),

      # ANyone in household has chronic disease
      hh_chronicdisease = ifelse(A24 == 1, 1, 0),

    ) %>% dummy_cols(select_columns = c("resp_age_group2", "resp_gender_group", "resp_age_group", "resp_marstat", "resp_edu_group", "resp_edu_group2", "resp_live_group", "resp_live_group2"))

  # Predicting missing personal monthly to impute NA values

  model_data <- data %>%
    select(resp_income, hh_wlth_index, hh_geo_urb_oth, hh_geo_urb_nbo, hh_geo_rur_asal, resp_disability, resp_marstat_div, resp_marstat_nmr, resp_gender_group_men,
           resp_age_group_age_25_45, resp_age_group_age_45_65, resp_age_group_age_65, resp_edu_group2_pri, resp_edu_group2_sec, resp_edu_group2_trt,
           resp_live_group_empl, resp_live_group_cwrk, resp_live_group_owbs, resp_live_group_trns, resp_live_group_othr, resp_incsource_N) %>%
    filter(!is.na(resp_income))

  model <- lm(
            data = model_data,
              resp_income ~
              hh_geo_urb_nbo + hh_geo_urb_oth + hh_geo_rur_asal +
              hh_wlth_index +
              resp_disability +
              resp_marstat_div + resp_marstat_nmr +
              resp_gender_group_men +
              resp_age_group_age_25_45 + resp_age_group_age_45_65 + resp_age_group_age_65 +
              resp_edu_group2_pri + resp_edu_group2_sec + resp_edu_group2_trt +
              resp_incsource_N +
              resp_live_group_empl + resp_live_group_cwrk + resp_live_group_owbs + resp_live_group_trns + resp_live_group_othr
            )

  data <- data %>% add_predictions(model, var = "resp_income_fit")

  data <- data %>%
    mutate(
      resp_income_w_pred = ifelse(is.na(resp_income), resp_income_fit, resp_income),
      resp_income_1000 = resp_income_w_pred/1000,
      resp_income_1000_c = resp_income_1000 - mean(resp_income_1000),

      resp_inc_group = case_when(
        resp_income_w_pred < 2500 ~ "inc_1",
        resp_income_w_pred < 5000 ~ "inc_2",
        resp_income_w_pred < 10000 ~ "inc_3",
        resp_income_w_pred < 15000 ~ "inc_4",
        resp_income_w_pred >= 15000 ~ "inc_5"
      ),
      resp_inc_group_fct = factor(inc_levels[resp_inc_group], levels = inc_levels, ordered = TRUE),

      resp_income_decile = cut(resp_income_w_pred,
                               breaks=quantile(resp_income, probs=seq(0,1, by=0.1), na.rm=TRUE),
                               labels=c("Poorest 10%","Q2","Q3","Q4","Q5", "Q6", "Q7", "Q8", "Q9", "Richest 10%"),
                               include.lowest=TRUE),

      resp_income_quintile = cut(resp_income_w_pred,
                                 breaks=quantile(resp_income, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                 labels=c("Poorest 20%","Q2","Q3","Q4","Richest 20%"),
                                 include.lowest=TRUE),

      # Is income below minimum wage?
      resp_income_lt_ag = ifelse(resp_income_w_pred < 9014, 1, 0),
      resp_income_lt_ag = ifelse(hh_geo2_fct == "Rural", resp_income_lt_ag, NA),

      resp_income_lt_nbo = ifelse(resp_income_w_pred < 21311, 1, 0),
      resp_income_lt_nbo = ifelse(hh_geo2_fct == "Nairobi", resp_income_lt_nbo, NA),

      resp_income_lt_our = ifelse(resp_income_w_pred < 16841, 1, 0),
      resp_income_lt_our = ifelse(hh_geo2_fct == "Non-Nairobi Urban", resp_income_lt_our, NA),

      resp_income_lt_mw = ifelse(hh_geo2_fct == "Nairobi", resp_income_lt_nbo,
                                 ifelse(hh_geo2_fct == "Rural", resp_income_lt_ag, resp_income_lt_our)),

    ) %>% ungroup() %>%  dummy_cols(select_columns = c("resp_inc_group"))

  # Behavioral variables
  # Financial numeracy
  # Digital numeracy

  data <- data %>%
    mutate(
      know_fin_numeracy = ifelse(B2F == 1, 1, 0),
      #know_fin_numeracy = ifelse(B2F %in% c(98,99), NA, know_fin_numeracy),
      know_dig_literacy = ifelse(B2G == 1, 1, 0 ),
      #know_dig_literacy = ifelse(B2G %in% c(9998, 9999), NA, know_dig_literacy)
    )


  # Creating financial service usage variables -----------------------

  clean_NAs <- function(x) { ifelse(is.na(x), 99, x) }

  data <- data %>%
    mutate_at(vars(starts_with("C1_")), clean_NAs) %>%
    mutate_at(vars(starts_with("E2B")), clean_NAs)

  # Most important lender
  mimp_lender <- c(`1` = "Bank: Personal & other",
                   `2` = "Sacco/MFI",
                   `3` = "Bank: Mobile",
                   `4` = "Fintech: App",
                   `5` = "Mobile money operator: Fuliza",
                   `6` = "Chama",
                   `7` = "Social network",
                   `8` = "Shopkeeper",
                   `9` = "Employer or buyer",
                   `10` = "Government",
                   `11` = "Other")

  mimp_reasons = c(
    `1` =  "Convenience/ Easy to get loan (including less paperwork/documentation)",
    `2` = "Affordable fees/ Low repayments",
    `3` =  "Easy to use and make repayments",
    `4` = "Features are suited to my needs (including need for privacy)",
    `5` = "Trust",
    `6` = "Only option/ Had no choice",
    `7` = "Building credit history/ Keeping option open for future borrowing",
    `8` = "Other reason"
  )

  data <- data %>%
    mutate(

      # Account ownership/ informal device usage
      fin_account_mm = ifelse(C1_3 == 1 | C1_9 == 1, 1, 0),

      fin_account_bank = ifelse(C1_28 == 1  | C1_32 == 1 |
                                  C1_11 == 1 | C1_33 == 1 | C1_31 == 1 |
                                  C1_29 == 1 | C1_27 == 1 | C1_30 == 1 |
                                  C1_12 == 1 | C1_2 == 1 | C1_10 == 1, 1, 0),

      fin_account_tbank =  ifelse(C1_28 == 1  | C1_32 == 1 |
                                    C1_11 == 1 | C1_33 == 1 | C1_31 == 1 |
                                    C1_29 == 1 | C1_27 == 1 | C1_30 == 1, 1, 0),

      fin_account_nbfi = ifelse(C1_14 == 1 | C1_15 == 1 | C1_1 == 1 | C1_4 == 1, 1, 0),

      fin_account_tbanknbfi = ifelse(fin_account_tbank == 1 | fin_account_nbfi == 1, 1, 0),

      fin_account_any = ifelse(fin_account_mm == 1 | fin_account_bank == 1 | fin_account_nbfi == 1, 1, 0),

      fin_chama_user = ifelse(C1_17 == 1 | C1_5 == 1, 1, 0),

      fin_socialnetwork_user = ifelse(C1_16 == 1 | C1_19 == 1 | C1_20  == 1 | C1_21 == 1 | C1_22 == 1 | C1_24 == 1 | C1_6 == 1 | C1_7 == 1, 1, 0),
      fin_socialnetwork_user_ff = ifelse( C1_6 == 1 | C1_7 == 1 | C1_20 == 1, 1, 0),

      fin_cash_user = ifelse(C1_8 == 1, 1, 0),

      fin_digital_user = ifelse(fin_account_mm == 1 | C1_2 == 1 | C1_10 == 1 | C1_12 == 1 | C1_13 == 1 | C1_23 == 1, 1, 0),

      fin_digital_mm = ifelse(C1_9 == 1, 1, 0),
      fin_digital_mb = ifelse(C1_10 == 1, 1, 0),
      fin_digital_app = ifelse(C1_23 == 1, 1, 0),

      fin_account_nobank = ifelse(C1_10 %in% c(2,3) & C1_28 %in% c(2,3) & C1_29 %in% c(2,3)  & C1_30 %in% c(2,3) & C1_31 %in% c(2,3) & C1_32 %in% c(2,3) & C1_33 %in% c(2,3) & C1_34 %in% c(2,3), 1, 0),

      # Savings:

      fst_savings_tbank = ifelse(C1_28 == 1 | C1_29 == 1 | C1_30 == 1 | C1_31 == 1, 1, 0),
      fst_savings_nbfi = ifelse(C1_1 == 1 | C1_4 == 1, 1, 0),
      fst_savings_mm = ifelse(C1_3 == 1, 1, 0),
      fst_savings_mbank = ifelse(C1_2 == 1, 1, 0),
      fst_savings_cash = ifelse(C1_8 == 1, 1, 0),
      fst_savings_network = ifelse(C1_7 == 1 | C1_6 == 1, 1, 0),
      fst_savings_sg = ifelse(C1_5 == 1, 1, 0),
      fst_savings_social = ifelse(fst_savings_sg == 1 | fst_savings_network == 1, 1, 0),
      fst_savings_any = ifelse(fst_savings_tbank == 1 | fst_savings_nbfi == 1 | fst_savings_mm == 1 | fst_savings_mbank == 1 | fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
      fst_savings_informal_any = ifelse(fst_savings_cash == 1 | fst_savings_network == 1 | fst_savings_sg == 1, 1, 0),
      fst_savings_formaloth_any = ifelse(fst_savings_tbank == 1 | fst_savings_mbank == 1 | fst_savings_nbfi == 1, 1, 0),

      #####################################
      # Credit market indicators
      #####################################

      # Identifies borrowers who have taken loans from any source (cash and in-kind)
      borrower_any_status =
        ifelse(
          C1_11 == 1 | # Personal loan/business loan from a bank /microfinance bank
            C1_12 == 1 | # Loan from mobile banking (e.g., Mshwari, KCB MPesa, MCoop cash, Eazzy Loan, Timiza, HF Whizz, Stawi loan, M-fanisi)
            C1_13 == 1 | # Loan from Fuliza
            C1_14 == 1 | # Loan at a Sacco / Savings and Credit Cooperative Society
            C1_15 == 1 | # Loan from a microfinance institution
            C1_16 == 1 | # Loan from Shylocks / Loan Sharks / Money Lenders / Money Merchants that are not from your phone (e.g. Platinum, Ngao, etc.)
            C1_17 == 1 | # Loan from a group/
            C1_18 == 1 | # Loan from a government institution for education, agriculture or a development loan (e.g. HELB, Agricultural Finance Corporation, Youth Fund, Women Fund)
            C1_19 == 1 | # Loan from an employer (ASK only if employed/Casual)
            C1_20 == 1 | # Loan from family/friend/neighbour
            C1_21 == 1 | # Cash loan from shopkeeper
            C1_22 == 1 | # Taking goods and services on credit from a shopkeeper
            C1_23 == 1 | # Digital loans that you get through the phone that you download through apps (e.g., Branch, Tala, Utunzi, Haraka loans, etc.)
            C1_24 == 1 | # Loan / credits from buyer of your harvest / supplier of agricultural inputs (e.g., coffee, tea, sugarcane, tobacco, vegetables)
            C1_25 == 1 | # Hire purchase
            C1_26 == 1 | # Loan from insurance
            C1_27 == 1 |  # Loan to buy / build a house (mortgage), or to buy land from a bank / building society or Sacco/Insurance/Government
            C1_32 == 1 | # Overdraft
            C1_34 == 1, #Credit card/ Kadi ya mkopo
          1, 0
        ),

      # Total demand for credit includes users and those who attempted to borrow from any source in the past year and were denied
      borrower_credit_demand =
        ifelse(
          borrower_any_status == 1 | E1 == 1, 1, 0
        ),

      # Currently borrowing from a bank or non-bank financial intermediary (SACCO, MFI, Insurance provider) (non-digital) / Includes loans for land/houses
      borrower_formalfi_status =
        ifelse(
          C1_11 == 1 |
            C1_14 == 1 |
            C1_15 == 1 |
            C1_26 == 1 |
            C1_27 == 1 |
            C1_32 == 1 |
            C1_34 == 1,
          1,
          0
        ),

      # Loan from government institution or hire purchase
      borrower_formaloth_status =
        ifelse(
          C1_18 == 1 |
            C1_25 == 1,
          1,
          0
        ),

      borrower_formalany_status =
        ifelse(borrower_formalfi_status == 1 | borrower_formaloth_status == 1, 1, 0),

      borrower_digital_status =
        ifelse(
          C1_12 == 1 | # Loan from mobile banking (e.g., Mshwari, KCB MPesa, MCoop cash, Eazzy Loan, Timiza, HF Whizz, Stawi loan, M-fanisi)
            C1_13 == 1 | # Loan from Fuliza
            C1_23 == 1, # Loan from app
          1,
          0
        ),

      # Currently borrowing from social network, private moneylender, chama, shopkeeper, employer or buyer
      borrower_informal_status =
        ifelse(
          C1_16 == 1 | # Private moneylender
          C1_17 == 1 | # Loan from a group/
            C1_19 == 1 | # Loan from an employer (ASK only if employed/Casual)
            C1_20 == 1 | # Loan from family/friend/neighbour
            C1_21 == 1 | # Cash loan from shopkeeper
            C1_22 == 1 | # Taking goods and services on credit from a shopkeeper
            C1_24 == 1,  # Buyer of harvest
          1,
          0 # Taking goods and services on credit from a shopkeeper
        ),

      # Mutually exclusive groups defined by most formal type of loan in use:
      # Digital only
      borrower_me_g1 = ifelse(borrower_digital_status == 1 & borrower_formalany_status == 0 & borrower_informal_status == 0, 1, 0),
      # Formal non-digital only
      borrower_me_g2 = ifelse(borrower_digital_status == 0 & borrower_formalany_status == 1 & borrower_informal_status == 0, 1, 0),
      # Informal only
      borrower_me_g3 = ifelse(borrower_digital_status == 0 & borrower_formalany_status == 0 & borrower_informal_status == 1, 1, 0),
      # Digital + other formal only
      borrower_me_g4 = ifelse(borrower_digital_status == 1 & borrower_formalany_status == 1 & borrower_informal_status == 0, 1, 0),
      # Digital + informal only
      borrower_me_g5 = ifelse(borrower_digital_status == 1 & borrower_formalany_status == 0 & borrower_informal_status == 1, 1, 0),
      # Other formal + informal
      borrower_me_g6 = ifelse(borrower_digital_status == 0 & borrower_formalany_status == 1 & borrower_informal_status == 1, 1, 0),
      # All 3
      borrower_me_g7 = ifelse(borrower_digital_status == 1 & borrower_formalany_status == 1 & borrower_informal_status == 1, 1, 0),

      # Simplified strands based on most formal out of formal (non-digital), digital and informal,
      borrower_me_formal = ifelse(borrower_formalany_status == 1, 1, 0),
      borrower_me_digital = ifelse(borrower_formalany_status == 0 & borrower_digital_status == 1, 1, 0),
      borrower_me_informal = ifelse(borrower_formalany_status == 0 & borrower_digital_status == 0 & borrower_informal_status == 1, 1, 0),

      #######
      # Lender (Agg 0):

      lender_agg0_1 = ifelse(borrower_formalany_status == 1, 1, 0),
      lender_agg0_2 = ifelse(borrower_digital_status == 1, 1, 0),
      lender_agg0_3 = ifelse(borrower_informal_status == 1, 1, 0),

      # Lender (Agg 1):

      lender_agg1_1 = ifelse(C1_11 == 1 | C1_32 == 1 | C1_34 == 1, 1, 0), # Bank: Personal & other",
      lender_agg1_2 = ifelse(C1_14 == 1 | C1_15 == 1 | C1_26 == 1, 1, 0), # "Other financial institution: Sacco/MFI/Insurance",
      lender_agg1_3 = ifelse(C1_12 == 1 | C1_23 == 1 | C1_13 == 1, 1, 0), # "Digital",
      lender_agg1_4 = ifelse(C1_17 == 1, 1, 0), # "Chama",
      lender_agg1_5 = ifelse(C1_20 == 1, 1, 0), # "Social network",
      lender_agg1_6 = ifelse(C1_21 == 1 | C1_22 == 1, 1, 0), # "Shopkeeper",
      lender_agg1_7 = ifelse(C1_18 == 1, 1, 0), # Government
      lender_agg1_8 = ifelse(C1_19 == 1 | C1_24 == 1 | C1_25 == 1 | C1_16 == 1 | C1_27 == 1, 1, 0), # "Other",

      # Lender (Agg 2):

      lender_agg2_1 = ifelse(C1_11 == 1 | C1_32 == 1 | C1_34 == 1, 1, 0), # Bank: Personal & other",
      lender_agg2_2 = ifelse(C1_14 == 1 | C1_15 == 1 | C1_26 == 1, 1, 0), # "Other financial institution: Sacco/MFI/Insurance",
      lender_agg2_3 = ifelse(C1_12 == 1, 1, 0), # "Bank: Mobile",
      lender_agg2_4 = ifelse(C1_23 == 1 | C1_13 == 1, 1, 0), # "Non-bank: Digital",
      lender_agg2_5 = ifelse(C1_17 == 1, 1, 0), # "Chama",
      lender_agg2_6 = ifelse(C1_20 == 1, 1, 0), # "Social network",
      lender_agg2_7 = ifelse(C1_21 == 1 | C1_22 == 1, 1, 0), # "Shopkeeper",
      lender_agg2_8 = ifelse(C1_18 == 1, 1, 0), # Government
      lender_agg2_9 = ifelse(C1_19 == 1 | C1_24 == 1, 1, 0), # "Employer or buyer",
      lender_agg2_10 = ifelse(C1_25 == 1 | C1_16 == 1 | C1_27 == 1, 1, 0), # "Other",

      # Lender (Agg 3):

      lender_agg3_1 = ifelse(C1_11 == 1, 1, 0), # Bank: Personal",
      lender_agg3_2 =  ifelse(C1_32 == 1 | C1_34 == 1, 1, 0), # "Bank: Overdraft or credit card"
      lender_agg3_3 = ifelse(C1_14 == 1 | C1_15 == 1 | C1_26 == 1, 1, 0), # "Other financial institution: Sacco/MFI/Insurance",
      lender_agg3_4 = ifelse(C1_12 == 1, 1, 0), # "Bank: Mobile",
      lender_agg3_5 = ifelse(C1_23 == 1, 1, 0), # "Fintech: App",
      lender_agg3_6 = ifelse(C1_13 == 1, 1, 0), # "Mobile money operator: Fuliza",
      lender_agg3_7 = ifelse(C1_17 == 1, 1, 0), # "Chama",
      lender_agg3_8 = ifelse(C1_20 == 1, 1, 0), # "Social network",
      lender_agg3_9 = ifelse(C1_21 == 1 | C1_22 == 1, 1, 0), # "Shopkeeper",
      lender_agg3_10 = ifelse(C1_18 == 1, 1, 0), # Government
      lender_agg3_11 = ifelse(C1_19 == 1 | C1_24 == 1, 1, 0), # "Employer or buyer",
      lender_agg3_12 = ifelse(C1_25 == 1, 1, 0), # "Hire purchase",
      lender_agg3_13 = ifelse(C1_16 == 1, 1, 0), # "Private moneylender",
      lender_agg3_14 = ifelse(C1_27 == 1, 1, 0),  # Other: loan for land or home

      # Debt & repayment difficulty --------------------

      debt_total = rowSums(.[grepl("E1x[a-zA-Z0-9]", names(.))], na.rm = T),
      debt_any = ifelse(debt_total > 0, 1, 0),
      debt_inc = debt_total/(ifelse(resp_income_w_pred == 0, 1, resp_income_w_pred)),
      debt_high = ifelse(debt_inc > 0.5, 1, 0),

      debt_monthly_repayment = E2A,
      debt_monthly_repayment = ifelse(is.na(E2A), 0, debt_monthly_repayment),
      debt_monthly_repayment = ifelse(E2A %in% c(98,99), NA, debt_monthly_repayment),
      debt_monthly_repayment_high = ifelse(debt_monthly_repayment/resp_income > 0.5, 1, 0),
      debt_monthly_repayment_cd = ifelse(borrower_any_status == 1, debt_monthly_repayment, NA),

      debt_repaystress = ifelse(E2B1 == 1 | E2B3 == 1 | E2B4 == 1, 1, 0),
      debt_repaystress = ifelse(is.na(debt_repaystress), 0, debt_repaystress),
      debt_delinquent = ifelse(E2Ci__1 == 1 | E2Ci__2 == 1 | E2Ci__3 == 1 | E2Ci__4 == 1, 1, 0),
      debt_delinquent = ifelse(is.na(debt_delinquent), 0, debt_delinquent),
      debt_default = ifelse(E2Ci__1 == 1, 1, 0),
      debt_default = ifelse(is.na(debt_default), 0, debt_default),
      debt_stressany = ifelse(debt_repaystress == 1 | debt_default == 1, 1, 0),

      # Lender from which loan was paid late, missed payment, paid less or never paid ---------------------

      # Lender: Aggregation Tier-0:

      # Any loan
      debt_default_lender_agg0_any = debt_delinquent,
      debt_default_lender_agg0_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg0_any),

      # 1) Any formal
      debt_default_lender_agg0_1 = ifelse(E2D__1 == 1 | E2D__17 == 1 | E2D__18 == 1 | E2D__3 == 1 | E2D__4 == 1 | E2D__5 == 1 | E2D__8 == 1 | E2D__15 == 1, 1, 0),
      debt_default_lender_agg0_1 = ifelse(lender_agg0_1 == 0, NA, debt_default_lender_agg0_1),

      # 2) Digital
      debt_default_lender_agg0_2 = ifelse(E2D__2 == 1 | E2D__13 == 1 | E2D__19 ==1, 1, 0),
      debt_default_lender_agg0_2 = ifelse(lender_agg0_2 == 0, NA, debt_default_lender_agg0_2),

      # 3) Any informal
      debt_default_lender_agg0_3 = ifelse(E2D__7 == 1 | E2D__10 == 1 | E2D__11 == 1 | E2__12 == 1 | E2D__9 == 1 | E2D__14 == 1 | E2D__6 == 1, 1, 0),
      debt_default_lender_agg0_3 = ifelse(lender_agg0_3 == 0, NA, debt_default_lender_agg0_3),

      # Lender: Aggregation Tier-1:

      # Any loan
      debt_default_lender_agg1_any = debt_delinquent,
      debt_default_lender_agg1_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg1_any),

      # 1) Bank: personal or other
      debt_default_lender_agg1_1 = ifelse(E2D__1 == 1 | E2D__17 == 1 | E2D__18 == 1, 1, 0),
      debt_default_lender_agg1_1 = ifelse(lender_agg1_1 == 0, NA, debt_default_lender_agg1_1),

      # 2) Other formal institution: SACCO/ MFI/ Insurance
      debt_default_lender_agg1_2 = ifelse(E2D__3 == 1 | E2D__4 == 1 | E2D__5 == 1, 1, 0),
      debt_default_lender_agg1_2 = ifelse(lender_agg1_2 == 0, NA, debt_default_lender_agg1_2),

      # 3) Digital
      debt_default_lender_agg1_3 = ifelse(E2D__2 == 1 | E2D__13 == 1 | E2D__19 ==1, 1, 0),
      debt_default_lender_agg1_3 = ifelse(lender_agg1_3 == 0, NA, debt_default_lender_agg1_3),

      # 4) Chama
      debt_default_lender_agg1_5 = ifelse(E2D__7 == 1, 1, 0),
      debt_default_lender_agg1_5 = ifelse(lender_agg1_4 == 0, NA, debt_default_lender_agg1_5),

      # 5) Social network
      debt_default_lender_agg1_6 = ifelse(E2D__10 == 1, 1, 0),
      debt_default_lender_agg1_6 = ifelse(lender_agg1_5 == 0, NA, debt_default_lender_agg1_6),

      # 6) Shopkeeper
      debt_default_lender_agg1_7 = ifelse(E2D__11 == 1 | E2__12 == 1, 1, 0),
      debt_default_lender_agg1_7 = ifelse(lender_agg1_6 == 0, NA, debt_default_lender_agg1_7),

      # 7) Government
      debt_default_lender_agg1_8 = ifelse(E2D__8 == 1, 1, 0),
      debt_default_lender_agg1_8 = ifelse(lender_agg1_7 == 0, NA, debt_default_lender_agg1_8),

      # 8) Other
      debt_default_lender_agg1_10= ifelse(E2D__9 == 1 | E2D__14 == 1 | E2D__15 == 1 | E2D__16 == 1 | E2D__6 == 1 , 1, 0),
      debt_default_lender_agg1_10 = ifelse(lender_agg1_8 == 0, NA, debt_default_lender_agg1_10),

      # Lender: Aggregation Tier-2:

      # Any loan
      debt_default_lender_agg2_any = debt_delinquent,
      debt_default_lender_agg2_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg2_any),

      # 1) Bank: personal or other
      debt_default_lender_agg2_1 = ifelse(E2D__1 == 1 | E2D__17 == 1 | E2D__18 == 1, 1, 0),
      debt_default_lender_agg2_1 = ifelse(lender_agg2_1 == 0, NA, debt_default_lender_agg2_1),

      # 2) Other formal institution: SACCO/ MFI/ Insurance
      debt_default_lender_agg2_2 = ifelse(E2D__3 == 1 | E2D__4 == 1 | E2D__5 == 1, 1, 0),
      debt_default_lender_agg2_2 = ifelse(lender_agg2_2 == 0, NA, debt_default_lender_agg2_2),

      # 3) Bank: mobile
      debt_default_lender_agg2_3 = ifelse(E2D__2 == 1, 1, 0),
      debt_default_lender_agg2_3 = ifelse(lender_agg2_3 == 0, NA, debt_default_lender_agg2_3),

      # 4) Non-bank digital: App, Fuliza
      debt_default_lender_agg2_4 = ifelse(E2D__13 == 1 | E2D__19 ==1, 1, 0),
      debt_default_lender_agg2_4 = ifelse(lender_agg2_4 == 0, NA, debt_default_lender_agg2_4),

      # 5) Chama
      debt_default_lender_agg2_5 = ifelse(E2D__7 == 1, 1, 0),
      debt_default_lender_agg2_5 = ifelse(lender_agg2_5 == 0, NA, debt_default_lender_agg2_5),

      # 6) Social network
      debt_default_lender_agg2_6 = ifelse(E2D__10 == 1, 1, 0),
      debt_default_lender_agg2_6 = ifelse(lender_agg2_6 == 0, NA, debt_default_lender_agg2_6),

      # 7) Shopkeeper
      debt_default_lender_agg2_7 = ifelse(E2D__11 == 1 | E2__12 == 1, 1, 0),
      debt_default_lender_agg2_7 = ifelse(lender_agg2_7 == 0, NA, debt_default_lender_agg2_7),

      # 8) Government
      debt_default_lender_agg2_8 = ifelse(E2D__8 == 1, 1, 0),
      debt_default_lender_agg2_8 = ifelse(lender_agg2_8 == 0, NA, debt_default_lender_agg2_8),

      # 9) Employer or buyer
      debt_default_lender_agg2_9 = ifelse(E2D__9 == 1 | E2D__14 == 1, 1, 0),
      debt_default_lender_agg2_9 = ifelse(lender_agg2_9 == 0, NA, debt_default_lender_agg2_9),

      # 10) Other
      debt_default_lender_agg2_10= ifelse(E2D__15 == 1 | E2D__16 == 1 | E2D__6 == 1, 1, 0),
      debt_default_lender_agg2_10 = ifelse(lender_agg2_10 == 0, NA, debt_default_lender_agg2_10),

      # Lender: Aggregation Tier-3:

      # Any loan
      debt_default_lender_agg3_any = debt_delinquent,
      debt_default_lender_agg3_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg3_any),

      # 1) Bank: personal or business
      debt_default_lender_agg3_1 = ifelse(E2D__1 == 1, 1, 0),
      debt_default_lender_agg3_1 = ifelse(lender_agg3_1 == 0, NA, debt_default_lender_agg3_1),

      # 2) Bank: other
      debt_default_lender_agg3_2 = ifelse(E2D__17 == 1 | E2D__18 == 1, 1, 0),
      debt_default_lender_agg3_2 = ifelse(lender_agg3_2 == 0, NA, debt_default_lender_agg3_2),

      # 3) Other formal institution: SACCO/ MFI/ Insurance
      debt_default_lender_agg3_3 = ifelse(E2D__3 == 1 | E2D__4 == 1 | E2D__5 == 1, 1, 0),
      debt_default_lender_agg3_3 = ifelse(lender_agg3_3 == 0, NA, debt_default_lender_agg3_3),

      # 4) Bank: mobile
      debt_default_lender_agg3_4 = ifelse(E2D__2 == 1, 1, 0),
      debt_default_lender_agg3_4 = ifelse(lender_agg3_4 == 0, NA, debt_default_lender_agg3_4),

      # 5) Non-bank digital: Fintech: App,
      debt_default_lender_agg3_5 = ifelse(E2D__13 == 1, 1, 0),
      debt_default_lender_agg3_5 = ifelse(lender_agg3_5 == 0, NA, debt_default_lender_agg3_5),

      # 6) Mobile money operator: Fuliza
      debt_default_lender_agg3_6 = ifelse(E2D__19 ==1, 1, 0),
      debt_default_lender_agg3_6 = ifelse(lender_agg3_6 == 0, NA, debt_default_lender_agg3_6),

      # 7) Chama
      debt_default_lender_agg3_7 = ifelse(E2D__7 == 1, 1, 0),
      debt_default_lender_agg3_7 = ifelse(lender_agg3_7 == 0, NA, debt_default_lender_agg3_7),

      # 8) Social network
      debt_default_lender_agg3_8 = ifelse(E2D__10 == 1, 1, 0),
      debt_default_lender_agg3_8 = ifelse(lender_agg3_8 == 0, NA, debt_default_lender_agg3_8),

      # 9) Shopkeeper
      debt_default_lender_agg3_9 = ifelse(E2D__11 == 1 | E2__12 == 1, 1, 0),
      debt_default_lender_agg3_9 = ifelse(lender_agg3_9 == 0, NA, debt_default_lender_agg3_9),

      # 10) Government
      debt_default_lender_agg3_10 = ifelse(E2D__8 == 1, 1, 0),
      debt_default_lender_agg3_10 = ifelse(lender_agg3_10 == 0, NA, debt_default_lender_agg3_10),

      # 11) Employer or buyer
      debt_default_lender_agg3_11 = ifelse(E2D__9 == 1 | E2D__14 == 1, 1, 0),
      debt_default_lender_agg3_11 = ifelse(lender_agg3_11 == 0, NA, debt_default_lender_agg3_11),

      # 12) Hire purchase
      debt_default_lender_agg3_12 = ifelse(E2D__15 == 1, 1, 0),
      debt_default_lender_agg3_12 = ifelse(lender_agg3_12 == 0, NA, debt_default_lender_agg3_12),

      # 13) Private moneylender
      debt_default_lender_agg3_13 = ifelse(E2D__6 == 1, 1, 0),
      debt_default_lender_agg3_13 = ifelse(lender_agg3_13 == 0, NA, debt_default_lender_agg3_13),

      # Main reason for delinquent payment:

      debt_default_reason = case_when(
        E2E %in% c(1, 13) ~ 1, # Did not plan well enough, Borrowed too much initially
        E2E %in% c(2) ~ 2, # Interest rates / repayment rates went up
        E2E %in% c(3,9) ~ 3, # Did not understand terms, payment more than expected
        E2E %in% c(4, 7, 8) ~ 4, # Poor business performance, lost income source
        E2E %in% c(5, 6) ~ E2E,
        E2E %in% c(10) ~ 7, # Unexpected emergency
        E2E %in% c(11, 12, 14) ~ 8, # Other
        E2E %in% c(98, 99) ~ NA,
      ),

      #######

      fin_digital_borrower = ifelse((C1_12 == 1 | C1_23 == 1 | C2_12 == 1), 1, 0),
      fin_digital_borrower = ifelse(is.na(fin_digital_borrower), 0, fin_digital_borrower),
      fst_loans_fint = ifelse(C1_11 == 1 | C1_14 == 1 | C1_15 == 1, 1, 0),
      fst_loans_mbank = ifelse(C1_12 == 1 | C1_23 == 1, 1, 0),
      fst_loans_fuliza = ifelse(C1_13 == 1, 1, 0),
      fst_loans_shopcredit = ifelse(C1_21  == 1 | C1_22 == 1 , 1, 0),
      fst_loans_sg = ifelse(C1_17 == 1, 1, 0),
      fst_loans_family = ifelse(C1_20 == 1, 1, 0),

      fst_loans_informal = ifelse(fst_loans_sg == 1 | fst_loans_family == 1 | fst_loans_shopcredit == 1, 1, 0),

      fst_loans_agg_social = ifelse(fst_loans_shopcredit == 1 | fst_loans_family == 1, 1, 0),
      fst_loans_agg_trad = ifelse(fst_loans_sg == 1 | fst_loans_fint == 1, 1, 0),
      fst_loans_agg_dig = ifelse(fst_loans_fuliza == 1 | fst_loans_mbank == 1, 1, 0),

      # Most important loan

      mimp_loan_agg3 = case_match(E1_ii1,
                                  #Traditional Bank loans including overdraft, credit card
                                  c(1, 19, 17) ~ 1,
                                  #Sacco/MFI/
                                  c(3, 4) ~ 2,
                                  #Mobile bank loans
                                  c(2) ~ 3,
                                  #Fintech: App
                                  c(12) ~ 4,
                                  #Mobile money operator: Fuliza
                                  c(18) ~ 5,
                                  #Chamas
                                  c(6) ~ 6,
                                  #Social network (friends/family/employer)
                                  c(9) ~ 7,
                                  #Shopkeeper loans/goods on credit
                                  c(10, 11) ~ 8,
                                  #Employer or buyer
                                  c(8, 13) ~ 9,
                                  #Government
                                  c(7, 16) ~ 10,
                                  #Other,
                                  c(14, 15, 5, 13) ~ 11
      ),
      mimp_loan_agg3_str = mimp_lender[mimp_loan_agg3],

      # Reason for choosing most important loan

      mimp_reason = case_when(
        E1_ii2 %in% c(1, 6) ~ 1,
        E1_ii2 == 2 ~ 2,
        E1_ii2 == 3 ~ 3,
        E1_ii2 %in% c(4,7) ~ 4,
        E1_ii2 == 5 ~ 5,
        E1_ii2 == 8 ~ 6,
        E1_ii2 %in% c(9, 10) ~ 7,
        E1_ii2 %in% c(11, 12, 13, 98) ~ 8
      ),
      mimp_reason_str = mimp_reasons[mimp_reason],

      fin_access_strand = case_when(
        access == 1 ~ "Formal_prudential",
        access %in% c(2, 3) ~ "Formal_other",
        access == 4 ~ "Informal",
        access == 5 ~ "Excluded"
      ),

    )  %>% dummy_cols(select_columns = c("fin_access_strand", "mimp_loan_agg3", "mimp_reason", "debt_default_reason"))

  # Creating analysis dataset -----------------------

  statvars <- c("hh_id", "mem_id", "fullsample", "psu", "probweights", "strata", "year")
  vars <- c(statvars, "County", "goals_", "hh_", "resp_", "know_", "shocks_", "_comp", "asset_", "rooms_", "dwelling_", "fin_", "debt_", "fst_", "mimp_", "borrower_", "lender_")
  data <- data %>% select(matches(paste(vars, collapse = "|")))

  return(data)

  }

prep_loans_data_2021 <- function(data, borrower_data) {
  # Preparing loan-level dataset for analysis

  # Statistical variables and borrower characteristics from borrower-level dataset
  statvars <- borrower_data %>% dplyr::select(year, year_fct, hh_id, mem_id, psu, probweights)
  byvars <- borrower_data %>% dplyr::select(hh_id, mem_id, fullsample, hh_geo_fct, hh_geo2_fct, resp_income_w_pred, resp_inc_group_fct, resp_gender_fct, hh_wlth_group, resp_live_group_fct, resp_live_group2_fct)

  data <- data %>%
    mutate(
      hh_id = paste(ClusterNo, HHNo, sep = "_"),
      mem_id = paste(hh_id, A14vi, sep = "_")
    )

  # Checking for duplicate househols in data
  dups_hh <- data %>%
    dplyr::group_by(hh_id) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)

  # Checking for duplicate household members in data
  dups_mem <- data %>%
    dplyr::group_by(mem_id) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)

  # Removing duplicate household members in data
  data <- data %>%
    group_by(mem_id) %>%
    filter(row_number() == 1) %>% ungroup()

  # Reshaping dataset
  loans <- data %>%
    dplyr::select(mem_id, starts_with("E1")) %>%
    dplyr::select(-E1, -E1_ii1, -E1_ii2, -E1_2, -E1_3) %>%
    rename_at(vars(starts_with("E1")), ~str_replace(., "E1", "")) %>%
    gather(key = "lender", value = "value", -mem_id) %>%
    separate(lender, into = c("quest", "lender"), sep = -1) %>%
    # Removing other credit card
    filter(lender != "i") %>%
    mutate(quest = ifelse(quest == "viiA", "vii", quest),
           value = as.numeric(value))

  # Reasons for taking loan
  reasons_num <- attr(data$E1iiiA, "labels")
  reasons <- names(reasons_num)
  names(reasons) <- reasons_num
  reasons <- reasons[2:length(reasons)]

  # Creating loan-level indicators
  loans <- loans %>%
    pivot_wider(names_from = quest, values_from = value) %>%
    rename(
      nloans_pastyear = `i`,
      nloans_outstanding = `ii`,
      loan_reason_det = `iii`,
      loan_collateral = `iv`,
      loan_repaymentfreq = `v`,
      loan_repaymentchan = `vi`,
      loan_principal = `vii`,
      loan_timetaken = `viii`,
      loan_timeleft = `ix`,
      loan_balance = `x`
    )

  loans <- loans %>%
    filter_at(vars(contains("loan")), any_vars(!is.na(.)))

  loans <- loans %>%
    mutate(
      #nloans_pastyear = ifelse(nloans_outstanding >0 & nloans_pastyear == 0, nloans_outstanding, nloans_pastyear),
      x1 = ifelse(nloans_outstanding > 0 & nloans_pastyear == 0, 1, 0),
      x2 = ifelse(nloans_outstanding == 0 & nloans_pastyear == 0, 1, 0),
      loan_balance = ifelse(loan_principal > 0 & is.na(loan_balance), 0, loan_balance),

      # Cleaning up number of loans in past year variable
      #nloans_pastyear_mod = ifelse(!is.na(loan_principal) & is.na(nloans_pastyear), 1, nloans_pastyear),
      #nloans_pastyear_mod = ifelse(!is.na(loan_reason_det) & is.na(nloans_pastyear), 1, nloans_pastyear_mod),
      nloans_pastyear_mod = ifelse(!is.na(loan_principal) & nloans_pastyear == 0, 1, nloans_pastyear),
      #nloans_pastyear_mod = ifelse(!is.na(nloans_outstanding) & is.na(nloans_pastyear), nloans_outstanding, nloans_pastyear_mod),
      nloans_pastyear_mod = ifelse(!is.na(nloans_outstanding) & nloans_pastyear_mod == 0, nloans_outstanding, nloans_pastyear_mod),
      #nloans_pastyear_mod = ifelse(is.na(nloans_pastyear_mod), 1, nloans_pastyear_mod),

      # THere are loan types (primarily Fuliza) that are reported having been taken more than 100 times in the past year, setting a ceiling on these.
      # likely value being reported is a monetery value
      nloans_pastyear_mod = ifelse(nloans_pastyear_mod > 100, 100, nloans_pastyear_mod),

      loan_principal = ifelse(!is.na(loan_balance) & is.na(loan_principal), loan_balance, loan_principal),

      loan_principal_str = case_when(
        loan_principal < 2500 ~ "< 2,500",
        loan_principal >= 2500 & loan_principal < 10000 ~ "[2,500 - 10,000)",
        loan_principal >= 10000 & loan_principal < 50000 ~ "[10,000 - 50,000)",
        loan_principal >= 50000 & loan_principal < 100000 ~ "[50,000+]",
      ),

      loan_principal_cat = case_when(
        loan_principal < 2500 ~ 1,
        loan_principal >= 2500 & loan_principal < 10000 ~ 2,
        loan_principal >= 10000 & loan_principal < 50000 ~ 3,
        loan_principal >= 50000 ~ 4
      ),

      # This is an estimate and assumes previous loans were of the same value
      total_borrowed = nloans_pastyear_mod*loan_principal,
      loan_reason_det_str = reasons[as.character(loan_reason_det)]
    ) %>% dummy_cols(select_columns = c("loan_principal_cat"))

  # Re-categorizing lenders
  # Recategorizing lender

  lenders <- c("A" = "Bank: Personal",
               "B" = "Bank: Mobile",
               "C" = "SACCO",
               "D" = "MFI",
               "E" = "Moneylender",
               "F" = "Chama",
               "G" = "Government (for education, ag, development)",
               "H" = "Employer",
               "I" = "Family, friend or neighbor",
               "J" = "Shopkeeper: Cash loan",
               "K" = "Shopkeeper: Goods on credit",
               "L" = "App",
               "M" = "Buyer",
               "N" = "Hire purchase",
               "O" = "Insurance provider",
               "P" = "Governmet (for land)",
               "Q" = "Bank: Overdraft",
               "R" = "Mobile money (Fuliza)",
               "S" = "Bank: Credit card")

  lenders_agg0 <- c(`1` = "Formal",
                    `2` = "Digital",
                    `3` = "Informal")

  lenders_agg1 <- c(`1` = "Bank",
                   `2` = "Sacco/MFI",
                   `3` = "Digital",
                   `4` = "Chama",
                   `5` = "Social network",
                   `6` = "Shopkeeper",
                   `7` = "Government",
                   `8` = "Other")

  lenders_agg2 <- c(`1` = "Bank: Personal, business & other",
                    `2` = "Sacco/MFI",
                    `3` = "Bank: Mobile",
                    `4` = "Non-bank digital: App, Fuliza",
                    `5` = "Chama",
                    `6` = "Social network",
                    `7` = "Shopkeeper",
                    `8` = "Government",
                    `9` = "Employer or buyer",
                    `10` = "Other")

  lenders_agg3 <- c(`1` = "Bank: Personal or business",
                    `2` = "Bank: Overdraft or credit card",
                    `3` = "Sacco/MFI",
                    `4` = "Bank: Mobile",
                    `5` = "Fintech: App",
                    `6` = "Mobile money operator: Fuliza",
                    `7` = "Chama",
                    `8` = "Social network",
                    `9` = "Shopkeeper",
                    `10` = "Government",
                    `11` = "Employer or buyer",
                    `12` = "Hire purchase",
                    `13` = "Private moneylender")

  loans <- loans %>%
    mutate(
      lender_type = lenders[lender],
      lender_agg0 = recode(lender,
                           #Traditional Bank loans (incl overdraft, credit card)
                           `A` = 1,  `Q` = 1, `S` = 1,
                           #Sacco/MFI loans, including insurance
                           `C` = 1, `D` = 1, `O` = 1,
                           #Digital loans
                           `B` = 2, `L` = 2, `R` = 2,
                           #Chamas
                           `F` = 3,
                           #Social network. employer, buyer
                           `I` = 3,  `M` = 3, `H` = 3,
                           #Shopkeeper loans/goods on credit
                           `J` = 3, `K` = 3,
                           #Government
                           `G` = 1, `P` = 1,
                           #Other,
                           `E` = 3, `N` = 1),
      lender_agg0_str = lenders_agg0[lender_agg0],
      lender_agg1 = recode(lender,
                          #Traditional Bank loans (incl overdraft, credit card)
                          `A` = 1,  `Q` = 1, `S` = 1,
                          #Sacco/MFI loans, including insurance
                          `C` = 2, `D` = 2, `O` = 2,
                          #Digital loans
                          `B` = 3, `L` = 3, `R` = 3,
                          #Chamas
                          `F` = 4,
                          #Social network (friends/family/employer/buyer)
                          `I` = 5,  `M` = 5, `H` = 5,
                          #Shopkeeper loans/goods on credit
                          `J` = 6, `K` = 6,
                          #Government
                          `G` = 7, `P` = 7,
                          #Other,
                          `E` = 8, `N` = 8),
      lender_agg1_str = lenders_agg1[lender_agg1],
      lender_agg2 = recode(lender,
                           #Traditional Bank loans including overdraft and credit card
                           `A` = 1, `Q` = 1, `S` = 1,
                           #Sacco/MFI loans
                           `C` = 2, `D` = 2, `O` = 2,
                           #Mobile bank loans
                           `B` = 3,
                           #Other digital loans
                           `L` = 4, `R` = 4,
                           #Chamas
                           `F` = 5,
                           #Social network (friends/family/employer)
                           `I` = 6,
                           #Shopkeeper loans/goods on credit
                           `J` = 7, `K` = 7,
                           #Government
                           `G` = 8, `P` = 8,
                           #Employer or Buyer
                           `M` = 9, `H` = 9,
                           #Other,
                           `E` = 10, `N` = 10),
      lender_agg2_str = lenders_agg2[lender_agg2],
      lender_agg3 = recode(lender,
                           #Traditional Bank loans
                           `A` = 1,
                           #Other bank loans (overdraft, credit card)
                           `Q` = 2, `S` = 2,
                           #Sacco/MFI loans
                           `C` = 3, `D` = 3, `O` = 3,
                           #Mobile bank loans
                           `B` = 4,
                           #Other digital loans: App
                           `L` = 5,
                           #Other digital loans: Fuliza
                           `R` = 6,
                           #Chamas
                           `F` = 7,
                           #Social network (friends/family)
                           `I` = 8,
                           #Shopkeeper loans/goods on credit
                           `J` = 9, `K` = 9,
                           #Government
                           `G` = 10, `P` = 10,
                           #Employer or Buyer
                           `M` = 11, `H` = 11,
                           # Hire purchase,
                           `N` = 12,
                           # Private moneylender
                           `E` = 13),
      lender_agg3_str = lenders_agg3[lender_agg3],
    ) %>% dummy_cols(select_columns = c("lender_agg0", "lender_agg1", "lender_agg2", "lender_agg3"))

  # Reasons for taking loans
  reasons_agg <- c(1, 2, 3, 4, 5, 6, 7)
  names(reasons_agg) <- c("Basic consumption", "Education", "Business/farm investment", "Infrequent/large purchase", "Emergency", "Debt repayment", "Other")

  loans <- loans %>% mutate(
    loan_reason_agg = case_match(as.numeric(loan_reason_det),
                                 #Basic personal consumption
                                 3 ~ 1, 13 ~ 1,
                                 # For education
                                 2 ~ 2,
                                 #For current production (Farm/ business investment)
                                 4 ~ 3,
                                 5 ~ 3,
                                 6 ~ 3,
                                 7 ~ 3,
                                 9 ~ 3,
                                 18 ~ 3,
                                 19 ~ 3,
                                 20 ~ 3,
                                 21 ~ 3,
                                 22 ~ 3,
                                 23 ~ 3,
                                 #For paying a larger/infrequent expense
                                 8 ~ 4,
                                 10 ~ 4,
                                 11 ~ 4,
                                 12 ~ 4,
                                 14 ~ 4,
                                 15 ~ 4,
                                 16 ~ 4,
                                 17 ~ 4,
                                 26 ~ 4,
                                 #For emergencies
                                 1 ~ 5,
                                 #To pay off debts
                                 25 ~ 6,
                                 24 ~ 6,
                                 #OTher
                                 27 ~ 7,
                                 28 ~ 7
    ),
    loan_reason_agg = ifelse(loan_reason_agg %in% c(999999999, 98, 99), NA, loan_reason_agg),
    loan_reason_agg_str = reasons_agg[loan_reason_agg]
  ) %>% dummy_cols(select_columns = c("loan_reason_agg", "loan_reason_det")) %>%
    mutate(
      loan_reason_det_23 = 0
    )

  loans <- loans %>%
    left_join(statvars, by = c("mem_id")) %>%
    left_join(byvars, by = c("hh_id", "mem_id"))

    return(loans)
}


