

prep_borrower_data_2019 <- function(data) {

  data %>% mutate(

    # IDs, PSU and Sampling weights
    hh_id = paste(a1, a8, a8_1, a8_2, sep = "_"),
    mem_id = paste(hh_id, a_line, sep = "_"),
    psu = a8_1,
    probweights = Pop_Wt_r,
    fullsample = "All adults"

  ) %>%

  # Removing duplicate observations
  group_by(mem_id) %>%
    filter(row_number() == 1) %>%
    ungroup() -> data

  # Gaols

  goals_levels = c(
      "food" = "Putting food on the table",
      "edu" = "Educating myself or my family",
      "live"= "Developing my livelihood/career",
      "other" = "Other"
    )

  data <- data %>%

    mutate(

      goals_group = case_when(
        b1_1 == 1 ~ "food",
        b1_1 == 2 ~ "educ",
        b1_1 %in% c(3) ~ "live",
        b1_1 %in% c(4, 5, 6, 7) ~ "other"
      ),
      goals_group_fct = factor(goals_levels[goals_group], goals_levels, ordered = TRUE),

  ) %>% dummy_cols(select_columns = c('goals_group')) %>%

  # Shocks
    mutate(
      # Has respondent experience any of the following adverse shocks in past year:
      shocks_illness = case_when(s3a_1 == 1 ~ 1, s3a_1 == 2 ~ 0, s3a_1 == 98 ~ NA_real_),
      shocks_climate = case_when(s3a_2 == 1 ~ 1, s3a_2 == 2 ~ 0, s3a_2 == 98 ~ NA_real_),
      shocks_deathinc = case_when(s3a_3 == 1 ~ 1, s3a_3 == 2 ~ 0, s3a_3 == 98 ~ NA_real_),
      shocks_deathoth = case_when(s3a_4 == 1 ~ 1, s3a_4 == 2 ~ 0, s3a_4 == 98 ~ NA_real_),
      shocks_asset = case_when(s3a_5 == 1 ~ 1, s3a_5 == 2 ~ 0, s3a_5 == 98 ~ NA_real_),
      shocks_other = case_when(s3a_7 == 1 ~ 1, s3a_7 == 2 ~ 0, s3a_7 == 98 ~ NA_real_),
      shocks_N = shocks_illness + shocks_climate + shocks_deathinc + shocks_deathoth + shocks_asset + shocks_other,
      shocks_health = ifelse(shocks_illness == 1 | shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
      shocks_health = ifelse(is.na(shocks_illness) & is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_health),
      shocks_deathany = ifelse(shocks_deathinc == 1 | shocks_deathoth == 1, 1, 0),
      shocks_deathany = ifelse(is.na(shocks_deathinc) & is.na(shocks_deathoth), NA, shocks_deathany),
      shocks_any = ifelse(shocks_illness == 1 | shocks_climate == 1 | shocks_deathinc == 1 | shocks_deathoth == 1 | shocks_asset == 1 | shocks_other == 1, 1, 0),
      shocks_any = ifelse(is.na(shocks_illness) & is.na(shocks_climate) & is.na(shocks_deathinc) & is.na(shocks_deathoth) & is.na(shocks_asset) & is.na(shocks_other), NA, shocks_any)
    )

  # Creating socio-economic, demographic variables -----------------------

  # Family structure, age, gender, household residence

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

  data <- data %>%
    mutate(
      hh_urbrur = ifelse(cluster_type == 1, "Rural", "Urban"),
      hh_geo = case_when(
        hh_urbrur == "Rural" & (a1 %in% ASAL_counties_FA) ~ "rur_asal",
        a1 == 47 ~ "urb_nbo",
        hh_urbrur == "Urban" ~ "urb_oth",
        hh_urbrur == "Rural" ~ "rur_oth"
      ),
      hh_geo_2 = case_when(
        a1 == 47 ~ "urb_nbo",
        hh_urbrur == "Urban" ~ "urb_oth",
        hh_urbrur == "Rural" ~ "rur"
      ),
      hh_geo_3 = case_when(
        hh_urbrur == "Rural" & (a1 %in% ASAL_counties_FA) ~ "rur_asal",
        hh_urbrur == "Urban" ~ "urb",
        hh_urbrur == "Rural" ~ "rur_oth"
      ),
      hh_geo_fct = factor(geo_levels[hh_geo], levels = geo_levels, ordered = TRUE),
      hh_geo2_fct = factor(geo2_levels[hh_geo_2], levels = geo2_levels, ordered = TRUE),
      hh_geo3_fct = factor(geo2_levels[hh_geo_2], levels = geo3_levels, ordered = TRUE),
      hh_size_all = a10,
      hh_size_all_c = hh_size_all - mean(hh_size_all),
      hh_size_children = a11,
      hh_size_adults = a10 - a11,
      hh_singlemember = ifelse(a10 == 1, 1, 0),
      hh_multimember = ifelse(a10 > 1, 1, 0),
      hh_children = ifelse(a11 > 0, 1, 0),
      hh_head_female = ifelse(HeadofHousehold_sex == 2, 1, 0),
      hh_head_sex_fct = ifelse(HeadofHousehold_sex == 2, "Female", "Male"),
      hh_head_age = Headofhousehold_Age,
      hh_urbrur = case_when(
        hh_urbrur == "Urban" ~ "urb",
        hh_urbrur == "Rural" ~ "rur"
      )
    ) %>% dummy_cols(select_columns = c("hh_urbrur", "hh_geo", "hh_geo_3"))

  # Household assets

  data <- data %>%
    mutate(

      # mobile phone
      asset_mobile_any = ifelse(u1 == 1 | u1 == 2, 1, 0),
      asset_mobile_smart = ifelse(asset_mobile_any == 1 & u2_e == 1, 1, 0),
      asset_mobile_basic = ifelse(asset_mobile_any == 1 & asset_mobile_smart == 0, 1, 0),

      # habitable rooms per person
      rooms_per_person = y13/a10,
      rooms_per_person = ifelse(is.na(rooms_per_person), mean(rooms_per_person, na.rm = TRUE), rooms_per_person),

      dwelling_type = y4,

      toilet_comp = case_when(
        y11 %in% c(1) ~ 3,
        y11 %in% c(2, 3, 4) ~ 2,
        y11 %in% c(5, 6, 7, 99) ~ 1
      ),
      asset_flushtoilet = ifelse(toilet_comp == 3, 1, 0),

      # Piped into home or plot/yard or bottled
      water_comp = case_when(
        y10 %in% c(1, 2, 11, 16) ~ 3,
        y10 %in% c(3, 4, 5, 6, 7, 10) ~ 2,
        y10 %in% c(8, 9, 12, 13, 14, 15, 17, 18, 99) ~ 1
      ),
      asset_pipedwater = ifelse(water_comp == 3, 1, 0),

      # Finished wall (brick, cement, stone/lime)
      wall_comp = case_when(
        y7 %in% c(1) ~ 3,
        y7 %in% c(2) ~ 2,
        y7 %in% c(3) ~ 1
      ),
      asset_brickwall = ifelse(wall_comp == 3, 1, 0),

      # Elecricity main source of lighting:
      lighting_comp = case_when(
        y9 %in% c(5, 6) ~ 3,
        y9 %in% c(4, 7, 8, 9, 10, 11, 12, 13) ~ 2,
        y9 %in% c(1, 2, 3, 14, 15, 99) ~ 1
      ),
      asset_electricity =  ifelse(lighting_comp == 3, 1, 0),

      # Electricity, gas or solar cooking fuel

      cookingfuel_comp = case_when(
        y8 %in% c(5, 6) ~ 3,
        y8 %in% c(4, 7, 9, 10, 11, 13) ~ 2,
        y8 %in% c(1, 2, 3, 8, 99) ~ 1
      ),
      asset_elcgascookfuel = ifelse(cookingfuel_comp == 3, 1, 0)

    ) %>% dummy_cols(select_columns = c("toilet_comp", "water_comp", "lighting_comp", "wall_comp", "cookingfuel_comp", "dwelling_type"))

  # Computing household wealth index

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
      fin_status_impr = ifelse(b1f == 1, 1, 0),
      fin_status_worse = ifelse(b1f == 3, 1, 0),

      fin_status_fct = ifelse(fin_status_impr == 1, "Financial status: Improved", "Financial status: Worsened"),
      fin_status_fct = ifelse(is.na(fin_status_impr) & is.na(fin_status_worse), NA, fin_status_fct),

      resp_gender_group = ifelse(gender == 1, "men", "wmn"),
      resp_gender_fct = factor(gender_levels[resp_gender_group], gender_levels, ordered = TRUE),

      resp_age_yrs = a13,
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
        a17 == 1 ~ "nmr",
        a17 %in% c(2,3)  ~ "div",
        a17 == 4  ~ "mar"
      ),
      resp_marstat_fct = factor(marstat_levels[resp_marstat], levels = marstat_levels, ordered = TRUE),
      resp_edu_group = case_when(
        a16 == 1 ~ "none",
        a16 == 2 ~ "smpr",
        a16 == 3 ~ "cmpr",
        a16 == 4 ~ "smsc",
        a16 == 5 ~ "cmsc",
        a16 %in% c(6, 7, 8, 9) ~ "smtr"
      ),
      resp_edu_group_fct = factor(edu_levels[resp_edu_group], levels = edu_levels, ordered = TRUE),
      resp_edu_group2 = case_when(
        a16 == 1 ~ "none",
        a16 %in% c(2, 3) ~ "pri",
        a16 %in% c(4, 5) ~ "sec",
        a16 %in% c(6, 7, 8, 9) ~ "trt",
      ),
      resp_edu_group2_fct = factor(edu_levels2[resp_edu_group2], levels = edu_levels2, ordered = TRUE),
      resp_live_group = case_when(
        b3b == 1 ~ "farm",
        b3b == 2 ~ "empl",
        b3b == 3 ~ "cwrk",
        b3b == 4 ~ "owbs",
        b3b %in% c(5, 8, 9) ~ "trns",
        b3b %in% c(6, 7, 10) ~ "othr"
      ),
      resp_live_group_fct = factor(live_levels[resp_live_group], levels = live_levels, ordered = TRUE),

      resp_live_group2 = case_when(
        b3b == 1 ~ "farm",
        b3b == 2 ~ "empl",
        b3b == 3 ~ "cwrk",
        b3b == 4 ~ "owbs",
        b3b %in% c(5, 8, 9) ~ "trns",
        b3b %in% c(6, 7) ~ "inv",
        b3b == 10 ~ "othr"
      ),
      resp_live_group2_fct = factor(live_levels2[resp_live_group2], levels = live_levels2, ordered = TRUE),

      # Incom from any source
      resp_live_agri = ifelse(b3a_1 == 1 | b3a_2 == 1 | b3a_3 == 1 | b3a_4 == 1 | b3a_5 == 1, 1, 0),
      resp_live_agri = ifelse(is.na(resp_live_agri), 0, resp_live_agri),
      resp_live_emp =  ifelse(b3a_1 == 2 | b3a_2 == 2 | b3a_3 == 2 | b3a_4 == 2 | b3a_5 == 2, 1, 0),
      resp_live_emp = ifelse(is.na(resp_live_emp), 0, resp_live_emp),
      resp_live_cwrk =  ifelse(b3a_1 == 3 | b3a_2 == 3 | b3a_3 == 3 | b3a_4 == 3 | b3a_5 == 3, 1, 0),
      resp_live_cwrk = ifelse(is.na(resp_live_cwrk), 0, resp_live_cwrk),
      resp_live_owbs =  ifelse(b3a_1 == 4 | b3a_2 == 4 | b3a_3 == 4 | b3a_4 == 4 | b3a_5 == 4, 1, 0),
      resp_live_owbs = ifelse(is.na(resp_live_owbs), 0, resp_live_owbs),
      resp_live_inv =  ifelse(b3a_1 %in% c(6,7) | b3a_2 %in% c(6,7) | b3a_3 %in% c(6,7) | b3a_4 %in% c(6,7) | b3a_5 %in% c(6,7), 1, 0),
      resp_live_inv = ifelse(is.na(resp_live_inv), 0, resp_live_inv),
      resp_live_trnsf =  ifelse(b3a_1 %in% c(5, 8, 9) | b3a_2 %in% c(5, 8, 9) | b3a_3 %in% c(5, 8, 9) | b3a_4 %in% c(5, 8, 9) | b3a_5 %in% c(5, 8, 9), 1, 0),
      resp_live_trnsf = ifelse(is.na(resp_live_trnsf), 0, resp_live_trnsf),
      resp_live_other = ifelse(b3a_1 == 10 | b3a_2 == 10 | b3a_3 == 10 | b3a_4 == 10 | b3a_5 == 10, 1, 0),
      resp_live_other = ifelse(is.na(resp_live_other), 0, resp_live_other),

      resp_live_trnsf_sn =  ifelse(b3a_1 %in% c(9) | b3a_2 %in% c(9) | b3a_3 %in% c(9) | b3a_4 %in% c(9) | b3a_5 %in% c(9), 1, 0),
      resp_live_trnsf_sn = ifelse(is.na(resp_live_trnsf_sn), 0, resp_live_trnsf_sn),

      resp_live_trnsf_in =  ifelse(b3a_1 %in% c(5, 8) | b3a_2 %in% c(5, 8) | b3a_3 %in% c(5, 8) | b3a_4 %in% c(5, 8) | b3a_5 %in% c(5, 8), 1, 0),
      resp_live_trnsf_in = ifelse(is.na(resp_live_trnsf_in), 0, resp_live_trnsf_in),

      resp_incsource_N = resp_live_agri + resp_live_emp + resp_live_cwrk + resp_live_owbs + resp_live_inv + resp_live_trnsf + resp_live_other,

      resp_live_agri_sec = ifelse(resp_live_agri == 1 & b3b != 1, 1, 0),
      resp_live_emp_sec = ifelse(resp_live_emp == 1 & b3b != 2, 1, 0),
      resp_live_cwrk_sec = ifelse(resp_live_cwrk == 1 & b3b != 3, 1, 0),
      resp_live_owbs_sec = ifelse(resp_live_owbs == 1 & b3b != 4, 1, 0),
      resp_live_trnsf_sec = ifelse(resp_live_trnsf == 1 & b3b %not_in% c(5, 8, 9), 1, 0),
      resp_live_inv_sec = ifelse(resp_live_inv == 1 & b3b %not_in% c(6, 7), 1, 0),
      resp_live_other_sec = ifelse(resp_live_other == 1 & b3b != 10, 1, 0),

      resp_live_agricon = ifelse(b3c == 1, 1, 0),

      resp_live_farm_any = ifelse(resp_live_agri == 1 | resp_live_agricon == 1, 1, 0),

      resp_live_informal_any = ifelse(resp_live_agri == 1 | resp_live_cwrk == 1 | resp_live_owbs == 1, 1, 0),
      resp_live_informal_main = ifelse(b3b %in% c(1,3,4), 1, 0),
      resp_live_informal_sec = ifelse(resp_live_agri_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1, 1, 0),

      resp_live_sec_any = ifelse(resp_live_agri_sec == 1 | resp_live_emp_sec == 1 | resp_live_cwrk_sec == 1 | resp_live_owbs_sec == 1 | resp_live_trnsf_sec == 1 | resp_live_other_sec == 1, 1, 0)
    )

    # Personal monthly income
   data <- data %>%

     mutate(

      x = case_when(
        b3h == 1 ~ 100/2,
        b3h == 2 ~ (1500 - 100)/2,
        b3h == 3 ~ (3000 - 1500)/2,
        b3h == 4 ~ (7500 - 3000)/2,
        b3h == 5 ~ (15000 - 7500)/2,
        b3h == 6 ~ (30000 - 15000)/2,
        b3h == 7 ~ (70000 - 30000)/2,
        b3h == 8 ~ (200000 - 70000)/2,
        b3h == 9 ~ (400000 - 200000)/2,
        b3h == 10 ~ (1000000 - 400000)/2,
        b3h == 11 ~ 1000000
      ),
      resp_income = ifelse(b3h1 %in% c(97, 98, 99), NA, b3h1),
      resp_income = ifelse(is.na(resp_income), x, resp_income),

      resp_incsource_N = resp_live_agri + resp_live_emp + resp_live_cwrk + resp_live_owbs + resp_live_inv + resp_live_trnsf + resp_live_other,

      resp_inc_group = case_when(
        resp_income < 2500 ~ "inc_1",
        resp_income < 5000 ~ "inc_2",
        resp_income < 10000 ~ "inc_3",
        resp_income < 15000 ~ "inc_4",
        resp_income >= 15000 ~ "inc_5"
      ),
      resp_inc_group_fct = factor(inc_levels[resp_inc_group], levels = inc_levels, ordered = TRUE),

      resp_income_decile = cut(resp_income,
                               breaks=quantile(resp_income, probs=seq(0,1, by=0.1), na.rm=TRUE),
                               labels=c("Porest 10%","Q2","Q3","Q4","Q5", "Q6", "Q7", "Q8", "Q9", "Richest 10%"),
                               include.lowest=TRUE),

      resp_income_quintile = cut(resp_income,
                                 breaks=quantile(resp_income, probs=seq(0,1, by=0.2), na.rm=TRUE),
                                 labels=c("Porest 20%","Q2","Q3","Q4","Richest 20%"),
                                 include.lowest=TRUE),

      resp_live_trnsf_sn_only = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 0, 1, 0),
      resp_live_trnsf_in_only = ifelse(resp_live_trnsf_sn == 0 & resp_live_trnsf_in == 1, 1, 0),
      resp_live_trnsf_both = ifelse(resp_live_trnsf_sn == 1 & resp_live_trnsf_in == 1, 1, 0),

      resp_ppi_pred_npl = probability_pov_npl,
      resp_ppi_decile_npl = cut(probability_pov_npl,
                                breaks=quantile(probability_pov_npl, probs=seq(0,1, by=0.1), na.rm=TRUE),
                                labels=c("Richest 10%","Q9","Q8","Q7","Q6", "Q5", "Q4", "Q3", "Q2", "Poorest 10%"),
                                include.lowest=TRUE),
      resp_ppi_group_npl = ifelse(resp_ppi_decile_npl %in% c("Q4", "Q3", "Q2", "Poorest 10%"), "Bottom 40%",
                                  ifelse(resp_ppi_decile_npl %in% c("Q8","Q7","Q6", "Q5"), "Middle 40%", "Top 20%")),

      # Has social support
      resp_socialfinsupport = ifelse(b1b_1 == 1, 1, 0),


    ) %>% dummy_cols(select_columns = c("resp_age_group2", "resp_gender_group", "resp_age_group", "resp_marstat", "resp_edu_group", "resp_edu_group2", "resp_live_group", "resp_live_group2", "resp_inc_group"))

   # Predicting missing personal monthly to impute remaining NA values -------

   model_data <- data %>%
     select(resp_income, hh_wlth_index, hh_geo_urb_oth, hh_geo_urb_nbo, hh_geo_rur_asal, resp_marstat_div, resp_marstat_nmr, resp_gender_group_men,
            resp_age_group_age_25_45, resp_age_group_age_45_65, resp_age_group_age_65, resp_edu_group2_pri, resp_edu_group2_sec, resp_edu_group2_trt,
            resp_live_group_empl, resp_live_group_cwrk, resp_live_group_owbs, resp_live_group_trns, resp_live_group_othr, resp_incsource_N) %>%
     filter(!is.na(resp_income))

   model <- lm(
     data = model_data,
     resp_income ~
       hh_geo_urb_nbo + hh_geo_urb_oth + hh_geo_rur_asal +
       hh_wlth_index +
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

     ) %>% ungroup() %>%  dummy_cols(select_columns = c("resp_inc_group"))

  # Behavioral variables
  # Financial numeracy
  # Digital numeracy

  data <- data %>%
    mutate(
      know_fin_numeracy = ifelse(b2g == 1, 1, 0),
      know_dig_literacy = ifelse(b2h == 1, 1, 0 )
    )

  # Borrowing and credit usage indicators

  data <- data %>%
    mutate(
      borrower_any_status = ifelse(c1a11 == 1 |      # Personal loan/business loan from a bank
                                     c1a12 == 1 |    # Loan from mobile banking
                                     c1a13 == 1 |    # Loan at a Saccco/Savings and Credit Cooperative organisation
                                     c1a14 == 1 |    # Loan from a microfinance institutions
                                     c1a15 == 1 |    # Loan from shylocks, money lender
                                     c1a16 == 1 |    # Loan from a group/chama
                                     c1a17 == 1 |    # Loan from a government institution for education, agriculture
                                     c1a18 == 1 |    # Loan from an employer
                                     c1a19 == 1 |    # Loan from fmaily/friend/neighbor
                                     c1a20 == 1 |    # Cash loan from a shopkeeper
                                     c1a21 == 1 |    # Taking goods and services on credit from a shopkeeper
                                     c1a22 == 1 |    # Digital loans that you get through the phone
                                     c1a23 == 1 |    # Loan/credits from buyer of your harvest/supplier
                                     c1a24 == 1 |    # Hire purchase
                                     c1a25 == 1 |   # Loan to buy/build a house
                                     c1a26 == 1 |    # Loan given by government
                                     c1a31 == 1 |    # Bank overdraft
                                     c1a33 == 1,     # Bank credit card
                                     1, 0),

      borrower_credit_demand = ifelse(borrower_any_status == 1 | e1 == 1, 1, 0),

      # Lender (Agg 0):

      lender_agg0_1 = ifelse(c1a11 == 1 | c1a13 == 1 | c1a14 == 1 | c1a17 == 1 | c1a24 == 1 | c1a25 == 1 | c1a26 == 1 | c1a31 == 1 | c1a33 == 1, 1, 0), # Any formal lender
      lender_agg0_2 = ifelse(c1a12 == 1 | c1a22 == 1, 1, 0), # Any digital type of loan
      lender_agg0_3 = ifelse(c1a15 == 1 | c1a16 == 1 | c1a18 == 1 |  c1a19 == 1 | c1a20 == 1 |  c1a21 == 1 | c1a23 == 1, 1, 0), # Any informal lender

      # Lender (Agg 1):

      lender_agg1_1 = ifelse(c1a11 == 1 | c1a31 == 1 | c1a33 == 1, 1, 0), # Bank: Personal & other",
      lender_agg1_2 = ifelse(c1a13 == 1 | c1a14 == 1, 1, 0), # "Other financial institution: Sacco/MFI/Insurance",
      lender_agg1_3 = ifelse(c1a12 == 1 | c1a22 == 1, 1, 0), # "Digital",
      lender_agg1_4 = ifelse(c1a16 == 1, 1, 0), # "Chama",
      lender_agg1_5 = ifelse(c1a16 == 1, 1, 0), # "Social network",
      lender_agg1_6 = ifelse(c1a20 == 1 | c1a21 == 1, 1, 0), # "Shopkeeper",
      lender_agg1_7 = ifelse(c1a17 == 1 | c1a26 == 1, 1, 0), # Government
      lender_agg1_8 = ifelse(c1a23 == 1 | c1a24 == 1 | c1a25 == 1 | c1a15 == 1 | c1a18 == 1, 1, 0), # "Other",

      # Lender (Agg 2):

      lender_agg2_1 = ifelse(c1a11 == 1 | c1a31 == 1 | c1a33 == 1, 1, 0), # Bank: Personal & other",
      lender_agg2_2 = ifelse(c1a13 == 1 | c1a14 == 1, 1, 0), # "Other financial institution: Sacco/MFI/Insurance",
      lender_agg2_3 = ifelse(c1a12 == 1, 1, 0), # "Bank: Mobile",
      lender_agg2_4 = ifelse(c1a22 == 1, 1, 0), # "Non-bank: Digital",
      lender_agg2_5 = ifelse(c1a16 == 1, 1, 0), # "Chama",
      lender_agg2_6 = ifelse(c1a16 == 1, 1, 0), # "Social network",
      lender_agg2_7 = ifelse(c1a20 == 1 | c1a21 == 1, 1, 0), # "Shopkeeper",
      lender_agg2_8 = ifelse(c1a17 == 1 | c1a26 == 1, 1, 0), # Government
      lender_agg2_9 = ifelse(c1a18 == 1 | c1a23 == 1, 1, 0), # "Employer or buyer",
      lender_agg2_10 = ifelse(c1a24 == 1 | c1a25 == 1 | c1a15 == 1, 1, 0), # "Other",

      # Lender (Agg 3):

      lender_agg3_1 = ifelse(c1a11 == 1, 1, 0), # Bank: Personal or business",
      lender_agg3_2 =  ifelse(c1a31 == 1 | c1a33 == 1, 1, 0), # "Bank: Overdraft or credit card"
      lender_agg3_3 = ifelse(c1a13 == 1 | c1a14 == 1, 1, 0), # "Other financial institution: Sacco/MFI/Insurance",
      lender_agg3_4 = ifelse(c1a12 == 1, 1, 0), # "Bank: Mobile",
      lender_agg3_5 = ifelse(c1a22 == 1, 1, 0), # "Fintech: App",
      lender_agg3_6 = 0, # "Mobile money operator",
      lender_agg3_7 = ifelse(c1a16 == 1, 1, 0), # "Chama",
      lender_agg3_8 = ifelse(c1a16 == 1, 1, 0), # "Social network",
      lender_agg3_9 = ifelse(c1a20 == 1 | c1a21 == 1, 1, 0), # "Shopkeeper",
      lender_agg3_10 = ifelse(c1a17 == 1 | c1a26 == 1, 1, 0), # Government
      lender_agg3_11 = ifelse(c1a18 == 1 | c1a23 == 1, 1, 0), # "Employer or buyer",
      lender_agg3_12 = ifelse(c1a24 == 1, 1, 0), # "Hire purchase",
      lender_agg3_13 = ifelse(c1a15 == 1, 1, 0), # "Private moneylender",
      lender_agg3_14 = ifelse(c1a25 == 1, 1, 0),  # Other: loan for land or home

      # Mutually exclusive groups defined by most formal type of loan in use:

      # Digital only
      borrower_me_g1 = ifelse(lender_agg0_2 == 1 & lender_agg0_1 == 0 & lender_agg0_3 == 0, 1, 0),
      # Formal non-digital only
      borrower_me_g2 = ifelse(lender_agg0_2 == 0 & lender_agg0_1 == 1 & lender_agg0_3 == 0, 1, 0),
      # Informal only
      borrower_me_g3 = ifelse(lender_agg0_2 == 0 & lender_agg0_1 == 0 & lender_agg0_3 == 1, 1, 0),
      # Digital + other formal only
      borrower_me_g4 = ifelse(lender_agg0_2 == 1 & lender_agg0_1 == 1 & lender_agg0_3 == 0, 1, 0),
      # Digital + informal only
      borrower_me_g5 = ifelse(lender_agg0_2 == 1 & lender_agg0_1 == 0 & lender_agg0_3 == 1, 1, 0),
      # Other formal + informal
      borrower_me_g6 = ifelse(lender_agg0_2 == 0 & lender_agg0_1 == 1 & lender_agg0_3 == 1, 1, 0),
      # All 3
      borrower_me_g7 = ifelse(lender_agg0_2 == 1 & lender_agg0_1 == 1 & lender_agg0_3 == 1, 1, 0),

      # Simplified strands based on most formal out of formal (non-digital), digital and informal,
      borrower_me_formal = ifelse(lender_agg0_1 == 1, 1, 0),
      borrower_me_digital = ifelse(lender_agg0_1 == 0 & lender_agg0_2 == 1, 1, 0),
      borrower_me_informal = ifelse(lender_agg0_1 == 0 & lender_agg0_2 == 0 & lender_agg0_3 == 1, 1, 0),

      # Debt & repayment difficulty --------------------

      debt_monthly_repayment = ifelse(e2a %in% c(98,99), NA, e2a),
      debt_monthly_repayment = ifelse(is.na(e2a), 0, debt_monthly_repayment),
      debt_monthly_repayment = ifelse(e2a %in% c(98,99), NA, debt_monthly_repayment),
      debt_monthly_repayment_high = ifelse(debt_monthly_repayment/resp_income > 0.5, 1, 0),
      debt_monthly_repayment_cd = ifelse(borrower_any_status == 1, debt_monthly_repayment, NA),

      debt_repaystress = ifelse(e2b1 == 1 | e2b3 == 1 | e2b4 == 1, 1, 0),
      debt_repaystress = ifelse(is.na(debt_repaystress), 0, debt_repaystress),
      debt_delinquent = 0, # This variable is not defined for 2019, setting to 0 to avoid error when summarizing across years
      debt_default = ifelse(e2c > 0, 1, 0),
      debt_default = ifelse(is.na(debt_default), 0, debt_default),
      debt_stressany = ifelse(debt_repaystress == 1 | debt_default == 1, 1, 0),

      # Lender from which loan was in default ---------------------

      default_resp_1 =
        recode(as.numeric(e2d_1),
               #Bank personal or business loan
               `11` = 1,
               # Bank overdraft or credit card
               `33` = 2, `31` = 2,
               #Sacc/MFI loan
               `13` = 3, `14` = 3,
               # Mobile banking
               `12` = 4,
               # Fintech: App
               `22` = 5,
               #Chama
               `16` = 7,
               #Social network
               `19` = 8, `18` = 8,
               #Shopkeeper loans/goods on credit
               `20` = 9, `21` =  9,
               #Government
               `17` = 10,
               # Buyer of harvest or employwer
               `23` = 11,  `18` = 11,
               # Moneylender
               `15` = 12,
               # Hire purchase
               `24` = 13,
               # Loan to build/buy a house
               `25` = 14, `26` = 14

        ),
      default_resp_2 =
        recode(as.numeric(e2d_2),
               #Bank personal or business loan
               `11` = 1,
               # Bank overdraft or credit card
               `33` = 2, `31` = 2,
               #Sacc/MFI loan
               `13` = 3, `14` = 3,
               # Mobile banking
               `12` = 4,
               # Fintech: App
               `22` = 5,
               #Chama
               `16` = 7,
               #Social network
               `19` = 8, `18` = 8,
               #Shopkeeper loans/goods on credit
               `20` = 9, `21` =  9,
               #Government
               `17` = 10,
               # Buyer of harvest or employwer
               `23` = 11,  `18` = 11,
               # Moneylender
               `15` = 12,
               # Hire purchase
               `24` = 13,
               # Loan to build/buy a house
               `25` = 14, `26` = 14
        ),
      default_resp_3 =
        recode(as.numeric(e2d_3),
               #Bank personal or business loan
               `11` = 1,
               # Bank overdraft or credit card
               `33` = 2, `31` = 2,
               #Sacco/MFI loan
               `13` = 3, `14` = 3,
               # Mobile banking
               `12` = 4,
               # Fintech: App
               `22` = 5,
               #Chama
               `16` = 7,
               #Social network
               `19` = 8, `18` = 8,
               #Shopkeeper loans/goods on credit
               `20` = 9, `21` =  9,
               #Government
               `17` = 10,
               # Buyer of harvest or employwer
               `23` = 11,  `18` = 11,
               # Hire purchase
               `24` = 12,
               # Moneylender
               `15` = 13,
               # Loan to build/buy a house
               `25` = 14, `26` = 14
        )) %>%

    mutate_at(vars(starts_with("default_resp_")), ~ ifelse(is.na(.), 0, .)) %>%

    mutate(

      # Lender: Aggregation Tier-3:
      debt_default_lender_agg3_any = debt_default,
      debt_default_lender_agg3_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg3_any),

      debt_default_lender_agg3_1 = ifelse(default_resp_1 == 1 | default_resp_2 == 1 | default_resp_3 == 1, 1, 0), # 1) Bank: personal or business
      debt_default_lender_agg3_2 = ifelse(default_resp_1 == 2 | default_resp_2 == 2 | default_resp_3 == 2, 1, 0), # 2) Bank: other
      debt_default_lender_agg3_3 = ifelse(default_resp_1 == 3 | default_resp_2 == 3 | default_resp_3 == 1, 1, 0), #  3) Other formal institution: SACCO/ MFI/ Insurance
      debt_default_lender_agg3_4 = ifelse(default_resp_1 == 4 | default_resp_2 == 4 | default_resp_3 == 3, 1, 0), # 4) Mobile banking
      debt_default_lender_agg3_5 = ifelse(default_resp_1 == 5 | default_resp_2 == 5 | default_resp_3 == 4, 1, 0), # 5) Fintech: App
      debt_default_lender_agg3_6 = 0, # 6) Mobile money operator
      debt_default_lender_agg3_7 = ifelse(default_resp_1 == 7 | default_resp_2 == 7 | default_resp_3 == 7, 1, 0), # 7) Chama
      debt_default_lender_agg3_8 = ifelse(default_resp_1 == 8 | default_resp_2 == 8 | default_resp_3 == 8, 1, 0), # 8) Social network
      debt_default_lender_agg3_9 = ifelse(default_resp_1 == 9 | default_resp_2 == 9 | default_resp_3 == 9, 1, 0), # 9) Shopkeeper
      debt_default_lender_agg3_10 = ifelse(default_resp_1 == 10 | default_resp_2 == 10 | default_resp_3 == 10, 1, 0), # 10) Government
      debt_default_lender_agg3_11 = ifelse(default_resp_1 == 11 | default_resp_2 == 11 | default_resp_3 == 11, 1, 0), # 11) Employer or buyer
      debt_default_lender_agg3_12 = ifelse(default_resp_1 == 12 | default_resp_2 == 12 | default_resp_3 == 12, 1, 0), # 12) Hire purchase
      debt_default_lender_agg3_13 = ifelse(default_resp_1 == 13 | default_resp_2 == 13 | default_resp_3 == 13, 1, 0), # 13) Moneylender
      debt_default_lender_agg3_14 = ifelse(default_resp_1 == 14 | default_resp_2 == 14 | default_resp_3 == 14, 1, 0), # 14) Other

      debt_default_lender_agg3_1 = ifelse(lender_agg3_1 == 0, NA, debt_default_lender_agg3_1),
      debt_default_lender_agg3_2 = ifelse(lender_agg3_2 == 0, NA, debt_default_lender_agg3_2),
      debt_default_lender_agg3_3 = ifelse(lender_agg3_3 == 0, NA, debt_default_lender_agg3_3),
      debt_default_lender_agg3_4 = ifelse(lender_agg3_4 == 0, NA, debt_default_lender_agg3_4),
      debt_default_lender_agg3_5 = ifelse(lender_agg3_5 == 0, NA, debt_default_lender_agg3_5),
      debt_default_lender_agg3_6 = ifelse(lender_agg3_6 == 0, NA, debt_default_lender_agg3_6),
      debt_default_lender_agg3_7 = ifelse(lender_agg3_7 == 0, NA, debt_default_lender_agg3_7),
      debt_default_lender_agg3_8 = ifelse(lender_agg3_8 == 0, NA, debt_default_lender_agg3_8),
      debt_default_lender_agg3_9 = ifelse(lender_agg3_9 == 0, NA, debt_default_lender_agg3_9),
      debt_default_lender_agg3_10 = ifelse(lender_agg3_10 == 0, NA, debt_default_lender_agg3_10),
      debt_default_lender_agg3_11 = ifelse(lender_agg3_11 == 0, NA, debt_default_lender_agg3_11),
      debt_default_lender_agg3_12 = ifelse(lender_agg3_12 == 0, NA, debt_default_lender_agg3_12),
      debt_default_lender_agg3_13 = ifelse(lender_agg3_13 == 0, NA, debt_default_lender_agg3_13),
      debt_default_lender_agg3_14 = ifelse(lender_agg3_14 == 0, NA, debt_default_lender_agg3_14),

      # Lender: Aggregation Tier-2:
      debt_default_lender_agg2_any = debt_default,
      debt_default_lender_agg2_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg2_any),

      debt_default_lender_agg2_1 = ifelse(debt_default_lender_agg3_1 == 1 | debt_default_lender_agg3_2 == 1, 1, 0 ), # 1) Bank: personal or other
      debt_default_lender_agg2_2 = ifelse(debt_default_lender_agg3_3 == 1, 1, 0), # 2) Other formal institution: SACCO/ MFI/ Insurance
      debt_default_lender_agg2_3 = ifelse(debt_default_lender_agg3_4 == 1, 1, 0),   # 3) Bank: mobile
      debt_default_lender_agg2_4 = ifelse(debt_default_lender_agg3_5 == 1, 1, 0),   # 4) Non-bank digital: App
      debt_default_lender_agg2_5 = ifelse(debt_default_lender_agg3_7 == 1, 1, 0),   # 5) Chama
      debt_default_lender_agg2_6 = ifelse(debt_default_lender_agg3_8 == 1, 1, 0),   # 6) Social network
      debt_default_lender_agg2_7 = ifelse(debt_default_lender_agg3_9 == 1, 1, 0),   # 7) Shopkeeper
      debt_default_lender_agg2_8 = ifelse(debt_default_lender_agg3_10 == 1, 1, 0),    # 8) Government
      debt_default_lender_agg2_9 = ifelse(debt_default_lender_agg3_11 == 1, 1, 0), # 9) Employer or buyer
      debt_default_lender_agg2_10 = ifelse(debt_default_lender_agg3_12 == 1 | debt_default_lender_agg3_13 == 1 | debt_default_lender_agg3_14 == 1, 1, 0), # 10 Other

      debt_default_lender_agg2_1 = ifelse(lender_agg2_1 == 0, NA, debt_default_lender_agg2_1),
      debt_default_lender_agg2_2 = ifelse(lender_agg2_2 == 0, NA, debt_default_lender_agg2_2),
      debt_default_lender_agg2_3 = ifelse(lender_agg2_3 == 0, NA, debt_default_lender_agg2_3),
      debt_default_lender_agg2_4 = ifelse(lender_agg2_4 == 0, NA, debt_default_lender_agg2_4),
      debt_default_lender_agg2_5 = ifelse(lender_agg2_5 == 0, NA, debt_default_lender_agg2_5),
      debt_default_lender_agg2_6 = ifelse(lender_agg2_6 == 0, NA, debt_default_lender_agg2_6),
      debt_default_lender_agg2_7 = ifelse(lender_agg2_7 == 0, NA, debt_default_lender_agg2_7),
      debt_default_lender_agg2_8 = ifelse(lender_agg2_8 == 0, NA, debt_default_lender_agg2_8),
      debt_default_lender_agg2_9 = ifelse(lender_agg2_9 == 0, NA, debt_default_lender_agg2_9),
      debt_default_lender_agg2_10 = ifelse(lender_agg2_10 == 0, NA, debt_default_lender_agg2_10),

      # Lender: Aggregation Tier-1:
      debt_default_lender_agg1_any = debt_default,
      debt_default_lender_agg1_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg1_any),

      debt_default_lender_agg1_1 = ifelse(debt_default_lender_agg2_1 == 1, 1, 0 ), # 1) "Bank",
      debt_default_lender_agg1_2 = ifelse(debt_default_lender_agg2_2 == 1, 1, 0), # 2) "Sacco/MFI"
      debt_default_lender_agg1_3 = ifelse(debt_default_lender_agg2_3 == 1 | debt_default_lender_agg2_4, 1, 0), # 3) "Digital"
      debt_default_lender_agg1_4 = ifelse(debt_default_lender_agg2_5 == 1, 1, 0), # 4) "Chama"
      debt_default_lender_agg1_5 = ifelse(debt_default_lender_agg2_6 == 1, 1, 0), # 5) "Social network"
      debt_default_lender_agg1_6 = ifelse(debt_default_lender_agg2_7 == 1, 1, 0), # 6) "Shopkeeper"
      debt_default_lender_agg1_7 = ifelse(debt_default_lender_agg2_8 == 1, 1, 0), # 7) "Government"
      debt_default_lender_agg1_8 = ifelse(debt_default_lender_agg2_9 == 1 | debt_default_lender_agg2_10 == 1 , 1, 0), # 9) "Other"

      debt_default_lender_agg1_1 = ifelse(lender_agg1_1 == 0, NA, debt_default_lender_agg1_1),
      debt_default_lender_agg1_2 = ifelse(lender_agg1_2 == 0, NA, debt_default_lender_agg1_2),
      debt_default_lender_agg1_3 = ifelse(lender_agg1_3 == 0, NA, debt_default_lender_agg1_3),
      debt_default_lender_agg1_4 = ifelse(lender_agg1_4 == 0, NA, debt_default_lender_agg1_4),
      debt_default_lender_agg1_5 = ifelse(lender_agg1_5 == 0, NA, debt_default_lender_agg1_5),
      debt_default_lender_agg1_6 = ifelse(lender_agg1_6 == 0, NA, debt_default_lender_agg1_6),
      debt_default_lender_agg1_7 = ifelse(lender_agg1_7 == 0, NA, debt_default_lender_agg1_7),
      debt_default_lender_agg1_8 = ifelse(lender_agg1_8 == 0, NA, debt_default_lender_agg1_8),

      # Lender: Aggregation Tier-0:
      debt_default_lender_agg0_any = debt_default,
      debt_default_lender_agg0_any = ifelse(borrower_any_status == 0, NA, debt_default_lender_agg0_any),

      # 1) Any formal
      debt_default_lender_agg0_1 = ifelse(debt_default_lender_agg3_1 == 1 | debt_default_lender_agg3_2 == 1 | debt_default_lender_agg3_3 == 1 | debt_default_lender_agg3_10 == 1 | debt_default_lender_agg3_12 == 1 | debt_default_lender_agg3_14 == 1, 1, 0),
      debt_default_lender_agg0_1 = ifelse(lender_agg0_1 == 0, NA, debt_default_lender_agg0_1),
      # 2) Digital
      debt_default_lender_agg0_2 = ifelse(debt_default_lender_agg3_4 == 1 | debt_default_lender_agg3_5 == 1, 1, 0),
      debt_default_lender_agg0_2 = ifelse(lender_agg0_2 == 0, NA, debt_default_lender_agg0_2),
      # 3) informal
      debt_default_lender_agg0_3 = ifelse(debt_default_lender_agg3_7 == 1 | debt_default_lender_agg3_8 == 1 | debt_default_lender_agg3_9 == 1 | debt_default_lender_agg3_11 == 1 | debt_default_lender_agg3_13 == 1, 1, 0),
      debt_default_lender_agg0_3 = ifelse(lender_agg0_3 == 0, NA, debt_default_lender_agg0_3),

      # Main reason for delinquent payment:

      debt_default_reason = case_when(
        e2e %in% c(1, 13) ~ 1, # Did not plan well enough, Borrowed too much initially
        e2e %in% c(2) ~ 2, # Interest rates / repayment rates went up
        e2e %in% c(3,9) ~ 3, # Did not understand terms, payment more than expected
        e2e %in% c(4, 7, 8) ~ 4, # Poor business performance, lost income source
        e2e %in% c(5, 6) ~ e2e,
        e2e %in% c(10) ~ 7, # Unexpected emergency
        e2e %in% c(11, 12, 14, 15) ~ 8, # Other
        e2e %in% c(98, 99) ~ NA,
      )

    ) %>% dummy_cols(select_columns = c("debt_default_reason"))


  # Creating analysis dataset -----------------------

  statvars <- c("year", "fullsample", "hh_id", "mem_id", "psu", "probweights", "strata")
  vars <- c(statvars, "goals_", "hh_", "resp_", "know_", "fin_", "shocks_", "_comp", "asset_", "rooms_", "borrower_", "lender_", "debt_")
  data <- data %>% select(matches(paste(vars, collapse = "|")))

  return(data)

}



prep_loans_data_2019 <- function(data, borrower_data) {

  # Analysis variables
  statvars <- borrower_data %>% dplyr::select(year, year_fct, hh_id, mem_id, psu, probweights)
  byvars <- borrower_data %>% dplyr::select(hh_id, mem_id, fullsample, hh_geo_fct, hh_geo2_fct, resp_income_w_pred, resp_inc_group_fct, resp_gender_fct, hh_wlth_group, resp_live_group2_fct, asset_mobile_smart)

  data <- data %>%
    mutate(
      hh_id = paste(a1, a8, a8_1, a8_2, sep = "_"),
      mem_id = paste(hh_id, a_line, sep = "_"),
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

  # Reshaping database to create loan-level database
  loans <- data %>%
    dplyr::select(mem_id, starts_with("e1")) %>%
    dplyr::select(-e1, -e1s, -e1t) %>%
    rename_at(vars(starts_with("e1")), funs(str_replace(., "e1", ""))) %>%
    gather(key = "lender", value = "value", -mem_id) %>%
    separate(lender, into = c("lender", "quest"), sep = 1)

  loans <- loans %>% spread(quest, value) %>%
    rename(
      nloans_pastyear = `1`,
      nloans_outstanding = `2`,
      loan_reason_det = `3`,
      loan_collateral = `4`,
      loan_repaymentfreq = `5`,
      loan_repaymentchan = `6`,
      loan_principal = `7`,
      loan_timetaken = `8`,
      loan_timeleft = `9`,
      loan_balance = `10`
    )

  loans <- loans %>%
    filter_at(vars(contains("loan")), any_vars(!is.na(.)))

  loans <- loans %>%
    mutate(
      #nloans_pastyear = ifelse(nloans_outstanding >0 & nloans_pastyear == 0, nloans_outstanding, nloans_pastyear),
      nloans_pastyear_mod = ifelse(!is.na(loan_principal) & nloans_pastyear == 0, 1, nloans_pastyear),
      nloans_pastyear_mod = ifelse(!is.na(nloans_outstanding) & nloans_pastyear_mod == 0, nloans_outstanding, nloans_pastyear_mod),

      x1 = ifelse(nloans_outstanding > 0 & nloans_pastyear == 0, 1, 0),
      x2 = ifelse(nloans_outstanding == 0 & nloans_pastyear == 0, 1, 0),

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
      total_borrowed = nloans_pastyear_mod*loan_principal

    ) %>%
    filter_at(vars(contains("loan")), any_vars(!is.na(.))) %>%
    dummy_cols(select_columns = c("loan_principal_cat"))

  # Lenders
  lenders <- c("a" = "Bank",
               "b" = "Mobile bank",
               "c" = "SACCO",
               "d" = "MFI",
               "e" = "Moneylender",
               "f" = "Chama",
               "g" = "Government",
               "h" = "Employer",
               "i" = "Social network",
               "j" = "Shopkeeper",
               "k" = "Goods on credit from shopkeeper",
               "l" = "Mobile app",
               "m" = "Buyer of harverst",
               "n" = "Hire purchase",
               "o" = "Housing or land loan (from Bank or Sacco)",
               "p" = "Housing or land loan (from Government)",
               "q" = "Overdraft",
               "r" = "Credit card"
  )

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

  # Recategorizing lender

  loans <- loans %>% mutate(
    lender_type = lenders[lender],
    lender_agg0 = recode(lender,
                         ## FORMAL (TRADITIONAL)
                         #Bank loans
                         `a` = 1, `q` = 1, `r` = 1,
                         #Sacco/MFI loans
                         `c` = 1, `d` = 1,
                         # Hire purchase, loan for housing or land
                         `n` = 1, `o` = 1, `p` = 1,
                         #Government
                         `g` = 1,

                         # DIGITAL
                         `b` = 2,  `l` = 2,

                         # INFORMAL
                         #Chamas
                         `f` = 3,
                         #Social network (friends/family)
                         `i` = 3,
                         #Shopkeeper loans/goods on credit
                         `j` = 3, `k` = 3,
                         #Other,
                         `m` = 3, `h` = 3, `e` = 3
    ),
    lender_agg0_str = lenders_agg0[lender_agg0],
    lender_agg1 = recode(lender,
                         #Bank loans
                         `a` = 1, `q` = 1, `r` = 1,
                         #Sacco/MFI loans
                         `c` = 2, `d` = 2,
                         # Digital
                         `b` = 3,  `l` = 3,
                         #Chamas
                         `f` = 4,
                         #Social network (friends/family) & employer or buyer
                         `i` = 5, `m` = 5, `h` = 5,
                         #Shopkeeper loans/goods on credit
                         `j` = 6, `k` = 6,
                         #Government
                         `g` = 7,
                         #Other,
                         `e` = 8,`n` = 8, `o` = 8, `p` = 8,
    ),
    lender_agg1_str = lenders_agg1[lender_agg1],
    lender_agg2 = recode(lender,
                         #Bank loans
                         `a` = 1, `q` = 1, `r` = 1,
                         #Sacco/MFI loans
                         `c` = 2, `d` = 2,
                         #Bank mobile
                         `b` = 3,
                         # Apps
                         `l` = 4,
                          #Chamas
                         `f` = 5,
                         #Social network (friends/family)
                         `i` = 6,
                         #Shopkeeper loans/goods on credit
                         `j` = 7, `k` = 7,
                         #Government
                         `g` = 8,
                         #Buyer of harvest or employer,
                         `m` = 9, `h` = 9,
                         #Other,
                         `e` = 10,`n` = 10, `o` = 10, `p` = 10,
    ),
    lender_agg2_str = lenders_agg2[lender_agg2],
    lender_agg3 = recode(lender,
                        #Bank loans
                        `a` = 1,
                        # Bank loans: overdraft or credit card
                        `q` = 2, `r` = 2,
                        #Sacco/MFI loans
                        `c` = 3, `d` = 3,
                        #Bank loans
                        `b` = 4,
                        # Apps
                        `l` = 5,
                        #Chamas
                        `f` = 7,
                        #Social network (friends/family)
                        `i` = 8,
                        #Shopkeeper loans/goods on credit
                        `j` = 9, `k` = 9,
                        #Government
                        `g` = 10,
                        #Buyer of harvest or employer,
                        `m` = 11, `h` = 11,
                        #Moneylender,
                        `e` = 12,
                        # Hire purchase,
                        `n` = 13,
                        # Other (housing or land, any source)
                        `o` = 14, `p` = 14,
    ),
    lender_agg3_str = lenders_agg3[lender_agg3],
  ) %>% dummy_cols(select_columns = c("lender_agg1", "lender_agg2", "lender_agg3"))

  # Reasons for taking loans: detailed
  reasons_num <- attr(data$e1a3, "labels")
  reasons <- names(reasons_num)
  names(reasons) <- reasons_num

  # Reasons for taking loans: Aggregate
  reasons_agg <- c(1, 2, 3, 4, 5, 6, 7)
  names(reasons_agg) <- c("Basic consumption", "Education", "Business/farm investment", "Infrequent/large purchase", "Emergency", "Debt repayment", "Other")

  loans <- loans %>% mutate(
    loan_reason_agg = recode(loan_reason_det,
                             #Basic personal consumption
                             `3` = 1, `13` = 1,
                             # For education
                             `2` = 2,
                             #For current production (Farm/ business investment)
                             `4` = 3, `5` = 3, `6` = 3, `7` = 3, `9` = 3,
                             `18` = 3, `19` = 3, `20` = 3, `21` = 3, `22` = 3, `23` = 3,
                             #For paying a larger/infrequent expense
                             `8` = 4, `10` = 4, `11` = 4, `12` = 4, `14` = 4, `15` = 4, `16` = 4, `17` = 4, `26` = 4,
                             #For emergencies
                             `1` = 5,
                             #To pay off debts
                             `25` = 6, `24` = 6,
                             #OTher
                             `27` = 7, `28` = 7, `29`= 7
    ),
    loan_reason_det_str = reasons[loan_reason_det],
    loan_reason_agg = ifelse(loan_reason_agg %in% c(98, 99), NA, loan_reason_agg),
    loan_reason_agg_str = reasons_agg[loan_reason_agg],
    drop = ifelse(loan_principal < 100 & lender_agg1_str == "Digital", 1, 0)
  ) %>% dummy_cols(select_columns = c("loan_reason_agg", "loan_reason_det"))

  loans <- loans %>%
    left_join(statvars, by = c("mem_id")) %>%
    left_join(byvars, by = c("hh_id", "mem_id"))

  return(loans)

  }


