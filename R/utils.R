
##################
# Custom functions
##################

options(survey.lonely.psu="certainty")

get_attr <- function(x, name) {
  attributes(x)[[name]]
}

# Custom chart theme
theme_custom <- function(scale_f = 1, ...) {
  theme_gray() +
    theme(
      plot.margin = margin(10, 5, 5, 5, "pt"),
      plot.subtitle = element_text(size = 12*scale_f, face = 'bold'),
      plot.title = element_text(size = 11.5*scale_f, hjust = 0, face = 'bold', color = '#0496FF'),
      axis.text.x = element_text(hjust = 0.5),
      axis.text.y = element_text(hjust = 1),
      axis.title.y = element_text(color="black", size = 10*scale_f, angle = 90, vjust = 1, hjust = 1),
      axis.title.x = element_text(color="black", hjust = 0.5, size=10*scale_f),
      axis.text = element_text(size=10*scale_f, color = "black"),
      #panel.grid = element_line(color = "#F5E3E0", size = 0.25),
      plot.caption=element_text(hjust=0, color="grey40", size=10.5*scale_f),
      panel.background = element_rect(fill = "#ffffff"),
      rect = element_rect(fill = "#ffffff", colour = "#ffffff", size = 0.5, linetype = 1),
      axis.line.x = element_line(color = "black", size=0.5),
      axis.line.y = element_line(color = "black", size=0.5),
      strip.background = element_rect(fill = "#ffffff"),
      strip.text = element_text(size = 10*scale_f, face = 'plain'),
      strip.text.y = element_text(angle = 90),
      legend.title = element_text(size = 10*scale_f),
      legend.text = element_text(size = 10*scale_f),
      legend.key.height = unit(1, 'lines'),
      legend.key.width = unit(1, 'lines'),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key = element_rect(colour = "transparent", fill = "white"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      ...
    )
}

# Helper functions
`%not_in%` <- negate(`%in%`)

pctclean <- function(x, n = 1) {
  if (max(x, na.rm = TRUE) <= 1) {
      round(x*100,n)
  } else {
    round(x, n)
  }
}

hyphen <- function(x) {
  paste(x, collapse = "-")
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

invert <- function(x) {
  x <- ifelse(x == 1, 0, 1)
}

#Defining inverse logit function for logistic regression predictions
invlogit <- function(x) { 1/(1 + exp(-x)) }

# Dichotomaize
dich <- function(x, toyes, tona) {
  ifelse(x %in% toyes, 1,
         ifelse(x %in% tona, NA, 0))
  }

scale_values <- function(x){

  min = min(x, na.rm = TRUE)
  max = max(x, na.rm = TRUE)

  (x-min)/(max-min)

}

scale_values_2 <- function(x, min, max){

  (x-min)/(max-min)

}

# Summary stats:

sumby <- function(df, indicator, ...) {
  i <- enquo(indicator)
  group_by <- quos(...)
  x <- group_by[[1]]
  c <- group_by[[2]]

  df <- df %>%
    group_by(!!!group_by) %>%
    summarise(
      n = n(),
      m = mean(!!i),
      sd = sd(!!i),
      uci = m + 1.96*(sd/sqrt(n)),
      lci = m - 1.96*(sd/sqrt(n))
    )

  names(df)[names(df) == "m"] <- as_string(get_expr(i))

  return(df)

}

sumby_svy <- function(data, yvar, xvar, cvar, psu, strata, wvar, reorder) {

  y <- enquo(yvar)
  x <- enquo(xvar)
  c <- enquo(cvar)
  psu <- enquo(psu)
  str <- enquo(strata)
  w <- enquo(wvar)

  #cat("Processing indicator: ", as_string(yvar), "for group: ", as_string(xvar) & as_string(cvar), "\n")

  # Defining survey design
  svydesign <- data %>%
    as_survey_design(ids = !!psu, strata = !!str, weight = !!w, variables = c(!!y, !!x, !!c), nest = TRUE)

  res <- svydesign %>%
    group_by(!!x, !!c) %>%
    summarize(
      n = unweighted(n()),
      N = survey_total(na.rm=TRUE),
      m = survey_mean(!!y, vartype = c("se", "ci"), na.rm=TRUE)
    ) %>%
    rename(lci = m_low, uci = m_upp, se = m_se)

  names(res)[names(res) == "m"] <- as_string(get_expr(y))

  if (reorder == TRUE) {
    xname <- quo_name(x)
    res <- res %>% mutate(!!xname := fct_reorder2(factor(!!x), !!c, !!y))
  }

  return(res)

}

mean_svy <- function(data, yvar, psu, strata, wvar) {

  y <- enquo(yvar)
  psu <- enquo(psu)
  str <- enquo(strata)
  w <- enquo(wvar)

  #cat("Processing indicator: ", as_string(yvar), "for group: ", as_string(xvar) & as_string(cvar), "\n")

  # Defining survey design
  svydesign <- data %>%
    as_survey_design(ids = !!psu, strata = !!str, weight = !!w, variables = c(!!y), nest = TRUE)

  res <- svydesign %>%
    mutate(x = 1) %>%
    summarize(
      n = unweighted(n()),
      N = survey_total(x),
      m = survey_mean(!!y, vartype = c("se", "ci"), na.rm=TRUE)
    ) %>%
    rename(lci = m_low, uci = m_upp, se = m_se)

  names(res)[names(res) == "m"] <- as_string(get_expr(y))

  return(res)

}

quintile_svy <- function(data, yvar, quantiles = c(0.2, 0.4, 0.6, 0.8), psu, strata, wvar) {

  y <- enquo(yvar)
  psu <- enquo(psu)
  str <- enquo(strata)
  w <- enquo(wvar)

  #cat("Processing indicator: ", as_string(yvar), "for group: ", as_string(xvar) & as_string(cvar), "\n")

  # Defining survey design
  svydesign <- data %>%
    as_survey_design(ids = !!psu, strata = !!str, weight = !!w, variables = c(!!y), nest = TRUE)

  svyquantile(y, svydesign, quantiles)

}

sumby1_svy <- function(data, yvar, xvar, psu, strata, wvar, reorder = FALSE, mergedata = NULL) {

  y <- enquo(yvar)
  x <- enquo(xvar)
  psu <- enquo(psu)
  str <- enquo(strata)
  w <- enquo(wvar)

  #cat("Processing indicator: ", as_string(yvar), "for group: ", as_string(xvar) & as_string(cvar), "\n")

  # Defining survey design
  svydesign <- data %>%
    as_survey_design(ids = !!psu, strata = !!str, weight = !!w, variables = c(!!y, !!x), nest = TRUE)

  res <- svydesign %>%
    group_by(!!x) %>%
    summarize(
      n = unweighted(n()),
      N = survey_total(na.rm=TRUE),
      m = survey_mean(!!y, vartype = c("se", "ci"), na.rm=TRUE)
    ) %>%
    rename(lci = m_low, uci = m_upp, se = m_se)

  names(res)[names(res) == "m"] <- as_string(get_expr(y))

  if (length(mergedata) > 0) {
    res <- res %>% inner_join(mergedata)
  }

  if (reorder == TRUE) {
    xname <- quo_name(x)
    res <- res %>% mutate(!!xname := fct_reorder(factor(!!x), !!y))
  }
  return(res)

}

# Version of quantile function that returns closest actual value in dataset

quantile_closest <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, na.rm=TRUE)
  q[2] <- min(x[ x >= q[2] ])
  q[3] <- min(x[ x >= q[3] ])
  q[4] <- min(x[ x >= q[4] ])
  return(q)
}

tercile_closest <- function(x) {
  x <- x[!is.na(x)]
  q <- quantile(x, na.rm=TRUE)
  t <- rep(NA,3)
  t[1] <- q[1]
  t[2] <- min(x[ x >= q[3] ])
  t[3] <- q[5]
  return(t)
}

# Version of quantile function that returns closest actual value in dataset

minmax <- function(x) {
  x <- x[!is.na(x)]
  c(min(x), max(x))
}

# Computes summary statistics from survey datasets ------------------------
svy_summary <- function(data, y, g, psu, w, strata, yref, gref) {

  y <- sym(y)
  g <- sym(g)
  w <- sym(w)
  psu <- sym(psu)
  str <- sym(strata)

  # cat("Processing indicator: ", y, "for group: ", g, "\n")

  # Defining survey design
  svydesign <- data %>%
    as_survey_design(ids = !!psu, strata = !!str, weight = !!w, variables = c(!!y, !!g), nest = TRUE)

  # Computing mean and 95% confidence interval, adding a name for the indicator, and group namees for group variable

  if (as_string(g) %in% c("fullsample")) {
    svydesign %>%
      summarize(
        total = sum(!!y, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!y, vartype = "ci", na.rm=TRUE)
      ) %>%
      rename(lci = mean_low, uci = mean_upp) %>%
      mutate(
        indicator = as_string(y),
        indicator_name = yref[as_string(y)],
        group = as_string(g),
        group_name = gref[as_string(g)],
        group_cat_val = gref[as_string(g)],
        count = NA
      ) %>%
      dplyr::select(
        indicator, indicator_name, group, group_name, group_cat_val, count, mean, lci, uci
      )
  }
  else {
    # Computing mean and 95% confidence interval, adding a name for the indicator, and group namees for group variable
    svydesign %>%
      group_by(!!g) %>%
      summarize(
        count = survey_total(na.rm=TRUE),
        mean = survey_mean(!!y, vartype = "ci", na.rm=TRUE)
      ) %>%
      rename(lci = mean_low, uci = mean_upp) %>%
      mutate(
        indicator = as_string(y),
        indicator_name = yref[as_string(y)],
        group = as_string(g),
        group_name = gref[as_string(g)],
        group_cat_val = !!g
      ) %>%
      dplyr::select(
        indicator, indicator_name, group, group_name, group_cat_val, count, mean, lci, uci
      )
  }

}

svy_summary_weights <- function(data, i, g, psu, strata, w, iref, gref) {

  i <- sym(i)
  g <- sym(g)
  w <- sym(w)

  if (!is.null(psu)) {
    psu <- sym(psu)
  }

  if (!is.null(strata)) {
    strata <- sym(strata)
  }

  # Defining survey design
  if (!is.null(psu) & !is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, strata = !!strata, variables = c(!!i, !!g), nest = TRUE)
  } else if (!is.null(psu) & is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, variables = c(!!i, !!g), nest = TRUE)
  }
  else {
    svydesign <- data %>%
      as_survey_design(weight = !!w, variables = c(!!i, !!g), nest = TRUE)
  }

  # Computing mean and 95% confidence interval, adding a name fot the indicator, and group namees for group variable

  if (as_string(g) %in% c("fullsample")) {
    results <- svydesign %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE)
       # median = survey_median(!!i, na.rm = TRUE)
      ) %>%
      mutate(
        group_cat_val = gref[as_string(g)]
      )
    n <- data %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% bind_cols(n)
  }

  else {
    results <- svydesign %>%
      group_by(!!g) %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE)
        #median = survey_median(!!i, na.rm = TRUE)
      ) %>%
      mutate(
        group_cat_val = !!g
      )
    n <- data %>% group_by(!!g) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = as_string(g))
  }

  results <- results %>%
    rename(lci = mean_low, uci = mean_upp) %>%
    mutate(
      indicator = as_string(i),
      indicator_name = iref[as_string(i)],
      group = as_string(g),
      group_name = gref[as_string(g)]
    ) %>%
    dplyr::select(
      indicator, indicator_name, group, group_name, group_cat_val, nobs,  total, mean, lci, uci
    )

}

svy_summary_weights_p50 <- function(data, i, g, psu, strata, w, iref, gref) {

  i <- sym(i)
  g <- sym(g)
  w <- sym(w)

  if (!is.null(psu)) {
    psu <- sym(psu)
  }

  if (!is.null(strata)) {
    strata <- sym(strata)
  }

  # Defining survey design
  if (!is.null(psu) & !is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, strata = !!strata, variables = c(!!i, !!g), nest = TRUE)
  } else if (!is.null(psu) & is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, variables = c(!!i, !!g), nest = TRUE)
  }
  else {
    svydesign <- data %>%
      as_survey_design(weight = !!w, variables = c(!!i, !!g), nest = TRUE)
  }

  # Computing mean and 95% confidence interval, adding a name fot the indicator, and group namees for group variable

  if (as_string(g) %in% c("fullsample")) {
    results <- svydesign %>%
      summarise(
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, na.rm = TRUE)
      ) %>%
      mutate(
        group_cat_val = gref[as_string(g)]
      )
    n <- data %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% bind_cols(n)
  }

  else {
    results <- svydesign %>%
      group_by(!!g) %>%
      summarise(
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, na.rm = TRUE)
      ) %>%
      mutate(
        group_cat_val = !!g
      )
    n <- data %>% group_by(!!g) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = as_string(g))
  }

  results <- results %>%
    rename(lci = mean_low, uci = mean_upp) %>%
    mutate(
      indicator = as_string(i),
      indicator_name = iref[as_string(i)],
      group = as_string(g),
      group_name = gref[as_string(g)]
    ) %>%
    dplyr::select(
      indicator, indicator_name, group, group_name, group_cat_val, nobs,  mean, lci, uci, median
    )

}

svy_summary_weights_v2 <- function(data, i, g_l2, g_l1, psu, strata, w, iref, gref) {

  i <- sym(i)
  g_l1 <- sym(g_l1)
  g_l2 <- sym(g_l2)
  w <- sym(w)

  if (!is.null(psu)) {
    psu <- sym(psu)
  }

  if (!is.null(strata)) {
    strata <- sym(strata)
  }

  # Defining survey design
  if (!is.null(psu) & !is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, strata = !!strata, variables = c(!!i, !!g_l1, !!g_l2), nest = TRUE)
  } else if (!is.null(psu) & is.null(strata)) {
    svydesign <- data %>%
      as_survey_design(ids = !!psu, weight = !!w, variables = c(!!i, !!g_l1, !!g_l2), nest = TRUE)
  }
  else {
    svydesign <- data %>%
      as_survey_design(weight = !!w, variables = c(!!i, !!g_l1, !!g_l2), nest = TRUE)
  }

  # Computing mean and 95% confidence interval, adding a name fot the indicator, and group namees for group variable

  if (as_string(g_l2) %in% c("fullsample")) {
    results <- svydesign %>%
      group_by(!!g_l1) %>%
    summarise(
      total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
      mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
      median = survey_median(!!i, na.rm = TRUE)
    ) %>%
      mutate(
        #group1_cat_val = !!g_l1,
        group_cat_val = gref[as_string(g_l2)],
      )
    n <- data %>% group_by(!!g_l1) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = as_string(g_l1))
  }

  else {
    results <- svydesign %>%
      group_by(!!g_l1, !!g_l2) %>%
      summarise(
        total = survey_total(!!i, vartype = "ci", na.rm = TRUE),
        mean = survey_mean(!!i, vartype = "ci", na.rm=TRUE),
        median = survey_median(!!i, na.rm = TRUE)
      ) %>%
      mutate(
        #group1_cat_val = !!g_l1,
        !!g_l2 := as.character(!!g_l2),
        group_cat_val = !!g_l2
      )
    n <- data %>% group_by(!!g_l1, !!g_l2) %>% summarise(nobs = sum(!is.na(!!i)))
    results <- results %>% inner_join(n, by = c(as_string(g_l1), as_string(g_l2)))
  }

  results <- results %>%
    rename(lci = mean_low, uci = mean_upp) %>%
    mutate(
      indicator = as_string(i),
      indicator_name = iref[as_string(i)],
      group = as_string(g_l2),
      group_name = gref[as_string(g_l2)]
    ) %>%
    dplyr::select(
      !!g_l1, indicator, indicator_name, group, group_name, group_cat_val, nobs, total, mean, lci, uci, median
    )

}


# Function to run regression model and retrieve results
runModel <- function(data, indepvar, cause, confounders, gender, year) {

  data <- data %>% filter(resp_age_yrs >= 25 & resp_age_yrs < 65 & resp_gender_fct == gender) %>% mutate(year = year)
  data <- data[, c(samplevars, indepvar, cause, confounders)]
  data <- data[complete.cases(data),]

  alldepvars <- paste(c(cause, confounders), collapse =" + ")
  f <- paste(indepvar, " ~ ", alldepvars, sep = "")
  m <- glm(as.formula(f), family = binomial(link = "logit"), data = data)

  # Computing error rate
  error.rate <- mean( (m$fitted.value > 0.5 & m$y == 0) | (m$fitted.value < 0.5 & m$y == 1))
  p <- mean(m$y)
  error.rate.null <- mean( (p > 0.5 & m$y == 0) | (p < 0.5 & m$y == 1) )

  tidy(m) %>%
    mutate(indepvar = indepvar,
           year = year,
           gender = gender,
           lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error,
           sig = ifelse(p.value < 0.05, 1, 0),
           color = case_when(
             sig == 1 & estimate < 0 ~ "red",
             sig == 1 & estimate > 0 ~ "blue",
             sig == 0 ~ "grey"),
           obs = length(m$y),
           df_res  = m$df.residual,
           dev_res = m$deviance,
           dev_null = m$null.deviance,
           model_error_rate = error.rate,
           null_error_rate = error.rate.null
    )
}


loans_by_slice <- function(data, indicators, slicevar, groups) {
  
  lendervar <- sym(names(groups))
  slicevar <- sym(slicevar)
  
  data <- data %>%
    group_by(!!slicevar, mem_id, !!lendervar) %>%
    mutate(ind_nloans_pastyear_bylender = sum(nloans_pastyear_mod, na.rm = T),
           ind_totalborrowed_pastyear_bylender = sum(total_borrowed, na.rm = T)
    ) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]
  
  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = as_string(slicevar),
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )
  
  return(results)
  
}