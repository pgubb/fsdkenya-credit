
TITLE_WRAP <- 110
CAP_WRAP <- 120


# 1. How large is the market for loans in Kenya? -------------

# Volume of loans by lender --------------
fig_tree_loans_by_volume <- function(data, indicators, groups, plot_text) {

  lendervar <- sym(names(groups))

  data <- data %>%
    group_by(mem_id, !!lendervar) %>%
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
         svy_summary_weights,
         data = data,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  levels_lender <- names(PALETTES[[names(groups)]])

  chart_df <- results %>%
    mutate(
      group_cat_val = factor(group_cat_val, levels = levels_lender, ordered = TRUE),
      totalvolume = sum(total),
      share = total/totalvolume,
      valuelab = str_wrap(paste0(round(total/1000000, 2), "m (", round(share*100, 0), "%)"), 12)
    )

  #png(file ="figures/fig6.png",width = 11, height = 7, units = c("in"), res = 300)

  treemap(
    as.data.frame(chart_df),
    index=c("group_cat_val", "valuelab"),
    vSize="total",
    vColor = "group_cat_val",
    type = "categorical",
    palette = PALETTES[[names(groups)]],
    algorithm = "pivotSize",
    sortID = "-total",
    title = plot_text[["title"]],
    title.legend = "",
    position.legend = "none",
    mirror.x = FALSE,
    align.labels = list(c("center", "top"), c("center", "center")),
    #ymod.labels = c(0.115),
    border.col = c('black', 'white'),
    border.lwds = c(2,1),
    fontsize.title = 14,
    fontsize.labels = c(11, 10),
    bg.labels = 0,
    aspRatio = 2/1
  )

  #dev.off()

}

# Value of loans by lender --------------
fig_tree_loans_by_value <- function(data, indicators, groups, plot_text) {

  lendervar <- sym(names(groups))

  data <- data %>%
    group_by(mem_id, !!lendervar) %>%
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
         svy_summary_weights,
         data = data,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  levels_lender <- names(PALETTES[[names(groups)]])

  chart_df <- results %>%
    mutate(
      group_cat_val = factor(group_cat_val, levels = levels_lender, ordered = TRUE),
      total_global = sum(total),
      share = total/total_global,
      valuelab = str_wrap(paste0("KSh", round(total/1e9, 2), "b (", round(share*100, 0), "%)"), 12)
    )

  #png(file ="figures/loans_by_value.png", width = 11, height = 7, units = c("in"), res = 300)

  treemap(
    as.data.frame(chart_df),
    index=c("group_cat_val", "valuelab"),
    vSize="total",
    vColor = "group_cat_val",
    type = "categorical",
    palette = PALETTES[[names(groups)]],
    position.legend = "none",
    algorithm = "pivotSize",
    sortID = "-total",
    title = plot_text[["title"]],
    title.legend = "",
    mirror.x = FALSE,
    align.labels = list(c("center", "top"), c("center", "center")),
    #ymod.labels = c(0.115),
    border.col = c('black', 'white'),
    border.lwds = c(2,1),
    fontsize.title = 14,
    fontsize.labels = c(11, 10),
    bg.labels = 0,
    aspRatio = 2/1
  )
  #dev.off()

}

# Volume/Value of loans by lender by year  --------------
fig_bar_loans_by_slice <- function(data, indicators, slicevar, groups, plot_text, plot_label) {

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

  levels_lender <- names(PALETTES[[names(groups)]])

  if (indicators == "ind_nloans_pastyear_bylender") {
    denom = 1e6
    suffix = "m"
  }

  if (indicators == "ind_totalborrowed_pastyear_bylender") {
    denom = 1e9
    suffix = "b"
  }

  chart_df <- results %>%
    group_by(!!slicevar) %>%
    mutate(
      label_sig = formatC(signif(total/denom, digits=3), digits=3, format="fg", flag="#"),
      group_cat_val = factor(group_cat_val, levels = levels_lender, ordered = TRUE),
      grand_total = sum(total),
      share = total/grand_total,
      label_total = paste0(prettyNum(round(grand_total/denom, 1), big.mark = ","), suffix),
      label_total = ifelse(group_cat_val == "Bank", label_total, NA)
    )

  if (plot_label[["type"]] == "both") {
    chart_df <- chart_df %>% mutate(
      valuelab = str_wrap(paste0(label_sig, suffix, " (", round(share*100, 0), "%)"), 10),
      valuelab = ifelse(total < plot_label[["threshold"]], NA, valuelab)
    )
  }

  if (plot_label[["type"]] == "valueonly") {
    chart_df <- chart_df %>% mutate(
      valuelab = paste0(label_sig, suffix),
      valuelab = ifelse(total < plot_label[["threshold"]], NA, valuelab)
    )
  }

  if (plot_label[["type"]] == "shareonly") {
    chart_df <- chart_df %>% mutate(
      valuelab = paste0(round(share*100, 0), "%)"),
      valuelab = ifelse(total < plot_label[["threshold"]], NA, valuelab)
    )
  }

  #png(file ="figures/fig6.png",width = 11, height = 7, units = c("in"), res = 300)

  ggplot(
    data = chart_df,
    aes(
      x = !!slicevar,
      y = total,
      fill = group_cat_val
    )
  ) +
    geom_col(width = 0.5) +
    geom_text(aes(y = grand_total, label = label_total), vjust = 0, nudge_y = plot_label[["nudge_y"]], fontface = "bold") +
    geom_text(aes(label = valuelab), position = position_stack(vjust = 0.5)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    scale_fill_manual(values = PALETTES[[names(groups)]]) +
    guides(fill = guide_legend(title = NULL)) +
    labs(
      x = plot_text[["xaxis"]],
      y = plot_text[["yaxis"]],
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    theme_custom() +
    theme(axis.line.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

  #dev.off()

}

# Function to create conmposite comparing across household wealth quintiles
fig_bar_loans_by_slice_year <- function(data, indicators, slicevar, groups, main_plot_text, plot_label) {

  plot_text <- list(
    subtitle = "2019",
    title = NULL,
    caption = NULL
  )
  p1 <- fig_bar_loans_by_slice(data %>% filter(year == 2019), indicators, slicevar, groups, plot_text, plot_label)

  plot_text <- list(
    subtitle = "2021",
    title = NULL,
    caption = NULL
  )
  p2 <- fig_bar_loans_by_slice(data %>% filter(year == 2021), indicators, slicevar, groups, plot_text, plot_label)

  scale_f <- 1.2

  p1 + p2 +
    plot_layout(guides = 'collect') +
    plot_annotation(
    title = main_plot_text[["title"]],
    subtitle = main_plot_text[["subtitle"]],
    caption =  main_plot_text[["caption"]]
  ) &
    ylim(0, plot_label[["ymax"]]) &
    theme(
      plot.subtitle = element_text(size = 12*scale_f, face = 'bold'),
      plot.title = element_text(size = 11.5*scale_f, hjust = 0, face = 'bold', color = '#0496FF'),
      plot.caption=element_text(hjust=0, color="grey40", size=10.5*scale_f)
    )

}

# Volume of loans by size and source of loan
fig_bar_loans_by_size_and_source <- function(data, indicators, slicevar, groups, plot_text, outfilename = FALSE) {

  indicators <- names(INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = slicevar,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  stem <- str_remove(names(groups), "str")
  lenders <- names(INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste(stem, collapse = '|'))])
  levels_group <- INDICATORS_REFLIST_LVL_LOAN[lenders]
  levels_indicator <- INDICATORS_REFLIST_LVL_LOAN[indicators]

  slicevar <- sym(slicevar)

  chart_df <- results %>%
    group_by(!!slicevar) %>%
    mutate(
      group_cat_val = factor(group_cat_val, levels = levels_group, ordered = TRUE),
      n_loans_total = sum(total),
      n_loans_share = total/n_loans_total,
      indicator_name = factor(str_wrap(indicator_name, 15), levels = str_wrap(levels_indicator, 15), ordered = TRUE),
      value_label = pctclean(n_loans_share, 1),
      value_label = ifelse(n_loans_share < 0.01, NA, value_label)
    )

  p <- ggplot(
    data = chart_df,
    aes(
      x = indicator_name,
      y = n_loans_share,
      fill = group_cat_val
    )
  ) +
    facet_wrap(vars(!!slicevar), nrow = 1) +
    geom_col(position = position_stack()) +
    geom_text(aes(label = value_label), position = position_stack(vjust = 0.5)) +

    guides(fill = guide_legend(title = NULL)) +

    scale_fill_manual(values = PALETTES[[names(groups)]]) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.65)) +

    labs(
      x = plot_text[["xaxis"]],
      y = plot_text[["yaxis"]],
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +

    theme_custom(scale_f = 1.3)

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

  }

#2. How prevalent are different forms of borrowing among adults? ---------------

# Prevalence of borrowing by type
fig_bar_borrowers_by_type <- function(data, indicators, slicevar, groups, plot_text, outfilename = FALSE) {

  #indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = slicevar,
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  slicevar <- sym(slicevar)

  p <- ggplot(
    data = results,
    aes(
      x = mean,
      y = fct_rev(fct_inorder(str_wrap(indicator_name, 45))),
      color = !!slicevar
    )
  ) +
    facet_wrap(~fct_inorder(group_cat_val), nrow = 1) +
    geom_linerange(aes(xmin = 0, xmax = mean), size = 1.75, position = position_dodge(width = 0.35)) +
    geom_point(shape = 21, size = 2.5, stroke = 1.75, fill = "white",  position = position_dodge(width = 0.35)) +
    geom_text(aes(x = mean + 0.025, label = pctclean(mean, 0)), hjust = 0, position = position_dodge2(width = 0.35, reverse = TRUE), color = "black") +
    guides(color = guide_legend(title = NULL)) +
    labs(
      x = plot_text[["xaxis"]],
      y = plot_text[["yaxis"]],
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    scale_color_manual(values = c("2019" = "grey35", "2021" = "#0496FF")) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.75)) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "top", legend.direction = "horizontal")

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

}

#
fig_bar_loansused_by_source <- function(data, indicators, slicevar, groups, plot_text, outfilename = FALSE) {
  # Prevalence of use

  indicators <- names(INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = slicevar,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  slicevar <- sym(slicevar)

  p <- ggplot(
    data = results,
    aes(
      x = mean,
      y = fct_reorder(indicator_name, mean),
      color = !!slicevar
    )
  ) +
    facet_wrap(~  fct_inorder(group_cat_val)) +
    geom_linerange(aes(xmin = 0, xmax = mean), size = 1.75, position = position_dodge(width = 0.55)) +
    geom_point(shape = 21, size = 2.5, stroke = 1.75, fill = "white",  position = position_dodge(width = 0.55)) +
    geom_text(aes(x = mean + 0.015, label = pctclean(mean, 1)), hjust = 0, position = position_dodge2(width = 0.55, reverse = TRUE), color = "black") +
    labs(
      x = plot_text[["xaxis"]],
      y = plot_text[["yaxis"]],
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    guides(color = guide_legend(title = NULL)) +
    scale_color_manual(values = c("2019" = "grey35", "2021" = "#0496FF")) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.4)) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "top", legend.direction = "horizontal")

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

}

#
fig_bar_borrowers_by_portfolio <- function(data, indicators, groups, plot_text, outfilename = FALSE) {

  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(borrower_any_status == 1),
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights"
    )
  )

  chart_df <- results %>%
    filter(mean > 0) %>%
    mutate(
      group_cat_val = fct_inorder(group_cat_val),
      indicator_name = fct_inorder(indicator_name),
      #indicator_name = fct_inorder(str_wrap(indicator_name, 18)),
      valuelab = ifelse(mean > 0, pctclean(mean, 0), NA)
    )

  p <- ggplot(data = chart_df,
         aes(y = mean,
             x = fct_rev(group_cat_val),
             fill = fct_rev(indicator_name))) +
    geom_col(position = "stack") +
    geom_vline(xintercept = c(3.5, 6.5, 9.5, 11.5), size = 0.5, color = "black") +
    geom_text(aes(label = valuelab), position = position_stack(vjust = 0.5), size = 4.2, color = "black") +
    annotate("text", y = 0.2, x = 10.5, label = "Digital borrowers", color = "red", vjust = 0, size = 1.25) +
    guides(fill = guide_legend(title = "", reverse = TRUE, byrow = TRUE)) +
    scale_x_discrete(
      expand=c(0.1,0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0","25", "50", "75", "100% of\n borrowers")
    ) +
    scale_fill_manual(values = c("Formal non-digital loans" = "#2A4494", "Digital loans" = "#0496FF", "Informal loans only" = "#FFBC42")) +
    labs(
      x = plot_text[["xaxis"]],
      y = plot_text[["yaxis"]],
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    coord_flip() +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "top", legend.direction = "horizontal")

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)
}

#
fig_bar_borrowing_freq_by_lender <- function(data, indicators, groups, plot_text, outfilename = FALSE) {

  lendervar <- sym(names(groups))

  data <- data %>%
    group_by(mem_id, !!lendervar) %>%
    mutate(ind_nloans_pastyear_bylender = sum(nloans_pastyear_mod, na.rm = T),
           ind_totalborrowed_pastyear_bylender = sum(total_borrowed, na.rm = T)
    ) %>%
    filter(row_number() == 1) %>%
    ungroup()

  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  all <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  ) %>% select(-total) %>% mutate(group2_cat_val = "All adults")

  men <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(resp_gender_fct == "Men"),
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  ) %>% select(-total) %>% mutate(group2_cat_val = "Men")

  women <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(resp_gender_fct == "Women"),
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  ) %>% select(-total) %>% mutate(group2_cat_val = "Women")

  avgloans_bylender <- bind_rows(men, women)

  p <- ggplot(
    data = all,
    aes(
      x = mean,
      y = fct_reorder(group_cat_val, mean),
      color = group2_cat_val
    )
  ) +
    geom_linerange(aes(xmin = 0, xmax = mean), size = 1.75, position = position_dodge(width = 0.55)) +
    geom_point(shape = 21, size = 2.5, stroke = 1.75, fill = "white",  position = position_dodge(width = 0.55)) +
    geom_text(aes(x = mean + 0.1, label = round(mean, 1)), hjust = 0, position = position_dodge2(width = 0.55, reverse = TRUE), color = "black") +
    scale_color_manual(values = c("All adults" = "black", "Men" = "#0496FF", "Women" = "#E36588")) +
    labs(
      y = plot_text[["yaxis"]],
      x = plot_text[["xaxis"]],
      title = str_wrap(plot_text[["title"]], 130),
      subtitle = str_wrap(plot_text[["subtitle"]], 130),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "none")

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

  }

# Reason for choosing most important loan
fig_comp_borrower_by_mostimplloan <- function(data, indicators_left, groups_left, indicators_right, groups_right, plot_text_left, plot_text_right, outfilename = FALSE) {

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
  # LEFT PANEL

  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators_left, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups_left), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(borrower_any_status == 1),
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups_left,
         psu = "psu",
         strata = NULL,
         w = "probweights"
    )
  )

  chart_df <- results %>%
    mutate(
      indicator_name = factor(indicator_name, levels = mimp_lender, ordered = TRUE)
    )

  plt_left <- ggplot(
    data = chart_df,
    aes(
      x = mean,
      y = fct_rev(indicator_name)
    )
  ) +
    geom_point(aes(x = 1, size = mean), shape = 21, fill = "white", color = "black") +
    geom_text(aes(x = 1, label = pctclean(mean, 0)), color = "black") +
    scale_x_continuous(limits = c(0.9, 1.1),  breaks = NULL, position = "top") +
    #scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey80") +
    scale_size(range = c(4,15)) +
    labs(
      x = NULL,
      y = NULL,
      title = str_wrap(plot_text_left[["title"]], TITLE_WRAP),
      subtitle = plot_text_left[["subtitle"]]
    ) +
    theme_custom(scale_f = 1.3, legend.position = "none") + theme(axis.line.x = NULL, axis.text.x = NULL)

  # RIGHT PANEL

  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators_right, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups_right), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(borrower_any_status == 1),
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups_right,
         psu = "psu",
         strata = NULL,
         w = "probweights"
    )
  )


  chart_df <- results %>%
    filter(!is.na(group_cat_val)) %>%
    mutate(
      value = ifelse(mean > 0.025, mean, NA),
      value_labs = pctclean(value, 0),
      group_cat_val = factor(group_cat_val, levels = mimp_lender, ordered = TRUE),
      textcolor = ifelse(value > 0.4, "white", "black")
    )

  plt_right <-
    ggplot(
      data = chart_df,
      aes(
        x = fct_inorder(str_wrap(indicator_name, 12)),
        y = fct_rev(group_cat_val),
        fill = value
      )) +
    geom_tile() +
    geom_text(aes(label = value_labs, color = textcolor)) +
    geom_hline(yintercept = seq(1.5, 10.5), color = "grey40", size = 0.5) +
    scale_x_discrete(position = "top") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey80") +
    scale_color_identity() +
    labs(
      x = "",
      y = NULL,
      title = str_wrap(plot_text_right[["title"]], TITLE_WRAP),
      subtitle = plot_text_right[["subtitle"]]
    ) +
    theme_custom(scale_f = 1.3, legend.position = "none")

  # COMBINING PANELS
  p <- plt_left + plt_right + plot_layout(axes = "collect", widths = c(0.25, 3)) +  plot_annotation(
    caption = str_wrap('Notes: Only reaons that make up at least 2.5% of borrower responses are shown. Source: 2021 FinAccess household survey (Nationally representative sample of 20,909  adults ages 18 and above).', CAP_WRAP)
  )

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

}


# 3. Why do adults borrow? ------------------------------

# Distribution of loans by reason for borrowing
fig_bar_loan_reasons <- function(data, indicators, slicevar, groups, plot_text, palette, outfilename = FALSE) {

  indicators <- names(INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  data <- data %>% mutate(resp_gender_fct = as.character(resp_gender_fct), hh_geo2_fct = as.character(hh_geo2_fct))

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = slicevar,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  slicevar <- sym(slicevar)

  chart_df <- results %>%
    filter(indicator_name %not_in% c("Don't Know", "Other (SPECIFY)")) %>%
    filter(!is.na(group_cat_val)) %>%
    mutate(
      valuelab = pctclean(mean, 0),
      valuelab = ifelse(mean < 0.01, NA, valuelab)
    )

  p <- ggplot(
    data = chart_df,
    aes(x = mean,
        y = fct_reorder(indicator_name, mean),
        fill = fct_rev(fct_inorder(group_cat_val)))
    ) +
    facet_wrap(vars(!!slicevar), nrow = 1) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = valuelab), hjust =0, position=position_dodge(width = 0.9)) +
    scale_fill_manual(values = palette) +
    scale_x_continuous(labels = scales::label_percent()) +
    guides(
      fill = guide_legend(title = NULL)
    ) +
    labs(
      y = plot_text[["yaxis"]],
      x = plot_text[["xaxis"]],
      title = str_wrap(plot_text[["title"]], 130),
      subtitle = str_wrap(plot_text[["subtitle"]], 130),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "top", legend.direction = "horizontal")

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

}

# 4. How many adults have difficulty repaying debt and why are loan repayments delinquent? -----------

# Status of debt by lender
fig_tile_debt_overview <- function(data, borrower_data, indicators, slicevar, groups, plot_text, outfilename = FALSE) {

  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = slicevar,
         iref = INDICATORS_REFLIST_LVL_LOAN,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  # Adding data on defaults by lender to merge into table

  indicators <- c("debt_default_lender_agg3")
  groups <- names(slicevar)
  names(groups) <- slicevar

  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  slicevar <- sym(slicevar)

  defaults <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = borrower_data,
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights"
    )
  ) %>% rename(!!slicevar := group_cat_val, group_cat_val = indicator_name) %>%
    mutate(indicator = "delinquent", indicator_name = "Delinquent payment (% of borrowers)*")


  chart_df <- results %>% bind_rows(defaults) %>%
    mutate(
      ordervar = ifelse(indicator == "has_recent_loan", mean, NA),
      value = ifelse(indicator %in% c("has_recent_loan", "has_debt", "delinquent"), mean, median),
      value_label = ifelse(indicator %in% c("has_recent_loan", "has_debt", "debt_balance_income_cd", "delinquent"), round(value*100, 1), prettyNum(round(value,0), big.mark = ","))
    ) %>%
    group_by(!!slicevar, indicator) %>%
    mutate(
      value_std = value/(max(value)),
      value_fill = ifelse(indicator %in% c("has_debt", "debt_balance_cd", "debt_balance_income_cd", "delinquent"), value_std, NA),
      xcats = factor(str_wrap(indicator_name, 12), str_wrap(unique(indicator_name), 12))
    ) %>% ungroup() %>%
    group_by(group_cat_val) %>%
    mutate(
      ordervar = mean(ordervar, na.rm = TRUE)
    ) %>% ungroup() %>% filter(nobs > 0)

  # Heat map

  p <- ggplot(data = chart_df,
         aes(x = fct_inorder(xcats),
             y = fct_reorder(group_cat_val, ordervar))
  ) +
    facet_wrap(vars(!!slicevar), ncol = 1) +
    geom_tile(aes(fill = value_fill)) +
    geom_text(aes(label = value_label)) +
    scale_x_discrete(position = "top") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "white") +
    labs(
      x = plot_text[["xaxis"]],
      y = plot_text[["yaxis"]],
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "none")

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)
}

# Debt stress and loan delinquency
fig_comp_debtstress <- function(data, slicevar, plot_text, outfilename = FALSE) {

  indicators <- c("debt_default_reason")
  groups <- c("fullsample" = "All adults", "resp_income_quintile" = "Income quintile")
  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
  indicators <- c("debt_monthly_repayment_high", "debt_repaystress", "debt_default", "debt_delinquent", "debt_stressany", indicators)
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  data <- data %>% filter(!is.na(resp_income_quintile))

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights_v2,
         data = data,
         g_l1 = slicevar,
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights")
  )

  slicevar <- sym(slicevar)

  chart_df <- results %>% filter(!is.na(group_cat_val)) %>%
    mutate(
      indicator_category = ifelse(indicator %in% c("debt_monthly_repayment_high", "debt_repaystress","debt_default", "debt_delinquent",  "debt_stressany"),
                                  "Prevalence of debt stress (% of adults)",
                                  "Main reason for default (2019) or delinquency (2021) (% of defaulted or delinquent borrowers)"),
      xcat = str_wrap(group_cat_val, 20),
    ) %>%
    group_by(!!slicevar, indicator) %>%
    mutate(
      xcat_num = row_number(),
      group_label = ifelse(xcat_num == 6, indicator_name, NA),
      group_label = ifelse(mean < 0.03, NA, group_label)
    ) %>% ungroup() %>%
    filter(mean > 0) # Dropping "indicators"debt_delinquent" indicator which was not collected in 2019

  chart_df1 <- chart_df %>%
    mutate(
      linegroup = paste(group, indicator, "_"),
      fullsample_label = ifelse(group_cat_val == "All adults", pctclean(mean, 0), NA)
      ) %>%
    filter(indicator_category == "Prevalence of debt stress (% of adults)")

  caption <- "Notes: 'Debt repayment stress' includes borrowers who report having had to reduce food expenditures, borrower more or sell assets to repay a loan. 'Delinquent loan payment' includes borrowes who in the past 12 months were either late in paying any of their loans, missed a payment, paid less or never paid anything as required.
              In 2019, only defaulting on a loan was asked, per the following question 'In the past 12 months, how many times have you defaulted on your loans/debts, if at all?'
              whereas in 2021, a wider range of outcomes was collected, per the following: 'In the past 12 months were you either late paying any of your loans, missed a payment, paid less or never paid any amount as required?'"

  p1 <- ggplot(data = chart_df1,
               aes(x = xcat_num,
                   y = mean,
                   color = indicator_name,
                   group = linegroup)) +
    facet_wrap(vars(!!slicevar), nrow = 1) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 1.5, color = "grey40", linetype = "dashed") +
    geom_text(aes(label = group_label), hjust = 0, nudge_x = 0.1, color = "black") +
    geom_text(aes(label = fullsample_label), nudge_y = 0.01,  vjust = 0, color = "black") +
    scale_x_continuous(limits = c(0.5, 10.5), breaks = seq(1,6,1), labels = str_wrap(unique(chart_df$group_cat_val), 8)) +
    scale_y_continuous(limits = c(0, 0.4), labels = scales::label_percent()) +
    scale_color_viridis_d() +
    labs(
      y = NULL,
      x = "Personal monthly income (quintiles)",
      title = plot_text[["title1"]],
      subtitle = unique(chart_df1$indicator_category),
      caption = str_wrap(caption, 120)
    ) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "none")

  chart_df2 <- chart_df %>%
    filter(indicator_category == "Main reason for default (2019) or delinquency (2021) (% of defaulted or delinquent borrowers)") %>%
    mutate(
      valuelabel = pctclean(mean, 0),
      valuelabel = ifelse(mean < 0.03, NA, valuelabel),
      fillgroup = factor(indicator_name,
                         levels = c("All money used up for basic needs (eg food or utility bills)",
                                    "Did not plan well enough",
                                    "Income declined, business performed poorly",
                                    "Had to pay off other loans",
                                    "Unexpected emergency expenditure",
                                    "Did not understand loan terms, payment more than expected",
                                    "Interest/repayment increased",
                                    "Other"
                         ),
                         ordered = TRUE),
      txt_color = ifelse(fillgroup %in% c("All money used up for basic needs (eg food or utility bills)", "Did not plan well enough"), "white", "black")
    )


  caption <- "Notes: Did not plan well enough includes the response 'Borrowed too much originally'.
              'Income declined, business performed poorly', encompasses both the respondent and someone else in the household losing a job or a source of income.
              This also includes 'Poor business performance'."

  p2 <- ggplot(data = chart_df2,
               aes(x = xcat_num,
                   y = mean,
                   fill = fillgroup,
                   group = fct_rev(fillgroup))) +
    facet_wrap(vars(!!slicevar), nrow = 1) +
    geom_col(position = position_stack(), width = 0.5) +
    geom_vline(xintercept = 1.5, color = "grey40", linetype = "dashed") +
    geom_text(aes(label = valuelabel, color = txt_color), position = position_stack(vjust = 0.5)) +
    geom_text(aes(x = xcat_num + 0.45, label = group_label), position = position_stack(vjust = 0.5), hjust = 0, color = "black") +
    scale_x_continuous(limits = c(0.5, 10.5), breaks = seq(1,6,1), labels = str_wrap(unique(chart_df$group_cat_val), 8)) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_color_identity() +
    labs(
      y = NULL,
      x = "Personal monthly income (quintiles)",
      title = plot_text[["title2"]],
      subtitle = unique(chart_df2$indicator_category),
      caption = str_wrap(caption, 120)
    ) +
    scale_fill_viridis_d() +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "none")

  p <-  p1 / p2

  if (outfilename) {
    filename <- paste0("figures/", outfilename, "png")
    ggsave(filename, plot = p, width = 14, height = 7, dpi = 300)
  }

  return(p)

  }

# 5. Other/ In development -----------

# The lending landscape
fig_lending_landscape <- function(data, plot_text) {

  indicators <- c("loan_principal")
  groups <- c("lender_agg3_str" = "Lender")
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  mean_loansize_bylender <- dplyr::bind_rows(
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

  # Main reason for loan:

  ind <- c("loan_reason_det")
  indicators <- names(INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste(ind, collapse = '|'))])
  groups <- c("lender_agg3_str" = "Lender")
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  loan_reason_bylender <- dplyr::bind_rows(
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

  loan_reason_bylender <- loan_reason_bylender %>%
    group_by(group_cat_val) %>%
    mutate(keep = ifelse(mean == max(mean), 1, 0)) %>%
    filter(keep == 1) %>%
    rename(mainreason_cat = indicator_name, mainreason_pct = mean) %>%
    dplyr::select(group_cat_val, mainreason_cat, mainreason_pct)

  # Prevalence of use

  ind <- c("lender_agg3")
  indicators <- names(INDICATORS_REFLIST_LVL_LOAN[str_detect(names(INDICATORS_REFLIST_LVL_LOAN), paste(ind, collapse = '|'))])
  groups <- c("fullsample" = "All adults")
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  usage_bylender <- dplyr::bind_rows(
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

  usage_bylender <- usage_bylender %>%
    dplyr::select(indicator_name, mean) %>%
    rename(group_cat_val = indicator_name, usage = mean)

  chart_df <- mean_loansize_bylender %>%
    inner_join(usage_bylender) %>%
    inner_join(loan_reason_bylender) %>%
    mutate(usage = usage*100)

  refprices <- tibble(
    item = c("Petrol (1 Litre)", "DAP Fertilizer (50kg)", "Electricity (50 KWh)", 'Maize grain (6kg)', 'Secondary day school term fee', 'Used CAP_WRAPcc motorbike'),
    price = c(125.79, 6500, 884.38, 288.42, 10000, 100000)
  )

  p <- ggplot(
    data = chart_df %>% filter(indicator == "loan_principal"),
    aes(
      x = usage,
      y = mean,
      color = mainreason_cat
    )
  ) +
    geom_point(shape  = 21, fill = "white", stroke = 2, size = 4) +
    geom_text(aes(label = group_cat_val), color = "black", nudge_x = 0.4, hjust = 0) +

    geom_hline(data = refprices, aes(yintercept = price), color = "grey90", linetype = "dashed") +
    geom_text(data = refprices, aes(x = 41, y = price, label = str_wrap(item, 25)), color = "black") +

    scale_y_continuous(breaks = c(0, 250, 500, 1000, 2500, 5000, 10000, 20000, 40000, 80000,160000, 320000),
                       labels = scales::label_dollar(prefix = "KShs "),
                       trans = "log") +
    scale_x_continuous(limits = c(0, 45)) +
    guides(
      color = guide_legend(title = "Most common reason\nfor taking loan")
    ) +
    labs(
      y = "Loan size [log scale]",
      x = "Share of loans in use [%]",
      title = plot_text[["title"]],
      subtitle = plot_text[["subtitle"]],
      caption = str_wrap(paste(plot_text[["caption"]], "Notes: Reference prices for Petrol, Maize and Electricity obtained from the 2021 Economic survey. Fertilizer prices are from the Ministry of Agriculture, Secondary day school term fees from the Ministry of Education and the cost of a used CAP_WRAPcc motorbike (Boxer Bajaj) from online classifieds."), 140)
    ) +
    theme_custom(legend.position = "top", scale_f = 1.3)

  filename <- paste0("figures/fig9.png")
  ggsave(filename, plot = p, width = 14, height = 7.5, dpi = 300)

  return(p)

}


# ANNEX

# Most detail (all borrowers)
fig2 <- function(data, indicators, groups, plot_text) {

  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(borrower_any_status == 1),
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights"
    )
  )

  chart_df <- results %>%
    filter(mean > 0) %>%
    mutate(
      group_cat_val = fct_inorder(group_cat_val),
      indicator_name = fct_inorder(indicator_name),
      #indicator_name = fct_inorder(str_wrap(indicator_name, 18)),
      valuelab = ifelse(mean > 0, pctclean(mean, 0), NA)
    ) %>%
    group_by(group_name, group_cat_val) %>%
    mutate(
      value_sub = ifelse(indicator_name %in% c("Non-digital formal loans only", "Other formal + informal loans"), mean, NA),
      value_rect_min = max(sum(value_sub, na.rm = T), na.rm = T),
      value_sub = ifelse(indicator_name %in% c("Digital + informal loans", "Digital loans only", "Digital + Other formal + Informal loans", "Digital + other formal loans"), mean, NA),
      value_rect_max = max(sum(value_sub, na.rm = T), na.rm = T) + value_rect_min
    ) %>% ungroup()

  p <- ggplot(data = chart_df,
              aes(y = mean,
                  x = fct_rev(group_cat_val),
                  fill = fct_rev(indicator_name))) +
    geom_col(position = "stack") +
    geom_vline(xintercept = c(3.5, 6.5, 9.5, 11.5), size = 0.5, color = "black") +
    geom_text(aes(label = valuelab), position = position_stack(vjust = 0.5), size = 4.2, color = "black") +
    annotate("text", y = 0.2, x = 10.5, label = "Digital borrowers", color = "red", vjust = 0, size = 1.25) +
    guides(fill = guide_legend(title = "", reverse = TRUE, byrow = TRUE)) +
    scale_x_discrete(
      expand=c(0.1,0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0","25", "50", "75", "100% of\n borrowers")
    ) +
    labs(
      x = NULL,
      y = "% of active borrowers (18+)",
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["subtitle"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["caption"]], CAP_WRAP)
    ) +
    coord_flip() +
    labs(
      y = NULL,
      x = NULL
    ) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "top", legend.direction = "horizontal")

  filename <- paste0("figures/fig2.png")
  ggsave(filename, plot = p, width = 14, height = 9, dpi = 300)

  return(p)

}

# Digital borrowers only
fig3 <- function(data, indicators, groups, plot_text) {

  indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
  combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
  is <- combinations[[1]]
  gs <- combinations[[2]]

  results <- dplyr::bind_rows(
    map2(is,
         gs,
         svy_summary_weights,
         data = data %>% filter(borrower_digital_status == 1),
         iref = INDICATORS_REFLIST_LVL_BORR,
         gref = groups,
         psu = "psu",
         strata = NULL,
         w = "probweights"
    )
  )

  chart_df <- results %>%
    filter(mean > 0) %>%
    mutate(
      group_cat_val = fct_inorder(group_cat_val),
      indicator_name = fct_inorder(indicator_name),
      #indicator_name = fct_inorder(str_wrap(indicator_name, 18)),
      valuelab = ifelse(mean > 0, pctclean(mean, 0), NA)
    ) %>%
    group_by(group_name, group_cat_val) %>%
    mutate(
      value_sub = ifelse(indicator_name %in% c("Non-digital formal loans only", "Other formal + informal loans"), mean, NA),
      value_rect_min = max(sum(value_sub, na.rm = T), na.rm = T),
      value_sub = ifelse(indicator_name %in% c("Digital + informal loans", "Digital loans only", "Digital + Other formal + Informal loans", "Digital + other formal loans"), mean, NA),
      value_rect_max = max(sum(value_sub, na.rm = T), na.rm = T) + value_rect_min
    ) %>% ungroup()

  p <- ggplot(data = chart_df,
              aes(y = mean,
                  x = fct_rev(group_cat_val),
                  fill = fct_rev(indicator_name))) +
    geom_col(position = "stack") +
    geom_vline(xintercept = c(3.5, 5.5, 7.5, 9.5), size = 0.5, color = "black") +
    geom_text(aes(label = valuelab), position = position_stack(vjust = 0.5), size = 4.2, color = "black") +
    annotate("text", y = 0.2, x = 10.5, label = "Digital borrowers", color = "red", vjust = 0, size = 1.25) +
    guides(fill = guide_legend(title = "", reverse = TRUE, byrow = TRUE)) +
    scale_x_discrete(
      expand=c(0.1,0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0","25", "50", "75", "100% of\n borrowers")
    ) +
    labs(
      x = NULL,
      y = "% of 12-month active borrowers (18+)",
      title = str_wrap(plot_text[["title"]], TITLE_WRAP),
      subtitle = str_wrap(plot_text[["title"]], TITLE_WRAP),
      caption = str_wrap(plot_text[["title"]], CAP_WRAP)
    ) +
    coord_flip() +
    labs(
      y = NULL,
      x = NULL
    ) +
    theme_custom(scale_f = 1.3) +
    theme(legend.position = "top", legend.direction = "horizontal")

  filename <- paste0("figures/fig3.png")
  ggsave(filename, plot = p, width = 14, height = 9, dpi = 300)

  return(p)

}


# Main livelihoods by year (% of adults)

# figlivebyyear <- function(data, indicators) { 
#   
#   
#   }
# 
# data <- borrower_data_pool
# indicators <- "resp_live_group"
# groups <- c("fullsample" = "All adults", "hh_wlth_group" = "Household wealth group")
# indicators <- names(INDICATORS_REFLIST_LVL_BORR[str_detect(names(INDICATORS_REFLIST_LVL_BORR), paste(indicators, collapse = '|'))])
# combinations <- expand.grid(indicators, names(groups), stringsAsFactors = FALSE)
# is <- combinations[[1]]
# gs <- combinations[[2]]
# 
# results <- dplyr::bind_rows(
#   map2(is,
#        gs,
#        svy_summary_weights_v2,
#        data = data,
#        g_l1 = "year_fct",
#        iref = INDICATORS_REFLIST_LVL_BORR,
#        gref = groups,
#        psu = "psu",
#        strata = NULL,
#        w = "probweights"
#   )
# )
# 
# ggplot(data = results, 
#        aes(
#          x = year_fct, 
#          y = mean, 
#          fill = indicator_name
#        )
#        ) + 
#   facet_wrap(~group_cat_val, nrow = 1) + 
#   geom_col(position = position_stack()) + 
#   geom_text(aes(label = pctclean(mean, 0)), position = position_stack(vjust = 0.5)) + 
#   theme_custom()



