# 20_charts.R
source(here::here("R/00_setup.R"))
source(here::here("R/02_clean.R"))

plot_extra_funds_sources <- function(dat) {
  d <- dat %>%
    filter(!is.na(extra_funds_source)) %>%
    count(extra_funds_source) %>%
    mutate(share = n / sum(n)) %>%
    arrange(share)

  ggplot(d, aes(x = share, y = reorder(extra_funds_source, share))) +
    geom_col() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Sources used when extra money is needed",
         subtitle = glue("Base: all respondents, N={sum(d$n)}"),
         caption = "Survey question 2.2") +
    theme_report()
}

plot_extra_funds_by_region <- function(dat) {
  d <- dat %>%
    filter(!is.na(extra_funds_source), !is.na(region)) %>%
    count(region, extra_funds_source) %>%
    group_by(region) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  # keep top sources overall to avoid unreadable stacks
  top_sources <- dat %>%
    filter(!is.na(extra_funds_source)) %>%
    count(extra_funds_source, sort = TRUE) %>%
    slice_head(n = 6) %>%
    pull(extra_funds_source)

  d <- d %>%
    mutate(extra_funds_source = if_else(extra_funds_source %in% top_sources, as.character(extra_funds_source), "Other")) %>%
    group_by(region, extra_funds_source) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  ggplot(d, aes(x = region, y = share, fill = extra_funds_source)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Sources of borrowing by region",
         subtitle = "Shares within each region; top 6 sources shown explicitly",
         caption = "Survey question 2.2") +
    theme_report() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_late_payment_reasons <- function(dat, reason_cols) {
  # reason cols are typically 0/1 or yes/no; treat non-missing as selected where == 1 or == 'yes'
  long <- dat %>%
    filter(borrower) %>%
    # IMPORTANT: columns can be mixed types (double/character/logical). Coerce before pivot_longer
    # to avoid vctrs type-combine errors.
    select(all_of(reason_cols)) %>%
    mutate(across(all_of(reason_cols), ~ as.character(.x))) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_longer(-row, names_to = "reason", values_to = "val") %>%
    mutate(selected = case_when(
      is.na(val) ~ FALSE,
      # after coercion, everything is character
      str_to_lower(trimws(val)) %in% c("1", "true", "yes", "ha", "да", "xa") ~ TRUE,
      # occasionally multi-response columns contain the option label itself
      # (e.g., a text string copied into the cell). Treat any non-empty, non-zero text as selected.
      str_to_lower(trimws(val)) %in% c("0", "false", "no", "yo'q", "йўқ", "нет") ~ FALSE,
      nzchar(trimws(val)) ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    filter(selected) %>%
    count(reason, sort = TRUE) %>%
    mutate(reason = str_replace_all(reason, "_", " "),
           share = n / sum(n)) %>%
    slice_head(n = 10) %>%
    arrange(share)

  ggplot(long, aes(x = share, y = reorder(reason, share))) +
    geom_col() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Main reasons for delaying financial obligations",
         subtitle = glue("Base: borrowers who reported reasons; top 10 shown"),
         caption = "Survey block 3.7 (multi-response)") +
    theme_report()
}

plot_debt_burden <- function(dat) {
  d <- dat %>%
    filter(!is.na(debt_burden_share)) %>%
    count(debt_burden_share) %>%
    mutate(share = n / sum(n)) %>%
    arrange(share)

  ggplot(d, aes(x = share, y = reorder(debt_burden_share, share))) +
    geom_col() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Share of household income going to credit payments",
         subtitle = glue("Base: respondents with an answer, N={sum(d$n)}"),
         caption = "Survey question 2.4.2") +
    theme_report()
}

plot_microloan_purpose <- function(dat) {
  d <- dat %>%
    filter(credit_type == "mikroqarz", !is.na(loan_purpose_main)) %>%
    count(loan_purpose_main) %>%
    mutate(share = n / sum(n)) %>%
    arrange(share)

  ggplot(d, aes(x = share, y = reorder(loan_purpose_main, share))) +
    geom_col() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Main spending purposes of microloans",
         subtitle = glue("Base: microloan borrowers, N={sum(d$n)}"),
         caption = "Survey question 2.7") +
    theme_report()
}

plot_read_contract <- function(dat) {
  d <- dat %>%
    filter(borrower, !is.na(read_contract)) %>%
    count(read_contract) %>%
    mutate(share = n / sum(n)) %>%
    arrange(share)

  ggplot(d, aes(x = share, y = reorder(read_contract, share))) +
    geom_col() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Whether borrowers read the contract before signing",
         subtitle = glue("Base: borrowers, N={sum(d$n)}"),
         caption = "Survey question 3.4") +
    theme_report()
}
