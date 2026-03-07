local_lib <- file.path(getwd(), 'r_libs')
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = '/', mustWork = FALSE), .libPaths()))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

df <- readRDS('data/intermediate/04_analytical.rds')
borrowers <- df %>% filter(has_loan == 1)

stopifnot('n_loans' %in% names(borrowers))

base <- borrowers %>%
  filter(!is.na(n_loans), n_loans >= 1) %>%
  mutate(
    loans_grp = case_when(
      n_loans >= 4 ~ '4+',
      TRUE ~ as.character(n_loans)
    ),
    loans_grp = factor(loans_grp, levels = c('1', '2', '3', '4+'), ordered = TRUE)
  )

share_2plus <- 100 * mean(base$n_loans >= 2, na.rm = TRUE)
excluded_zero <- sum(borrowers$n_loans == 0, na.rm = TRUE)

plot_df <- base %>%
  count(loans_grp, name = 'n') %>%
  mutate(
    pct = 100 * n / sum(n),
    label = sprintf('%.1f%%', pct),
    highlight = loans_grp != '1'
  )

theme_uz <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0, lineheight = 1.05),
      plot.subtitle = element_text(colour = 'grey40'),
      plot.caption = element_text(colour = 'grey40', hjust = 0),
      axis.title = element_text(size = rel(0.92)),
      axis.text = element_text(size = rel(0.92)),
      legend.position = 'none',
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(10, 24, 10, 10)
    )
}

p <- ggplot(plot_df, aes(x = loans_grp, y = pct, fill = highlight)) +
  geom_col(width = 0.66) +
  geom_text(aes(label = label), vjust = -0.35, size = 3.9) +
  scale_fill_manual(values = c('TRUE' = '#d62728', 'FALSE' = '#9aa5b1')) +
  scale_y_continuous(labels = label_number(suffix = '%'), expand = expansion(mult = c(0, 0.18)), limits = c(0, 80)) +
  labs(
    title = '18-1-расм. Респондентлардаги қарзлар (кредитлар) сони бўйича тақсимот',
    x = 'Кредитлар (қарз/насия тўловлари) сони',
    y = 'Қарздорлар улуши (%)',
    ) +
  theme_uz()

dir.create('outputs/figures/final_uz', recursive = TRUE, showWarnings = FALSE)
ggsave('outputs/figures/final_uz/chart_43_multi_debt_share.png', p, width = 9.8, height = 5.8, dpi = 170, bg = 'white')
cat('OK: outputs/figures/final_uz/chart_43_multi_debt_share.png\n')
