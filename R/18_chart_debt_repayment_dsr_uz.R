local_lib <- file.path(getwd(), 'r_libs')
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = '/', mustWork = FALSE), .libPaths()))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(forcats)
})

df <- readRDS('data/intermediate/04_analytical.rds')
borrowers <- df %>% filter(has_loan == 1)

stopifnot(all(c('loan_purpose__debt_repayment', 'dsr_midpoint', 'dsr_cat') %in% names(borrowers)))

sub_all <- borrowers %>% filter(loan_purpose__debt_repayment == 1)
sub_obs <- sub_all %>% filter(!is.na(dsr_midpoint))

n_all <- nrow(sub_all)
n_obs <- nrow(sub_obs)
n_missing <- n_all - n_obs
n_over35 <- sum(sub_obs$dsr_midpoint > 0.35, na.rm = TRUE)
share_obs <- 100 * n_over35 / n_obs
share_all <- 100 * n_over35 / n_all

plot_df <- sub_obs %>%
  count(dsr_cat, name = 'n') %>%
  mutate(
    pct = 100 * n / sum(n),
    high_dsr = as.character(dsr_cat) %in% c('35% дан – 50%гача', '50%дан кўп'),
    dsr_cat = factor(as.character(dsr_cat), levels = levels(sub_obs$dsr_cat), ordered = TRUE),
    label = sprintf('%.1f%% | n=%d', pct, n)
  )

theme_uz <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = 'bold', hjust = 0, lineheight = 1.05),
      plot.subtitle = element_text(colour = 'grey40'),
      plot.caption = element_text(colour = 'grey40', hjust = 0),
      axis.title = element_text(size = rel(0.92)),
      axis.text = element_text(size = rel(0.9)),
      legend.position = 'none',
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(10, 24, 10, 10)
    )
}

p <- ggplot(plot_df, aes(x = pct, y = fct_rev(dsr_cat), fill = high_dsr)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.8) +
  scale_fill_manual(values = c('TRUE' = '#d62728', 'FALSE' = '#9aa5b1')) +
  scale_x_continuous(labels = label_number(suffix = '%'), expand = expansion(mult = c(0, 0.24)), limits = c(0, 40)) +
  labs(
    title = '44-расм. Бошқа қарзни ёпиш учун кредит олганларда DSR тақсимоти',
    subtitle = sprintf('DSR маълум бўлган 35 нафар ичида 22 нафарида DSR 35%%дан юқори: 62.9%%. Агар барча 39 нафар олинса, бу улуш 56.4%% бўлади.'),
    x = 'Улуш (%)',
    y = NULL,
    caption = sprintf('Танланма: бошқа қарзни ёпиш мақсадини кўрсатган қарздорлар. DSR маълум эмас: n=%d.', n_missing)
  ) +
  theme_uz()

dir.create('outputs/figures/final_uz', recursive = TRUE, showWarnings = FALSE)
ggsave('outputs/figures/final_uz/chart_44_debt_repayment_dsr.png', p, width = 10.8, height = 5.8, dpi = 170, bg = 'white')
cat('OK: outputs/figures/final_uz/chart_44_debt_repayment_dsr.png\n')
