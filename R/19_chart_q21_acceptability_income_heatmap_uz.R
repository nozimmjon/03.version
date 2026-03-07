local_lib <- file.path(getwd(), "r_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
})

infile  <- file.path("data", "intermediate", "04_analytical.rds")
outdir  <- file.path("outputs", "figures", "enrichment")
outfile <- file.path(outdir, "EH7_q21_acceptability_by_income_heatmap.png")

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

df <- readRDS(infile)

q_mobile <- "3-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Уяли алоқа воситасини сотиб олиш учун"
q_wedding <- "6-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Тўй ёки оилавий маросимлар ўтказиш учун"

need <- c("income_midpoint_mln_uzs", "hh_size", q_mobile, q_wedding)
stopifnot(all(need %in% names(df)))

plot_df <- df %>%
  transmute(
    pc_income_mln = income_midpoint_mln_uzs / hh_size,
    `Уяли алоқа воситаси` = as.integer(.data[[q_mobile]]),
    `Тўй/оилавий маросим` = as.integer(.data[[q_wedding]])
  ) %>%
  filter(!is.na(pc_income_mln)) %>%
  mutate(
    income_group = if_else(pc_income_mln < 0.7, "Паст даромад\n(<700 минг сўм)", "Юқори даромад\n(>=700 минг сўм)")
  ) %>%
  pivot_longer(cols = c(`Уяли алоқа воситаси`, `Тўй/оилавий маросим`), names_to = "purpose", values_to = "ok") %>%
  filter(!is.na(ok)) %>%
  group_by(income_group, purpose) %>%
  summarise(
    n = n(),
    pct_ok = 100 * mean(ok == 1L),
    .groups = "drop"
  ) %>%
  mutate(
    income_group = factor(income_group, levels = c("Паст даромад\n(<700 минг сўм)", "Юқори даромад\n(>=700 минг сўм)")),
    purpose = factor(purpose, levels = c("Уяли алоқа воситаси", "Тўй/оилавий маросим")),
    label = sprintf("%.1f%%\n(n=%d)", pct_ok, n)
  )

sub_txt <- paste0(
  "Жон бошига даромад = уй хўжалиги даромади мидпойнти / уй аъзолари сони. ",
  "Кўрсаткичлар deyarli bir xil: уяли алоқа воситаси — 18.2% vs 20.0%; ",
  "тўй/оилавий маросим — 23.7% vs 24.3%."
)

p <- ggplot(plot_df, aes(x = income_group, y = purpose, fill = pct_ok)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = label), size = 4.0, lineheight = 0.95) +
  scale_fill_gradient(low = "#edf8fb", high = "#006d2c", labels = label_number(suffix = "%")) +
  labs(
    title = str_wrap("EH7. Зарур бўлмаган товар ва маросимлар учун кредит олишни мақбул деб ҳисоблаш: жон бошига даромад кесимида", width = 55),
    subtitle = str_wrap(sub_txt, width = 85),
    x = NULL,
    y = NULL,
    fill = "Мақбул деб билганлар улуши"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "grey35"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold")
  )

ggsave(outfile, p, width = 9.5, height = 5.8, dpi = 180, bg = "white")
cat(sprintf("[OK] %s\n", outfile))