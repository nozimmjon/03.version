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
  library(forcats)
})

infile <- file.path("data", "intermediate", "04_analytical.rds")
outdir <- file.path("outputs", "figures", "enrichment")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

df <- readRDS(infile)

q_mobile <- "3-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Уяли алоқа воситасини сотиб олиш учун"
q_auto <- "5-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Автомобил хариди учун"
q_wedding <- "6-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Тўй ёки оилавий маросимлар ўтказиш учун"
q_appliances <- "7-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Маиший техника, уй жиҳозлари (мебел, ошҳона жиҳозлари в.б.)"
q_other <- "10-2.1. Сизнинг фикрингизча, одамлар қандай мақсадларда қарз, кредит ёки насия олиши мақсадга мувофиқ?/Бошқа мақсадлар учун (сайёҳат, чет элда ишлаш ва ҳ.к)"

need <- c("income_midpoint_mln_uzs", "hh_size", q_mobile, q_auto, q_wedding, q_appliances, q_other)
stopifnot(all(need %in% names(df)))

base_df <- df %>%
  transmute(
    pc_income_mln = income_midpoint_mln_uzs / hh_size,
    income_group = case_when(
      is.na(pc_income_mln) ~ NA_character_,
      pc_income_mln < 0.7 ~ "Паст даромад\n(<700 минг сўм)",
      TRUE ~ "Юқори даромад\n(>=700 минг сўм)"
    ),
    mobile = as.integer(.data[[q_mobile]]),
    auto = as.integer(.data[[q_auto]]),
    wedding = as.integer(.data[[q_wedding]]),
    appliances = as.integer(.data[[q_appliances]]),
    other = as.integer(.data[[q_other]])
  ) %>%
  filter(!is.na(income_group)) %>%
  mutate(income_group = factor(income_group, levels = c("Паст даромад\n(<700 минг сўм)", "Юқори даромад\n(>=700 минг сўм)")))

item_labels <- c(
  mobile = "Уяли алоқа воситаси",
  wedding = "Тўй/оилавий маросим",
  other = "Бошқа мақсадлар\n(сайёҳат ва ҳ.к.)",
  appliances = "Маиший техника/\nуй жиҳозлари",
  auto = "Автомобил хариди"
)

# EH7: item-level heatmap for discretionary purposes
heat_df <- base_df %>%
  select(income_group, mobile, wedding, other, appliances, auto) %>%
  pivot_longer(cols = -income_group, names_to = "purpose", values_to = "ok") %>%
  filter(!is.na(ok)) %>%
  group_by(income_group, purpose) %>%
  summarise(n = n(), pct_ok = 100 * mean(ok == 1L), .groups = "drop") %>%
  mutate(
    purpose = factor(purpose, levels = c("mobile", "wedding", "other", "appliances", "auto"), labels = item_labels[c("mobile", "wedding", "other", "appliances", "auto")]),
    label = sprintf("%.1f%%", pct_ok)
  )

sub1 <- paste0(
  "Зарур бўлмаган нарсаларни қарзга сотиб олишга муносабат даромадлар кесимида кескин фарқ қилмайди."
)

p1 <- ggplot(heat_df, aes(x = income_group, y = purpose, fill = pct_ok)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = label), size = 3.5, lineheight = 0.95) +
  scale_fill_gradient(low = "#edf8fb", high = "#006d2c") +
  # scale_fill_gradient(low = "#edf8fb", high = "#006d2c", labels = label_number(suffix = "%")) +
  labs(
    title = str_wrap("Зарур бўлмаган товар ва маросимлар учун кредит олишни мақбул деб ҳисоблаш ҳолати", width = 70),
    subtitle = str_wrap(sub1, width = 70),
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "grey35"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold")
  )

ggsave(file.path(outdir, "EH7_q21_discretionary_items_by_income_heatmap.png"), p1, width = 10.2, height = 6.8, dpi = 180, bg = "white")

# EH8: respondent-level grouped summary
summary_df <- base_df %>%
  transmute(
    income_group,
    strict_core = if_else(rowSums(cbind(mobile, wedding, other) == 1L, na.rm = TRUE) > 0, 1L, 0L),
    expanded = if_else(rowSums(cbind(mobile, wedding, other, appliances, auto) == 1L, na.rm = TRUE) > 0, 1L, 0L),
    strict_complete = rowSums(is.na(cbind(mobile, wedding, other))) == 0,
    expanded_complete = rowSums(is.na(cbind(mobile, wedding, other, appliances, auto))) == 0
  ) %>%
  group_by(income_group) %>%
  summarise(
    strict_core = 100 * mean(strict_core[strict_complete] == 1L),
    strict_n = sum(strict_complete),
    expanded = 100 * mean(expanded[expanded_complete] == 1L),
    expanded_n = sum(expanded_complete),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(strict_core, expanded), names_to = "grouping", values_to = "pct_ok") %>%
  mutate(
    n = if_else(grouping == "strict_core", strict_n, expanded_n),
    grouping = factor(grouping, levels = c("strict_core", "expanded"), labels = c("Қатъий тоифа\n(уяли алоқа, тўй, бошқа)", "Кенгайтирилган тоифа\n(+ маиший техника, авто)")),
    label = sprintf("%.1f%%\n(n=%d)", pct_ok, n)
  )

sub2 <- paste0(
  "Қатъий тоифа: уяли алоқа воситаси, тўй/оилавий маросим ва бошқа мақсадлар. ",
  "Кенгайтирилган тоифа: буларга қўшимча маиший техника/уй жиҳозлари ва автомобил хариди."
)

p2 <- ggplot(summary_df, aes(x = income_group, y = pct_ok, fill = grouping)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(aes(label = label), position = position_dodge(width = 0.72), vjust = -0.25, size = 3.5, lineheight = 0.95) +
  scale_fill_manual(values = c("Қатъий тоифа\n(уяли алоқа, тўй, бошқа)" = "#3182bd", "Кенгайтирилган тоифа\n(+ маиший техника, авто)" = "#31a354")) +
  scale_y_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = str_wrap("EH8. Камида битта зарур бўлмаган ёки баҳсли истеъмол мақсади учун кредит олишни мақбул деб ҳисоблайдиганлар улуши", width = 58),
    subtitle = str_wrap(sub2, width = 90),
    x = NULL,
    y = "Респондентлар улуши",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "grey35"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "EH8_q21_discretionary_group_summary.png"), p2, width = 10.2, height = 6.2, dpi = 180, bg = "white")

cat('[OK] outputs/figures/enrichment/EH7_q21_discretionary_items_by_income_heatmap.png\n')
cat('[OK] outputs/figures/enrichment/EH8_q21_discretionary_group_summary.png\n')