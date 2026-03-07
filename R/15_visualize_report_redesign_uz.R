local_lib <- file.path(getwd(), "r_libs")
if (dir.exists(local_lib)) {
  .libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(forcats)
})

# =============================================================================
# 15_visualize_report_redesign_uz.R
# Focused redesign pack based on visual audit of 14_visualize_report_recheck_uz.R
# =============================================================================
# Inputs:
#   data/intermediate/04_analytical.rds
# Outputs:
#   outputs/figures/redesign_uz/*.png
# =============================================================================

df <- readRDS("data/intermediate/04_analytical.rds") %>%
  mutate(
    repay_group = recode(as.character(repay_group),
      "On-time" = "Ўз вақтида тўловчи",
      "1-3 month delay" = "1-3 ой кечикувчи",
      "NPL (3+ months)" = "NPL"
    ),
    repay_group = factor(repay_group, levels = c("Ўз вақтида тўловчи", "1-3 ой кечикувчи", "NPL"), ordered = TRUE)
  )

borrowers <- df %>% filter(has_loan == 1)
out_dir <- file.path("outputs", "figures", "redesign_uz")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

col_on_time   <- "#2E8B57"
col_delay     <- "#E58F00"
col_npl       <- "#C0392B"
col_formal    <- "#2C6E91"
col_close     <- "#D97706"
col_struct    <- "#4E79A7"
col_neutral   <- "#7A7A7A"
col_light     <- "#CFD8DC"
col_low_n     <- "#C44536"

palette_status <- c(
  "Ўз вақтида тўловчи" = col_on_time,
  "1-3 ой кечикувчи" = col_delay,
  "NPL" = col_npl
)

theme_redesign <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0, lineheight = 1.05),
      plot.subtitle = element_text(colour = "grey35"),
      plot.caption = element_text(colour = "grey35", hjust = 0),
      axis.title = element_text(size = rel(0.92)),
      axis.text = element_text(size = rel(0.9)),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(10, 24, 10, 10)
    )
}

save_fig <- function(name, w = 10.5, h = 6.2, dpi = 170) {
  ggsave(
    filename = file.path(out_dir, paste0(name, ".png")),
    width = w,
    height = h,
    dpi = dpi,
    bg = "white"
  )
  cat(sprintf("  [OK] %s.png\n", name))
}

classify_primary_source <- function(x) {
  case_when(
    x %in% c(
      "Банк ташкилотларига",
      "Микромолия ташкилотларига",
      "Расмий насия хизматларига (“Техномарт”, “Узум”, “Ишонч” в.б.)"
    ) ~ "Расмий",
    x %in% c("Оила аъзолари, дўстлар ёки танишларга") ~ "Яқинлар",
    x %in% c(
      "Кўчада фоиз эвазига пул (қарз) берувчи норасмий шахсларга",
      "Норасмий насия хизматига (бозорлар, дўконлар в.б.)",
      "Ломбардларга"
    ) ~ "Норасмий",
    x %in% c("Қарз, кредит, насия олмасдан бошқа ечим қидирар эдим") ~ "Қарз олмаслик",
    TRUE ~ "Бошқа"
  )
}

source_palette <- c(
  "Расмий" = col_formal,
  "Яқинлар" = col_close,
  "Норасмий" = col_npl,
  "Қарз олмаслик" = col_neutral,
  "Бошқа" = col_light
)

status_n_text <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  count(repay_group) %>%
  mutate(txt = sprintf("%s: n=%d", repay_group, n)) %>%
  pull(txt) %>%
  paste(collapse = " | ")


wrap_axis <- function(width = 20) {
  scale_x_discrete(labels = \(x) stringr::str_wrap(x, width))
}

cat("\n--- Redesign Pack ---\n")

# -----------------------------------------------------------------------------
# RD01. Region sample size
# -----------------------------------------------------------------------------
reg_n <- df %>%
  filter(!is.na(region)) %>%
  count(region, name = "n") %>%
  mutate(region = fct_reorder(str_wrap(region, 24), n))

ggplot(reg_n, aes(x = n, y = region)) +
  geom_col(fill = col_struct, width = 0.72) +
  geom_text(aes(label = sprintf("n=%d", n)), hjust = -0.1, size = 3.7) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Ҳудудлар бўйича танланма ҳажми",
    subtitle = sprintf("Барча респондентлар, n=%d", nrow(df)),
    x = "Респондентлар сони",
    y = NULL
  ) +
  theme_redesign()
save_fig("rd01_region_sample_size")

# -----------------------------------------------------------------------------
# RD02. Borrower share by region
# -----------------------------------------------------------------------------
reg_share <- df %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    n = n(),
    borrower_share = 100 * mean(has_loan == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    low_n = n < 120,
    region = fct_reorder(str_wrap(region, 24), borrower_share)
  )

ggplot(reg_share, aes(x = borrower_share, y = region)) +
  geom_segment(aes(x = 0, xend = borrower_share, yend = region), colour = "grey80", linewidth = 0.8) +
  geom_point(aes(colour = low_n), size = 4.2, alpha = 0.95, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%% | n=%d", borrower_share, n), x = borrower_share + 2.2), hjust = 0, size = 3.2) +
  scale_colour_manual(values = c("TRUE" = col_low_n, "FALSE" = col_struct)) +
  scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.20))) +
  labs(
    title = "Ҳудудлар бўйича қарздорлар улуши",
    subtitle = "Ҳар бир нуқта ёнида n кўрсатилган; қизил нуқталар n<120 бўлган ҳудудлар",
    x = "Қарздор респондентлар улуши",
    y = NULL,
    caption = "Бу график структурани кўрсатади; миллий улуш сифатида талқин қилинмаслиги керак."
  ) +
  theme_redesign()
save_fig("rd02_region_borrower_share", h = 6.5)

# -----------------------------------------------------------------------------
# RD03. Primary credit source preference (top categories)
# -----------------------------------------------------------------------------
source_top <- df %>%
  filter(!is.na(credit_source_primary)) %>%
  count(credit_source_primary, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n),
    source_type = classify_primary_source(credit_source_primary),
    label = case_when(
      credit_source_primary == "Расмий насия хизматларига (“Техномарт”, “Узум”, “Ишонч” в.б.)" ~ "Расмий насия хизматлари",
      credit_source_primary == "Қарз, кредит, насия олмасдан бошқа ечим қидирар эдим" ~ "Қарз олмасдан бошқа ечим қидирар эдим",
      TRUE ~ credit_source_primary
    ),
    label = str_wrap(label, 28)
  ) %>%
  arrange(desc(n)) %>%
  slice_head(n = 8) %>%
  mutate(label = fct_reorder(label, n))

ggplot(source_top, aes(x = n, y = label, fill = source_type)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("n=%d | %.1f%%", n, pct)), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = source_palette) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Биринчи мурожаат қилинадиган қарз манбалари",
    subtitle = str_wrap(sprintf("Барча респондентлар, n=%d. Ранглар: расмий, яқинлар, норасмий ва қолган тоифалар.", nrow(df)), width = 68),
    x = "Респондентлар сони",
    y = NULL
  ) +
  theme_redesign()
save_fig("rd03_primary_credit_source_top")

# -----------------------------------------------------------------------------
# RD04. Loan purpose ranking (top 8)
# -----------------------------------------------------------------------------
purp_cols <- c(
  "loan_purpose__daily_expenses", "loan_purpose__healthcare",
  "loan_purpose__appliances",     "loan_purpose__auto",
  "loan_purpose__education",      "loan_purpose__wedding",
  "loan_purpose__mortgage",       "loan_purpose__renovation",
  "loan_purpose__debt_repayment", "loan_purpose__migration_work",
  "loan_purpose__business",       "loan_purpose__family_entrepreneurship",
  "loan_purpose__help_relatives", "loan_purpose__other"
)

purp_labels <- c(
  "Кунлик харажатлар", "Соғлиқ/Таълим", "Маиший техника", "Автомобил",
  "Таълим", "Тўй/Маросим", "Ипотека", "Таъмирлаш", "Бошқа қарзни тўлаш",
  "Миграция", "Бизнес", "Оилавий тадбиркорлик", "Қариндошларга ёрдам", "Бошқа"
)
names(purp_labels) <- purp_cols

purp_top <- borrowers %>%
  summarise(across(all_of(purp_cols), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "purpose", values_to = "n") %>%
  mutate(
    pct = 100 * n / nrow(borrowers),
    label = purp_labels[purpose]
  ) %>%
  arrange(desc(n)) %>%
  slice_head(n = 8) %>%
  mutate(label = fct_reorder(label, n))

ggplot(purp_top, aes(x = n, y = label)) +
  geom_col(fill = col_neutral, width = 0.72) +
  geom_text(aes(label = sprintf("n=%d | %.1f%%", n, pct)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(
    title = "Қарз мақсадлари: энг кўп учрайдиган йўналишлар",
    subtitle = sprintf("Қарздорлар, n=%d | Топ-8 мақсад", nrow(borrowers)),
    x = "Белгиланган ҳолатлар сони",
    y = NULL,
    caption = "Кўп жавобли савол: улушлар йиғиндиси 100%% бўлмаслиги мумкин."
  ) +
  theme_redesign()
save_fig("rd04_loan_purpose_top8")

# -----------------------------------------------------------------------------
# RD05. Purpose by repayment status (faceted horizontal bars)
# -----------------------------------------------------------------------------
key_purps <- c(
  "loan_purpose__daily_expenses", "loan_purpose__appliances",
  "loan_purpose__wedding",        "loan_purpose__debt_repayment",
  "loan_purpose__business",       "loan_purpose__family_entrepreneurship"
)

purp_status <- borrowers %>%
  filter(!is.na(repay_group)) %>%
  group_by(repay_group) %>%
  summarise(across(all_of(key_purps), ~ 100 * mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-repay_group, names_to = "purpose", values_to = "pct") %>%
  mutate(label = purp_labels[purpose])

order_tbl <- purp_status %>%
  group_by(label) %>%
  summarise(max_pct = max(pct), .groups = "drop")

purp_status <- purp_status %>%
  left_join(order_tbl, by = "label") %>%
  mutate(label = fct_reorder(label, max_pct))

ggplot(purp_status, aes(x = pct, y = label, fill = repay_group)) +
  geom_col(show.legend = FALSE, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.3) +
  facet_wrap(~ repay_group, nrow = 1) +
  scale_fill_manual(values = palette_status) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title = "9-расм.Тўлов ҳолатига кўра олинган қарз мақсадлари",
    subtitle = status_n_text,
    x = "Улуш (%)",
    y = NULL
  ) +
  theme_redesign() +
  theme(strip.text = element_text(face = "bold"))
save_fig("rd05_purpose_by_status_faceted", w = 13.2, h = 6.6)

# -----------------------------------------------------------------------------
# RD06. NPL rate by credit type (lollipop)
# -----------------------------------------------------------------------------
npl_type <- borrowers %>%
  filter(!is.na(credit_type_label)) %>%
  group_by(credit_type_label) %>%
  summarise(
    n = n(),
    npl_rate = 100 * mean(is_npl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  mutate(
    low_n = n < 30,
    credit_type_label = recode(
      credit_type_label,
      "Истеъмол кредити (consumer)" = "Истеъмол кредити",
      "Оилавий кредит (family business)" = "Оилавий кредит",
      "Ипотека (mortgage)" = "Ипотека",
      .default = credit_type_label
    ),
    credit_type_label = fct_reorder(credit_type_label, npl_rate)
  )

ggplot(npl_type, aes(x = npl_rate, y = credit_type_label)) +
  geom_segment(aes(x = 0, xend = npl_rate, yend = credit_type_label), colour = "grey80", linewidth = 0.8) +
  geom_point(aes(colour = low_n), size = 4.5, alpha = 0.95, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%% | n=%d", npl_rate, n), x = npl_rate + 2.2), hjust = 0, size = 3.5) +
  scale_colour_manual(values = c("TRUE" = col_low_n, "FALSE" = col_npl)) +
  scale_x_continuous(labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Кредит тури бўйича NPL даражаси",
    subtitle = "Ҳар бир нуқта ёнида n кўрсатилган; қизил нуқталар n<30 бўлган кредит турлари",
    x = "NPL даражаси",
    y = NULL,
    caption = "Бу график қиёсий тавсиф учун; кичик гуруҳлар эҳтиёткор талқин қилиниши керак."
  ) +
  theme_redesign()
save_fig("rd06_npl_by_credit_type_lollipop")

# -----------------------------------------------------------------------------
# RD07. Q2.3 average borrowing amount by source
# -----------------------------------------------------------------------------
q23_cols <- grep("^[0-9]+-2\\.3", names(df), value = TRUE)

q23_src_labels <- c(
  "Банк",
  "Расмий насия\n(техно-дўкон платформалари)",
  "Норасмий насия\n(бозор ва дўконлар)",
  "Кўчада фоиз\nберувчи шахс",
  "Микромолия\nташкилоти",
  "Ломбард",
  "Оила аъзолари /\nДўстлар",
  "Бошқа манба"
)

if (length(q23_cols) >= 8) {
  q23_means <- df %>%
    summarise(across(all_of(q23_cols[1:8]), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "mean_mln") %>%
    mutate(
      source = q23_src_labels[seq_len(n())],
      source = fct_reorder(source, mean_mln),
      source_type = case_when(
        str_detect(source, "Банк|Микромолия|Расмий насия") ~ "Расмий",
        str_detect(source, "Оила") ~ "Яқинлар",
        TRUE ~ "Норасмий"
      )
    )

  q23_palette <- c(
    "Расмий" = col_formal,
    "Яқинлар" = col_close,
    "Норасмий" = col_npl
  )

  ggplot(q23_means, aes(x = mean_mln, y = source, fill = source_type)) +
    geom_col(width = 0.72, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f млн", mean_mln)), hjust = -0.1, size = 3.7) +
    scale_fill_manual(values = q23_palette) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.26))) +
    labs(
      title = str_wrap("Q2.3. Қарз олиш манбаларига кўра олинадиган ўртача қарз миқдори", width = 52),
      subtitle = str_wrap("Ранглар манба турини кўрсатади; қийматлар ўртача сумма сифатида берилган", width = 62),
      x = "Ўртача сумма (млн сўм)",
      y = NULL
    ) +
    theme_redesign()
  save_fig("rd07_q23_amount_by_source", h = 6.1)
} else {
  message("[SKIP rd07_q23_amount_by_source] Q2.3 ustunlari yetarli emas.")
}

cat("\n=== Redesign pack complete ===\n")