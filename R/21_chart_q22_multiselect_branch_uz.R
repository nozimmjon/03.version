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

infile <- file.path("data", "intermediate", "01_raw.rds")
outdir <- file.path("outputs", "figures", "enrichment")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

df <- readRDS(infile)

inf_cols <- names(df)[startsWith(names(df), "2.2.а.")]
bank_cols <- names(df)[startsWith(names(df), "2.2.б")]
formal_cols <- names(df)[startsWith(names(df), "2.2.в.")]
noborrow_cols <- names(df)[startsWith(names(df), "2.2.г.")]

stopifnot(length(inf_cols) > 0, length(bank_cols) > 0, length(formal_cols) > 0, length(noborrow_cols) > 0)

plot_df <- tibble(
  group = c(
    "Тижорат банклари",
    "Норасмий каналлар\n(оила/дўст, кўча, норасмий насия)",
    "Бошқа расмий каналлар\n(расмий насия, МФТ, ломбард)",
    "Қарз олмасликни афзал кўради"
  ),
  n = c(
    sum(rowSums(!is.na(df[bank_cols])) > 0),
    sum(rowSums(!is.na(df[inf_cols])) > 0),
    sum(rowSums(!is.na(df[formal_cols])) > 0),
    sum(rowSums(!is.na(df[noborrow_cols])) > 0)
  )
) %>%
  mutate(
    pct = 100 * n / nrow(df),
    group = factor(group, levels = rev(group)),
    label = sprintf("n=%d | %.1f%%", n, pct)
  )

p <- ggplot(plot_df, aes(x = pct, y = group, fill = group)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = label), hjust = -0.08, size = 3.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = label_number(suffix = "%")) +
  scale_fill_manual(values = c(
    "Тижорат банклари" = "#1f77b4",
    "Норасмий каналлар\n(оила/дўст, кўча, норасмий насия)" = "#d62728",
    "Бошқа расмий каналлар\n(расмий насия, МФТ, ломбард)" = "#2ca02c",
    "Қарз олмасликни афзал кўради" = "#7a7a7a"
  )) +
  labs(
    title = "Q2.2 бўйича қарз манбалари: кўп жавобли маршрутлар кесимида",
    subtitle = str_wrap("Бу чизма raw сўров маршрутлари асосида тузилган. Q2.2 саволи кўп жавобли бўлгани учун улушлар 100%дан ошиб кетиши мумкин. Мавжуд raw экспортда манбаларнинг барча алоҳида вариантлари сақланмагани сабабли чизма аниқ манба эмас, балки маршрут/кластер даражасини кўрсатади.", 90),
    x = "Респондентлар улуши",
    y = NULL,
    caption = sprintf("База: барча респондентлар, n=%d. Маршрутлар: 2.2.а, 2.2.б, 2.2.в, 2.2.г.", nrow(df))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "grey35"),
    plot.caption = element_text(color = "grey40", hjust = 0),
    panel.grid.minor = element_blank()
  )

outfile <- file.path(outdir, "Q22_multiselect_branch_routes.png")
ggsave(outfile, p, width = 10.5, height = 6.2, dpi = 180, bg = "white")
cat(sprintf("[OK] %s\n", outfile))