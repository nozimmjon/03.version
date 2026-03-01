export_pipeline_outputs <- function(cleaned, split_data, manifest, cfg) {
  output_dir <- cfg$paths$output_dir
  ensure_output_dir(output_dir)

  cleaned_path <- file.path(output_dir, cfg$exports$cleaned_master_csv)
  borrowers_path <- file.path(output_dir, cfg$exports$borrowers_csv)
  nonborrowers_path <- file.path(output_dir, cfg$exports$nonborrowers_csv)
  manifest_path <- file.path(output_dir, cfg$exports$run_manifest_csv)

  write_csv_utf8(cleaned, cleaned_path)
  write_csv_utf8(split_data$borrowers, borrowers_path)
  write_csv_utf8(split_data$nonborrowers, nonborrowers_path)
  write_csv_utf8(manifest, manifest_path)

  tibble::tibble(
    artifact = c("cleaned_master", "borrowers", "nonborrowers", "run_manifest"),
    path = c(cleaned_path, borrowers_path, nonborrowers_path, manifest_path)
  )
}
