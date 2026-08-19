library(data.table)

################################################################
# Filter all segment-filled CSV files by colony-specific tags


input_dir <- "../segment filled data"
output_dir <- file.path(input_dir, "wanted_tags_filtered")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

identity_column <- "filled_tag_identity"

# Indistinguishable categories:
# 6 & 9   -> 6
# 66 & 99 -> 66
# 16 & 91 -> 16
# 19 & 61 -> 19
# 18 & 81 -> 18
# 68 & 89 -> 68
# 86 & 98 -> 86
tag_conversion <- c(
  "9"  = "6",
  "99" = "66",
  "91" = "16",
  "61" = "19",
  "81" = "18",
  "89" = "68",
  "98" = "86"
)

# define the known tag numbers
wanted_tags_by_colony <- list(
  colony1 = c(
    15,57,67,77,79,80,18,82,84,85,68,90,93,94,95,66,
    100,2,4,5,10,36,37,38,42,43,44,45,46,47,48
  ),
  colony3 = c(
    65,1,2,4,5,6,10,12,14,15,19,20,24,25,26,29,
    30,31,34,36,40,42,43,45,46,47,48,51,52,53,54
  ),
  colony4 = c(
    17,65,66,68,69,74,75,76,79,80,18,82,84,88,90,
    16,92,93,94,95,10,11,12,1,2,3,4,5,6,13
  ),
  colony6 = c(
    67,1,2,3,4,6,8,12,13,14,15,16,17,19,20,21,22,
    24,28,29,30,51,52,54,55,56,59,60,62,64
  ),
  colony7 = c(
    93,48,54,55,59,60,19,62,63,65,70,71,73,74,75,
    79,83,84,85,68,16,21,22,23,24,25,26,29,30,31,32
  ),
  colony8 = c(
    52,8,6,11,14,19,23,25,29,30,31,33,34,35,36,39,
    41,44,46,50,51,56,59,60,62,63,64,65,53,54,55
  ),
  colony9 = c(
    20,74,79,18,82,83,85,86,68,16,92,95,96,1,2,3,
    4,5,10,12,15,51,52,58,27,40,42,44,45,47,50
  ),
  colony10 = c(
    80,13,19,27,28,31,32,33,34,35,36,39,40,50,51,
    55,56,59,60,69,70,93,94,95,100,1,3,4,90,16,92
  )
)

# Only process source files ending in "_segment_filled.csv".
# Files already written into output_dir will not be reprocessed.
csv_files <- list.files(
  input_dir,
  pattern = "_segment_filled\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

summary_list <- vector("list", length(csv_files))

for (i in seq_along(csv_files)) {
  file <- csv_files[i]
  filename <- basename(file)
  
  # Match colony10 before colony1, preventing colony10 from being mistaken for colony1.
  colony_match <- regmatches(
    tolower(filename),
    regexpr("colony(?:10|[1346789])", tolower(filename), perl = TRUE)
  )
  
  dt <- fread(file)
  
  if (!identity_column %in% names(dt)) {
    warning("Skipped because column '", identity_column, "' is absent: ", filename)
    summary_list[[i]] <- data.table(
      file = filename,
      colony = colony_match,
      rows_before = nrow(dt),
      rows_after = NA_integer_,
      status = paste0("skipped: missing ", identity_column)
    )
    next
  }
  
  rows_before <- nrow(dt)
  
  # Clean identities.
  dt[, (identity_column) := trimws(as.character(get(identity_column)))]
  
  dt <- dt[
    !is.na(get(identity_column)) &
      get(identity_column) != "" &
      !tolower(get(identity_column)) %in% c("unknown") #filter out remaining unknown
  ]
  
  # Convert indistinguishable tags to their canonical category.
  dt[
    get(identity_column) %in% names(tag_conversion),
    (identity_column) := unname(tag_conversion[get(identity_column)])
  ]
  
  # Keep only valid numeric identities from the correct colony.
  wanted <- as.character(wanted_tags_by_colony[[colony_match]])
  dt <- dt[get(identity_column) %in% wanted]
  
  output_name <- sub(
    "_segment_filled\\.csv$",
    "_segment_filled_wanted_tags.csv",
    filename,
    ignore.case = TRUE
  )
  output_file <- file.path(output_dir, output_name)
  
  fwrite(dt, output_file)
  
  summary_list[[i]] <- data.table(
    file = filename,
    colony = colony_match,
    rows_before = rows_before,
    rows_after = nrow(dt),
    status = "filtered and saved"
  )
  
  cat(
    sprintf(
      "[%d/%d] %s | %s | rows: %d -> %d\n",
      i, length(csv_files), filename, colony_match, rows_before, nrow(dt)
    )
  )
}
