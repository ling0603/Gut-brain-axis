library(data.table)

input_dir <- "../pairwise_interactions/with_interaction_types"

date_day_lookup <- data.table(
  date_code = c(
    "0507", "0508", "0509", "0510", "0511",
    "0512", "0513", "0514", "0515", "0516", "0517",
    "0602", "0603", "0604", "0605", "0606",
    "0607", "0608", "0609", "0610", "0611", "0612"
  ),
  day = rep(0:10, 2),
  batch = rep(1:2, each = 11)
)

files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

all_interactions <- rbindlist(
  lapply(files, function(file) {
    dt <- fread(file)
    dt[, source_csv := basename(file)]
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)

# Extract date from source_video
all_interactions[
  ,
  date_code := substr(
    regmatches(
      source_video,
      regexpr("26[0-9]{4}", source_video)
    ),
    3,
    6
  )
]

# Add day and batch
all_interactions[
  date_day_lookup,
  on = "date_code",
  `:=`(
    day = i.day,
    batch = i.batch
  )
]
