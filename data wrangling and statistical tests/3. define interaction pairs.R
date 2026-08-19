library(data.table)

###########################################
#filter out head likelihood lower than 0.6

#read data
input_dir <- "../wanted_tags_filtered"
output_dir <- "../corrected_data"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(
  input_dir,
  pattern = "_segment_filled_wanted_tags\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

for (file in files) {
  
  dt <- fread(file)
  
  # Keep only rows with head_likelihood >= 0.6
  dt_filtered <- dt[
    !is.na(head_likelihood) &
      head_likelihood >= 0.6
  ]
  
  output_file <- file.path(
    output_dir,
    basename(file)
  )
  
  fwrite(dt_filtered, output_file)
  
  cat(
    basename(file),
    ": kept", nrow(dt_filtered),
    "of", nrow(dt), "rows\n"
  )
}


#########################
#define interaction pairs

#read data
input_dir <- "../corrected_data"
output_dir <- file.path(input_dir, "pairwise_interactions")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#setting 
distance_threshold <- 114
frame_column <- "frame"
identity_column <- "filled_tag_identity"
head_x_column <- "head_x"
head_y_column <- "head_y"
frames_per_chunk <- 1000L

csv_files <- list.files(
  input_dir,
  pattern = "_segment_filled_wanted_tags\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

all_video_summaries <- vector("list", length(csv_files))
processing_log <- vector("list", length(csv_files)) #get the filtering outcome for data report

for (file_index in seq_along(csv_files)) {
  file <- csv_files[file_index]
  filename <- basename(file)
  cat(sprintf("\n[%d/%d] %s\n", file_index, length(csv_files), filename))
  
  dt <- fread(file)
  required_columns <- c(frame_column, identity_column, head_x_column, head_y_column)
  missing_columns <- setdiff(required_columns, names(dt))
  
  #make a simpler data table
  d <- dt[, .(
    frame = get(frame_column),
    tag = trimws(as.character(get(identity_column))),
    head_x = suppressWarnings(as.numeric(get(head_x_column))), #make sure the data is numberic
    head_y = suppressWarnings(as.numeric(get(head_y_column)))
  )]
  
  numeric_tag <- suppressWarnings(as.numeric(d$tag))
  d[!is.na(numeric_tag), tag := as.character(as.integer(numeric_tag))]
  
  # One position per identity per frame. If duplicate rows exist, average their head positions.
  d <- d[, .(
    head_x = mean(head_x, na.rm = TRUE),
    head_y = mean(head_y, na.rm = TRUE)
  ), by = .(frame, tag)]
  
  setorder(d, frame, tag)
  unique_frames <- sort(unique(d$frame))
  frame_chunks <- split(unique_frames, ceiling(seq_along(unique_frames) / frames_per_chunk))
  chunk_results <- vector("list", length(frame_chunks))
  
  #divided to chunks, to reduce memory use for laptop
  for (chunk_index in seq_along(frame_chunks)) {
    x <- d[frame %in% frame_chunks[[chunk_index]]]
    
    pairs <- merge(
      x, x,
      by = "frame",
      allow.cartesian = TRUE,
      suffixes = c("_1", "_2")
    )
    
    #make sure not counting the same pair with different orders
    pairs <- pairs[tag_1 < tag_2]
    if (nrow(pairs) == 0L) next
    
    #calculate the distance of head
    pairs[, head_distance := sqrt(
      (head_x_1 - head_x_2)^2 +
        (head_y_1 - head_y_2)^2
    )]
    
    #count coocurrence frames
    #Each pair contributes at most one co-occurrence and one interaction outcome per frame per pair.
    pairs <- unique(
      pairs[, .(
        frame,
        tag_1,
        tag_2,
        interacted = head_distance < distance_threshold
      )],
      by = c("frame", "tag_1", "tag_2")
    )
    
    chunk_results[[chunk_index]] <- pairs[, .(
      cooccurrence_frames = uniqueN(frame),
      interaction_frames = uniqueN(frame[interacted])
    ), by = .(tag_1, tag_2)]
    
    cat(sprintf("  chunk %d/%d completed\n", chunk_index, length(frame_chunks)))
  }
  
  chunk_results <- Filter(Negate(is.null), chunk_results)
  if (length(chunk_results) == 0L) next
  
  pair_summary <- rbindlist(chunk_results)[, .(
    cooccurrence_frames = sum(cooccurrence_frames),
    interaction_frames = sum(interaction_frames)
  ), by = .(tag_1, tag_2)]
  
  pair_summary[, pair := paste(tag_1, tag_2, sep = "-")]
  
  video_base <- sub(
    "_segment_filled_wanted_tags\\.csv$",
    "",
    filename,
    ignore.case = TRUE
  )
  
  colony_match <- regmatches(
    tolower(filename),
    regexpr("colony(?:10|[1346789])", tolower(filename), perl = TRUE)
  )
  if (length(colony_match) == 0L || colony_match == "") colony_match <- NA_character_
  
  pair_summary[, `:=`(
    source_video = video_base,
    colony = colony_match
  )]
  
  pair_summary <- pair_summary[, .(
    source_video,
    colony,
    pair,
    tag_1,
    tag_2,
    cooccurrence_frames,
    interaction_frames
  )]
  
  setorder(pair_summary, tag_1, tag_2)
  
  output_file <- file.path(
    output_dir,
    paste0(video_base, "_pairwise_interactions.csv")
  )
  fwrite(pair_summary, output_file)
  all_video_summaries[[file_index]] <- pair_summary
  
  processing_log[[file_index]] <- data.table(
    file = filename,
    status = "completed",
    details = paste0(
      "valid rows=", nrow(d),
      "; unique frames=", uniqueN(d$frame),
      "; pairs=", nrow(pair_summary)
    )
  )
}

#########################################
#assign interaction types and treatments

#read data
input_dir <- paste0(
  "..",
  "pairwise_interactions"
)

output_dir <- file.path(input_dir, "with_interaction_types")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#define the types using tags
classification_lists <- list(
  colony1 = list(
    Q  = c(15),
    H  = c(57,67,77,79,80,81,82,84,85,89,90,93,94,95,99,100,2,4,5,10),
    MC = c(36,37,38),
    MD = c(42,43,44,45,46,47,48)
  ),
  colony3 = list(
    Q  = c(65),
    H  = c(1,2,4,5,6,10,12,14,15,19,20,24,25,26,29,30,31,34,36,40),
    MC = c(42,43,45,46,47,48,51),
    MD = c(52,53,54)
  ),
  colony4 = list(
    Q  = c(17),
    H  = c(65,66,68,69,74,75,76,79,80,81,82,84,88,89,90,91,92,93,94,95),
    MC = c(10,11,12),
    MD = c(1,2,3,4,5,6,13)
  ),
  colony6 = list(
    Q  = c(67),
    H  = c(1,2,3,4,6,8,12,13,14,15,16,17,19,20,21,22,24,28,29,30),
    MC = c(51,52,54,55,56,59,60),
    MD = c(61,62,64)
  ),
  colony7 = list(
    Q  = c(93),
    H  = c(48,54,55,59,60,61,62,63,65,70,71,73,74,75,79,83,84,85,89,91),
    MC = c(24,25,26,29,30,31,32),
    MD = c(21,22,23)
  ),
  colony8 = list(
    Q  = c(52),
    H  = c(8,9,11,14,19,23,25,29,30,31,33,34,35,36,39,41,44,46,50,51),
    MC = c(53,54,55),
    MD = c(56,59,60,62,63,64,65)
  ),
  colony9 = list(
    Q  = c(20),
    H  = c(74,79,81,82,83,85,86,89,91,92,95,96,1,2,3,4,5,10,12,15),
    MC = c(27,40,42,44,45,47,50),
    MD = c(51,52,58)
  ),
  colony10 = list(
    Q  = c(80),
    H  = c(13,19,27,28,31,32,33,34,35,36,39,40,50,51,55,56,59,60,69,70),
    MC = c(90,91,92),
    MD = c(93,94,95,100,1,3,4)
  )
)

#define treatments
colony_treatments <- data.table(
  colony = c(
    "colony1", "colony3", "colony4", "colony6",
    "colony7", "colony8", "colony9", "colony10"
  ),
  colony_treatment = c(
    "Antibiotic", "Sucrose", "Sucrose", "Antibiotic",
    "Antibiotic", "Antibiotic", "Sucrose", "Sucrose"
  ),
  newcomer_MC_n = c(3L, 7L, 3L, 7L, 7L, 3L, 7L, 3L),
  newcomer_MD_n = c(7L, 3L, 7L, 3L, 3L, 7L, 3L, 7L)
)

colony_treatments[
  ,
  newcomer_ratio := paste0("MC", newcomer_MC_n, "_MD", newcomer_MD_n)
]

colony_treatments[
  ,
  treatment_group := paste(colony_treatment, newcomer_ratio, sep = "_")
]

# Build one lookup table.
lookup_parts <- list()
k <- 1L

for (colony_name in names(classification_lists)) {
  for (class_name in names(classification_lists[[colony_name]])) {
    original <- classification_lists[[colony_name]][[class_name]]
    
    lookup_parts[[k]] <- data.table(
      colony = colony_name,
      original_tag = as.character(original),
      converted_tag = convert_tags(original),
      bee_class = class_name
    )
    k <- k + 1L
  }
}

lookup_raw <- rbindlist(lookup_parts)

# Identify any converted tag that maps to multiple biological classes.(Mistakes made to have duplicated number tags)
lookup <- lookup_raw[
  ,
  .(
    bee_class = if (uniqueN(bee_class) == 1L) {
      unique(bee_class)
    } else {
      paste0("AMBIGUOUS_", paste(sort(unique(bee_class)), collapse = "_"))
    },
    original_tags = paste(sort(unique(original_tag)), collapse = ",")
  ),
  by = .(colony, converted_tag)
]

ambiguous_lookup <- lookup[grepl("^AMBIGUOUS", bee_class)]

if (nrow(ambiguous_lookup) > 0L) {
  warning(
    "Ambiguous converted identities exist:\n",
    paste(
      paste0(
        ambiguous_lookup$colony, " tag ", ambiguous_lookup$converted_tag,
        " from original tags ", ambiguous_lookup$original_tags,
        " = ", ambiguous_lookup$bee_class
      ),
      collapse = "\n"
    )
  )
}

#define 
# H-H   = HH
# H-MC  = HMC
# H-MD  = HMD
# MC-MC = MCMC
# MD-MD = MDMD
# H-Q   = HQ
# MC-Q  = MCQ
# MD-Q  = MDQ
# MC-MD = MCMD
interaction_type <- function(class_1, class_2) {
  
  result <- rep(NA_character_, length(class_1))
  
  unknown <- is.na(class_1) | is.na(class_2)
  ambiguous <- grepl("^AMBIGUOUS", class_1) |
    grepl("^AMBIGUOUS", class_2)
  
  result[unknown] <- "UNCLASSIFIED"
  result[ambiguous & !unknown] <- "AMBIGUOUS"
  
  valid <- !unknown & !ambiguous
  
  a <- class_1[valid]
  b <- class_2[valid]
  
  key <- vapply(
    seq_along(a),
    function(i) paste(sort(c(a[i], b[i])), collapse = "-"),
    character(1)
  )
  
  type_map <- c(
    "H-H"   = "HH",
    "H-MC"  = "HMC",
    "H-MD"  = "HMD",
    "MC-MC" = "MCMC",
    "MD-MD" = "MDMD",
    "H-Q"   = "HQ",
    "MC-Q"  = "MCQ",
    "MD-Q"  = "MDQ",
    "MC-MD" = "MCMD"
  )
  
  result[valid] <- unname(type_map[key])
  result[valid & is.na(result)] <- "UNCLASSIFIED"
  
  result
}

# Process each per-video pairwise interaction file.
files <- list.files(
  input_dir,
  pattern = "_pairwise_interactions\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

# Avoid treating the already-combined output as a per-video input.
files <- files[
  !basename(files) %in% c(
    "all_videos_pairwise_interactions.csv",
    "pooled_pairwise_interactions_by_colony.csv"
  )
]

if (length(files) == 0L) {
  stop("No per-video pairwise interaction CSVs found in: ", input_dir)
}

all_results <- vector("list", length(files))

for (i in seq_along(files)) {
  
  file <- files[i]
  dt <- fread(file)
  
  required <- c("tag_1", "tag_2")
  
  # Obtain colony identity from existing column or filename.
  if (!"colony" %in% names(dt)) {
    colony_match <- regmatches(
      tolower(basename(file)),
      regexpr("colony(?:10|[1346789])", tolower(basename(file)), perl=TRUE)
    )
    
    dt[, colony := colony_match]
  } else {
    dt[, colony := tolower(trimws(as.character(colony)))]
    dt[grepl("^[0-9]+$", colony), colony := paste0("colony", colony)]
  }
  
  # Add colony-level treatment information to every pair row.
  dt <- merge(
    dt,
    colony_treatments,
    by = "colony",
    all.x = TRUE,
    sort = FALSE
  )
  
  dt[, tag_1 := convert_tags(tag_1)]
  dt[, tag_2 := convert_tags(tag_2)]
  
  # Join class for tag 1.
  lookup_1 <- copy(lookup)
  setnames(
    lookup_1,
    c("converted_tag", "bee_class", "original_tags"),
    c("tag_1", "class_1", "possible_original_tags_1")
  )
  
  dt <- merge(
    dt,
    lookup_1,
    by = c("colony", "tag_1"),
    all.x = TRUE,
    sort = FALSE
  )
  
  # Join class for tag 2.
  lookup_2 <- copy(lookup)
  setnames(
    lookup_2,
    c("converted_tag", "bee_class", "original_tags"),
    c("tag_2", "class_2", "possible_original_tags_2")
  )
  
  dt <- merge(
    dt,
    lookup_2,
    by = c("colony", "tag_2"),
    all.x = TRUE,
    sort = FALSE
  )
  
  dt[
    ,
    interaction_type := interaction_type(class_1, class_2)
  ]
  
  # Put classification columns near the pair columns.
  preferred_order <- c(
    "source_video", "colony",
    "colony_treatment", "newcomer_MC_n", "newcomer_MD_n",
    "newcomer_ratio", "treatment_group",
    "pair",
    "tag_1", "class_1", "possible_original_tags_1",
    "tag_2", "class_2", "possible_original_tags_2",
    "interaction_type",
    "cooccurrence_frames", "interaction_frames"
  )
  
  setcolorder(
    dt,
    c(
      intersect(preferred_order, names(dt)),
      setdiff(names(dt), preferred_order)
    )
  )
  
  output_file <- file.path(
    output_dir,
    sub(
      "_pairwise_interactions\\.csv$",
      "_pairwise_interactions_with_types.csv",
      basename(file),
      ignore.case = TRUE
    )
  )
  
  fwrite(dt, output_file)
  all_results[[i]] <- dt
  
  cat(sprintf("[%d/%d] Saved %s\n", i, length(files), output_file))
}



