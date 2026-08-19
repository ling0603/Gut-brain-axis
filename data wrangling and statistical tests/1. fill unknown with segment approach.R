library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tibble)

#####################
# setting

# Folder containing the original CSV files
input_folder <- "../all_interval1_csvs"

# New output folder
output_folder <- "../segment filled data"

# A movement larger than this starts a new track segment
movement_break_threshold <- 12

#output folder
if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
}


#define unknown

is_unknown <- function(x) {
  
  x <- trimws(as.character(x))
  
  is.na(x) |
    x == "" |
    tolower(x) %in% c(
      "unknown"
    )
}


###########################################
# put in one csv


process_one_csv <- function(input_file) {
  
  file_name <- basename(input_file)
  
  file_stem <- tools::file_path_sans_ext(file_name)
  
  output_file <- file.path(
    output_folder,
    paste0(file_stem, "_segment_filled.csv")
  )
  
  cat(
    "\nProcessing:\n",
    input_file,
    "\n"
  )
  
  # Read CSV
  df <- read_csv(
    input_file,
    show_col_types = FALSE
  )
  
  # Check required columns
  required_columns <- c(
    "frame",
    "individual",
    "tag_center_x",
    "tag_center_y",
    "raw_model_prediction",
    "model_confidence"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(df)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
  
  # Prepare data
  df <- df %>%
    mutate(
      raw_model_prediction =
        as.character(raw_model_prediction),
      
      model_confidence =
        as.numeric(model_confidence),
      
      frame =
        as.numeric(frame),
      
      tag_center_x =
        as.numeric(tag_center_x),
      
      tag_center_y =
        as.numeric(tag_center_y)
    ) %>%
    arrange(
      individual,
      frame
    )
  
  
  ######################
  # define segment
  
  result <- df %>%
    group_by(individual) %>%
    arrange(
      frame,
      .by_group = TRUE
    ) %>%
    
    mutate(
      previous_frame = lag(frame),
      previous_x = lag(tag_center_x),
      previous_y = lag(tag_center_y),
      
      frame_difference =
        frame - previous_frame,
      
      movement = sqrt(
        (tag_center_x - previous_x)^2 +
          (tag_center_y - previous_y)^2
      ),
      
      # Start a new segment when:
      # 1. It is the first row for the individual
      # 2. Frames are not consecutive
      # 3. Coordinates or movement are invalid
      # 4. Movement exceeds the threshold
      new_segment =
        is.na(previous_frame) |
        frame_difference != 1 |
        !is.finite(movement) |
        movement > movement_break_threshold,
      
      segment_id =
        cumsum(new_segment)
    ) %>%
    
    ungroup() %>%
    
    # Process every segment separately
    group_by(
      individual,
      segment_id
    ) %>%
    
    group_modify(~ {
      
      x <- .x
      
      known_rows <- which(
        !is_unknown(x$raw_model_prediction) &
          !is.na(x$model_confidence)
      )
      
      number_known <- length(known_rows)
      
      # Preserve known predictions and leave Unknowns unchanged initially
      x$filled_tag_identity <- ifelse(
        is_unknown(x$raw_model_prediction),
        "unknown",
        x$raw_model_prediction
      )
      
      x$fill_source <- ifelse(
        is_unknown(x$raw_model_prediction),
        "Not filled",
        "Raw prediction"
      )
      
      # Do not fill if there are no known predictions
      if (number_known < 1) {
        
        x$fill_source[
          is_unknown(x$raw_model_prediction)
        ] <- "Not filled: no known prediction in segment"
        
        return(x)
      }
      
      # Calculate the mean confidence for each predicted identity
      identity_summary <- x %>%
        filter(
          !is_unknown(raw_model_prediction),
          !is.na(model_confidence)
        ) %>%
        group_by(raw_model_prediction) %>%
        summarise(
          mean_confidence = mean(
            model_confidence,
            na.rm = TRUE
          ),
          number_of_predictions = n(),
          .groups = "drop"
        ) %>%
        arrange(
          desc(mean_confidence),
          desc(number_of_predictions)
        )
      
      # Select the identity with the highest mean confidence
      selected_identity <-
        identity_summary$raw_model_prediction[1]
      
      selected_mean_confidence <-
        identity_summary$mean_confidence[1]
      
      # Fill all Unknown rows in the segment
      unknown_rows <- is_unknown(
        x$raw_model_prediction
      )
      
      x$filled_tag_identity[unknown_rows] <-
        selected_identity
      
      x$fill_source[unknown_rows] <-
        paste0(
          "Filled using highest mean-confidence identity: ",
          selected_identity,
          " (mean confidence = ",
          round(selected_mean_confidence, 4),
          ")"
        )
      
      x
    }) %>%
    
    ungroup() %>%
    
    arrange(
      frame,
      individual
    ) %>%
    
    select(
      -previous_frame,
      -previous_x,
      -previous_y,
      -frame_difference,
      -movement,
      -new_segment
    )
  
  
  ###########################################
  # calculate process result for decription 
  
  original_unknown <- sum(
    is_unknown(result$raw_model_prediction)
  )
  
  remaining_unknown <- sum(
    is_unknown(result$filled_tag_identity)
  )
  
  filled_unknown <-
    original_unknown - remaining_unknown
  
  filling_rate <- ifelse(
    original_unknown == 0,
    0,
    round(
      100 * filled_unknown / original_unknown,
      2
    )
  )
  
  
  ######################
  # save csv
  
  write_csv(
    result,
    output_file
  )
  
  cat(
    "Saved to:\n",
    output_file,
    "\n",
    "Original unknown rows: ",
    original_unknown,
    "\n",
    "Filled unknown rows: ",
    filled_unknown,
    "\n",
    "Remaining unknown rows: ",
    remaining_unknown,
    "\n",
    "Filling rate: ",
    filling_rate,
    "%\n",
    sep = ""
  )
  
  
  # Return one summary row
  tibble(
    file_name = file_name,
    output_file = output_file,
    total_rows = nrow(result),
    total_segments = n_distinct(
      paste(
        result$individual,
        result$segment_id,
        sep = "_"
      )
    ),
    original_unknown = original_unknown,
    filled_unknown = filled_unknown,
    remaining_unknown = remaining_unknown,
    filling_rate_percent = filling_rate,
    processing_status = "Completed"
  )
}


######################
# find all csv files

csv_files <- list.files(
  path = input_folder,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE,
  ignore.case = TRUE
)

# Avoid accidentally processing previously filled files
csv_files <- csv_files[
  !str_detect(
    basename(csv_files),
    regex(
      "_segment_filled\\.csv$",
      ignore_case = TRUE
    )
  )
]

if (length(csv_files) == 0) {
  stop(
    paste0(
      "No CSV files were found in:\n",
      input_folder
    )
  )
}

cat(
  "Found ",
  length(csv_files),
  " CSV files.\n",
  sep = ""
)


###########################################
# process all csv files

batch_summary <- map_dfr(
  csv_files,
  function(current_file) {
    
    tryCatch(
      
      process_one_csv(current_file),
      
      error = function(e) {
        
        cat(
          "\nERROR processing:\n",
          current_file,
          "\n",
          conditionMessage(e),
          "\n"
        )
        
        tibble(
          file_name = basename(current_file),
          output_file = NA_character_,
          total_rows = NA_integer_,
          total_segments = NA_integer_,
          original_unknown = NA_integer_,
          filled_unknown = NA_integer_,
          remaining_unknown = NA_integer_,
          filling_rate_percent = NA_real_,
          processing_status = paste0(
            "Failed: ",
            conditionMessage(e)
          )
        )
      }
    )
  }
)
