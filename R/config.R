APP_NAME <- "dPCR Analyseplattform"
APP_VERSION <- "0.1.0"

DEVICE_LEVELS <- c("qiaquity", "roche", "biorad", "unknown")

DPCR_STANDARD_COLUMNS <- c(
  "plate_name",
  "plate_id",
  "plate_type",
  "well",
  "sample",
  "channel",
  "color_channel",
  "volume",
  "threshold",
  "partition",
  "rfu",
  "invalid_partition",
  "positive_control",
  "reference",
  "device_type",
  "source_file"
)

DPCR_REQUIRED_COLUMNS <- c(
  "plate_name",
  "plate_id",
  "plate_type",
  "well",
  "sample",
  "channel",
  "volume",
  "threshold",
  "partition",
  "rfu",
  "invalid_partition",
  "positive_control",
  "reference",
  "device_type",
  "source_file"
)

DPCR_CHARACTER_COLUMNS <- c(
  "plate_name",
  "plate_id",
  "plate_type",
  "well",
  "sample",
  "channel",
  "color_channel",
  "reference",
  "source_file"
)

DPCR_NUMERIC_COLUMNS <- c("volume", "threshold", "partition", "rfu")
DPCR_LOGICAL_COLUMNS <- c("invalid_partition", "positive_control")
DPCR_PLOT_NUMERIC_CANDIDATES <- c("partition", "rfu", "volume", "threshold")
PLOTLY_USE_WEBGL_DEFAULT <- FALSE
SCATTER_ALPHA_DEFAULT <- 0.3
TWODDPCR_DROPLET_VOLUME_DEFAULT <- 0.85
TWODDPCR_MAHALANOBIS_MAX_DEFAULT <- 30
TWODDPCR_SD_ERROR_DEFAULT <- 5
TWODDPCR_KNN_K_DEFAULT <- 3
TWODDPCR_KNN_PROB_DEFAULT <- 0

empty_issues_table <- function() {
  tibble::tibble(
    severity = character(),
    field = character(),
    message = character(),
    n_rows = integer()
  )
}

get_example_data_dir <- function() {
  candidate_dirs <- c("Example_Data", "exampledata")
  existing <- candidate_dirs[dir.exists(candidate_dirs)]

  if (length(existing) == 0) {
    return(NULL)
  }

  existing[[1]]
}

list_example_data_files <- function() {
  data_dir <- get_example_data_dir()

  if (is.null(data_dir)) {
    return(character())
  }

  list.files(data_dir, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
}
