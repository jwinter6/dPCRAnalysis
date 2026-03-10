log_info <- function(message) {
  cat(sprintf("[%s] INFO  %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message))
}

log_warn <- function(message) {
  cat(sprintf("[%s] WARN  %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message))
}

log_error <- function(message) {
  cat(sprintf("[%s] ERROR %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message))
}
