# Convert RStudio history database files into a readable format
# With thanks to https://gist.github.com/vzemlys/751887b31657649a6a5c for inspiration 

process_rstudio_history <- function(input, # single file path, vector, or directory with history_database files
                                    output_file = NULL, # optional filename; default has timestamp
                                    session_gap_hours = 2, # hours of inactivity before new coding session
                                    tz = "GMT", # timezone
                                    pattern = "^history_database(\\.[0-9]+)?$" # regex pattern for history files if input is directory
                                   ) {
  
  # Resolve input into a vector of file paths
  if (length(input) == 1 && dir.exists(input)) {
    files <- list.files(input, pattern = pattern, full.names = TRUE)
    if (length(files) == 0) {
      stop("No history files found in directory: ", input)
    }
  } else {
    files <- input
    missing <- files[!file.exists(files)]
    if (length(missing) > 0) {
      stop("File(s) not found: ", paste(missing, collapse = ", "))
    }
  }
  
  # Read and parse each file, then row-bind
  parse_history_file <- function(f) {
    lns <- readLines(f, warn = FALSE)
    lns <- lns[nzchar(lns)]  # drop empty lines
    if (length(lns) == 0) return(NULL)
    split <- str_split(lns, pattern = ":", n = 2)
    
    # Keep only well-formed lines (epoch:command)
    ok <- vapply(split, function(x) length(x) == 2 && !is.na(suppressWarnings(as.numeric(x[[1]]))), logical(1))
    split <- split[ok]
    if (length(split) == 0) return(NULL)
    tibble(
      epoch   = as.integer64(sapply(split, "[[", 1)),
      history = sapply(split, "[[", 2),
      source  = basename(f)
    )
  }
  
  hist_db <- bind_rows(lapply(files, parse_history_file))
  
  if (nrow(hist_db) == 0) {
    stop("No valid history entries found in the provided file(s).")
  }
  
  # Convert to POSIXct and sort chronologically
  hist_db <- hist_db %>%
    mutate(nice_date = as.POSIXct(as.numeric(epoch) / 1000,
                                  origin = "1970-01-01", tz = tz)) %>%
    arrange(nice_date) %>%
    distinct(epoch, history, .keep_all = TRUE)  # de-duplicate across files
  
  # Assign session IDs: a new session begins whenever the gap from the
  # previous entry exceeds `session_gap_hours`
  gap_secs <- session_gap_hours * 3600
  time_diffs <- as.numeric(difftime(hist_db$nice_date,
                                    lag(hist_db$nice_date),
                                    units = "secs"))
  time_diffs[1] <- Inf  # force first entry to start a session
  hist_db$session_id <- cumsum(time_diffs > gap_secs)
  
  # Default output filename
  if (is.null(output_file)) {
    ts <- format(Sys.time(), "%B_%d_%y")
    output_file <- paste0("hist_nice_", ts, ".txt")
  }
  
  # Write output
  con <- file(output_file, open = "w")
  on.exit(close(con))
  
  cat("R history\n", strrep("-", 80), "\n",
      "Files processed: ", length(files), "\n",
      "Total entries:   ", nrow(hist_db), "\n",
      "Sessions:        ", length(unique(hist_db$session_id)), "\n",
      file = con, sep = "")
  
  sessions <- split(hist_db, hist_db$session_id)
  for (s in sessions) {
    start <- min(s$nice_date)
    end   <- max(s$nice_date)
    header <- sprintf("Session: %s  to  %s   (%d commands)",
                      format(start, "%Y-%m-%d %H:%M:%S"),
                      format(end,   "%Y-%m-%d %H:%M:%S"),
                      nrow(s))
    cat("\n\n", header, "\n", strrep("-", 80), "\n",
        file = con, sep = "")
    
    writeLines(s$history, con)
  }
  
  message("Wrote ", nrow(hist_db), " entries across ",
          length(unique(hist_db$session_id)), " sessions to ", output_file)
  invisible(output_file)
}
