# Core data processing engine.
#
# Reads the Catapult stats_df.xlsx, computes match day offsets,
# categorizes periods, and builds the per-minute statistical model.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

# ── Metric configuration ────────────────────────────────────────────────────

METRICS <- list(
  distance = list(
    column = "total_distance",
    multiplier = 1000, # km -> m
    label = "Distancia Total",
    unit = "m"
  ),
  hsr = list(
    column = "high_speed_distance",
    multiplier = 1000, # km -> m
    label = "HSR",
    unit = "m"
  ),
  sprint = list(
    column = "sprint_distance_>15mph",
    multiplier = 1000, # km -> m
    label = "Sprint",
    unit = "m"
  ),
  accel = list(
    column = "gen2_acceleration_band3plus_total_effort_count",
    multiplier = 1,
    label = "Aceleraciones",
    unit = "#"
  ),
  decel = list(
    column = "gen2_acceleration_band6plus_total_effort_count",
    multiplier = 1,
    label = "Desaceleraciones",
    unit = "#"
  ),
  player_load = list(
    column = "total_player_load",
    multiplier = 1, # already in AU
    label = "Player Load",
    unit = "AU"
  )
)

# ── Match day calculation ────────────────────────────────────────────────────

#' Add a 'match_day' column by computing the offset from the nearest
#' match date (Partido or Amistoso).
#'
#' Logic:
#' - Identify match dates from activity_name starting with 'Partido' or 'Amistoso'
#' - For each training session date, find the NEXT match date
#' - Offset = training_date - next_match_date (so MD-1 = 1 day before)
#' - If no next match, look at previous match and use MD+N
compute_match_days <- function(df) {
  date_parsed <- as.Date(df$date, format = "%m/%d/%Y")

  is_match <- grepl("^(partido|amistoso)", tolower(df$activity_name))
  is_match[is.na(is_match)] <- FALSE
  match_dates <- sort(unique(date_parsed[is_match]))

  get_md_label <- function(session_date) {
    if (is.na(session_date)) return(NA_character_)

    if (session_date %in% match_dates) return("MD")

    future <- match_dates[match_dates > session_date]
    past <- match_dates[match_dates < session_date]

    if (length(future) > 0) {
      next_match <- min(future)
      diff <- as.integer(session_date - next_match) # negative number
      return(paste0("MD", diff))
    } else if (length(past) > 0) {
      prev_match <- max(past)
      diff <- as.integer(session_date - prev_match) # positive number
      return(paste0("MD+", diff))
    } else {
      return(NA_character_)
    }
  }

  df$match_day <- vapply(date_parsed, get_md_label, character(1))
  df
}

#' Normalize a raw match_day / DayCode tag value from the data-processing
#' script into the label used internally for grouping.
#'
#' - "Game": friendly-match sessions the day after MD, played by squad
#'   members who didn't get minutes on MD -- functionally a compensatory
#'   session, so folded into "MD+1" (Compensatorio).
#' - "Other": kept as its own bucket ("Otro"), for sessions the staff
#'   couldn't assign to the standard microcycle.
#' - "Tuesday", blank, or NA: dropped (returns NA), since they don't map
#'   to a usable match-day offset.
#' - Anything else (MD, MD-N, MD+N, ...): passed through unchanged.
normalize_match_day <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "") return(NA_character_)
  if (x == "Game") return("MD+1")
  if (x == "Other") return("Otro")
  if (x == "Tuesday") return(NA_character_)
  x
}

# ── Data loading and processing ─────────────────────────────────────────────

#' Load the stats_df.xlsx, filter, categorize, and compute per-minute rates.
#'
#' Returns a clean data.frame with columns:
#'   activity_type, match_day, duration_min, and per-minute rates for each metric.
load_and_process <- function(xlsx_path) {
  df <- as.data.frame(read_excel(xlsx_path))

  # Filter out matches
  starts_partido <- startsWith(as.character(df$activity_name), "Partido")
  starts_partido[is.na(starts_partido)] <- FALSE
  df <- df[!starts_partido, , drop = FALSE]

  # Use the match_day column from the data processing script when present
  # (e.g. "MD", "MD-2", "MD+1"); otherwise fall back to computing it from
  # date offsets to the nearest match, for older exports that lack it.
  if ("match_day" %in% names(df)) {
    df$match_day <- vapply(as.character(df$match_day), normalize_match_day, character(1), USE.NAMES = FALSE)
  } else {
    df <- compute_match_days(df)
  }

  # Categorize periods
  df$activity_type <- categorize_periods(df$period_name)

  # Drop uncategorized and missing match_day
  df <- df[!is.na(df$activity_type) & !is.na(df$match_day), , drop = FALSE]

  df$duration_min <- suppressWarnings(as.numeric(df$total_duration)) / 60
  df <- df[!is.na(df$duration_min) & df$duration_min > 0.5, , drop = FALSE] # at least 30 seconds

  # Compute per-minute rates for each metric
  for (key in names(METRICS)) {
    cfg <- METRICS[[key]]
    col <- cfg$column
    mult <- cfg$multiplier
    raw <- suppressWarnings(as.numeric(df[[col]]))
    raw[is.na(raw)] <- 0
    raw <- raw * mult
    df[[paste0(key, "_per_min")]] <- raw / df$duration_min
    df[[paste0(key, "_total")]] <- raw
  }

  keep_cols <- c(
    "activity_name", "period_name", "activity_type", "match_day",
    "athlete_name", "date", "duration_min"
  )
  for (key in names(METRICS)) {
    keep_cols <- c(keep_cols, paste0(key, "_per_min"), paste0(key, "_total"))
  }

  rownames(df) <- NULL
  df[, keep_cols]
}

# ── Statistical model ───────────────────────────────────────────────────────

#' Compute mean, std, and CI using t-distribution.
compute_ci <- function(values, confidence = 0.95) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n == 0) {
    return(list(mean = 0, std = 0, ci_lower = 0, ci_upper = 0, n = 0))
  }

  mean_val <- mean(values)
  std_val <- if (n > 1) sd(values) else 0

  if (n >= 2) {
    se <- std_val / sqrt(n)
    t_crit <- qt(0.5 + confidence / 2, df = n - 1)
    ci_lower <- max(0, mean_val - t_crit * se)
    ci_upper <- mean_val + t_crit * se
  } else {
    ci_lower <- mean_val
    ci_upper <- mean_val
  }

  list(
    mean = round(mean_val, 4),
    std = round(std_val, 4),
    ci_lower = round(ci_lower, 4),
    ci_upper = round(ci_upper, 4),
    n = n
  )
}

#' Build the full statistical model from processed data.
#'
#' Returns nested list:
#' list(
#'   activity_type = list(
#'     match_day = list(
#'       metric_key = list(mean, std, ci_lower, ci_upper, n)
#'     )
#'   )
#' )
build_model <- function(df) {
  model <- list()

  groups <- split(df, list(df$activity_type, df$match_day), drop = TRUE, sep = "")

  for (gname in names(groups)) {
    parts <- strsplit(gname, "", fixed = TRUE)[[1]]
    act_type <- parts[1]
    md <- parts[2]
    group <- groups[[gname]]

    if (is.null(model[[act_type]])) model[[act_type]] <- list()

    metrics <- list()
    for (key in names(METRICS)) {
      col <- paste0(key, "_per_min")
      metrics[[key]] <- compute_ci(group[[col]])
    }

    model[[act_type]][[md]] <- metrics
  }

  model
}

#' End-to-end: load data -> process -> build model.
#'
#' Returns list(
#'   model = ...,
#'   activity_types = list(list(id, display_name, match_days, total_samples)),
#'   match_days = c(...),
#'   metrics = list(...),
#'   data_summary = list(...)
#' )
build_full_model <- function(xlsx_path) {
  df <- load_and_process(xlsx_path)
  model <- build_model(df)

  activity_types <- list()
  for (act_type in sort(names(model))) {
    mds <- sort(names(model[[act_type]]))
    total_n <- sum(vapply(mds, function(md) model[[act_type]][[md]][["distance"]][["n"]], numeric(1)))
    if (total_n >= 3) { # at least 3 data points total
      activity_types[[length(activity_types) + 1]] <- list(
        id = act_type,
        display_name = get_display_name(act_type),
        match_days = mds,
        total_samples = total_n
      )
    }
  }

  # Sort by total samples descending
  order_idx <- order(-vapply(activity_types, function(x) x$total_samples, numeric(1)))
  activity_types <- activity_types[order_idx]

  all_match_days <- sort(unique(unlist(lapply(model, names))))

  list(
    model = model,
    activity_types = activity_types,
    match_days = all_match_days,
    metrics = lapply(METRICS, function(v) list(label = v$label, unit = v$unit)),
    data_summary = list(
      total_records = nrow(df),
      unique_athletes = length(unique(df$athlete_name)),
      date_range = paste(min(df$date), "—", max(df$date)),
      activity_type_count = length(activity_types)
    )
  )
}

#' Predict session load for a planned session.
#'
#' activities: list of list(activity_type = ..., duration_minutes = ...)
predict_session <- function(model, match_day, activities) {
  results <- list()
  totals <- lapply(names(METRICS), function(m) list(predicted = 0, ci_lower = 0, ci_upper = 0))
  names(totals) <- names(METRICS)

  for (act in activities) {
    act_type <- act$activity_type
    duration <- act$duration_minutes
    act_result <- list(
      activity_type = act_type,
      display_name = get_display_name(act_type),
      duration_minutes = duration,
      metrics = list(),
      has_data = FALSE
    )

    if (!is.null(model[[act_type]]) && !is.null(model[[act_type]][[match_day]])) {
      act_result$has_data <- TRUE
      md_model <- model[[act_type]][[match_day]]

      for (metric_key in names(METRICS)) {
        if (!is.null(md_model[[metric_key]])) {
          m <- md_model[[metric_key]]
          pred <- round(m$mean * duration, 1)
          ci_low <- round(m$ci_lower * duration, 1)
          ci_high <- round(m$ci_upper * duration, 1)

          act_result$metrics[[metric_key]] <- list(
            predicted = pred,
            ci_lower = ci_low,
            ci_upper = ci_high,
            per_min_mean = round(m$mean, 2),
            sample_count = m$n
          )
          totals[[metric_key]]$predicted <- totals[[metric_key]]$predicted + pred
          totals[[metric_key]]$ci_lower <- totals[[metric_key]]$ci_lower + ci_low
          totals[[metric_key]]$ci_upper <- totals[[metric_key]]$ci_upper + ci_high
        }
      }
    }

    results[[length(results) + 1]] <- act_result
  }

  for (m in names(METRICS)) {
    totals[[m]]$predicted <- round(totals[[m]]$predicted, 1)
    totals[[m]]$ci_lower <- round(totals[[m]]$ci_lower, 1)
    totals[[m]]$ci_upper <- round(totals[[m]]$ci_upper, 1)
  }

  list(
    match_day = match_day,
    activities = results,
    session_totals = totals
  )
}
