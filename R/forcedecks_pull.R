# ============================================================================
# forcedecks_pull.R: one function that pulls ForceDecks data for ANY team
# ============================================================================
# This is the config-driven version of the per-team ForceDecks script. The
# logic is the same (failed-trial floor, rank/top-trial flag, within-session
# outliers, manual exclusions, team landing-force percentile, athlete-history
# plausibility), with these changes for running unattended in the cloud:
#
#   1. Incremental pull. The accumulated CSV is downloaded from storage first;
#      the VALD pull starts a few days before its newest test (OVERLAP) instead
#      of re-pulling from the season start every run.
#   2. Athlete IDs only in the published CSV. Names go to a separate roster
#      file the dashboard matches on athleteId.
#   3. No local drive paths. Storage is Cloudflare R2 (see publish.R).
#   4. The manual exclusion workbook is read from storage, not a network drive.
# ============================================================================

suppressPackageStartupMessages({
  library(valdr)
  library(dplyr)
  library(tidyr)
})

# Settings carried over from the original MBB script. Override any of these by
# passing `settings = modifyList(FD_DEFAULTS, list(...))` to pull_forcedecks().
FD_DEFAULTS <- list(
  keep_metrics = c(
    # Overall
    "JUMP_HEIGHT_IMP_MOM", "FLIGHT_CONTRACTION_TIME_RATIO", "RSI_MODIFIED",
    # Eccentric
    "MEAN_ECCENTRIC_FORCE", "PEAK_ECCENTRIC_FORCE",
    "MEAN_ECCENTRIC_POWER", "ECCENTRIC_PEAK_POWER",
    "MEAN_ECCENTRIC_DECELERATION_FORCE", "ECCENTRIC_DECEL_RFD",
    "ECCENTRIC_BRAKING_IMPULSE", "ECCENTRIC_DECEL_IMPULSE",
    # Concentric
    "MEAN_TAKEOFF_FORCE", "PEAK_CONCENTRIC_FORCE",
    "MEAN_CONCENTRIC_POWER", "PEAK_TAKEOFF_POWER",
    "CONCENTRIC_IMPULSE",
    # Landing
    "LANDING_IMPULSE", "MEAN_LANDING_FORCE"
  ),
  rank_metric = "JUMP_HEIGHT_IMP_MOM",
  n_keep_trials = 3,
  landing_force_percentile = 0.95,
  quality_check_metrics = c("JUMP_HEIGHT_IMP_MOM", "MEAN_LANDING_FORCE", "RSI_MODIFIED"),
  outlier_modified_z = 3.5,
  min_trials_for_outlier_check = 3,
  min_landing_force_n = 250,
  min_athlete_trials_for_history = 5,
  athlete_history_z = 3.5,
  # Days before the newest stored test to start the next pull. Re-pulled
  # trials replace their stored copy, so overlap is safe and covers late syncs.
  overlap_days = 7
)

## ---- helpers --------------------------------------------------------------

vald_connect <- function() {
  vars <- c("VALD_CLIENT_ID", "VALD_CLIENT_SECRET", "VALD_ORG_ID", "VALD_REGION")
  missing <- vars[!nzchar(Sys.getenv(vars))]
  if (length(missing) > 0) {
    stop("Missing VALD credentials: ", paste(missing, collapse = ", "),
         ". Add them as GitHub Secrets (or to your local .Renviron).")
  }
  set_credentials(
    client_id     = Sys.getenv("VALD_CLIENT_ID"),
    client_secret = Sys.getenv("VALD_CLIENT_SECRET"),
    tenant_id     = Sys.getenv("VALD_ORG_ID"),
    region        = Sys.getenv("VALD_REGION")
  )
}

# "2026-06-30" or "2026-06-30T00:00:00Z" -> "2026-06-30T00:00:00Z"
as_vald_start <- function(x) paste0(substr(as.character(x), 1, 10), "T00:00:00Z")

# Where the next VALD pull should begin: newest stored test minus overlap, or
# the configured season start when nothing is stored yet.
next_start_date <- function(existing, fallback, overlap_days) {
  if (is.null(existing) || nrow(existing) == 0 || !"recordedDateUtc" %in% names(existing)) {
    return(as_vald_start(fallback))
  }
  latest <- suppressWarnings(max(as.Date(substr(existing$recordedDateUtc, 1, 10)), na.rm = TRUE))
  if (!is.finite(latest)) return(as_vald_start(fallback))
  start <- max(latest - overlap_days, as.Date(substr(fallback, 1, 10)))
  as_vald_start(format(start))
}

# Keep the freshest copy of any row that appears in both new and stored data.
merge_history <- function(new_df, existing_df) {
  combined <- if (is.null(existing_df)) new_df else {
    # Column types can differ between the stored CSV and fresh data.
    for (col in intersect(names(new_df), names(existing_df))) {
      if (is.character(new_df[[col]]) || is.character(existing_df[[col]])) {
        new_df[[col]] <- as.character(new_df[[col]])
        existing_df[[col]] <- as.character(existing_df[[col]])
      }
    }
    dplyr::bind_rows(new_df, existing_df)
  }
  id_col <- intersect(c("trialId", "testId"), names(combined))
  if (length(id_col) > 0) {
    combined <- dplyr::distinct(combined, .data[[id_col[1]]], .keep_all = TRUE)
  } else {
    warning("No id column found, writing without de-duplication.")
  }
  combined
}

## ---- steps 2-8: turn freshly pulled VALD data into flagged trial rows ------

fd_process_new <- function(fd_tests, fd_trials, s) {
  fd_trials <- fd_trials |> mutate(value = as.numeric(value))

  # Human-readable position in the session ("1st jump today"), used by the
  # manual exclusion list instead of the unreadable trialId.
  trial_order <- fd_trials |>
    distinct(testId, trialId, recordedUTC) |>
    group_by(testId) |>
    mutate(trialNumberInSession = min_rank(recordedUTC)) |>
    ungroup() |>
    select(testId, trialId, trialNumberInSession)

  fd_kept <- if (is.null(s$keep_metrics)) fd_trials else
    fd_trials[fd_trials$definition_result %in% s$keep_metrics, ]

  fd_final <- fd_kept |>
    mutate(metric_col = paste0(definition_result, "_", resultLimb)) |>
    pivot_wider(
      id_cols = c(testId, trialId, athleteId),
      names_from = metric_col,
      values_from = value
    ) |>
    left_join(fd_tests |> select(testId, recordedDateUtc, testType, weight), by = "testId") |>
    left_join(trial_order, by = c("testId", "trialId"))

  # rank + flag_TopTrial
  rank_col <- paste0("rank_", s$rank_metric)
  rank_src <- paste0(s$rank_metric, "_Trial")
  if (rank_src %in% names(fd_final)) {
    fd_final <- fd_final |>
      group_by(testId) |>
      mutate(
        !!rank_col := min_rank(desc(.data[[rank_src]])),
        flag_TopTrial = .data[[rank_col]] <= s$n_keep_trials
      ) |>
      ungroup()
  } else {
    warning("Rank metric column '", rank_src, "' not found; skipping rank/flag_TopTrial.")
  }

  # Failed-trial floor: the one deliberate exception to "flag, never discard".
  # Rows with NA are kept (no data is not the same as bad data).
  lc <- "MEAN_LANDING_FORCE_Left"; rc <- "MEAN_LANDING_FORCE_Right"
  if (lc %in% names(fd_final) && rc %in% names(fd_final)) {
    n_before <- nrow(fd_final)
    fd_final <- fd_final |>
      filter(
        is.na(.data[[lc]]) | .data[[lc]] >= s$min_landing_force_n,
        is.na(.data[[rc]]) | .data[[rc]] >= s$min_landing_force_n
      )
    message(n_before - nrow(fd_final), " new trial(s) removed as clearly failed (landing force < ",
            s$min_landing_force_n, " N on a leg).")
  } else {
    message("Skipping failed-trial removal: MEAN_LANDING_FORCE not in pulled data.")
  }

  # Within-session outliers (median/MAD modified z-score)
  q_cols <- intersect(paste0(s$quality_check_metrics, "_Trial"), names(fd_final))
  if (length(q_cols) > 0 && nrow(fd_final) > 0) {
    fd_final <- fd_final |>
      group_by(testId) |>
      mutate(.n_trials_this_session = n()) |>
      mutate(across(
        all_of(q_cols),
        ~ ifelse(
          .n_trials_this_session >= s$min_trials_for_outlier_check &
            !is.na(mad(.x, na.rm = TRUE)) & mad(.x, na.rm = TRUE) > 0,
          abs((.x - median(.x, na.rm = TRUE)) / mad(.x, na.rm = TRUE)) > s$outlier_modified_z,
          FALSE
        ),
        .names = "outlier_{.col}"
      )) |>
      ungroup()

    flag_cols <- paste0("outlier_", q_cols)
    fd_final <- fd_final |>
      rowwise() |>
      mutate(
        flag_SessionOutlier = any(c_across(all_of(flag_cols))),
        flag_SessionOutlier_Reason = paste(
          gsub("^outlier_|_Trial$", "", flag_cols[c_across(all_of(flag_cols))]),
          collapse = ", "
        )
      ) |>
      ungroup() |>
      select(-all_of(flag_cols), -.n_trials_this_session)
  } else {
    message("Skipping session-outlier flags: no quality-check metrics in pulled data.")
  }

  fd_final
}

## ---- step 9: work on the FULL accumulated history --------------------------

fd_finalize <- function(accumulated, names_lookup, exclusion_path, s) {
  # Manual exclusions. Names are attached only to evaluate the name/date/trial
  # matching method and are dropped again immediately, so they never persist.
  if (!is.null(exclusion_path) && file.exists(exclusion_path)) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Package 'readxl' is required.")
    exclusions <- readxl::read_excel(exclusion_path, sheet = "Exclusions")
    n_before <- nrow(accumulated)

    excluded_ids <- if ("TrialId" %in% names(exclusions)) exclusions$TrialId[!is.na(exclusions$TrialId)] else character(0)

    name_hit <- rep(FALSE, nrow(accumulated))
    req <- c("GivenName", "FamilyName", "TestDate", "TrialNumber")
    if (all(req %in% names(exclusions)) &&
        all(c("recordedDateUtc", "trialNumberInSession") %in% names(accumulated))) {
      keys <- exclusions |>
        filter(!is.na(GivenName), !is.na(FamilyName), !is.na(TestDate), !is.na(TrialNumber)) |>
        mutate(matchKey = paste(GivenName, FamilyName, as.Date(TestDate), TrialNumber)) |>
        distinct(matchKey)
      named <- accumulated |> left_join(names_lookup, by = "athleteId")
      acc_key <- paste(named$givenName, named$familyName,
                       as.Date(substr(named$recordedDateUtc, 1, 10)),
                       named$trialNumberInSession)
      name_hit <- acc_key %in% keys$matchKey
    }

    remove <- (accumulated$trialId %in% excluded_ids) | name_hit
    accumulated <- accumulated[!remove, ]
    message(n_before - nrow(accumulated), " trial(s) removed via the manual exclusion list.")
  } else {
    message("No manual exclusion list found; skipping (fine if you don't use one).")
  }

  # Team-wide extreme-landing flag. Recomputed from ALL history each run, so
  # older rows can change as the percentile shifts. That is expected.
  landing_col <- "MEAN_LANDING_FORCE_Trial"
  if (landing_col %in% names(accumulated) && "weight" %in% names(accumulated)) {
    accumulated <- accumulated |>
      mutate(landingForceBWMultiple = .data[[landing_col]] / (as.numeric(weight) * 9.80665))
    cutoff <- quantile(accumulated$landingForceBWMultiple, s$landing_force_percentile, na.rm = TRUE)
    accumulated <- accumulated |> mutate(flag_ExtremeLanding = landingForceBWMultiple > cutoff)
    message("Extreme-landing cutoff across ", nrow(accumulated), " trials: ", round(cutoff, 2),
            "x bodyweight (top ", round((1 - s$landing_force_percentile) * 100), "%).")
  } else {
    message("Skipping landing-force flag: '", landing_col, "' or 'weight' not available.")
  }

  # Implausible jump height vs the athlete's own full history
  jh <- paste0(s$rank_metric, "_Trial")
  if (jh %in% names(accumulated)) {
    accumulated <- accumulated |>
      group_by(athleteId) |>
      mutate(
        .n = sum(!is.na(.data[[jh]])),
        .med = median(.data[[jh]], na.rm = TRUE),
        .mad = mad(.data[[jh]], na.rm = TRUE),
        flag_ImplausibleJumpHeight = ifelse(
          .n >= s$min_athlete_trials_for_history & !is.na(.mad) & .mad > 0,
          abs((.data[[jh]] - .med) / .mad) > s$athlete_history_z,
          FALSE
        )
      ) |>
      ungroup() |>
      select(-.n, -.med, -.mad)
    message(sum(accumulated$flag_ImplausibleJumpHeight, na.rm = TRUE),
            " trial(s) flagged implausible vs the athlete's own history.")
  }

  accumulated
}

## ---- main entry point ------------------------------------------------------

# team: one row of config/teams.csv (team_slug, team_name, group_id, start_date, test_type)
pull_forcedecks <- function(team, settings = FD_DEFAULTS) {
  s <- settings
  slug <- team$team_slug
  test <- team$test_type
  data_key <- sprintf("forcedecks/%s/%s.csv", slug, tolower(test))
  roster_key <- sprintf("roster/%s.csv", slug)
  excl_key <- sprintf("config/exclusions/%s_forcedecks.xlsx", slug)
  dataset <- sprintf("forcedecks/%s/%s", slug, tolower(test))
  message("==== ", team$team_name, " | ForceDecks ", test, " ====")

  vald_connect()

  # Stored history (if any) decides where this pull starts.
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  existing <- NULL
  if (store_get(data_key, tmp_csv)) {
    existing <- utils::read.csv(tmp_csv, stringsAsFactors = FALSE)
    existing <- existing |> select(-any_of(c("givenName", "familyName")))  # legacy seeds may have names
    message("Loaded ", nrow(existing), " stored trials.")
  } else {
    message("No stored history; starting from ", team$start_date, ".")
  }
  start <- next_start_date(existing, team$start_date, s$overlap_days)
  set_start_date(start)
  message("Pulling VALD data from ", start)

  profiles <- get_profiles_only()
  group_map <- get_profiles_groups_categories_mapping()
  team_ids <- group_map$profileId[group_map$groupId == team$group_id]
  message(length(team_ids), " athletes in group.")
  if (length(team_ids) == 0) stop("No athletes found for group_id in config/teams.csv.")

  names_lookup <- profiles |>
    filter(profileId %in% team_ids) |>
    transmute(athleteId = profileId, givenName, familyName)

  # Filter to team + test type BEFORE fetching trials (one API call per test).
  fd_tests <- get_forcedecks_tests_only()
  fd_tests <- fd_tests[fd_tests$testType == test & fd_tests$profileId %in% team_ids, ]
  message(nrow(fd_tests), " test(s) in the pull window.")

  accumulated <- existing
  if (nrow(fd_tests) > 0) {
    fd_trials <- get_forcedecks_trials_only(fd_tests)
    new_rows <- fd_process_new(fd_tests, fd_trials, s)
    accumulated <- merge_history(new_rows, existing)
  } else {
    message("Nothing new from VALD this run.")
  }

  if (is.null(accumulated) || nrow(accumulated) == 0) {
    update_manifest(dataset, list(status = "no_data", checked_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
    return(invisible(NULL))
  }

  # Exclusion workbook lives in storage so anyone can update it.
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)
  excl_path <- if (store_get(excl_key, tmp_xlsx)) tmp_xlsx else NULL

  accumulated <- fd_finalize(accumulated, names_lookup, excl_path, s)

  # Sanity check before publishing: never replace good data with a shrunken file.
  if (!is.null(existing) && nrow(accumulated) < 0.9 * nrow(existing)) {
    stop("New file has far fewer rows (", nrow(accumulated), ") than stored (", nrow(existing),
         "). Not publishing. Check the exclusion list and the VALD pull.")
  }

  out <- accumulated |> select(-any_of(c("givenName", "familyName")))
  utils::write.csv(out, tmp_csv, row.names = FALSE)
  store_put(tmp_csv, data_key, "text/csv")

  tmp_roster <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_roster), add = TRUE)
  utils::write.csv(names_lookup, tmp_roster, row.names = FALSE)
  store_put(tmp_roster, roster_key, "text/csv")

  latest <- max(substr(out$recordedDateUtc, 1, 10), na.rm = TRUE)
  update_manifest(dataset, list(
    status = "ok",
    file = data_key,
    rows = nrow(out),
    athletes = length(unique(out$athleteId)),
    latest_test_date = latest,
    updated_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ))
  message("Published ", nrow(out), " rows to ", data_key, " (latest test ", latest, ").")
  invisible(out)
}
