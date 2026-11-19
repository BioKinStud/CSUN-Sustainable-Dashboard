# Runs every row of config/teams.csv. Used by the GitHub Actions workflow, and
# you can run it yourself:  Rscript R/run_all.R
# Optional: ONLY_TEAM=mbb to run a single team_slug.

source("R/publish.R")
source("R/forcedecks_pull.R")

cfg <- utils::read.csv("config/teams.csv", stringsAsFactors = FALSE)
cfg <- cfg[cfg$system == "forcedecks", ]

only <- Sys.getenv("ONLY_TEAM")
if (nzchar(only)) cfg <- cfg[cfg$team_slug == only, ]
if (nrow(cfg) == 0) stop("No matching rows in config/teams.csv.")

failed <- character(0)
for (i in seq_len(nrow(cfg))) {
  team <- cfg[i, ]
  ok <- tryCatch({ pull_forcedecks(team); TRUE }, error = function(e) {
    message("FAILED ", team$team_slug, "/", team$test_type, ": ", conditionMessage(e))
    FALSE
  })
  if (!ok) failed <- c(failed, paste(team$team_slug, team$test_type))
}

# One team failing must not block the others, but the run still ends red so
# GitHub emails you.
if (length(failed) > 0) {
  message("Failed: ", paste(failed, collapse = "; "))
  quit(status = 1)
}
message("All datasets refreshed.")
