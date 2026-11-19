# ============================================================================
# publish.R: storage helpers (Cloudflare R2, or a local folder for testing)
# ============================================================================
# Two modes, chosen automatically:
#   - LOCAL_DATA_DIR set  -> read/write plain files in that folder (testing).
#   - otherwise           -> Cloudflare R2 through the AWS CLI (S3 protocol).
#
# R2 mode needs these environment variables (GitHub Secrets in the workflow):
#   R2_ACCOUNT_ID, R2_ACCESS_KEY_ID, R2_SECRET_ACCESS_KEY, R2_BUCKET
# The AWS CLI is preinstalled on GitHub's ubuntu runners.
# ============================================================================

store_mode <- function() {
  if (nzchar(Sys.getenv("LOCAL_DATA_DIR"))) "local" else "r2"
}

.r2_env <- function() {
  need <- c("R2_ACCOUNT_ID", "R2_ACCESS_KEY_ID", "R2_SECRET_ACCESS_KEY", "R2_BUCKET")
  missing <- need[!nzchar(Sys.getenv(need))]
  if (length(missing) > 0) {
    stop("Missing R2 settings: ", paste(missing, collapse = ", "),
         ". Add them as GitHub Secrets (or set LOCAL_DATA_DIR to test locally).")
  }
  Sys.setenv(
    AWS_ACCESS_KEY_ID = Sys.getenv("R2_ACCESS_KEY_ID"),
    AWS_SECRET_ACCESS_KEY = Sys.getenv("R2_SECRET_ACCESS_KEY"),
    AWS_DEFAULT_REGION = "auto",
    # Newer AWS CLI versions add checksum headers that R2 can reject.
    AWS_REQUEST_CHECKSUM_CALCULATION = "when_required",
    AWS_RESPONSE_CHECKSUM_VALIDATION = "when_required"
  )
  list(
    bucket = Sys.getenv("R2_BUCKET"),
    endpoint = sprintf("https://%s.r2.cloudflarestorage.com", Sys.getenv("R2_ACCOUNT_ID"))
  )
}

.aws <- function(args) {
  out <- suppressWarnings(system2("aws", args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, output = paste(out, collapse = "\n"))
}

# Download `key` to `dest`. Returns TRUE if downloaded, FALSE if the key does
# not exist yet. Any other failure (bad credentials, network) stops the run,
# so a glitch can never be mistaken for "no history yet" and overwrite data.
store_get <- function(key, dest) {
  if (store_mode() == "local") {
    src <- file.path(Sys.getenv("LOCAL_DATA_DIR"), key)
    if (!file.exists(src)) return(FALSE)
    return(file.copy(src, dest, overwrite = TRUE))
  }
  r2 <- .r2_env()
  res <- .aws(c("s3", "cp", sprintf("s3://%s/%s", r2$bucket, key), dest,
                "--endpoint-url", r2$endpoint, "--only-show-errors"))
  if (res$status == 0L) return(TRUE)
  if (grepl("404|Not Found|NoSuchKey|does not exist", res$output)) return(FALSE)
  stop("R2 download failed for '", key, "': ", res$output)
}

store_put <- function(src, key, content_type = "text/csv") {
  if (store_mode() == "local") {
    dest <- file.path(Sys.getenv("LOCAL_DATA_DIR"), key)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(src, dest, overwrite = TRUE)
    return(invisible(TRUE))
  }
  r2 <- .r2_env()
  res <- .aws(c("s3", "cp", src, sprintf("s3://%s/%s", r2$bucket, key),
                "--endpoint-url", r2$endpoint, "--content-type", content_type,
                "--cache-control", "no-cache", "--only-show-errors"))
  if (res$status != 0L) stop("R2 upload failed for '", key, "': ", res$output)
  invisible(TRUE)
}

# Records when each dataset was last refreshed, so the dashboard can show
# "data through <date>" and you can see at a glance if a run silently stopped.
update_manifest <- function(dataset_key, info) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  manifest <- list(datasets = list())
  if (store_get("manifest.json", tmp)) {
    manifest <- tryCatch(jsonlite::read_json(tmp), error = function(e) list(datasets = list()))
    if (is.null(manifest$datasets)) manifest$datasets <- list()
  }
  manifest$datasets[[dataset_key]] <- info
  manifest$generated_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  jsonlite::write_json(manifest, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  store_put(tmp, "manifest.json", "application/json")
}
