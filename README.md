# CSUN-Sustainable-Dashboard

Performance dashboard for California State University, Northridge Athletics. A scheduled job pulls VALD data, cleans and flags it, and publishes it to Cloudflare R2. The dashboard (Cloudflare Pages, behind Cloudflare Access) reads from there.

```
VALD API ──► GitHub Actions (R, daily) ──► Cloudflare R2 ──► dashboard (Pages + Access)
```

## What is in this repo

| Path | Purpose |
|---|---|
| `R/forcedecks_pull.R` | One function that pulls ForceDecks data for any team in `config/teams.csv` |
| `R/publish.R` | Reads and writes Cloudflare R2 (or a local folder for testing) |
| `R/run_all.R` | Runs every row of the config; one team failing does not block the rest |
| `config/teams.csv` | Teams, VALD group IDs, season start dates, test types |
| `.github/workflows/refresh.yml` | The schedule |

## Data rules

- **No data in this repo.** CSVs, Excel files and credentials are git-ignored. Data lives only in R2.
- Published trial files use `athleteId` only. Names are in `roster/<team>.csv` and matched in the dashboard.
- Keep the R2 bucket private. Only the dashboard's Pages Function reads it, behind Cloudflare Access.

## What lands in R2

```
forcedecks/<team>/<test>.csv     all trials with quality flags (IDs, no names)
roster/<team>.csv                athleteId, givenName, familyName
config/exclusions/<team>_forcedecks.xlsx   manual exclusion list (you upload this)
manifest.json                    last-updated time, rows, latest test date per dataset
```

## One-time setup

1. **Cloudflare R2.** Dashboard > R2 > Create bucket (private). Then R2 > Manage API tokens > create a token with Object Read and Write for that bucket. Note the Account ID, Access Key ID and Secret Access Key.
2. **GitHub secrets.** Repo > Settings > Secrets and variables > Actions > New repository secret. Add:
   `VALD_CLIENT_ID`, `VALD_CLIENT_SECRET`, `VALD_ORG_ID`, `VALD_REGION`, `R2_ACCOUNT_ID`, `R2_ACCESS_KEY_ID`, `R2_SECRET_ACCESS_KEY`, `R2_BUCKET`.
3. **Seed existing history (recommended).** Upload your current MBB CMJ CSV once to `forcedecks/mbb/cmj.csv` in the bucket (R2 dashboard > Upload). The first run then continues from your newest test instead of re-pulling the season. Names in that file are stripped automatically on the next run.
4. **Manual exclusion list (optional).** Upload `forcedecks_exclusions.xlsx` to `config/exclusions/mbb_forcedecks.xlsx`.
5. **Run it.** Actions > Refresh VALD data > Run workflow. Check the log, then check `manifest.json` in the bucket.

## Adding a team or test

Add a row to `config/teams.csv` (copy the MBB row, change the slug, name and group ID). Find group IDs with your existing `Find_Your_Team_ID.R`.

## Notes

- Scheduled GitHub workflows only run from the default branch. On a **public** repo, GitHub pauses schedules after 60 days without repo activity; private repos are not paused. Keep the repo private once the app access is sorted.
- A failed run emails the person who last edited the workflow schedule. Check Settings > Notifications.
- The run publishes nothing if the new file shrinks by more than 10 percent versus what is stored.

## Test locally without R2

```r
Sys.setenv(LOCAL_DATA_DIR = "local_data")   # needs VALD_* in .Renviron
source("R/run_all.R")
```
