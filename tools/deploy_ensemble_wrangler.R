if (!requireNamespace("rsconnect", quietly = TRUE)) stop("rsconnect is required")
if (!file.exists("app.R") || !dir.exists("data")) {
  stop("Run from the NFL-projections app repository root")
}

# This existing Gmail-owned content auto-publishes from GitHub main. The available
# Gmail publishing credential exposes the complete private deployment status.
# A credential for another account can return an incomplete public view.
content_id <- "019e0a00-0c1e-7122-33bd-b94ad82403e6"
gmail_account_id <- "019dc509-a64c-835a-1b16-6493af888629"
source_repo <- "https://github.com/rogerroot01/NFL-projections"
account <- rsconnect::accountInfo("rogerroot", "connect.posit.cloud")
if (!identical(account$accountId, gmail_account_id)) stop("Wrong Wrangler account mapping")
client <- rsconnect:::clientForAccount(account)
content <- client$getContent(content_id)
revision <- content$current_revision
head_sha <- system2("git", "rev-parse HEAD", stdout = TRUE)

if (!identical(content$account_id, gmail_account_id) ||
    !identical(content$source_repository_url, source_repo) ||
    !isTRUE(content$auto_publish) ||
    !identical(revision$commit_sha, head_sha) ||
    !identical(revision$publish_result, "success")) {
  stop("The existing Gmail Wrangler has not published this GitHub commit successfully.")
}
cat("Verified Gmail Wrangler content", content_id, "at", revision$url, "\n")
