# Make the vignette design ----------------------------------------------------
#
# Run this script ONCE before launching the survey. It writes data/design.csv.
# Each respondent gets one "block" (respID) of n_tasks vignettes.
#
# Each attribute is drawn independently and uniformly (standard randomized
# conjoint design). Two display rules are applied after drawing:
#   - bias = 0%    -> the "proportion of variables" attribute is not shown
#   - cost = Equal -> the "funds" attribute shows the neutral message
# No respondent sees the same displayed vignette twice.

library(dplyr)
library(readr)
library(here)

set.seed(2026)

n_resp  <- 1000 # number of blocks (more blocks than respondents is fine)
n_tasks <- 8    # vignettes per respondent; must match the pages in survey.qmd

# Attributes and levels -------------------------------------------------------

profiles <- expand.grid(
  importance = c("exploratory", "decisive"),
  bias       = c(0, 5, 10, 30),
  proportion = c("1/20", "1/10", "1/5", "1/2", "all"),
  budget     = c(100000, 300000, 500000),
  cost       = c("equal", "2x", "5x", "10x"),
  frame      = c("savings", "loss"),
  stringsAsFactors = FALSE
)

# Apply display rules
profiles <- profiles |>
  mutate(
    proportion = if_else(bias == 0, NA_character_, proportion),
    frame      = if_else(cost == "equal", "neutral", frame),
    nwp_cost   = 4000,
    prob_cost  = c(equal = 4000, `2x` = 8000, `5x` = 20000, `10x` = 40000)[cost],
    difference = prob_cost - nwp_cost,
    percent    = round(100 * difference / budget, 1), # share of the total project budget
    profile_key = paste(importance, bias, proportion, budget, cost, frame, sep = "|")
  )

# Draw blocks -----------------------------------------------------------------

draw_block <- function(id) {
  rows <- integer(0)
  keys <- character(0)
  while (length(rows) < n_tasks) {
    r <- sample(nrow(profiles), 1)
    if (!profiles$profile_key[r] %in% keys) {
      rows <- c(rows, r)
      keys <- c(keys, profiles$profile_key[r])
    }
  }
  profiles[rows, ] |>
    mutate(respID = id, qID = seq_len(n_tasks), .before = 1)
}

design <- bind_rows(lapply(seq_len(n_resp), draw_block)) |>
  select(-profile_key)

# Quick balance check (each level should appear about equally often)
for (a in c("importance", "bias", "proportion", "budget", "cost", "frame")) {
  print(round(prop.table(table(design[[a]], useNA = "ifany")), 3))
}

write_csv(design, here("data", "design.csv"), na = "")
