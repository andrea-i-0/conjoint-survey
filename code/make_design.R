# Make the vignette design ----------------------------------------------------
#
# Run this script ONCE before launching the survey. It writes data/design.csv.
# Each respondent gets one "block" (respID) of n_tasks comparison tasks.
# Each task compares 1 probability panel with 1 nonprobability panel.
#
# Rules:
#   - Importance and project budget: same for both panels in a task
#   - Probability panel: always 0% bias, scope of error "none"
#   - Probability cost: 10% chance of 4,000 (equal); 90% higher (8k / 20k / 40k equally)
#   - NWP cost: always 4,000
#   - NWP bias: 10% chance of 0%; 90% biased (5% / 10% / 30% equally)
#   - NWP unbiased -> scope of error "none"; biased -> scope drawn from 5 levels
#   - Equal costs -> funds text "no impact"; otherwise savings or loss frame (50/50)
#   - Left/right position of the two panels is randomized (50/50) per task
#   - No respondent sees the same task twice
#
# Analysis note: levels are drawn 10/30/30/30 (cost, bias) to limit tasks without
# a trade-off; estimates are reweighted to equal weights per level in the analysis.

library(dplyr)
library(readr)
library(here)

set.seed(2026)

n_resp  <- 1000 # number of blocks (more blocks than respondents is fine)
n_tasks <- 6    # tasks per respondent; must match the pages in survey.qmd
                # and n_tasks in app.R (approved consent says 6 vignettes)

draw_task <- function() {
  prob_cost <- sample(c(4000, 8000, 20000, 40000), 1, prob = c(.10, .30, .30, .30))
  nwp_bias  <- sample(c(0, 5, 10, 30), 1, prob = c(.10, .30, .30, .30))
  tibble(
    importance = sample(c("exploratory", "decisive"), 1),
    budget     = sample(c(100000, 300000, 500000), 1),
    prob_cost  = prob_cost,
    nwp_cost   = 4000,
    nwp_bias   = nwp_bias,
    nwp_scope  = if (nwp_bias == 0) "none" else sample(c("1/20", "1/10", "1/5", "1/2", "all"), 1),
    frame      = if (prob_cost == 4000) "no_impact" else sample(c("savings", "loss"), 1),
    prob_left  = sample(c(TRUE, FALSE), 1)
  )
}

draw_block <- function(id) {
  tasks <- tibble()
  key_cols <- c("importance", "budget", "prob_cost", "nwp_bias", "nwp_scope", "frame")
  while (nrow(tasks) < n_tasks) {
    t <- draw_task()
    if (nrow(tasks) == 0 || nrow(semi_join(t, tasks, by = key_cols)) == 0) {
      tasks <- bind_rows(tasks, t)
    }
  }
  tasks |> mutate(respID = id, qID = seq_len(n_tasks), .before = 1)
}

design <- bind_rows(lapply(seq_len(n_resp), draw_block))

# Quick balance check
for (a in c("importance", "budget", "prob_cost", "nwp_bias", "nwp_scope", "frame", "prob_left")) {
  print(round(prop.table(table(design[[a]])), 3))
}

# Should print 6000 rows and 6 tasks per respondent
print(nrow(design))
print(table(table(design$respID)))

write_csv(design, here("data", "design.csv"))
