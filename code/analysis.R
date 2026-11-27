# Analysis plan code ----------------------------------------------------------
#
# Conjoint experiment: probability vs. nonprobability online panel choice.
# Causal-inference estimands: marginal means (MMs) and AMCEs, with
# respondent-clustered standard errors, following the course approach
# ({marginaleffects}, Hainmueller et al. 2014, Leeper et al. 2020).
#
# Run order:
#   Part A (simulation) runs NOW, before fieldwork, using only data/design.csv.
#   Part B onwards runs on the real data (data/responses.csv exported from Supabase).
#
# Packages:
# install.packages(c("dplyr", "tidyr", "readr", "here", "marginaleffects", "sandwich"))

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(marginaleffects)

options(marginaleffects_safe = FALSE)

# Attribute levels (reference level first) ------------------------------------

lv <- list(
  importance = c("exploratory", "decisive"),
  budget     = c("100000", "300000", "500000"),
  bias       = c("5", "0", "10", "30"),               # reference: 5%
  scope      = c("none", "1/20", "1/10", "1/5", "1/2", "all"),
  cost       = c("8000", "4000", "20000", "40000"),   # reference: EUR 8,000
  frame      = c("no_impact", "savings", "loss"),
  prob_left  = c("TRUE", "FALSE")
)

# Turn design columns into analysis factors.
# Linked attributes are combined so the model never mixes impossible cells:
#   bias_scope = bias x proportion of biased variables (16 possible cells)
#   cost_frame = cost x funds framing (7 possible cells)
prep_attributes <- function(d) {
  d |>
    mutate(
      importance = factor(importance, lv$importance),
      budget     = factor(format(budget, scientific = FALSE, trim = TRUE), lv$budget),
      bias       = factor(as.character(nwp_bias), lv$bias),
      scope      = factor(nwp_scope, lv$scope),
      cost       = factor(format(prob_cost, scientific = FALSE, trim = TRUE), lv$cost),
      frame      = factor(frame, lv$frame),
      prob_left  = factor(as.character(prob_left), lv$prob_left),
      bias_scope = factor(paste(bias, scope, sep = "|")),
      cost_frame = factor(paste(cost, frame, sep = "|"))
    )
}

# Reference grid with EQUAL weights per level ---------------------------------
#
# Each importance, budget, cost and bias level gets equal weight.
# Proportion of biased variables: equal weights within bias > 0%.
# Funds framing: equal weights within cost > EUR 4,000.
# Panel position: equal weights.
# Only combinations that can occur are in the grid.

make_grid <- function(data) {
  w_bias <- bind_rows(
    tibble(bias = "0", scope = "none", w_b = 1 / 4),
    expand_grid(bias = c("5", "10", "30"), scope = lv$scope[-1]) |>
      mutate(w_b = (1 / 4) * (1 / 5))
  )
  w_cost <- bind_rows(
    tibble(cost = "4000", frame = "no_impact", w_c = 1 / 4),
    expand_grid(cost = c("8000", "20000", "40000"), frame = c("savings", "loss")) |>
      mutate(w_c = (1 / 4) * (1 / 2))
  )
  expand_grid(
    importance = lv$importance,
    budget     = lv$budget,
    prob_left  = lv$prob_left
  ) |>
    cross_join(w_bias) |>
    cross_join(w_cost) |>
    mutate(
      w = (1 / 2) * (1 / 3) * (1 / 2) * w_b * w_c,
      importance = factor(importance, lv$importance),
      budget     = factor(budget, lv$budget),
      bias       = factor(bias, lv$bias),
      scope      = factor(scope, lv$scope),
      cost       = factor(cost, lv$cost),
      frame      = factor(frame, lv$frame),
      prob_left  = factor(prob_left, lv$prob_left),
      bias_scope = factor(paste(bias, scope, sep = "|"), levels(data$bias_scope)),
      cost_frame = factor(paste(cost, frame, sep = "|"), levels(data$cost_frame)),
      session_id = data$session_id[1] # placeholder; not used by the model
    ) |>
    select(-w_b, -w_c)
}

# Main model formula (linear probability model, additive in attributes)
f_main <- ~ importance + budget + bias_scope + cost_frame + prob_left

# Estimate MMs and AMCEs for every attribute ----------------------------------
#
# MM   = weighted average prediction at each level of the attribute.
# AMCE = difference between a level's MM and the reference level's MM
#        (in an additive model this equals the AMCE).
# scope is estimated among bias > 0% only; frame among cost > EUR 4,000 only.

estimate_all <- function(model, data, cluster = ~session_id) {
  grid <- make_grid(data)
  sub <- list(
    importance = grid,
    budget     = grid,
    bias       = grid,
    scope      = filter(grid, bias != "0") |> mutate(scope = droplevels(scope)),
    cost       = grid,
    frame      = filter(grid, cost != "4000") |> mutate(frame = droplevels(frame))
  )
  mm <- lapply(names(sub), function(a) {
    avg_predictions(model, newdata = sub[[a]], by = a, wts = "w", vcov = cluster) |>
      as_tibble() |>
      transmute(attribute = a, level = as.character(.data[[a]]),
                estimate, std.error, conf.low, conf.high)
  }) |> bind_rows()
  amce <- lapply(names(sub), function(a) {
    avg_predictions(model, newdata = sub[[a]], by = a, wts = "w", vcov = cluster,
                    hypothesis = difference ~ reference) |>
      as_tibble() |>
      transmute(attribute = a, contrast = hypothesis,
                estimate, std.error, conf.low, conf.high, p.value)
  }) |> bind_rows()
  list(mm = mm, amce = amce)
}

# =============================================================================
# PART A: Simulation check (run before fieldwork)
# =============================================================================
# Simulates choices from the real design with assumed effects, then checks that
# the model recovers them and how precise the estimates are at N = 300 and 400.

simulate_choices <- function(n_resp, seed = 1) {
  set.seed(seed)
  design <- read_csv(here("data", "design.csv"), show_col_types = FALSE)
  blocks <- sample(unique(design$respID), n_resp, replace = TRUE)
  d <- tibble(session_id = seq_len(n_resp), respID = blocks) |>
    left_join(design, by = "respID", relationship = "many-to-many") |>
    prep_attributes()
  # Assumed true effects (probability scale), plus a respondent random intercept
  u <- rnorm(n_resp, 0, 0.10)
  p <- 0.45 +
    0.08 * (d$importance == "decisive") +
    0.04 * (d$budget == "300000") + 0.08 * (d$budget == "500000") -
    0.15 * (d$bias == "0") + 0.05 * (d$bias == "10") + 0.15 * (d$bias == "30") +
    0.02 * (d$scope %in% c("1/5")) + 0.05 * (d$scope == "1/2") + 0.10 * (d$scope == "all") +
    0.15 * (d$cost == "4000") - 0.10 * (d$cost == "20000") - 0.20 * (d$cost == "40000") +
    0.03 * (d$frame == "loss") +
    u[d$session_id]
  d$prob <- rbinom(nrow(d), 1, pmin(pmax(p, 0.01), 0.99))
  d
}

for (n in c(300, 400)) {
  sim <- simulate_choices(n)
  m_sim <- lm(update(f_main, prob ~ .), data = sim)
  res <- estimate_all(m_sim, sim)
  cat("\n==== Simulated N =", n, "(AMCEs; compare with assumed effects) ====\n")
  print(res$amce, n = Inf)
}

# =============================================================================
# PART B: Real data (run after fieldwork)
# =============================================================================
# Export the Supabase 'responses' table to data/responses.csv first.

if (file.exists(here("data", "responses.csv"))) {

  design <- read_csv(here("data", "design.csv"), show_col_types = FALSE)
  raw    <- read_csv(here("data", "responses.csv"), show_col_types = FALSE)

  # B1. Long format: one row per respondent x task ----------------------------
  long <- raw |>
    filter(consent == "yes") |>   # also drop test rows here (by date or flag)
    select(session_id, respID, sector, experience, used_panels,
           matches("^(choice|strength|maxprice)_[0-9]+$")) |>
    mutate(across(matches("^(choice|strength|maxprice)_"), as.character)) |>
    pivot_longer(matches("^(choice|strength|maxprice)_"),
                 names_to = c(".value", "qID"), names_sep = "_",
                 names_transform = list(qID = as.integer)) |>
    filter(!is.na(choice)) |>
    left_join(design, by = c("respID", "qID")) |>
    prep_attributes() |>
    mutate(
      prob     = as.integer(choice == "prob"),
      strength = as.numeric(strength),
      maxprice = suppressWarnings(as.numeric(maxprice)),
      maxprice = if_else(maxprice > 0 & maxprice <= 200000, maxprice, NA_real_),
      no_tradeoff = cost == "4000" & bias != "0"
    )

  n_tasks_done <- count(long, session_id, name = "n_tasks")
  long <- left_join(long, n_tasks_done, by = "session_id")

  # B2. Descriptives ----------------------------------------------------------
  cat("Respondents:", n_distinct(long$session_id), " Tasks:", nrow(long), "\n")
  print(count(n_tasks_done, n_tasks))

  # B3. Main analysis: primary outcome ----------------------------------------
  m1 <- lm(update(f_main, prob ~ .), data = long)
  main <- estimate_all(m1, long)
  print(main$mm, n = Inf)
  print(main$amce, n = Inf)

  # B4. Secondary and tertiary outcomes ---------------------------------------
  m2 <- lm(update(f_main, strength ~ .), data = long)
  sec <- estimate_all(m2, long)
  m3 <- lm(update(f_main, log(maxprice) ~ .), data = filter(long, !is.na(maxprice)))
  ter <- estimate_all(m3, filter(long, !is.na(maxprice)))

  # B5. Quality check: tasks without a trade-off ------------------------------
  fails <- long |> filter(no_tradeoff) |>
    group_by(session_id) |> summarise(fail = any(prob == 0))
  cat("Share choosing nonprobability in no-trade-off tasks:",
      round(mean(long$prob[long$no_tradeoff] == 0), 3), "\n")

  # Rating vs. choice consistency: rating on the opposite side of the choice
  cat("Rating opposite to forced choice:",
      round(mean((long$prob == 1 & long$strength < 4) |
                 (long$prob == 0 & long$strength > 4), na.rm = TRUE), 3), "\n")

  # Randomization balance: respondent characteristics by attribute level
  for (a in c("importance", "budget", "bias", "cost")) {
    print(round(prop.table(table(long[[a]], long$sector), 1), 2))
  }

  # B6. Robustness ------------------------------------------------------------
  # (a) Excluding respondents who failed a no-trade-off task
  keep <- setdiff(long$session_id, fails$session_id[fails$fail])
  d_a  <- filter(long, session_id %in% keep)
  rob_a <- estimate_all(lm(update(f_main, prob ~ .), data = d_a), d_a)
  # (b) Complete respondents only (all 6 tasks)
  d_b  <- filter(long, n_tasks == 6)
  rob_b <- estimate_all(lm(update(f_main, prob ~ .), data = d_b), d_b)
  # (c) Logit instead of linear probability model
  m_c  <- glm(update(f_main, prob ~ .), data = long, family = binomial)
  rob_c <- estimate_all(m_c, long)
  # (d) As-fielded weights (10/30/30/30) instead of equal weights
  rob_d <- avg_comparisons(m1, variables = c("importance", "budget", "prob_left"),
                           vcov = ~session_id)
  # (e) Task order (stability): tasks 1-3 vs 4-6
  long$late <- long$qID > 3
  m_e <- lm(prob ~ late * (importance + budget + bias_scope + cost_frame) + prob_left,
            data = long)
  rob_e <- avg_predictions(m_e, newdata = make_grid(long) |> cross_join(tibble(late = c(FALSE, TRUE))),
                           by = c("bias", "late"), wts = "w", vcov = ~session_id)

  # B7. Exploratory subgroups (differences in marginal means; Leeper et al. 2020)
  for (g in c("sector", "experience", "used_panels")) {
    long$grp <- factor(long[[g]])
    m_g <- lm(prob ~ grp * (importance + budget + bias_scope + cost_frame) + prob_left,
              data = long)
    grid_g <- make_grid(long) |> cross_join(tibble(grp = levels(long$grp)))
    print(avg_predictions(m_g, newdata = grid_g, by = c("bias", "grp"),
                          wts = "w", vcov = ~session_id))
  }
}
