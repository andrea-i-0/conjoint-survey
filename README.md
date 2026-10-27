# Vignette survey (surveydown)

1. Install: `pak::pak(c("surveydown-dev/surveydown", "here", "dplyr", "glue", "readr"))`
2. Open `survey.Rproj` in RStudio.
3. Run `code/make_design.R` once -> writes `data/design.csv`.
4. Open `app.R` and click **Run App** to test locally (no data is saved while `ignore = TRUE`).
5. To go live: run `sd_db_config()` with your supabase details, set `sd_db_connect(ignore = FALSE)`, deploy to Posit Connect Cloud.

Change the number of vignettes: edit `n_tasks` in `make_design.R` AND add/remove `task_k` pages in `survey.qmd`.

Analysis: join the survey data to `data/design.csv` by `respID` and task number
(`choice_k` / `strength_k` -> `qID = k`). Each row is one vignette:
outcome = choice (prob / nwp) and strength (1-7); predictors = the 6 attributes.
