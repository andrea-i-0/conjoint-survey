# Packages --------------------------------------------------------------------
#
# install.packages("pak")
# pak::pak(c("surveydown-dev/surveydown", "here", "dplyr", "glue", "readr"))

library(surveydown)
library(here)
library(dplyr)
library(glue)
library(readr)

# Design file (made by code/make_design.R) ------------------------------------

design  <- read_csv(here("data", "design.csv"), show_col_types = FALSE)
n_tasks <- max(design$qID)

# Database --------------------------------------------------------------------
#
# Run sd_db_config() once to save your supabase credentials in a .env file.

db <- sd_db_connect()

# UI --------------------------------------------------------------------------

ui <- sd_ui()

# Vignette text ---------------------------------------------------------------

eur <- function(x) paste0("&euro;", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))

img <- function(file) {
  glue('<img src="images/{file}" style="width:100%; max-width:240px; display:block; margin-top:0.5em;">')
}

txt_importance <- c(
  exploratory = "The survey results will provide an exploratory preliminary insight into the studied area, which we want to be of the highest quality possible, but they will not directly influence policy-making in this area.",
  decisive    = "The survey results will have a decisive impact on policy-making in the studied area, so it is crucial that the population parameter estimates are as accurate as possible."
)

# Bias text for one panel
txt_bias <- function(b) {
  if (b == 0) {
    return("Unbiased: the correct proportion or average would be obtained.")
  }
  glue(
    "Prior research shows that this sample would yield some estimates with {b}% relative bias. ",
    "For example, instead of the correct proportion of 0.30, an estimate of ",
    "{format(0.30 * (1 + b / 100), nsmall = 2)} might be obtained, or instead of the correct ",
    "average of 3.0 on a scale of 1-5, a biased estimate of {format(3 * (1 + b / 100), nsmall = 1)} might be obtained."
  )
}

txt_scope <- c(
  "none" = "None.",
  "1/20" = "The above-mentioned bias affects 1/20 (5%) of all key variables (e.g., 1 out of 20 variables).",
  "1/10" = "The above-mentioned bias affects 1/10 (10%) of all key variables (e.g., 2 out of 20 variables).",
  "1/5"  = "The above-mentioned bias affects 1/5 (20%) of all key variables (e.g., 4 out of 20 variables).",
  "1/2"  = "The above-mentioned bias affects 1/2 (50%) of all key variables (e.g., 10 out of 20 variables).",
  "all"  = "The above-mentioned bias affects all key variables (e.g., 20 out of 20 variables)."
)

img_bias  <- c(`5` = "bias_5.png", `10` = "bias_10.png", `30` = "bias_30.png")
img_scope <- c("1/20" = "prop_1_20.png", "1/10" = "prop_1_10.png", "1/5" = "prop_1_5.png",
               "1/2" = "prop_1_2.png", "all" = "prop_all.png")

txt_frame <- c(
  savings   = "You have budgeted for the higher cost of the Probability Sample. If you choose the Non-Probability Web Panel, the significant cost difference becomes surplus funding which you can freely spend on professional development (e.g., conferences, training) or on improving the project (e.g., larger sample size, additional staff). If you choose the Probability Sample, these extra funds will not be available.",
  loss      = "The project budget is tight. If you choose the Probability Sample, the higher cost means you will have significantly less funding available for other project activities. This will require you to cut costs elsewhere, reducing the budget for professional development, staff hours, or other project services.",
  no_impact = "Costs are identical; no impact."
)

# One panel (column) of the comparison
make_panel <- function(type, row) {
  if (type == "prob") {
    list(
      name  = "Probability-based online panel",
      bias  = txt_bias(0),
      scope = txt_scope[["none"]],
      cost  = eur(row$prob_cost)
    )
  } else {
    list(
      name  = "Nonprobability online panel",
      bias  = paste0(txt_bias(row$nwp_bias),
                     if (row$nwp_bias > 0) img(img_bias[[as.character(row$nwp_bias)]]) else ""),
      scope = paste0(txt_scope[[row$nwp_scope]],
                     if (row$nwp_scope != "none") img(img_scope[[row$nwp_scope]]) else ""),
      cost  = eur(row$nwp_cost)
    )
  }
}

make_vignette <- function(row) {
  order <- if (row$prob_left) c("prob", "nwp") else c("nwp", "prob")
  L <- make_panel(order[1], row)
  R <- make_panel(order[2], row)

  shared <- function(label, text) {
    glue("<tr><th>{label}</th><td colspan='2' class='shared'>{text}</td></tr>")
  }
  split <- function(label, a, b) {
    glue("<tr><th>{label}</th><td>{a}</td><td>{b}</td></tr>")
  }

  HTML(paste0(
    "<style>
      .vig {border-collapse:collapse; width:100%; font-size:0.95em; table-layout:fixed;}
      .vig th, .vig td {border:1px solid #ccc; padding:8px; vertical-align:top; text-align:left;}
      .vig th {width:20%; background:#f5f5f5;}
      .vig td {width:40%;}
      .vig thead td {font-weight:bold; text-align:center; background:#e8eef7;}
      .vig td.shared {text-align:center; font-style:italic;}
    </style>",
    "<p>Please consider the following specific circumstances for this decision:</p>",
    "<table class='vig'>",
    "<thead><tr><th></th><td>", L$name, "</td><td>", R$name, "</td></tr></thead>",
    shared("Importance of research results", txt_importance[[row$importance]]),
    split("Relative bias", L$bias, R$bias),
    split("Proportion of variables with relative bias", L$scope, R$scope),
    shared("Project value", paste0("The entire budget of the project is ", eur(row$budget), ".")),
    split("Cost of the survey", L$cost, R$cost),
    shared("Funds for other activities", txt_frame[[row$frame]]),
    "</table>",
    "<p style='font-size:0.85em; margin-top:0.5em;'>All key variables are assumed to be equally important.</p>"
  ))
}

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  # Assign this respondent a random block and save it
  resp_id <- sample(unique(design$respID), 1)
  sd_store_value(resp_id, "respID")

  df <- design |> filter(respID == resp_id) |> arrange(qID)

  # One vignette output per task: vignette_1, vignette_2, ...
  lapply(seq_len(n_tasks), function(k) {
    output[[paste0("vignette_", k)]] <- renderUI(make_vignette(df[k, ]))
  })

  completion_code <- sd_completion_code(10)
  sd_store_value(completion_code)

  sd_server(db = db)
}

shiny::shinyApp(ui = ui, server = server)
