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
# ignore = TRUE: test locally without writing to the database.
# Set ignore = FALSE (or delete it) when you go live.

db <- sd_db_connect()

# UI --------------------------------------------------------------------------

ui <- sd_ui()

# Vignette text ---------------------------------------------------------------

eur <- function(x) paste0("&euro;", format(x, big.mark = ",", scientific = FALSE, trim = TRUE))

img <- function(file) {
  glue('<p><img src="images/{file}" style="width:100%; max-width:420px;"></p>')
}

txt_importance <- c(
  exploratory = "The survey results will provide an exploratory preliminary insight into the studied area, which we want to be of the highest quality possible, but they will not directly influence policy-making in this area.",
  decisive    = "The survey results will have a decisive impact on policy-making in the studied area, so it is crucial that the population parameter estimates are as accurate as possible."
)

txt_bias <- function(b) {
  if (b == 0) {
    return("Regardless of the sample type selected, all estimates would be unbiased. This means that the correct proportion or average would be obtained.")
  }
  nwp_label <- if (b == 5) "the non-probability sample (NWP)" else "the NWP"
  glue(
    "Based on prior research in this area, we know that the probability sample provides ",
    "unbiased estimates, while in {nwp_label}, some estimates would have a relative bias of {b}%. ",
    "This means, for example, that instead of the correct proportion of 0.30, an estimate of ",
    "{format(0.30 * (1 + b / 100), nsmall = 2)} might be obtained, or instead of the correct ",
    "average of 3.0 on a scale of 1-5, a biased estimate of {format(3 * (1 + b / 100), nsmall = 1)} might be obtained."
  )
}

txt_proportion <- c(
  "1/20" = "The above-mentioned bias affects 1/20 (5%) of all key variables (e.g., 1 out of 20 variables). It is assumed that all key variables are equally important.",
  "1/10" = "The above-mentioned bias affects 1/10 (10%) of all key variables (e.g., 2 out of 20 variables). It is assumed that all key variables are equally important.",
  "1/5"  = "The above-mentioned bias affects 1/5 (20%) of all key variables (e.g., 4 out of 20 variables). It is assumed that all key variables are equally important.",
  "1/2"  = "The above-mentioned bias affects 1/2 (50%) of all key variables (e.g., 10 out of 20 variables). It is assumed that all key variables are equally important.",
  "all"  = "The above-mentioned bias affects all key variables (e.g., 20 out of 20 variables). It is assumed that all key variables are equally important."
)

img_bias       <- c(`0` = "bias_0.png", `5` = "bias_5.png", `10` = "bias_10.png", `30` = "bias_30.png")
img_proportion <- c("1/20" = "prop_1_20.png", "1/10" = "prop_1_10.png", "1/5" = "prop_1_5.png",
                    "1/2" = "prop_1_2.png", "all" = "prop_all.png")

txt_frame <- function(row) {
  pc <- eur(row$prob_cost); d <- eur(row$difference); p <- paste0(row$percent, "%")
  switch(
    row$frame,
    neutral = "Since the cost is identical for both methods, there is no impact on other project funds.",
    savings = glue(
      "In the project, you have allocated <b>{pc}</b> for the survey, which is the cost of a probability sample. ",
      "If you choose the NWP, you will have <b>{d}</b> ({pc} - &euro;4,000) in savings available, which you can freely ",
      "spend on professional development (e.g., conference attendance, professional training) or on further improving ",
      "the project (e.g., increasing the sample size, hiring additional collaborators, procuring additional services). ",
      "If you choose the probability sample, these additional funds will not be available."
    ),
    loss = glue(
      "In the project, the funds for the survey are not strictly predetermined. If you choose a probability sample, ",
      "which is somewhat more expensive than the NWP, you will have less funding for other project activities, ",
      "specifically <b>{d}</b> less ({pc} - &euro;4,000), meaning you will need to save about <b>{p}</b> of the funds on other costs."
    )
  )
}

make_vignette <- function(row) {
  items <- c(
    paste0("<b>Importance of research results.</b> ", txt_importance[[row$importance]]),
    paste0("<b>Bias.</b> ", txt_bias(row$bias), img(img_bias[[as.character(row$bias)]])),
    if (row$bias != 0) {
      paste0("<b>Proportion of variables with bias.</b> ", txt_proportion[[row$proportion]],
             img(img_proportion[[row$proportion]]))
    },
    paste0("<b>Project value.</b> The entire budget of the project is ", eur(row$budget), "."),
    paste0("<b>Cost.</b> If the survey were conducted in the NWP, it would cost &euro;4,000; if it were conducted in a probability sample, it would ",
           if (row$cost == "equal") "also cost &euro;4,000." else paste0("cost ", eur(row$prob_cost), ".")),
    paste0("<b>Funds for other activities.</b> ", txt_frame(row))
  )
  HTML(paste0(
    "<p>Please consider the following specific circumstances for this decision:</p><ol>",
    paste0("<li style='margin-bottom:1em;'>", items, "</li>", collapse = ""),
    "</ol>"
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
