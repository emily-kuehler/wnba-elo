source("helper-functions.R")
library(gt)
library(htmltools)

my_con <- connect_to_aws_db()

sim_results <- DBI::dbGetQuery(my_con, "SELECT * FROM simulation_results")

summary_sim_results <- map(.x = INIT_SEASON:FINAL_SEASON, .f = summarise_sim_results, sim_results) %>% 
  bind_rows()

sim_results_summary_df <- DBI::dbGetQuery(my_con, "SELECT * FROM sim_results_summary")


extrafont::font_import(pattern = "Roboto Mono")
test_df <- sim_results_summary_df %>% 
  filter(season == 1997) %>% 
  select(-pct, -season) %>% 
  pivot_wider(names_from = round,
              values_from = prop) %>% 
  gt() %>% 
  data_color(
    columns = c("Semifinals","Finals"),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  fmt_percent(
    columns = c("Semifinals", "Finals"),
    decimals = 1
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c("Semifinals")
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  cols_label(
    team = "Team"
  ) %>% 
  tab_header(title = md("**Playoff Probabilities**")) %>% 
  tab_style(
    style = list(
      cell_text(
        font = "Roboto Mono",
        align = "center"
      )
    ),
    locations = list(
      cells_body(columns = c("team", "Semifinals", "Finals"))
    )
  )
test_df

playoff_salary <- read_csv("https://raw.githubusercontent.com/jthomasmock/radix_themockup/master/_posts/2020-05-13-qb-salaries-vs-playoff-appearances/playoff_salary.csv")














