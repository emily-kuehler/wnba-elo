
# elo line plot -----------------------------------------------------------

plot_elo_values <- function(selected_teams, elo_vals, team_color_df) {
  
  selected_colors <- team_color_df %>% 
    filter(club_code %in% selected_teams)
  
  color_vector <- as.character(selected_colors$hex_color)
  names(color_vector) <- selected_colors$club_code
  
  season_df <- elo_vals %>% 
    group_by(season) %>% 
    filter(game_idx == min(game_idx)) %>% 
    distinct(game_idx, season) %>% 
    filter(season %% 4 == 0)
  
  season_vector <- as.character(season_df$game_idx)
  names(season_vector) <- season_df$season
  
  elo_vals <- elo_vals %>%
    group_by(team_club_code) %>% 
    arrange(team_club_code, game_date) %>%
    mutate(roll_avg_elo = RcppRoll::roll_mean(pregame_elo_tm, n = 10, align = "right", fill = NA)) %>% 
    select(team_club_code,game_date, pregame_elo_tm, roll_avg_elo, game_idx)
  
  elo_plot <- ggplot() +
    geom_line(data = elo_vals %>% filter(!team_club_code %in% selected_teams),
              aes(x = game_idx, y = roll_avg_elo, group = team_club_code), 
              color = "grey",
              size = 1) +
    geom_line(data = elo_vals %>% filter(team_club_code %in% selected_teams),
              aes(x = game_idx, y = roll_avg_elo, color = team_club_code),
              size = 1.25) +
    scale_color_manual(values = color_vector) +
    scale_x_continuous("", 
                       breaks = season_df$game_idx,
                       labels = season_df$season) +
    theme(
      text = element_text(family = "Roboto Mono"),
      panel.grid.major = element_line(colour = "#DCDCDC"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 20),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.background = element_rect(fill="white"),
      plot.background = element_rect(fill="white"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(size = 12)
    ) +
    labs(title = "Historical Team Strength",
         subtitle = "Measured By ELO Score",
         y = "ELO Score",
         color = "Team")
  return (elo_plot)
  
}


# playoff probability table -----------------------------------------------

get_playoff_probability_table <- function(sim_results_summary) {
  
  playoff_table <- sim_results_summary %>% 
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
  return (playoff_table)
  
}


