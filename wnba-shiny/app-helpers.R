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
      panel.grid.major = element_line(colour = "#DCDCDC"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 24),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.background = element_rect(fill="white"),
      plot.background = element_rect(fill="white"),
      legend.key = element_rect(fill = "white"),
      legend.title = element_text(size = 12)
    ) +
    labs(title = "Historical ELO Values",
         y = "ELO Score",
         color = "Team")
  return (elo_plot)
  
}