library(shiny)
library(nflfastR)
library(tidyverse)
library(gt)
library(DT)
library(ggimage)
library(plotly)
library(ggrepel)
library(httr)
library(jsonlite)

team_colors <- nflfastR::teams_colors_logos %>%
  select(posteam = team_abbr, team_color, team_logo_espn)

# Load data --------------------------------------------------------------
load_pbp_supabase <- function(url = Sys.getenv("SUPABASE_URL"),
                              key = Sys.getenv("SUPABASE_KEY")) {

  if (url == "" || key == "") {
    stop("SUPABASE_URL and SUPABASE_KEY must be set")
  }

  res <- httr::GET(
    paste0(url, "/rest/v1/pbp"),
    httr::add_headers(
      apikey = key,
      Authorization = paste("Bearer", key),
      Accept = "application/json"
    ),
    query = list(select = "*")
  )

  if (httr::status_code(res) == 200) {
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  } else {
    stop("Supabase request failed with status ", httr::status_code(res))
  }
}

process_pbp <- function(dat) {
  dat %>%
    filter(down == 4, !is.na(play_type), !is.na(ydstogo), !is.na(yards_gained), !is.na(score_differential)) %>%
    mutate(
      went_for_it = play_type %in% c("run", "pass"),
      go_for_it_manual = case_when(
        ydstogo <= 4 & yardline_100 <= 50 & (!goal_to_go | ydstogo != yardline_100) ~ TRUE,
        TRUE ~ FALSE
      ),
      followed_model = went_for_it == go_for_it_manual,
      fourth_down_success = if_else(went_for_it & yards_gained >= ydstogo, TRUE, FALSE, missing = NA)
    )
}

# Load data from Supabase when the app starts
pbp <- process_pbp(load_pbp_supabase())

# UI for the 4th down analysis project
project_4th_down_ui <- fluidPage(
  titlePanel("NFL 4th Down Decision-Making: Analytics vs Reality"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team:", choices = unique(pbp$posteam)),
      sliderInput(
        "yardline", "Field Position (distance from end zone):",
        min = 1, max = 99, value = c(1, 99), step = 1
      ),
      selectInput(
        "quarter", "Quarter:",
        choices = c("All" = "all", "1", "2", "3", "4"), selected = "all"
      ),
      sliderInput(
        "score_diff", "Score Differential (team - opponent):",
        min = -30, max = 30, value = c(-30, 30), step = 1
      ),
      checkboxInput(
        "show_all_seasons", "Show All Seasons for Selected Team",
        value = FALSE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Team Summary", gt_output("summary_table"), plotOutput("comparison_plot")),
        tabPanel("Play Explorer", DTOutput("play_table")),
        tabPanel("Success vs Failure", gt_output("success_table"), plotOutput("success_plot")),
        tabPanel("League Trends", plotOutput("league_trend_plot"))
      )
    )
  )
)

# Simple home page with links to projects
home_ui <- fluidPage(
  h2("Welcome to the Sports Analytics Portfolio"),
  p("Select a project from the navigation bar above."),
  tags$ul(
    tags$li(actionLink("open_4th_down", "NFL 4th Down Decision-Making")),
    tags$li("More projects coming soon!")
  )
)

# Portfolio navigation structure
ui <- navbarPage(
  title = "Sports Analytics Portfolio",
  id = "main_nav",
  tabPanel("Home", home_ui),
  tabPanel("NFL 4th Down", project_4th_down_ui)
)

# Server
server <- function(input, output, session) {

  observeEvent(input$open_4th_down, {
    updateTabsetPanel(session, "main_nav", selected = "NFL 4th Down")
  })
  
  filtered_pbp <- reactive({
    pbp %>%
      filter(
        yardline_100 >= input$yardline[1],
        yardline_100 <= input$yardline[2],
        (if (input$quarter != "all") qtr == as.numeric(input$quarter) else TRUE),
        score_differential >= input$score_diff[1],
        score_differential <= input$score_diff[2]
      )
  })

  team_summary_combined <- reactive({
    filtered_pbp() %>%
      group_by(posteam) %>%
      summarize(
        Total_4th_Downs = n(),
        Went_For_It = sum(went_for_it, na.rm = TRUE),
        Go_Rate = Went_For_It / Total_4th_Downs,
        Recommended_Rate = mean(go_for_it_manual, na.rm = TRUE),
        Success_Rate = mean(fourth_down_success, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$summary_table <- render_gt({
    team_summary_combined() %>%
      filter(posteam == input$team) %>%
      select(posteam, Total_4th_Downs, Went_For_It, Go_Rate, Recommended_Rate, Success_Rate) %>%
      gt() %>%
      fmt_percent(columns = c(Go_Rate, Recommended_Rate, Success_Rate), decimals = 1) %>%
      cols_label(
        posteam = "Team",
        Total_4th_Downs = "Total 4th Downs",
        Went_For_It = "Went For It",
        Go_Rate = "Go Rate (Actual)",
        Recommended_Rate = "Recommended Go Rate",
        Success_Rate = "Success Rate"
      ) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_header(title = md(glue::glue("**4th Down Summary for {input$team}**")))
  })
  
  output$play_table <- renderDT({
    filtered_pbp() %>%
      filter(posteam == input$team) %>%
      select(game_id, week, yardline_100, qtr, play_type, go_for_it_manual, fourth_down_success, qb_epa, desc)
  })
  
  output$success_table <- render_gt({
    team_summary_combined() %>%
      filter(posteam == input$team) %>%
      select(posteam, Total_4th_Downs, Went_For_It, Go_Rate, Recommended_Rate, Success_Rate) %>%
      gt() %>%
      fmt_percent(columns = c(Go_Rate, Recommended_Rate, Success_Rate), decimals = 1) %>%
      cols_label(
        posteam = "Team",
        Total_4th_Downs = "Total 4th Downs",
        Went_For_It = "Went For It",
        Go_Rate = "Go Rate (Actual)",
        Recommended_Rate = "Recommended Go Rate",
        Success_Rate = "Success Rate"
      ) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_header(title = md(glue::glue("**4th Down Success Summary for {input$team}**")))
  })
  
  output$comparison_plot <- renderPlot({
    data <- team_summary_combined() %>% left_join(team_colors, by = "posteam")
    team_highlighted <- input$team
    color_selected <- data %>% filter(posteam == team_highlighted) %>% pull(team_color)

    data <- data %>% mutate(
      is_highlighted = posteam == team_highlighted,
      point_size = ifelse(is_highlighted, 5, 2)
    )

    ggplot(data, aes(x = Recommended_Rate, y = Go_Rate)) +
      geom_point(aes(color = is_highlighted, size = point_size), show.legend = FALSE) +
      geom_text(data = filter(data, !is_highlighted),
                aes(label = posteam, color = is_highlighted),
                fontface = "plain", show.legend = FALSE) +
      geom_text_repel(data = filter(data, is_highlighted),
                      aes(label = posteam, color = is_highlighted),
                      fontface = "bold", show.legend = FALSE) +
      geom_image(data = filter(data, is_highlighted),
                 aes(x = Recommended_Rate, y = Go_Rate, image = team_logo_espn), size = 0.06, asp = 1.5, inherit.aes = FALSE) +
      scale_color_manual(values = c("FALSE" = "gray", "TRUE" = color_selected), guide = "none") +
      scale_size_identity() +
      geom_abline(linetype = "dashed") +
      labs(title = "Actual vs Recommended 4th Down Rate", x = "Analytics Recommendation", y = "Coach Decision")
  })
  
  output$success_plot <- renderPlot({
    data <- team_summary_combined() %>% left_join(team_colors, by = "posteam")
    team_highlighted <- input$team
    color_selected <- data %>% filter(posteam == team_highlighted) %>% pull(team_color)

    data <- data %>% mutate(
      is_highlighted = posteam == team_highlighted,
      point_size = ifelse(is_highlighted, 5, 2)
    )

    league_avg_success <- mean(filtered_pbp()$fourth_down_success, na.rm = TRUE)
    league_avg_go_rate <- mean(filtered_pbp()$went_for_it, na.rm = TRUE)

    max_x <- max(data$Go_Rate, na.rm = TRUE)
    max_y <- max(data$Success_Rate, na.rm = TRUE)

    ggplot(data, aes(x = Go_Rate, y = Success_Rate)) +
      geom_point(aes(color = is_highlighted, size = point_size), show.legend = FALSE) +
      geom_text(data = filter(data, !is_highlighted),
                aes(label = posteam, color = is_highlighted),
                fontface = "plain", show.legend = FALSE) +
      geom_text_repel(data = filter(data, is_highlighted),
                      aes(label = posteam, color = is_highlighted),
                      fontface = "bold", show.legend = FALSE) +
      geom_image(data = filter(data, is_highlighted),
                 aes(x = Go_Rate, y = Success_Rate, image = team_logo_espn), size = 0.06, asp = 1.5, inherit.aes = FALSE) +
      geom_hline(yintercept = league_avg_success, linetype = "dashed", color = "black") +
      geom_vline(xintercept = league_avg_go_rate, linetype = "dashed", color = "black") +
      annotate("text", x = max_x, y = league_avg_success + 0.005, label = "Avg Success Rate", hjust = 0.5, size = 3.5) +
      annotate("text", x = league_avg_go_rate + 0.0025, y = max_y - 0.005, label = "Avg Go Rate", angle = 90, hjust = 0, size = 3.5) +
      scale_color_manual(values = c("FALSE" = "gray", "TRUE" = color_selected), guide = "none") +
      scale_size_identity() +
      labs(title = "Go-For-It Rate vs Success Rate", x = "Go Rate", y = "Success Rate")
  })
  
  output$league_trend_plot <- renderPlot({
    data <- pbp %>%
      mutate(
        season = as.numeric(substr(game_id, 1, 4)),
        success = went_for_it & yards_gained >= ydstogo
      )
    
    league_summary <- data %>%
      group_by(season) %>%
      summarize(
        Go_Rate = mean(went_for_it, na.rm = TRUE),
        Success_Rate = mean(success, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Go_Rate, Success_Rate), names_to = "Metric", values_to = "Rate") %>%
      mutate(Label = ifelse(Metric == "Go_Rate", "League Go Rate", "League Success Rate"))
    
    team_trend <- data %>%
      filter(posteam == input$team) %>%
      group_by(season) %>%
      summarize(
        Go_Rate = mean(went_for_it, na.rm = TRUE),
        Success_Rate = mean(success, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Go_Rate, Success_Rate), names_to = "Metric", values_to = "Rate") %>%
      mutate(Label = ifelse(Metric == "Go_Rate", paste(input$team, "Go Rate"), paste(input$team, "Success Rate")))
    
    combined <- bind_rows(league_summary, if (input$show_all_seasons) team_trend else NULL)
    
    ggplot(combined, aes(x = season, y = Rate, color = Label, linetype = Label)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = setNames(
        c("blue", "darkgreen", "red", "orange"),
        c("League Go Rate", "League Success Rate", 
          paste(input$team, "Go Rate"), 
          paste(input$team, "Success Rate"))
      )) +
      scale_linetype_manual(values = setNames(
        c("solid", "dashed", "solid", "dashed"),
        c("League Go Rate", "League Success Rate", 
          paste(input$team, "Go Rate"), 
          paste(input$team, "Success Rate"))
      )) +
      labs(title = "League-Wide and Team 4th Down Trends", x = "Season", y = "Rate", color = "Trend", linetype = "Trend") +
      theme_minimal()
  })
}

# Run app
shinyApp(ui, server)
