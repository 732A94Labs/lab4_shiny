# app.R
required_pkgs <- c("shiny", "shinyjs", "bslib", "sf", "httr2", "jsonlite")
missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

if (length(missing_pkgs)) {
  stop("Install required packages before launching the app: ",
       paste(missing_pkgs, collapse = ", "),
       call. = FALSE)
}

source("api_client.R")
library(httr2)
library(jsonlite)
library(shiny)
library(sf)
library(bslib)
library(shinyjs)

ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#4F46E5",
    secondary = "#14B8A6",
    success = "#10B981",
    info = "#3B82F6",
    warning = "#F59E0B",
    danger = "#EF4444",
    base_font = font_google("Inter"),
    heading_font = font_google("Plus Jakarta Sans")
  ),
  sidebar = sidebar(
    useShinyjs(),
    class = "px-3 pt-3",
    tags$div(
      class = "app-title mb-3",
      tags$h2(class = "mb-0 fw-bold", "Country Guessr"),
      tags$p(class = "text-muted mb-0 small",
             "Guess the country from its outline. You have 3 health.")
    ),
    tags$div(class = "status-row d-flex gap-2 mb-3",
      tags$div(class = "pill pill-score",  textOutput("score")),
      tags$div(class = "pill pill-health", textOutput("health"))
    ),
    tags$div(class = "mb-2",
      textInput("guess", label = NULL, placeholder = "Type country name...")
    ),
    tags$div(class = "d-grid gap-2",
      actionButton("guess_btn", "Guess!", class = "btn btn-primary btn-lg"),
      actionButton("next_btn",  "Next Country", class = "btn btn-outline-primary"),
      actionButton("reset_btn", "New Game", class = "btn btn-light")
    ),
    tags$hr(class = "my-4"),
    tags$div(class = "feedback-area",
      tags$div(class = "alert alert-info mb-0", textOutput("feedback"))
    )
  ),
  # Main area
  tags$div(class = "main-wrap container-fluid py-4",
    tags$div(class = "card shadow-lg border-0 rounded-4",
      tags$div(class = "card-header bg-gradient text-white rounded-top-4",
               tags$h5(class = "mb-0", "Current Outline")),
      tags$div(class = "card-body",
               plotOutput("countryPlot", height = 520))
    ),
    tags$footer(class = "text-center text-muted small mt-4",
      "Tip: Press Enter in the input to submit your guess.")
  ),
  # enter shortcut
  tags$script(HTML("
    $(document).on('keyup', function(e) {
      if (e.key === 'Enter' && $('#guess').is(':focus')) {
        $('#guess_btn').click();
      }
    });
  ")),
  # css
  tags$style(HTML("
    :root {
      --glass-bg: rgba(255,255,255,0.65);
      --glass-blur: 10px;
    }
    body {
      background: radial-gradient(1200px 800px at 20% 0%, #e6f0ff 0%, #f7fafc 40%, #ffffff 100%);
    }
    .app-title h2 { letter-spacing: 0.3px; }
    .pill {
      padding: .35rem .75rem;
      border-radius: 999px;
      font-weight: 600;
      line-height: 1.2;
      display: inline-flex;
      align-items: center;
      gap: .5rem;
      backdrop-filter: blur(var(--glass-blur));
      background: var(--glass-bg);
      border: 1px solid rgba(0,0,0,0.05);
    }
    .pill-score  { color:#1e293b; }
    .pill-health { color:#065f46; }
    .status-row .pill { font-size: .95rem; }
    #guess { height: 3rem; font-size: 1.05rem; }
    .btn-lg { padding:.75rem 1rem; font-weight:700; }
    .feedback-area .alert {
      border: none;
      background: rgba(59, 130, 246, .08);
    }
    .card-header {
      background: linear-gradient(90deg, #4F46E5, #14B8A6);
    }
    .card { overflow: hidden; }
    .main-wrap { max-width: 1100px; }
    /* Make textOutput inside pills not jump */
    .pill > div { margin: 0 !important; }
  "))
)

server <- function(input, output, session) {
  # Load once, non-reactive (plain data)
  countries_df <- fetch_countries()[, c("@id", "name:en")]

  # Game state (no countries here!)
  rv <- reactiveValues(
    current = NULL,
    geom = NULL,
    score = 0L,
    health = 3L,
    game_over = FALSE
  )

  pick_new_country <- function() {
    choice <- countries_df[sample.int(nrow(countries_df), 1), , drop = FALSE]
    rel_id <- choice[["@id"]]

    rv$current <- choice
    rv$geom <- fetch_country_geom_by_relation(rel_id)

    output$feedback <- renderText("")
    updateTextInput(session, "guess", value = "")
    shinyjs::enable("guess_btn")
    shinyjs::enable("guess")
  }


  start_game <- function() {
    rv$score <- 0L
    rv$health <- 3L
    rv$game_over <- FALSE
    pick_new_country()
  }

  start_game()

  # Outputs
  output$score  <- renderText(sprintf("Score: %d", rv$score))
  output$health <- renderText(sprintf("Health: %d", rv$health))

  output$countryPlot <- renderPlot({
    req(rv$geom)
    plot(st_geometry(rv$geom), axes = FALSE, main = "")
    box(lwd = 1)
  })

  # Next Country
  observeEvent(input$next_btn, {
    if (!rv$game_over) {
      pick_new_country()
    } else {
      start_game()
    }
  })

  # Guess handling
  observeEvent(input$guess_btn, {
  req(!rv$game_over, rv$current)

  user_guess <- trimws(tolower(input$guess))
  answer <- tolower(rv$current[["name:en"]])

  shinyjs::disable("guess_btn")
  shinyjs::disable("guess")

  if (identical(user_guess, "")) {
    output$feedback <- renderText("Type a guess before clicking Guess!")
    shinyjs::enable("guess_btn")
    shinyjs::enable("guess")
    return(invisible())
  }

  is_correct <- identical(user_guess, answer)

  if (is_correct) {
    rv$score <- rv$score + 1L
    output$feedback <- renderText(sprintf(
      "CORRECT! It was %s. Loading next country...",
      rv$current[["name:en"]]
    ))

    shinyjs::delay(5000, pick_new_country())
  } else {
    rv$health <- rv$health - 1L
    if (rv$health <= 0L) {
      rv$game_over <- TRUE
      output$feedback <- renderText(sprintf(
        "WRONG! It was %s. You lost 1 health and have 0 left. Game over! Final score: %d.",
        rv$current[["name:en"]], rv$score
      ))
    } else {
      output$feedback <- renderText(sprintf(
        "WRONG! It was %s. You lost 1 health. %d health remaining. Loading next country...",
        rv$current[["name:en"]], rv$health
      ))
      shinyjs::delay(5000, pick_new_country())
    }
  }
})

  observeEvent(input$reset_btn, start_game)
}

shinyApp(ui, server)
