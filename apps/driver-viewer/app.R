## =============================================================================
## Chesapeake environmental-driver viewer (Ches-ICAT)
##
## Read-only Shiny app that visualizes the monthly CBEFS ROMS-ECB hindcast
## drivers (1985-2024) already produced by make-environmental-drivers/ on the
## Ecospace F01-F04 grids. It only READS finished NetCDF stacks; it does not
## re-run the pipeline.
##
## Launch from the repo root:  shiny::runApp("apps/driver-viewer")
## =============================================================================

library(shiny)

## --- CONFIG  <-- EDIT HERE (mirrors make-environmental-drivers/cbefs-helpers.R)
## Paths are resolved relative to this app directory so the app is portable.
APP_DIR     <- if (nzchar(Sys.getenv("SHINY_APP_DIR"))) {
                 Sys.getenv("SHINY_APP_DIR")
               } else {
                 getwd()
               }
REPO_ROOT   <- normalizePath(file.path(APP_DIR, "..", ".."), mustWork = FALSE)

OUT_ROOT      <- file.path(REPO_ROOT, "output-for-ecospace/env-drivers/CBEFS-hindcast")
BASEMAP_DIR   <- file.path(REPO_ROOT, "output-for-ecospace/habitat/basemaps")
REGRID_SUBDIR <- "var-stack-NC-monthly-regridded"

RESOLUTIONS <- c("F01-176x111", "F02-88x56", "F03-59x37", "F04-44x28")  # F00 has no regrid
DEFAULT_RES <- "F02-88x56"
YEAR_START  <- 1985L
YEAR_END    <- 2024L

VARS   <- c("salinity", "temperature", "diss_o2", "phytoplankton", "NO3")
DEPTHS <- c("surf", "bott", "davg")

## Per-variable display + palette. Units confirmed against
## make-environmental-drivers/CBEFS-notes-metadata.txt and the methods report.
## Palettes are grDevices::hcl.colors() names (reuse the repo's PDF/GIF
## vocabulary from cbefs-helpers.R::cbefs_var_styles); domain is shared across
## depths so panels and animation frames are directly comparable.
VAR_STYLES <- list(
  temperature   = list(label = "Temperature",      units = "°C",
                       palette = "Heat",   rev = TRUE),
  salinity      = list(label = "Salinity",         units = "PSU",
                       palette = "viridis", rev = FALSE),
  diss_o2       = list(label = "Dissolved oxygen", units = "mg O₂ L⁻¹",
                       palette = "YlGnBu", rev = FALSE),
  phytoplankton = list(label = "Phytoplankton",    units = "mmol N m⁻³",
                       palette = "YlGn",   rev = FALSE),
  NO3           = list(label = "Nitrate",          units = "mmol N m⁻³",
                       palette = "Purples", rev = FALSE)
)

VAR_CHOICES   <- setNames(VARS,   vapply(VARS, function(v) VAR_STYLES[[v]]$label, ""))
DEPTH_LABELS  <- list(surf = "Surface", bott = "Bottom", davg = "Depth-averaged")
DEPTH_CHOICES <- setNames(DEPTHS, unlist(DEPTH_LABELS[DEPTHS]))

MAX_PANELS <- 8L

source(file.path(APP_DIR, "R", "helpers.R"), local = FALSE)

## --- month <-> index helpers (480 months, 1985-01 .. 2024-12) ----------------
idx_to_label <- function(res, i) month_index(res)$label[i]
ym_to_idx <- function(res, year, month) {
  lab <- sprintf("%04d-%02d", year, month)
  match(lab, month_index(res)$label)
}

## =============================================================================
## UI
## =============================================================================
ui <- fluidPage(
  titlePanel("Chesapeake environmental drivers — CBEFS hindcast (1985–2024)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("res", "Grid resolution", choices = RESOLUTIONS, selected = DEFAULT_RES),
      checkboxGroupInput("vars", "Variables", choices = VAR_CHOICES,
                         selected = c("salinity", "temperature", "diss_o2",
                                      "phytoplankton", "NO3")),
      checkboxGroupInput("depths", "Depths", choices = DEPTH_CHOICES,
                         selected = c("bott", "surf")),
      uiOutput("panel_warn"),
      tags$hr(),
      sliderInput("midx", "Month", min = 1, max = 480, value = 1, step = 1,
                  width = "100%", ticks = FALSE),
      div(style = "text-align:center; font-weight:bold;", textOutput("ym_label")),
      fluidRow(
        column(6, selectInput("jump_year", "Year",
                              choices = YEAR_START:YEAR_END, selected = YEAR_START)),
        column(6, selectInput("jump_month", "Month",
                              choices = setNames(1:12, month.abb), selected = 1))
      ),
      tags$hr(),
      div(
        actionButton("play",  "▶ Play"),
        actionButton("pause", "❚❚ Pause"),
        actionButton("step",  "▶❘ Step")
      ),
      sliderInput("speed", "Animation speed (ms/frame)",
                  min = 200, max = 2000, value = 600, step = 100),
      tags$hr(),
      checkboxInput("show_mask",  "Land mask",    value = TRUE),
      checkboxInput("show_coast", "Coastline",    value = TRUE),
      checkboxInput("show_grid",  "Grid outline", value = FALSE)
    ),
    mainPanel(
      width = 9,
      plotOutput("panels", height = "780px")
    )
  )
)

## =============================================================================
## SERVER
## =============================================================================
server <- function(input, output, session) {

  playing <- reactiveVal(FALSE)

  ## Default depth pre-selection per the spec's example view is approximated by
  ## the bott/surf choice above; the var x depth product builds the panel set.

  ## Keep the month slider's max in sync with the active resolution.
  observeEvent(input$res, {
    n <- n_months(input$res)
    updateSliderInput(session, "midx", max = n,
                      value = min(isolate(input$midx), n))
  })

  ## Slider -> jump-to controls (one-way echo to avoid feedback loops; the
  ## jump-to selectors drive the slider below).
  observeEvent(input$midx, {
    lab <- idx_to_label(input$res, input$midx)
    if (!is.na(lab)) {
      yr <- as.integer(substr(lab, 1, 4)); mo <- as.integer(substr(lab, 6, 7))
      updateSelectInput(session, "jump_year",  selected = yr)
      updateSelectInput(session, "jump_month", selected = mo)
    }
  }, ignoreInit = TRUE)

  ## Jump-to controls -> slider.
  observeEvent(list(input$jump_year, input$jump_month), {
    i <- ym_to_idx(input$res, as.integer(input$jump_year), as.integer(input$jump_month))
    if (!is.na(i) && i != isolate(input$midx)) updateSliderInput(session, "midx", value = i)
  }, ignoreInit = TRUE)

  output$ym_label <- renderText({
    lab <- idx_to_label(input$res, input$midx)
    if (is.na(lab)) "" else lab
  })

  ## --- Animation -------------------------------------------------------------
  observeEvent(input$play,  playing(TRUE))
  observeEvent(input$pause, playing(FALSE))
  observeEvent(input$step, {
    playing(FALSE)
    n <- n_months(input$res)
    updateSliderInput(session, "midx", value = (input$midx %% n) + 1)
  })
  observe({
    if (!playing()) return()
    invalidateLater(input$speed, session)
    isolate({
      n <- n_months(input$res)
      updateSliderInput(session, "midx", value = (input$midx %% n) + 1)
    })
  })

  ## --- Panel-count warning ---------------------------------------------------
  output$panel_warn <- renderUI({
    n <- length(input$vars) * length(input$depths)
    if (n == 0) {
      div(style = "color:#a00;", "Select at least one variable and one depth.")
    } else if (n > MAX_PANELS) {
      div(style = "color:#a60;",
          sprintf("%d panels selected (> %d). Rendering may be slow.", n, MAX_PANELS))
    }
  })

  ## --- Main render -----------------------------------------------------------
  output$panels <- renderPlot({
    req(length(input$vars) > 0, length(input$depths) > 0)
    res  <- input$res
    midx <- input$midx
    ## var x depth grid, variables grouped together (rows = variables).
    combos <- expand.grid(depth = input$depths, var = input$vars,
                          stringsAsFactors = FALSE)
    plots <- Map(function(v, d)
      plot_driver(res, v, d, midx,
                  show_mask  = input$show_mask,
                  show_coast = input$show_coast,
                  show_grid  = input$show_grid),
      combos$var, combos$depth)

    patchwork::wrap_plots(plots, ncol = length(input$depths)) +
      patchwork::plot_annotation(
        title = idx_to_label(res, midx),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5)))
  }, res = 96)
}

shinyApp(ui, server)
