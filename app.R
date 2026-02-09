library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(readxl)
library(dplyr)
library(scales)
library(tidyr)

# ---- Load data ----
FILE <- "GygaGermany.xlsx"

station <- read_excel(FILE, sheet = "Station") %>%
  mutate(
    yield_gap_tha = `YP-YA`,
    yield_gap_pct = ifelse(YP > 0, (`YP-YA`) / YP, NA_real_)
  )

cz <- read_excel(FILE, sheet = "Climate zone") %>%
  mutate(
    yield_gap_tha = `YP-YA`,
    yield_gap_pct = ifelse(YP > 0, (`YP-YA`) / YP, NA_real_)
  )

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Crop Yield Gap (GYGA)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("seedling")),
      hr(),
      selectInput("crop", "Crop", choices = sort(unique(station$CROP)),
                  selected = if ("Rainfed wheat" %in% station$CROP) "Rainfed wheat" else unique(station$CROP)[1]),
      selectInput("country", "Country", choices = sort(unique(station$COUNTRY)),
                  selected = if ("Netherlands" %in% station$COUNTRY) "Netherlands" else unique(station$COUNTRY)[1]),
      radioButtons("metric", "Map metric",
                   choices = c("Yield gap (t/ha)" = "yield_gap_tha",
                               "Yield gap (%)"    = "yield_gap_pct",
                               "Actual yield YA"  = "YA",
                               "Potential yield YP" = "YP"),
                   selected = "yield_gap_tha")
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("kpi_ya"),
      valueBoxOutput("kpi_yp"),
      valueBoxOutput("kpi_gap"),
      valueBoxOutput("kpi_gap_pct")
    ),
    fluidRow(
      box(width = 7, title = "Yield gap map (stations)", status = "primary",
          leafletOutput("map", height = 420)),
      box(width = 5, title = "Yield gap by climate zone", status = "primary",
          plotlyOutput("cz_bar", height = 420))
    ),
    fluidRow(
      box(width = 6, title = "Actual vs potential (by climate zone)", status = "info",
          plotlyOutput("ya_yp", height = 320)),
      box(width = 6, title = "Station distribution", status = "info",
          plotlyOutput("hist", height = 320))
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  station_f <- reactive({
    station %>%
      filter(COUNTRY == input$country, CROP == input$crop) %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE))
  })
  
  cz_f <- reactive({
    cz %>%
      filter(COUNTRY == input$country, CROP == input$crop)
  })
  
  # Area-weighted KPI helpers using climate-zone TOTAL_AREA_HA
  kpi <- reactive({
    d <- cz_f()
    if (nrow(d) == 0) return(NULL)
    
    area <- d$TOTAL_AREA_HA
    ya_w <- sum(d$YA * area) / sum(area)
    yp_w <- sum(d$YP * area) / sum(area)
    gap_w <- yp_w - ya_w
    gap_pct <- ifelse(yp_w > 0, gap_w / yp_w, NA_real_)
    
    list(ya = ya_w, yp = yp_w, gap = gap_w, gap_pct = gap_pct)
  })
  
  output$kpi_ya <- renderValueBox({
    x <- kpi(); req(x)
    valueBox(round(x$ya, 1), "Actual yield YA (t/ha)", icon = icon("chart-line"), color = "aqua")
  })
  
  output$kpi_yp <- renderValueBox({
    x <- kpi(); req(x)
    valueBox(round(x$yp, 1), "Potential yield YP (t/ha)", icon = icon("chart-area"), color = "teal")
  })
  
  output$kpi_gap <- renderValueBox({
    x <- kpi(); req(x)
    valueBox(round(x$gap, 1), "Yield gap (t/ha)", icon = icon("arrow-down"), color = "yellow")
  })
  
  output$kpi_gap_pct <- renderValueBox({
    x <- kpi(); req(x)
    valueBox(percent(x$gap_pct, accuracy = 0.1), "Yield gap (%)", icon = icon("percent"), color = "orange")
  })
  
  # Leaflet map (station points)
  output$map <- renderLeaflet({
    d <- station_f(); req(nrow(d) > 0)
    
    metric <- input$metric
    d <- d %>% mutate(metric_value = .data[[metric]])
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = d$metric_value,
      reverse = TRUE,
      na.color = "#cccccc"
    )
    
    leaflet(d) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        radius = 7,
        color = ~pal(metric_value),
        fillOpacity = 0.85,
        stroke = TRUE, weight = 1,
        label = ~paste0(
          STATIONNAME,
          "<br>YA: ", round(YA, 2),
          "<br>YP: ", round(YP, 2),
          "<br>Gap: ", round(`YP-YA`, 2),
          "<br>Gap%: ", scales::percent(yield_gap_pct, accuracy = 0.1)
        ) %>% lapply(htmltools::HTML)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~metric_value,
        title = metric,
        opacity = 0.8
      )
  })
  
  
  # Climate zone yield gap bar chart (area-weighted within climate zones)
  output$cz_bar <- renderPlotly({
    d <- cz_f(); req(nrow(d) > 0)
    
    d2 <- d %>%
      group_by(CLIMATEZONE) %>%
      summarise(
        area = sum(TOTAL_AREA_HA, na.rm = TRUE),
        ya = sum(YA * TOTAL_AREA_HA, na.rm = TRUE) / sum(TOTAL_AREA_HA, na.rm = TRUE),
        yp = sum(YP * TOTAL_AREA_HA, na.rm = TRUE) / sum(TOTAL_AREA_HA, na.rm = TRUE),
        gap = yp - ya,
        gap_pct = ifelse(yp > 0, gap / yp, NA_real_),
        .groups = "drop"
      ) %>%
      arrange(desc(gap))
    
    plot_ly(
      d2,
      x = ~gap,
      y = ~factor(CLIMATEZONE, levels = d2$CLIMATEZONE),
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "Climate zone: %{y}<br>",
        "Gap (t/ha): %{x:.2f}<br>",
        "YA: %{customdata[0]:.2f}<br>",
        "YP: %{customdata[1]:.2f}<br>",
        "Gap%: %{customdata[2]:.1%}<extra></extra>"
      ),
      customdata = ~cbind(ya, yp, gap_pct)
    ) %>%
      layout(
        xaxis = list(title = "Yield gap (t/ha)"),
        yaxis = list(title = ""),
        margin = list(l = 80, r = 20, t = 20, b = 40)
      )
  })
  
  # YA vs YP by climate zone
  output$ya_yp <- renderPlotly({
    d <- cz_f(); req(nrow(d) > 0)
    
    d2 <- d %>%
      group_by(CLIMATEZONE) %>%
      summarise(
        area = sum(TOTAL_AREA_HA, na.rm = TRUE),
        ya = sum(YA * TOTAL_AREA_HA, na.rm = TRUE) / sum(TOTAL_AREA_HA, na.rm = TRUE),
        yp = sum(YP * TOTAL_AREA_HA, na.rm = TRUE) / sum(TOTAL_AREA_HA, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(d2, x = ~ya, y = ~yp, type = "scatter", mode = "markers+text",
            text = ~CLIMATEZONE, textposition = "top center",
            marker = list(size = 12),
            hovertemplate = "CZ: %{text}<br>YA: %{x:.2f}<br>YP: %{y:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Actual yield YA (t/ha)"),
        yaxis = list(title = "Potential yield YP (t/ha)"),
        margin = list(l = 60, r = 20, t = 20, b = 50)
      )
  })
  
  # Histogram of station yield gaps (or chosen metric)
  output$hist <- renderPlotly({
    d <- station_f(); req(nrow(d) > 0)
    metric <- input$metric
    
    plot_ly(d, x = d[[metric]], type = "histogram") %>%
      layout(
        xaxis = list(title = input$metric),
        yaxis = list(title = "Count"),
        margin = list(l = 60, r = 20, t = 20, b = 50)
      )
  })
}

shinyApp(ui, server)
