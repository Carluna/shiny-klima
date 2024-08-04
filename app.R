# ------------------------------------------------------------------------------
# Shiny app to display temperature in germany - Data from openmeteo
# R 4.4.1 | Shiny 1.9.1
# Author: R. Kubina
# Date: 04.08.2024
# ------------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(shiny)
library(shinyjs)
library(bslib)
library(openmeteo)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(ggplot2)
# library(rnaturalearth)
library(echarts4r)


# Functions ---------------------------------------------------------------

get_temp_now <- function(cities = c(
                           "Freiburg", "Köln", "munich", "Berlin",
                           "Hamburg", "Dresden", "Düsseldorf", "Bremen",
                           "Stuttgart", "Potsdam", "Erfurt", "Hannover",
                           "Kiel", "Magdeburg", "Mainz", "Saarbrücken",
                           "Schwerin", "Wiesbaden", "Lahr"
                         )) {
  # Get coordinates of cities
  df_coords <- apply(as.array(cities), 1, geocode) |>
    list_rbind() |>
    select(name, latitude, longitude) |>
    st_as_sf(coords = c("longitude", "latitude"))

  # Get climate of the moment of the cities
  df_map <- apply(as.array(cities), 1, weather_now, response_unit = list(temperature_unit = "celsius")) |>
    list_rbind() |>
    cbind(df_coords)

  return(df_map)
}

get_forecast <- function(city = "Freiburg") {
  df <- weather_forecast(city,
    hourly = c("temperature_2m", "precipitation"),
    response_units = list(
      temperature_unit = "celsius",
      precipitation_unit = "mm"
    )
  ) |>
    rename(
      date = datetime,
      "Temperatur (°C)" = hourly_temperature_2m,
      "Niederschlag (mm)" = hourly_precipitation
    )

  return(df)
}


# Initialisation ----------------------------------------------------------
# Contour of Germany

# sh_ger <- rnaturalearth::ne_countries(scale = 50) |>
#   filter(name == "Germany")

sh_ger <- read_sf("./data/sh_ger.shp")

# Rivers in Germany

# rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical") |>
#   st_intersection(sh_ger)

rivers <- read_sf("./data/rivers.shp")

# Shiny-UI ----------------------------------------------------------------
# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(preset = "minty"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:inherit;",
      titlePanel("Temperaturen in Deutschland"),
      # Button to reload the actual temperature
      actionButton("reload", label = "Aktualisieren"),
      hr(),
      selectInput("select",
        label = h3("Vorhersage für:"),
        choices = list(
          "Berlin" = "Berlin", "Bremen" = "Bremen", "Dresden" = "Dresden",
          "Düsseldorf" = "Düsseldorf", "Erfurt" = "Erfurt", "Freiburg" = "Freiburg",
          "Hamburg" = "Hamburg", "Hannover" = "Hannover", "Kiel" = "Kiel",
          "Köln" = "Köln", "Lahr" = "Lahr", "Magdeburg" = "Magdeburg",
          "Mainz" = "Mainz", "München" = "Munich", "Potsdam" = "Potsdam",
          "Saarbrücken" = "Saarbrücken", "Schwerin" = "Schwerin",
          "Stuttgart" = "Stuttgart", "Wiesbaden" = "Wiesbaden"
        ),
        selected = "Freiburg"
      ),
      hr(),
      card(
        markdown(
          "Die Klimadaten wurden erhalten von [openmeteo](https://open-meteo.com/) mit dem R-Paket `openmeteo` und [Natural Earth](https://www.naturalearthdata.com/) mit dem R-Paket `rnaturalearth`.
          Zu sehen sind die Temperaturen in den Bundesländerhauptstädten und einigen zusätzlichen Städten."
        )
      )
    ),

    # Map of cities
    mainPanel(
      h3("   Aktuelle Temperatur:" ),
      plotOutput("map"),
      hr(),
      h3("   Vorhersage:"),
      echarts4rOutput("forecast"),
      hr()
    )
  )
)

# Shiny-Server ------------------------------------------------------------
# Define server logic
server <- function(input, output, session) {
  # Logic of reset button/get data
  df_map <- eventReactive(input$reload, {
    get_temp_now()
  })

  # Click reset button once to initiate the data
  start_up <- observe({
    shinyjs::click("reload")
    start_up$destroy() # destroy observer as it has no use after initial button click
  })

  df_forecast <- reactive({
    get_forecast(input$select)
  })

  # Plot the map
  output$map <- renderPlot({
    ggplot() +
      geom_sf(
        data = sh_ger,
        aes(geometry = geometry),
        fill = NA
      ) +
      geom_sf(
        data = rivers,
        aes(geometry = geometry),
        color = "darkblue"
      ) +
      geom_sf(
        data = df_map(),
        mapping = aes(
          geometry = geometry,
          fill = temperature,
          color = temperature
        ),
        size = 7.5, alpha = 0.3
      ) +
      geom_sf(
        data = df_map(),
        mapping = aes(
          geometry = geometry,
          fill = temperature,
          color = temperature
        ),
        size = 4
      ) +
      scale_fill_gradient(low = "blue", high = "darkred", name = "Temperatur (°C)") +
      scale_color_gradient(low = "blue", high = "darkred", name = "Temperatur (°C)") +
      theme_void() +
      theme()
  })

  output$forecast <- renderEcharts4r({
    df_forecast() |>
      e_charts(date) |>
      e_line(`Temperatur (°C)`, x_index = 1, y_index = 1) |>
      e_bar(`Niederschlag (mm)`) |>
      e_grid(height = "35%") |>
      e_grid(height = "35%", top = "50%") |>
      e_y_axis(gridIndex = 1) |>
      e_x_axis(gridIndex = 1) |>
      e_tooltip(trigger = "axis") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_color(color = c("darkred", "darkblue")) |>
      e_show_loading()
  })
}

# Run ---------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)
