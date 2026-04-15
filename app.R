# WITU TESTING

# ---------------------------
# LIBRARIES
# ---------------------------
library(shiny)
library(data.table)
library(leaflet)
library(sf)

# ---------------------------
# FILE PATHS
# ---------------------------
witu_rds_file <- "data_processed/ebd/witu_data.rds"
va_boundary_rds <- "data_processed/shapefiles/va_counties.rds"

# ---------------------------
# FILE CHECKS
# ---------------------------
if (!file.exists(witu_rds_file)) {
  stop("WITU data file not found at: ", witu_rds_file)
}

if (!file.exists(va_boundary_rds)) {
  stop("Virginia boundary RDS not found at: ", va_boundary_rds)
}

# ---------------------------
# LOAD & PREP DATA
# ---------------------------
witu_dt <- readRDS(witu_rds_file)
witu_dt <- as.data.table(witu_dt)

required_cols <- c(
  "observation_date",
  "county",
  "latitude",
  "longitude",
  "observation_count",
  "locality",
  "sampling_event_identifier",
  "observer_id",
  "month"
)

missing_cols <- setdiff(required_cols, names(witu_dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns in witu_data.rds: ", paste(missing_cols, collapse = ", "))
}

witu_dt[, observation_date := as.IDate(observation_date)]
witu_dt[, year := as.integer(format(observation_date, "%Y"))]
witu_dt <- witu_dt[!is.na(latitude) & !is.na(longitude)]

# ---------------------------
# UI INPUT CHOICES
# ---------------------------
county_choices <- c("All counties", sort(unique(na.omit(witu_dt$county))))
month_choices <- c("All months" = "all", setNames(1:12, month.name))
year_choices <- as.character(2002:as.integer(format(Sys.Date(), "%Y")))

# ---------------------------
# LOAD BOUNDARY
# ---------------------------
va_boundary <- readRDS(va_boundary_rds)

va_bbox <- st_bbox(va_boundary)
va_lng <- unname((va_bbox["xmin"] + va_bbox["xmax"]) / 2)
va_lat <- unname((va_bbox["ymin"] + va_bbox["ymax"]) / 2)

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Virginia Wild Turkey Sightings"),
  
  div(
    style = "font-size: 16px; margin-top: 10px; margin-bottom: 15px;",
    "Select at least one county to display sightings. Use month and year range to refine the results."
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # ---- COUNTY ----
      tags$div(
        class = "form-group shiny-input-container",
        style = "margin-bottom: 2px;",
        tags$label(`for` = "county", "County"),
        selectizeInput(
          "county",
          label = NULL,
          choices = county_choices,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select one or more",
            onBlur = I(
              "function() {
          Shiny.setInputValue('county_committed', this.items, {priority: 'event'});
          }"
            )
          )
        )
      ),
      
      div(
        style = "font-size: 12px; color: #555555; margin-top: -12px; margin-bottom: 14px;",
        "Click outside the county field to apply your selection."
      ),
      
      # ---- MONTH ----
      selectizeInput(
        "month",
        "Month",
        choices = month_choices,
        selected = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select one or more")
      ),
      
      # ---- YEAR RANGE ----
      div(
        class = "form-group shiny-input-container",
        style = "margin-bottom: 0px;",
        div(
          style = "display: block; font-weight: 600; margin-bottom: 5px;",
          "Year Range"
        ),
        div(
          style = "display: flex; gap: 2%;",
          div(
            style = "width: 49%;",
            selectInput(
              "start_year",
              label = NULL,
              choices = year_choices,
              selected = min(year_choices),
              selectize = FALSE,
              width = "100%"
            )
          ),
          div(
            style = "width: 49%;",
            selectInput(
              "end_year",
              label = NULL,
              choices = year_choices,
              selected = max(year_choices),
              selectize = FALSE,
              width = "100%"
            )
          )
        )
      ),
      
      br(),
      
      # ---- SUMMARY ----
      strong("Summary"),
      
      div(
        style = "padding: 10px; background-color: #f2f2f2; border: 1px solid #2A5235; border-radius: 3px; margin-top: 10px; margin-bottom: 10px;",
        div(
          style = "font-size: 12px; color: #111111; margin-bottom: 8px;",
          textOutput("total_individuals")
        ),
        div(
          style = "font-size: 12px; color: #111111; margin-bottom: 8px;",
          textOutput("average_rafter_size")
        ),
        div(
          style = "font-size: 12px; color: #111111;",
          textOutput("checklist_count")
        )
      ),
      
      # ---- LEGEND ----
      div(
        style = "margin-top: 15px; margin-bottom: 15px;",
        strong("Legend"),
        uiOutput("map_legend")
      ),
      
      # ---- RESET ----
      actionButton("reset_filters", "Reset Filters")
    ),
    
    # ---------------------------
    # MAIN PANEL
    # ---------------------------
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel(
          "Map",
          leafletOutput("witu_map", height = 500)
        ),
        
        tabPanel(
          "County Summary",
          br(),
          htmlOutput("county_summary_title"),
          br(),
          tableOutput("county_summary_table"),
          br(),
          downloadButton("download_county_detail_csv", "Download Detailed CSV")
        )
      )
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  # ---- COUNTY COMMIT ----
  county_committed <- reactiveVal(character(0))
  
  observeEvent(input$county_committed, {
    county_committed(input$county_committed)
  }, ignoreNULL = FALSE)
  
  # ---- FILTERED DATA ----
  filtered_data <- reactive({
    dt <- copy(witu_dt)
    
    if (!is.null(input$start_year) && !is.null(input$end_year)) {
      dt <- dt[year >= as.integer(input$start_year) & year <= as.integer(input$end_year)]
    }
    
    if (!is.null(input$county) && length(input$county) > 0 && !("All counties" %in% input$county)) {
      dt <- dt[county %in% input$county]
    }
    
    if (!is.null(input$month) && length(input$month) > 0 && !("all" %in% input$month)) {
      dt <- dt[month %in% as.integer(input$month)]
    }
    
    dt
  })
  
  # ---- MAP FILTERED DATA ----
  map_filtered_data <- reactive({
    dt <- copy(witu_dt)
    selected_counties <- county_committed()
    
    if (!is.null(input$start_year) && !is.null(input$end_year)) {
      dt <- dt[year >= as.integer(input$start_year) & year <= as.integer(input$end_year)]
    }
    
    if (!is.null(selected_counties) && length(selected_counties) > 0 && !("All counties" %in% selected_counties)) {
      dt <- dt[county %in% selected_counties]
    }
    
    if (!is.null(input$month) && length(input$month) > 0 && !("all" %in% input$month)) {
      dt <- dt[month %in% as.integer(input$month)]
    }
    
    dt
  })
  
  # ---- SUMMARY OUTPUTS ----
  output$total_individuals <- renderText({
    dt <- filtered_data()
    total <- sum(as.numeric(dt$observation_count), na.rm = TRUE)
    paste("Total Individuals:", format(total, big.mark = ","))
  })
  
  output$average_rafter_size <- renderText({
    dt <- filtered_data()
    avg_rafter <- mean(as.numeric(dt$observation_count), na.rm = TRUE)
    paste("Average Rafter Size:", round(avg_rafter, 1))
  })
  
  output$checklist_count <- renderText({
    dt <- filtered_data()
    paste("Unique checklists:", format(uniqueN(dt$sampling_event_identifier), big.mark = ","))
  })
  
  # ---- LEGEND ----
  output$map_legend <- renderUI({
    div(
      style = "margin-top: 5px; font-size: 13px;",
      HTML("<span style='font-size:18px; color:#3388ff;'>●</span> Unique Checklist")
    )
  })
  
  # ---- COUNTY SUMMARY TITLE ----
  output$county_summary_title <- renderUI({
    month_label <- if (
      is.null(input$month) || length(input$month) == 0 || "all" %in% input$month
    ) {
      "All months"
    } else {
      paste(month.name[as.integer(input$month)], collapse = ", ")
    }
    
    HTML(
      paste0(
        "<div style='font-weight:600; font-size:14px;'>",
        "County summary for years ", input$start_year, " to ", input$end_year,
        " | Months: ", month_label,
        "</div>"
      )
    )
  })
  
  # ---- COUNTY SUMMARY TABLE ----
  output$county_summary_table <- renderTable({
    if (is.null(input$county) || length(input$county) == 0) {
      return(NULL)
    }
    
    dt <- filtered_data()
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    summary_dt <- dt[
      ,
      .(
        `Total Individuals` = sum(as.numeric(observation_count), na.rm = TRUE),
        `Average Rafter Size` = round(mean(as.numeric(observation_count), na.rm = TRUE), 1),
        `Unique Checklists` = uniqueN(sampling_event_identifier)
      ),
      by = county
    ][order(county)]
    
    setnames(summary_dt, "county", "County")
    summary_dt
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")
  
  # ---- COUNTY DETAIL DATA ----
  county_detail_data <- reactive({
    if (is.null(input$county) || length(input$county) == 0) {
      return(NULL)
    }
    
    dt <- filtered_data()
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    dt[, Month := month.name[month]]
    
    detail_dt <- dt[
      ,
      .(
        `Total Individuals` = sum(as.numeric(observation_count), na.rm = TRUE),
        `Average Rafter Size` = round(mean(as.numeric(observation_count), na.rm = TRUE), 1),
        `Unique Checklists` = uniqueN(sampling_event_identifier)
      ),
      by = .(county, year, Month)
    ][order(county, year, match(Month, month.name))]
    
    setnames(detail_dt, c("county", "year"), c("County", "Year"))
    detail_dt
  })
  
  # ---- COUNTY DETAIL CSV DOWNLOAD ----
  output$download_county_detail_csv <- downloadHandler(
    filename = function() {
      paste0("county_detailed_breakdown_", Sys.Date(), ".csv")
    },
    content = function(file) {
      detail_dt <- county_detail_data()
      
      if (is.null(detail_dt) || nrow(detail_dt) == 0) {
        write.csv(data.frame(Message = "No data available for current filters."), file, row.names = FALSE)
      } else {
        write.csv(detail_dt, file, row.names = FALSE)
      }
    }
  )
  
  # ---- INITIAL MAP ----
  output$witu_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomSnap = 0,
        zoomDelta = 1
      )
    ) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        data = va_boundary,
        color = "#2A5235",
        weight = 1,
        fill = FALSE,
        opacity = 1
      ) |>
      setView(
        lng = va_lng,
        lat = va_lat,
        zoom = 6.7
      )
  })
  
  # ---- RESET FILTERS ----
  observeEvent(input$reset_filters, {
    county_committed(character(0))
    updateSelectInput(session, "start_year", selected = min(year_choices))
    updateSelectInput(session, "end_year", selected = max(year_choices))
    updateSelectizeInput(session, "county", selected = character(0))
    updateSelectizeInput(session, "month", selected = character(0))
  })
  
  # ---- MAP UPDATE ----
  observe({
    dt <- map_filtered_data()
    selected_counties <- county_committed()
    
    proxy <- leafletProxy("witu_map")
    
    proxy |>
      clearMarkers() |>
      clearMarkerClusters() |>
      clearShapes() |>
      clearControls()
    
    proxy |>
      addPolygons(
        data = va_boundary,
        color = "#2A5235",
        weight = 1,
        fill = FALSE,
        opacity = 1
      )
    
    if (
      nrow(dt) == 0 ||
      is.null(selected_counties) || length(selected_counties) == 0
    ) {
      proxy |>
        setView(
          lng = va_lng,
          lat = va_lat,
          zoom = 6.7
        )
      return()
    }
    
    cluster_opts <- markerClusterOptions(
      iconCreateFunction = htmlwidgets::JS("
      function(cluster) {
        var count = cluster.getChildCount();
        return L.divIcon({
          html: '<div style=\"background-color:#F36C21;\"><span>' + count + '</span></div>',
          className: 'marker-cluster',
          iconSize: new L.Point(40, 40)
          });
        }
        ")
    )
    
    popup_text <- paste0(
      "<b>Date:</b> ", dt$observation_date, "<br/>",
      "<b>County:</b> ", ifelse(is.na(dt$county), "Unknown", dt$county), "<br/>",
      "<b>Locality:</b> ", ifelse(is.na(dt$locality), "Unknown", dt$locality), "<br/>",
      "<b>Count:</b> ", ifelse(is.na(dt$observation_count), "NA", as.character(dt$observation_count)), "<br/>",
      "<b>Checklist:</b> ", ifelse(is.na(dt$sampling_event_identifier), "Unknown", dt$sampling_event_identifier), "<br/>",
      "<b>Observer:</b> ", ifelse(is.na(dt$observer_id), "Unknown", dt$observer_id)
    )
    
    if ("All counties" %in% selected_counties || length(selected_counties) > 1) {
      
      if (!("All counties" %in% selected_counties)) {
        dt <- dt[county %in% selected_counties]
        popup_text <- paste0(
          "<b>Date:</b> ", dt$observation_date, "<br/>",
          "<b>County:</b> ", ifelse(is.na(dt$county), "Unknown", dt$county), "<br/>",
          "<b>Locality:</b> ", ifelse(is.na(dt$locality), "Unknown", dt$locality), "<br/>",
          "<b>Count:</b> ", ifelse(is.na(dt$observation_count), "NA", as.character(dt$observation_count)), "<br/>",
          "<b>Checklist:</b> ", ifelse(is.na(dt$sampling_event_identifier), "Unknown", dt$sampling_event_identifier), "<br/>",
          "<b>Observer:</b> ", ifelse(is.na(dt$observer_id), "Unknown", dt$observer_id)
        )
      }
      
      proxy |>
        addCircleMarkers(
          lng = dt$longitude,
          lat = dt$latitude,
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = popup_text,
          clusterOptions = cluster_opts
        )
      
      if ("All counties" %in% selected_counties) {
        proxy |>
          setView(
            lng = va_lng,
            lat = va_lat,
            zoom = 6.7
          )
        return()
      }
    } else {
      proxy |>
        addCircleMarkers(
          lng = dt$longitude,
          lat = dt$latitude,
          radius = 4,
          stroke = FALSE,
          fillOpacity = 0.7,
          popup = popup_text
        )
    }
    
    if (length(selected_counties) == 1) {
      buffer <- 0.08
    } else if (length(selected_counties) <= 3) {
      buffer <- 0.15
    } else {
      buffer <- 0.3
    }
    
    proxy |>
      fitBounds(
        lng1 = unname(min(dt$longitude, na.rm = TRUE) - buffer),
        lat1 = unname(min(dt$latitude, na.rm = TRUE) - buffer),
        lng2 = unname(max(dt$longitude, na.rm = TRUE) + buffer),
        lat2 = unname(max(dt$latitude, na.rm = TRUE) + buffer),
        options = list(maxZoom = 10)
      )
  })
}

# ---------------------------
# RUN APP
# ---------------------------
shinyApp(ui, server)