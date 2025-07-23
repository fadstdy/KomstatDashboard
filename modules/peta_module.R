# modules/peta_module.R

# UI function untuk Peta 
petaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol Horizontal di Atas 
    column(12,
           box(
             title = "🗺️ Panel Kontrol Peta",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             fluidRow(
               column(4,
                      selectInput(ns("map_variable"), 
                                  "Pilih Variabel untuk Visualisasi:",
                                  choices = list("Memuat variabel..." = ""))
               ),
               column(3,
                      numericInput(ns("n_bins"), 
                                   "Jumlah Interval Warna:",
                                   value = 5, min = 3, max = 10, step = 1)
               ),
               column(3,
                      selectInput(ns("classification_method"), 
                                  "Metode Klasifikasi:",
                                  choices = list(
                                    "Quantile" = "quantile",
                                    "Equal Interval" = "equal",
                                    "Natural Breaks (Jenks)" = "jenks"
                                  ))
               ),
               column(2,
                      br(),
                      actionButton(ns("generate_map"), 
                                   "Buat Peta", 
                                   class = "btn-primary btn-block")
               )
             ),
             
             # Info tambahan
             div(
               style = "margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
               p(style = "margin: 0; color: #007bff; font-weight: bold;")
             )
           )
    ),
    
    # Layout dengan peta dan panel statistik 
    column(8,
           box(
             title = "Peta Interaktif",
             status = "success",
             solidHeader = TRUE,
             width = NULL,
             height = "650px",
             withSpinner(leafletOutput(ns("distribution_map"), height = "600px"))
           )
    ),
    
    # Kolom panel statistik detail 
    column(4,
           box(
             title = "Statistik Detail Wilayah",
             status = "info",
             solidHeader = TRUE,
             width = NULL,
             height = "650px",
             style = "overflow-y: auto;",
             uiOutput(ns("district_info_panel"))
           )
    ),
    
    # Statistik Umum dan Interpretasi 
    column(4,
           box(
             title = "Statistik Spasial",
             status = "warning",
             solidHeader = TRUE,
             width = NULL,
             collapsible = TRUE,
             withSpinner(tableOutput(ns("spatial_stats")))
           )
    ),
    column(8,
           box(
             title = "Interpretasi Peta",
             status = "warning", 
             solidHeader = TRUE,
             width = NULL,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("map_interpretation")))
           )
    ),
    
    # Download section 
    column(12,
           box(
             title = "Download",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             div(
               style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
               h5("ℹ️ Penjelasan Download:", style = "margin: 0; color: #495057;"),
               tags$ul(
                 tags$li("Data Peta (CSV): Data numerik hasil join antara statistik dan geometri"),
                 tags$li("Interpretasi (TXT): Analisis statistik dan interpretasi pola spasial"),
                 style = "margin: 5px 0 0 0; color: #495057; font-size: 12px;"
               )
             ),
             
             fluidRow(
               column(6,
                      downloadButton(ns("download_data"), 
                                     "Download Data (CSV)", 
                                     class = "btn-info btn-block")
               ),
               column(6,
                      downloadButton(ns("download_interpretation"), 
                                     "📝 Download Interpretasi (TXT)", 
                                     class = "btn-warning btn-block")
               )
             )
           )
    )
  )
}

# Server function untuk Peta 
petaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values untuk menyimpan data peta dan klik
    map_data <- reactiveValues(
      geojson = NULL,
      merged_data = NULL,
      interpretation = NULL,
      geojson_loaded = FALSE
    )
    
    # REACTIVE VALUES UNTUK MENYIMPAN DATA KLIK 
    map_click_data <- reactiveValues(
      selected_district = NULL,
      district_data = NULL,
      show_stats = FALSE
    )
    
    # Load GeoJSON file otomatis saat startup 
    observe({
      geojson_path <- "Administrasi_Kabupaten_ind.geojson" 
      
      if (!file.exists(geojson_path)) {
        showNotification(
          paste("KRITIS: File peta tidak ditemukan:", geojson_path),
          type = "error", 
          duration = NULL
        )
        return()
      }
      
      tryCatch({
        # Gunakan geojsonio langsung 
        sp_data <- geojsonio::geojson_read(geojson_path, what = "sp")
        
        # Pastikan CRS
        if (is.na(sp::proj4string(sp_data))) {
          sp::proj4string(sp_data) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
        }
        
        map_data$geojson <- sp_data
        map_data$geojson_loaded <- TRUE
        
        showNotification(paste("File GeoJSON berhasil dimuat -", length(sp_data), "wilayah"), type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error membaca GeoJSON:", e$message), type = "error")
        map_data$geojson_loaded <- FALSE
      })
    })
    
    # Update pilihan variabel 
    observe({
      if (!is.null(values$current_data) && nrow(values$current_data) > 0) {
        numeric_vars <- names(values$current_data)[sapply(values$current_data, is.numeric)]
        numeric_vars <- numeric_vars[!numeric_vars %in% c("DISTRICTCODE")]
        
        if (length(numeric_vars) > 0) {
          updateSelectInput(session, "map_variable",
                            choices = setNames(numeric_vars, numeric_vars),
                            selected = if("POVERTY" %in% numeric_vars) "POVERTY" else numeric_vars[1])
        }
      }
    })
    
    # Generate peta dengan leaflet 
    observeEvent(input$generate_map, {
      req(map_data$geojson, input$map_variable, values$current_data)
      
      if (!map_data$geojson_loaded) {
        showNotification("File GeoJSON belum dimuat", type = "error")
        return()
      }
      
      tryCatch({
        geojson <- map_data$geojson
        sovi_data_current <- values$current_data
        variable_to_map <- input$map_variable
        
        # Cari kolom ID 
        id_col <- NULL
        for (col in c("ID", "KODE", "CODE", "id", "kode")) {
          if (col %in% names(geojson@data)) {
            id_col <- col
            break
          }
        }
        
        if (is.null(id_col)) {
          showNotification("Kolom ID tidak ditemukan dalam GeoJSON", type = "error")
          return()
        }
        
        # Standarisasi format
        sovi_data_current$DISTRICTCODE <- as.character(sovi_data_current$DISTRICTCODE)
        geojson@data[[id_col]] <- as.character(geojson@data[[id_col]])
        
        # Merge menggunakan sp
        merged_data <- sp::merge(geojson, sovi_data_current, 
                                 by.x = id_col, by.y = "DISTRICTCODE", 
                                 all.x = FALSE)
        
        if (nrow(merged_data@data) == 0) {
          showNotification("Tidak ada data yang berhasil di-join", type = "error")
          return()
        }
        
        map_data$merged_data <- merged_data
        
        # Ekstrak nilai variabel
        map_values <- as.numeric(merged_data@data[[variable_to_map]])
        map_values_clean <- na.omit(map_values)
        
        # Buat breaks berdasarkan metode klasifikasi 
        if (input$classification_method == "quantile") {
          breaks <- quantile(map_values_clean, probs = seq(0, 1, length.out = input$n_bins + 1))
        } else if (input$classification_method == "equal") {
          breaks <- seq(min(map_values_clean), max(map_values_clean), length.out = input$n_bins + 1)
        } else if (input$classification_method == "jenks") {
          if (requireNamespace("classInt", quietly = TRUE)) {
            breaks <- classInt::classIntervals(map_values_clean, n = input$n_bins, style = "jenks")$brks
          } else {
            breaks <- quantile(map_values_clean, probs = seq(0, 1, length.out = input$n_bins + 1))
          }
        }
        
        # Buat palette 
        pal <- colorNumeric(
          palette = "plasma",
          domain = range(map_values, na.rm = TRUE),
          na.color = "#808080"
        )
        
        # Labels 
        labels <- sprintf(
          "<strong>%s</strong><br/>%s: %.2f",
          merged_data@data[[id_col]],
          variable_to_map,
          map_values
        ) %>% lapply(htmltools::HTML)
        
        # Generate interpretasi 
        interpretation_text <- paste(
          "INTERPRETASI PETA TEMATIK:\n\n",
          "INFORMASI DATASET:\n",
          "- Variabel yang dipetakan:", variable_to_map, "\n",
          "- Jumlah wilayah total:", nrow(merged_data), "\n",
          "- Wilayah dengan data:", sum(!is.na(map_values)), "\n",
          "- Wilayah tanpa data:", sum(is.na(map_values)), "\n\n",
          "PENGATURAN VISUALISASI:\n",
          "- Metode klasifikasi:", input$classification_method, "\n",
          "- Jumlah interval:", input$n_bins, "\n\n",
          "STATISTIK DESKRIPTIF:\n",
          "- Nilai minimum:", round(min(map_values_clean), 3), "\n",
          "- Nilai maksimum:", round(max(map_values_clean), 3), "\n",
          "- Rata-rata:", round(mean(map_values_clean), 3), "\n",
          "- Median:", round(median(map_values_clean), 3), "\n",
          "- Standar deviasi:", round(sd(map_values_clean), 3), "\n\n",
          "CATATAN:\n",
          "- Klik pada wilayah untuk melihat detail statistik\n",
          "- Warna yang lebih terang menunjukkan nilai yang lebih tinggi\n",
          "- Wilayah abu-abu menunjukkan data yang tidak tersedia"
        )
        
        map_data$interpretation <- interpretation_text
        
        showNotification("Peta berhasil dibuat! Klik pada wilayah untuk detail.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error membuat peta:", e$message), type = "error")
      })
    })
    
    # Render peta leaflet 
    output$distribution_map <- renderLeaflet({
      req(map_data$merged_data, input$map_variable)
      
      merged_data <- map_data$merged_data
      variable_to_map <- input$map_variable
      
      # Ekstrak nilai variabel
      map_values <- as.numeric(merged_data@data[[variable_to_map]])
      
      # Buat palette
      pal <- colorNumeric(
        palette = "plasma",
        domain = range(map_values, na.rm = TRUE),
        na.color = "#808080"
      )
      
      # Cari kolom ID
      id_col <- NULL
      for (col in c("ID", "KODE", "CODE", "id", "kode")) {
        if (col %in% names(merged_data@data)) {
          id_col <- col
          break
        }
      }
      
      # Labels
      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %.2f",
        merged_data@data[[id_col]],
        variable_to_map,
        map_values
      ) %>% lapply(htmltools::HTML)
      
      # Render peta
      leaflet(merged_data) %>%
        addTiles() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addPolygons(
          fillColor = ~pal(merged_data@data[[variable_to_map]]),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          ),
          layerId = ~merged_data@data[[id_col]]
        ) %>%
        addLegend(
          pal = pal, 
          values = ~merged_data@data[[variable_to_map]], 
          opacity = 0.7, 
          title = variable_to_map,
          position = "bottomright"
        )
    })
    
    # Observer untuk menangkap klik pada peta 
    observe({
      req(input$distribution_map_shape_click)
      
      click <- input$distribution_map_shape_click
      clicked_id <- click$id
      
      # Ambil data untuk district yang diklik
      sovi_data_current <- values$current_data
      district_info <- sovi_data_current[sovi_data_current$DISTRICTCODE == clicked_id, ]
      
      if (nrow(district_info) > 0) {
        map_click_data$selected_district <- clicked_id
        map_click_data$district_data <- district_info
        map_click_data$show_stats <- TRUE
      }
    })
    
    # Output untuk panel statistik detail 
    output$district_info_panel <- renderUI({
      if (!map_click_data$show_stats || is.null(map_click_data$district_data)) {
        return(
          div(
            style = "text-align: center; padding: 30px; color: #666;",
            icon("mouse-pointer", style = "font-size: 3em; color: #3498db; margin-bottom: 15px;"),
            h4("Klik pada peta untuk melihat statistik"),
            p("Pilih kabupaten/kota di peta untuk menampilkan informasi detail"),
            hr(),
            h5("Petunjuk:"),
            tags$ul(
              tags$li("Klik tombol 'Buat Peta' untuk memuat peta"),
              tags$li("Pilih variabel yang ingin divisualisasikan"),
              tags$li("Klik pada wilayah di peta untuk detail"),
              style = "text-align: left; color: #999;"
            )
          )
        )
      }
      
      district_data <- map_click_data$district_data
      district_id <- map_click_data$selected_district
      all_data <- values$current_data
      
      # Panel informasi district 
      div(
        # Header
        div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                   color: white; padding: 15px; border-radius: 8px 8px 0 0; margin-bottom: 15px;",
          h3(style = "margin: 0; text-align: center;", 
             paste("Wilayah:", district_id)),
          p(style = "margin: 5px 0 0 0; text-align: center; font-size: 14px; opacity: 0.9;",
            "Detail Statistik Kerentanan Sosial")
        ),
        
        # Konten statistik
        div(
          style = "padding: 0 15px;",
          
          # Variabel yang sedang dipetakan
          if (!is.null(input$map_variable)) {
            current_var <- input$map_variable
            current_value <- as.numeric(district_data[[current_var]])
            
            # Hitung persentil
            all_values <- as.numeric(all_data[[current_var]])
            percentile <- round(100 * sum(all_values <= current_value, na.rm = TRUE) / sum(!is.na(all_values)), 1)
            
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #007bff;",
              h4(style = "color: #007bff; margin-top: 0;", "Variabel Terpilih"),
              p(strong(current_var), ": ", 
                span(style = "font-size: 18px; color: #28a745; font-weight: bold;", 
                     round(current_value, 2))),
              p("Persentil: ", 
                span(style = paste0("color: ", if(percentile > 75) "#dc3545" else if(percentile > 50) "#ffc107" else "#28a745", "; font-weight: bold;"), 
                     paste0(percentile, "%"))),
              p(style = "font-size: 12px; color: #6c757d; margin: 0;",
                "Ranking ", percentile, " dari 100 wilayah")
            )
          },
          
          # Ringkasan semua indikator 
          h4("Ringkasan Indikator Utama"),
          div(
            style = "max-height: 300px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 5px;",
            {
              numeric_vars <- names(all_data)[sapply(all_data, is.numeric)]
              numeric_vars <- numeric_vars[!numeric_vars %in% c("DISTRICTCODE")]
              top_vars <- head(numeric_vars, 8)  # Tampilkan 8 teratas
              
              lapply(top_vars, function(var) {
                value <- as.numeric(district_data[[var]])
                all_vals <- as.numeric(all_data[[var]])
                pct <- round(100 * sum(all_vals <= value, na.rm = TRUE) / sum(!is.na(all_vals)), 1)
                status <- if(pct > 75) "Tinggi" else if(pct > 50) "Sedang" else "Rendah"
                
                div(
                  style = "border-bottom: 1px solid #dee2e6; padding: 8px 0;",
                  strong(var), ": ", round(value, 2),
                  span(style = paste0("float: right; color: ", 
                                      if(status == "Tinggi") "#dc3545" else if(status == "Sedang") "#ffc107" else "#28a745"),
                       status)
                )
              })
            }
          ),
          
          # Tombol tutup
          div(
            style = "margin-top: 20px; text-align: center;",
            actionButton(session$ns("close_stats"), "Tutup Detail", 
                         style = "background: #6c757d; color: white; border: none; padding: 8px 16px; border-radius: 4px;")
          )
        )
      )
    })
    
    # Observer untuk menutup panel statistik 
    observeEvent(input$close_stats, {
      map_click_data$show_stats <- FALSE
      map_click_data$selected_district <- NULL
      map_click_data$district_data <- NULL
    })
    
    # Output statistik spasial 
    output$spatial_stats <- renderTable({
      req(map_data$merged_data, input$map_variable)
      
      map_values <- na.omit(as.numeric(map_data$merged_data@data[[input$map_variable]]))
      
      data.frame(
        Statistik = c("Jumlah Wilayah", "Data Tersedia", "Data Kosong", 
                      "Min", "Max", "Mean", "Median", "Std Dev"),
        Nilai = c(
          nrow(map_data$merged_data),
          length(map_values),
          nrow(map_data$merged_data) - length(map_values),
          round(min(map_values), 3),
          round(max(map_values), 3),
          round(mean(map_values), 3),
          round(median(map_values), 3),
          round(sd(map_values), 3)
        )
      )
    })
    
    # Output interpretasi 
    output$map_interpretation <- renderText({
      req(map_data$interpretation)
      map_data$interpretation
    })
    
    # Download handlers 
    output$download_map <- downloadHandler(
      filename = function() {
        paste("peta_", input$map_variable, "_", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        if (!is.null(map_data$merged_data)) {
          # Untuk leaflet, capture sebagai screenshot atau buat plot ggplot
          showNotification("Fitur download sedang dalam pengembangan", type = "warning")
        }
      }
    )
    
    output$download_data <- downloadHandler(
      filename = function() paste("data_peta_", input$map_variable, "_", Sys.Date(), ".csv", sep=""),
      content = function(file) {
        if (!is.null(map_data$merged_data)) {
          # Convert sp object to data frame
          export_data <- map_data$merged_data@data
          write.csv(export_data, file, row.names = FALSE)
        }
      }
    )
    
    output$download_interpretation <- downloadHandler(
      filename = function() paste("interpretasi_peta_", input$map_variable, "_", Sys.Date(), ".txt", sep=""),
      content = function(file) {
        if (!is.null(map_data$interpretation)) {
          writeLines(map_data$interpretation, file)
        }
      }
    )
    
  })
}