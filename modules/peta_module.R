# Peta Module
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
               column(3,
                      # Pemilihan Variabel untuk Visualisasi
                      selectInput(ns("map_variable"), 
                                  "Pilih Variabel untuk Visualisasi:",
                                  choices = list("Memuat variabel..." = ""))
               ),
               column(3,
                      numericInput(ns("n_bins"), 
                                   "Jumlah Interval Warna:",
                                   value = 5, min = 3, max = 10, step = 1)
               ),
               column(4,
                      selectInput(ns("classification_method"), 
                                  "Metode Klasifikasi:",
                                  choices = list(
                                    "Quantile" = "quantile",
                                    "Equal Interval" = "equal",
                                    "Natural Breaks (Jenks)" = "jenks",
                                    "Standard Deviation" = "sd"
                                  ))
               ),
               column(2,
                      br(),
                      actionButton(ns("generate_map"), 
                                   "Buat Peta", 
                                   class = "btn-primary btn-block")
               )
             )
           )
    ),
    
    # Area Peta
    column(12,
           box(
             title = "Peta Tematik Interaktif",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             height = "650px",
             withSpinner(plotlyOutput(ns("plotly_map"), height = "600px"))
           )
    ),
    
    # Download di bawah peta
    column(12,
           box(
             title = "Download",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             
             # Info penjelasan download
             div(
               style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
               h5("ℹ️ Penjelasan Download:", style = "margin: 0; color: #495057;"),
               tags$ul(
                 tags$li("Peta (HTML): File interaktif yang bisa dibuka di browser dengan fitur zoom/pan"),
                 tags$li("Data Peta (CSV): Data numerik hasil join antara statistik dan geometri"),
                 tags$li("Interpretasi (TXT): Analisis statistik dan interpretasi pola spasial"),
                 style = "margin: 5px 0 0 0; color: #495057; font-size: 12px;"
               )
             ),
             
             fluidRow(
               column(4,
                      downloadButton(ns("download_map"), 
                                     "Download Peta (HTML)", 
                                     class = "btn-success btn-block")
               ),
               column(4,
                      downloadButton(ns("download_data"), 
                                     "Download Data (CSV)", 
                                     class = "btn-info btn-block")
               ),
               column(4,
                      downloadButton(ns("download_interpretation"), 
                                     "📝 Download Interpretasi (TXT)", 
                                     class = "btn-warning btn-block")
               )
             )
           )
    ),
    
    # Statistik dan Interpretasi
    column(12,
           box(
             title = "Statistik Spasial",
             status = "warning",
             solidHeader = TRUE,
             width = 4,
             collapsible = TRUE,
             withSpinner(tableOutput(ns("spatial_stats")))
           ),
           
           box(
             title = "Interpretasi Peta",
             status = "warning", 
             solidHeader = TRUE,
             width = 8,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("map_interpretation")))
           )
    )
  )
}

# Fungsi Server untuk Peta
petaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Nilai reaktif untuk peta
    map_data <- reactiveValues(
      geojson = NULL,
      merged_data = NULL,
      plotly_map = NULL,
      interpretation = NULL,
      geojson_loaded = FALSE
    )
    
    # Load GeoJSON file otomatis saat startup
    observe({
      # Path file GeoJSON - disesuaikan dengan nama file yang ada
      geojson_path <- "data/indonesia_kabkota2.geojson"
      
      # Cek apakah file ada
      if (file.exists(geojson_path)) {
        # Cek apakah sf package tersedia
        if (!requireNamespace("sf", quietly = TRUE)) {
          showNotification("Package 'sf' diperlukan untuk membaca GeoJSON. Install dengan: install.packages('sf')", 
                           type = "error")
          return()
        }
        
        tryCatch({
          # Baca GeoJSON
          geojson_data <- sf::st_read(geojson_path, quiet = TRUE)
          map_data$geojson <- geojson_data
          map_data$geojson_loaded <- TRUE
          
          showNotification(paste("File GeoJSON berhasil dimuat -", nrow(geojson_data), "wilayah"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("Error membaca GeoJSON:", e$message), type = "error")
          map_data$geojson_loaded <- FALSE
        })
      } else {
        showNotification(paste("File GeoJSON tidak ditemukan di:", geojson_path, 
                               "Silakan letakkan file 'indonesia_kabkota.geojson' di folder 'data/'"), 
                         type = "warning")
        map_data$geojson_loaded <- FALSE
      }
    })
    
    # Update pilihan variabel berdasarkan data yang tersedia
    observe({
      # Pastikan ada data yang dimuat dari module lain (values$current_data)
      if (!is.null(values$current_data) && nrow(values$current_data) > 0) {
        
        # Dapatkan kolom numerik yang bisa dipetakan (exclude DISTRICTCODE dan kolom non-numerik)
        exclude_cols <- c("DISTRICTCODE", "DISTRICT", "PROVINCE", "REGION") # Kolom ID dan kategoris
        
        numeric_cols <- names(values$current_data)[sapply(values$current_data, is.numeric)]
        available_variables <- setdiff(numeric_cols, exclude_cols)
        
        if (length(available_variables) > 0) {
          # Buat choices dengan nama yang lebih deskriptif
          choices_list <- setNames(available_variables, available_variables)
          
          # Update pilihan di selectInput
          updateSelectInput(session, "map_variable",
                            choices = choices_list,
                            selected = available_variables[1])
        } else {
          # Jika tidak ada variabel numerik
          updateSelectInput(session, "map_variable",
                            choices = list("Tidak ada variabel numerik" = ""),
                            selected = "")
          
          showNotification("Tidak ada variabel numerik yang dapat dipetakan", type = "warning")
        }
      } else {
        # Jika belum ada data
        updateSelectInput(session, "map_variable",
                          choices = list("Belum ada data dimuat..." = ""),
                          selected = "")
      }
    })
    
    # Observer untuk monitoring data changes
    observe({
      if (!is.null(values$current_data)) {
        # Cek apakah ada DISTRICTCODE untuk join
        if (!"DISTRICTCODE" %in% names(values$current_data)) {
          showNotification("DISTRICTCODE tidak ditemukan dalam data. Pastikan kolom ini ada untuk join dengan peta.", 
                           type = "warning")
        }
      }
    })
    
    # Generate peta
    observeEvent(input$generate_map, {
      req(map_data$geojson)
      req(input$map_variable)
      req(input$map_variable != "") # Pastikan bukan pilihan kosong
      
      if (!map_data$geojson_loaded) {
        showNotification("File GeoJSON belum dimuat. Pastikan file ada di folder data/", type = "error")
        return()
      }
      
      if (is.null(values$current_data) || nrow(values$current_data) == 0) {
        showNotification("Data belum dimuat. Silakan muat data terlebih dahulu.", type = "error")
        return()
      }
      
      # Cek apakah variabel yang dipilih ada dalam data
      if (!input$map_variable %in% names(values$current_data)) {
        showNotification(paste("Variabel", input$map_variable, "tidak ditemukan dalam data"), type = "error")
        return()
      }
      
      # Cek apakah packages yang diperlukan tersedia
      required_packages <- c("sf", "ggplot2", "dplyr", "plotly")
      
      missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
      
      if (length(missing_packages) > 0) {
        showNotification(paste("Packages berikut diperlukan:", paste(missing_packages, collapse = ", ")), 
                         type = "error")
        return()
      }
      
      tryCatch({
        # Siapkan data untuk join (menggunakan DISTRICTCODE otomatis)
        data_for_join <- values$current_data %>%
          select(DISTRICTCODE, all_of(input$map_variable)) %>%
          rename(join_id = DISTRICTCODE,
                 map_value = !!input$map_variable)
        
        # Siapkan GeoJSON untuk join - ambil juga nama kabupaten jika ada
        geojson_cols <- names(map_data$geojson)
        
        # Kolom nama adalah CITY_NAME
        geojson_for_join <- map_data$geojson %>%
          rename(join_id = DISTRICTCODE,
                 district_name = CITY_NAME)
        
        # Join data
        merged_data <- geojson_for_join %>%
          left_join(data_for_join, by = "join_id")
        
        # Cek hasil join
        missing_data <- sum(is.na(merged_data$map_value))
        successful_joins <- sum(!is.na(merged_data$map_value))
        
        if (missing_data > 0) {
          showNotification(paste("Peringatan:", missing_data, "wilayah tidak memiliki data"), 
                           type = "warning")
        }
        
        if (successful_joins == 0) {
          showNotification("Tidak ada data yang berhasil di-join. Periksa kembali DISTRICTCODE.", type = "error")
          return()
        }
        
        map_data$merged_data <- merged_data
        
        # Buat klasifikasi untuk data yang ada
        map_values <- na.omit(merged_data$map_value)
        
        if (length(map_values) == 0) {
          showNotification("Tidak ada data yang dapat dipetakan", type = "error")
          return()
        }
        
        # Hitung breaks berdasarkan metode
        if (input$classification_method == "quantile") {
          breaks <- quantile(map_values, probs = seq(0, 1, length.out = input$n_bins + 1))
        } else if (input$classification_method == "equal") {
          breaks <- seq(min(map_values), max(map_values), length.out = input$n_bins + 1)
        } else if (input$classification_method == "jenks") {
          if (requireNamespace("classInt", quietly = TRUE)) {
            breaks <- classInt::classIntervals(map_values, n = input$n_bins, style = "jenks")$brks
          } else {
            showNotification("Package 'classInt' diperlukan untuk Natural Breaks", type = "warning")
            breaks <- quantile(map_values, probs = seq(0, 1, length.out = input$n_bins + 1))
          }
        } else if (input$classification_method == "sd") {
          mean_val <- mean(map_values)
          sd_val <- sd(map_values)
          breaks <- c(min(map_values), 
                      mean_val - sd_val, 
                      mean_val, 
                      mean_val + sd_val, 
                      max(map_values))
        }
        
        # Buat kategori
        merged_data$map_category <- cut(merged_data$map_value, 
                                        breaks = breaks, 
                                        include.lowest = TRUE,
                                        dig.lab = 3)
        
        # Generate peta interaktif dengan plotly
        
        # Transform ke WGS84 untuk plotly
        merged_data_wgs84 <- sf::st_transform(merged_data, 4326)
        
        # Konversi sf object ke ggplot dulu, lalu ke plotly
        base_plot <- ggplot(merged_data_wgs84) +
          geom_sf(aes(fill = map_value, 
                      text = paste0("Wilayah: ", district_name, "\n",
                                    input$map_variable, ": ", 
                                    ifelse(is.na(map_value), 
                                           "Data tidak tersedia", 
                                           format(map_value, big.mark = ",", scientific = FALSE)))),
                  color = NA, size = 0.1) +  # Hilangkan border (color = NA)
          theme_void() +
          theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            legend.position = "right"  # Legend selalu ditampilkan
          )
        
        # Gunakan plasma color scale
        base_plot <- base_plot + 
          scale_fill_viridis_c(option = "plasma", 
                               na.value = "grey90",
                               name = input$map_variable)
        
        # Konversi ke plotly dengan pengaturan yang lebih baik
        plotly_map <- ggplotly(base_plot, tooltip = "text") %>%
          layout(
            title = list(
              text = paste("Peta Tematik:", input$map_variable),
              font = list(size = 16),
              y = 0.98  # Posisi judul lebih tinggi agar tidak terpotong
            ),
            showlegend = TRUE,  # Legend selalu ditampilkan
            plot_bgcolor = 'white',
            paper_bgcolor = 'white',
            margin = list(t = 50, b = 20, l = 20, r = 20),  # Margin untuk judul
            # Hilangkan axis lines dan ticks
            xaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE,
              ticks = ""
            ),
            yaxis = list(
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE,
              ticks = ""
            )
          ) %>%
          config(
            displayModeBar = TRUE,
            scrollZoom = TRUE,  # Enable scroll zoom
            doubleClick = 'reset',  # Double click to reset zoom
            modeBarButtonsToRemove = c('select2d', 'lasso2d', 'autoScale2d', 
                                       'hoverClosestCartesian', 'hoverCompareCartesian',
                                       'toggleHover', 'sendDataToCloud'),
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = 'png',
              filename = paste0('peta_', input$map_variable),
              height = 600,
              width = 1000,
              scale = 2
            )
          )
        
        map_data$plotly_map <- plotly_map
        
        # Hitung statistik spasial
        spatial_stats <- data.frame(
          Statistik = c("Jumlah Wilayah", "Wilayah dengan Data", "Wilayah Tanpa Data", 
                        "Min", "Max", "Mean", "Median", "Std Dev"),
          Nilai = c(
            nrow(merged_data),
            sum(!is.na(merged_data$map_value)),
            sum(is.na(merged_data$map_value)),
            round(min(map_values), 3),
            round(max(map_values), 3),
            round(mean(map_values), 3),
            round(median(map_values), 3),
            round(sd(map_values), 3)
          )
        )
        
        # Interpretasi
        interpretation_text <- paste(
          "INTERPRETASI PETA TEMATIK:\n",
          "=========================\n\n",
          "INFORMASI DATASET:\n",
          "- Variabel yang dipetakan:", input$map_variable, "\n",
          "- Jumlah wilayah total:", nrow(merged_data), "\n",
          "- Wilayah dengan data:", sum(!is.na(merged_data$map_value)), "\n",
          "- Wilayah tanpa data:", sum(is.na(merged_data$map_value)), "\n\n",
          "PENGATURAN VISUALISASI:\n",
          "- Metode klasifikasi:", input$classification_method, "\n",
          "- Jumlah interval:", input$n_bins, "\n",
          "- Palet warna: Plasma\n\n",
          "STATISTIK DESKRIPTIF:\n",
          "- Nilai minimum:", round(min(map_values), 3), "\n",
          "- Nilai maksimum:", round(max(map_values), 3), "\n",
          "- Rata-rata:", round(mean(map_values), 3), "\n",
          "- Median:", round(median(map_values), 3), "\n",
          "- Standar deviasi:", round(sd(map_values), 3), "\n\n",
          "POLA SPASIAL:\n",
          "- Rentang nilai:", round(max(map_values) - min(map_values), 3), "\n",
          "- Koefisien variasi:", round(sd(map_values)/mean(map_values) * 100, 2), "%\n\n",
          "INTERPRETASI:\n",
          if (sd(map_values)/mean(map_values) > 0.5) {
            "Variabilitas tinggi - terdapat perbedaan yang besar antar wilayah"
          } else if (sd(map_values)/mean(map_values) > 0.3) {
            "Variabilitas sedang - terdapat perbedaan moderat antar wilayah"
          } else {
            "Variabilitas rendah - nilai relatif merata antar wilayah"
          }, "\n\n",
          "CATATAN:\n",
          "- Warna yang lebih terang (kuning) menunjukkan nilai yang lebih tinggi\n",
          "- Warna yang lebih gelap (ungu) menunjukkan nilai yang lebih rendah\n",
          "- Wilayah abu-abu menunjukkan data yang tidak tersedia\n",
          "- Gunakan zoom dan pan untuk melihat detail wilayah tertentu"
        )
        
        map_data$interpretation <- interpretation_text
        
        showNotification("Peta berhasil dibuat!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error membuat peta:", e$message), type = "error")
      })
    })
    
    # Output untuk peta plotly
    output$plotly_map <- renderPlotly({
      if (!is.null(map_data$plotly_map)) {
        map_data$plotly_map
      } else {
        # Plot kosong dengan pesan
        plot_ly() %>%
          add_annotations(
            text = "Klik 'Buat Peta' untuk menampilkan peta interaktif\n\nFitur peta:\n• Scroll mouse untuk zoom in/out\n• Drag untuk pan\n• Hover untuk info detail\n\nTunggu beberapa saat untuk memuat peta",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = "gray")
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            plot_bgcolor = 'white',
            paper_bgcolor = 'white'
          )
      }
    })
    
    output$spatial_stats <- renderTable({
      req(map_data$merged_data)
      
      map_values <- na.omit(map_data$merged_data$map_value)
      
      data.frame(
        Statistik = c("Jumlah Wilayah", "Wilayah dengan Data", "Wilayah Tanpa Data", 
                      "Min", "Max", "Mean", "Median", "Std Dev"),
        Nilai = c(
          nrow(map_data$merged_data),
          sum(!is.na(map_data$merged_data$map_value)),
          sum(is.na(map_data$merged_data$map_value)),
          round(min(map_values), 3),
          round(max(map_values), 3),
          round(mean(map_values), 3),
          round(median(map_values), 3),
          round(sd(map_values), 3)
        )
      )
    })
    
    output$map_interpretation <- renderText({
      req(map_data$interpretation)
      map_data$interpretation
    })
    
    # Download handlers
    output$download_map <- downloadHandler(
      filename = function() {
        paste("peta_interaktif_", input$map_variable, "_", Sys.Date(), ".html", sep="")
      },
      content = function(file) {
        if (!is.null(map_data$plotly_map)) {
          # Untuk peta plotly, simpan sebagai HTML
          htmlwidgets::saveWidget(map_data$plotly_map, file = file, selfcontained = TRUE)
        }
      }
    )
    
    output$download_data <- downloadHandler(
      filename = function() paste("data_peta_", input$map_variable, "_", Sys.Date(), ".csv", sep=""),
      content = function(file) {
        if (!is.null(map_data$merged_data)) {
          # Convert sf object to regular data frame untuk export
          export_data <- map_data$merged_data
          sf::st_geometry(export_data) <- NULL  # Remove geometry column
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