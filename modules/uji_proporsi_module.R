# Uji Proporsi Module
# modules/uji_proporsi_module.R

# ==========================================
# UJI PROPORSI - UI FUNCTION
# ==========================================

ujiProporsiUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "📊 Panel Kontrol Uji Proporsi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             selectInput(ns("test_type"), 
                         "Jenis Uji Proporsi:",
                         choices = list(
                           "Proporsi Satu Kabupaten/Kota" = "one_region",
                           "Perbandingan Dua Kabupaten/Kota" = "two_regions",
                           "Perbandingan Antar Provinsi" = "two_provinces"
                         )),
             
             selectInput(ns("variable"), 
                         "Variabel untuk Analisis Proporsi:",
                         choices = NULL),
             
             # Input untuk one region
             conditionalPanel(
               condition = "input.test_type == 'one_region'",
               ns = ns,
               selectInput(ns("selected_city"), 
                           "Pilih Kabupaten/Kota:",
                           choices = NULL),
               numericInput(ns("p0"), 
                            "Proporsi Hipotesis (p₀):",
                            value = 0.5, min = 0, max = 1, step = 0.01),
               helpText("Proporsi yang akan dibandingkan dengan data observasi")
             ),
             
             # Input untuk two regions
             conditionalPanel(
               condition = "input.test_type == 'two_regions'",
               ns = ns,
               selectInput(ns("city1"), 
                           "Kabupaten/Kota Pertama:",
                           choices = NULL),
               selectInput(ns("city2"), 
                           "Kabupaten/Kota Kedua:",
                           choices = NULL),
               helpText("Bandingkan proporsi antara dua kabupaten/kota")
             ),
             
             # Input untuk two provinces
             conditionalPanel(
               condition = "input.test_type == 'two_provinces'",
               ns = ns,
               selectInput(ns("province1"), 
                           "Provinsi Pertama:",
                           choices = NULL),
               selectInput(ns("province2"), 
                           "Provinsi Kedua:",
                           choices = NULL),
               helpText("Bandingkan proporsi rata-rata kabupaten/kota antar provinsi")
             ),
             
             # Threshold untuk analisis proporsi (untuk data kontinu)
             conditionalPanel(
               condition = "input.test_type != 'one_region'",
               ns = ns,
               numericInput(ns("threshold_percentile"), 
                            "Threshold Persentil (%):",
                            value = 50, min = 0, max = 100, step = 5),
               helpText("Contoh: 50% = di atas median, 75% = di atas kuartil 3")
             ),
             
             # Threshold untuk one region (data kontinu)
             conditionalPanel(
               condition = "input.test_type == 'one_region'",
               ns = ns,
               checkboxInput(ns("use_custom_threshold"), 
                             "Gunakan Threshold Kustom", 
                             value = FALSE),
               conditionalPanel(
                 condition = "input.use_custom_threshold == true",
                 ns = ns,
                 numericInput(ns("custom_threshold_value"), 
                              "Nilai Threshold:",
                              value = 0),
                 helpText("Hitung proporsi di atas nilai threshold ini")
               )
             ),
             
             selectInput(ns("alternative"), 
                         "Hipotesis Alternatif:",
                         choices = list(
                           "Dua arah (≠)" = "two.sided",
                           "Pertama > Kedua" = "greater",
                           "Pertama < Kedua" = "less"
                         )),
             
             numericInput(ns("alpha"), 
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             br(),
             actionButton(ns("run_test"), 
                          "Jalankan Uji Proporsi", 
                          class = "btn-success btn-lg"),
             
             hr(),
             h5("📥 Download:"),
             downloadButton(ns("download_results"), "Hasil", class = "btn-sm btn-primary"),
             br(), br(),
             downloadButton(ns("download_plot"), "Plot", class = "btn-sm btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), "Laporan", class = "btn-sm btn-info")
           )
    ),
    
    # Hasil
    column(8,
           box(
             title = "Hasil Uji Proporsi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           box(
             title = "Visualisasi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("test_plot"), height = "400px"))
           )
    ),
    
    # Interpretasi
    column(12,
           box(
             title = "Interpretasi Uji Proporsi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# ==========================================
# UJI PROPORSI - SERVER FUNCTION
# ==========================================

ujiProporsiServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      
      updateSelectInput(session, "variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      
      # Update pilihan kabupaten/kota
      if ("CITY_NAME" %in% names(values$current_data)) {
        cities <- sort(unique(values$current_data$CITY_NAME))
        updateSelectInput(session, "selected_city", 
                          choices = setNames(cities, cities))
        updateSelectInput(session, "city1", 
                          choices = setNames(cities, cities))
        updateSelectInput(session, "city2", 
                          choices = setNames(cities, cities))
      }
      
      # Update pilihan provinsi
      if ("PROVINCE_NAME" %in% names(values$current_data)) {
        provinces <- sort(unique(values$current_data$PROVINCE_NAME))
        updateSelectInput(session, "province1", 
                          choices = setNames(provinces, provinces))
        updateSelectInput(session, "province2", 
                          choices = setNames(provinces, provinces))
      }
    })
    
    # Nilai reaktif untuk hasil
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL
    )
    
    # Jalankan uji
    observeEvent(input$run_test, {
      req(input$variable)
      
      data <- values$current_data
      
      if (input$test_type == "one_region") {
        req(input$selected_city)
        
        # Filter data untuk kabupaten/kota terpilih
        city_data <- data[data$CITY_NAME == input$selected_city, ]
        if (nrow(city_data) == 0) {
          showNotification("Tidak ada data untuk kabupaten/kota terpilih", type = "error")
          return()
        }
        
        variable_values <- na.omit(city_data[[input$variable]])
        if (length(variable_values) == 0) {
          showNotification("Tidak ada data valid untuk variabel terpilih", type = "error")
          return()
        }
        
        # Tentukan threshold dan hitung proporsi
        if (input$use_custom_threshold) {
          threshold_value <- input$custom_threshold_value
          successes <- sum(variable_values > threshold_value)
          threshold_type <- "kustom"
        } else {
          # Check if binary data (0/1)
          if (all(variable_values %in% c(0, 1))) {
            successes <- sum(variable_values == 1)
            threshold_value <- NA
            threshold_type <- "biner"
          } else {
            # Use p0 as percentile
            threshold_value <- quantile(variable_values, input$p0, na.rm = TRUE)
            successes <- sum(variable_values > threshold_value)
            threshold_type <- "persentil"
          }
        }
        
        trials <- length(variable_values)
        sample_prop <- successes / trials
        
        test_result <- prop.test(successes, trials, p = input$p0, 
                                 alternative = input$alternative,
                                 conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot untuk satu daerah
        if (threshold_type == "biner") {
          plot_data <- data.frame(
            Kategori = c("0", "1"),
            Jumlah = c(trials - successes, successes),
            Proporsi = c((trials - successes)/trials, successes/trials)
          )
          test_results$plot <- ggplot(plot_data, aes(x = Kategori, y = Proporsi)) +
            geom_col(fill = c("lightcoral", "lightblue"), alpha = 0.7) +
            geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(n=", Jumlah, ")")), 
                      vjust = -0.5) +
            labs(title = paste("Proporsi", input$variable, "di", input$selected_city),
                 subtitle = paste("Proporsi kategori '1' =", round(sample_prop, 3)),
                 y = "Proporsi") +
            theme_custom()
        } else {
          test_results$plot <- ggplot(city_data, aes(x = .data[[input$variable]])) +
            geom_histogram(bins = 20, fill = "lightgreen", alpha = 0.7, color = "black") +
            geom_vline(xintercept = threshold_value, color = "red", linetype = "dashed", linewidth = 1.2) +
            labs(title = paste("Distribusi", input$variable, "di", input$selected_city),
                 subtitle = paste("Garis Merah: Threshold =", round(threshold_value, 3), 
                                  "| Proporsi di atas threshold =", round(sample_prop, 3)),
                 x = input$variable, y = "Frekuensi") +
            theme_custom()
        }
        
        # Interpretasi
        test_results$interpretation <- paste(
          "INTERPRETASI UJI PROPORSI SATU DAERAH:\n",
          "=====================================\n\n",
          "DAERAH YANG DIANALISIS:\n",
          "- Kabupaten/Kota:", input$selected_city, "\n",
          "- Variabel:", input$variable, "\n",
          "- Jumlah observasi:", trials, "\n\n",
          "METODE ANALISIS:\n",
          "- Jenis data:", switch(threshold_type,
                                  "biner" = "Data biner (0/1)",
                                  "persentil" = paste("Data kontinu dengan threshold persentil", input$p0*100, "%"),
                                  "kustom" = paste("Data kontinu dengan threshold kustom =", threshold_value)), "\n",
          if (!is.na(threshold_value)) paste("- Nilai threshold:", round(threshold_value, 3), "\n") else "",
          "- Proporsi sampel (p̂):", round(sample_prop, 4), "\n",
          "- Jumlah 'sukses':", successes, "dari", trials, "observasi\n\n",
          "HIPOTESIS:\n",
          "- H₀: p =", input$p0, "\n",
          "- H₁: p", switch(input$alternative, 
                            "two.sided" = "≠", 
                            "greater" = ">", 
                            "less" = "<"), input$p0, "\n\n",
          "HASIL UJI STATISTIK:\n",
          "- Chi-square statistik =", round(test_result$statistic, 4), "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) = [", 
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀. Proporsi di", input$selected_city, "berbeda signifikan dari", input$p0,
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Proporsi di", input$selected_city, "tidak berbeda signifikan dari", input$p0,
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (test_result$p.value < input$alpha) {
            paste("Proporsi", input$variable, "di", input$selected_city, "berbeda secara nyata dari nilai hipotesis.")
          } else {
            paste("Proporsi", input$variable, "di", input$selected_city, "tidak berbeda secara nyata dari nilai hipotesis.")
          }
        )
        
      } else if (input$test_type == "two_regions") {
        req(input$city1, input$city2)
        
        if (input$city1 == input$city2) {
          showNotification("Pilih dua kabupaten/kota yang berbeda", type = "error")
          return()
        }
        
        # Data untuk kedua kota
        city1_data <- data[data$CITY_NAME == input$city1, ]
        city2_data <- data[data$CITY_NAME == input$city2, ]
        
        if (nrow(city1_data) == 0 || nrow(city2_data) == 0) {
          showNotification("Salah satu atau kedua kabupaten/kota tidak memiliki data", type = "error")
          return()
        }
        
        var1 <- na.omit(city1_data[[input$variable]])
        var2 <- na.omit(city2_data[[input$variable]])
        
        if (length(var1) == 0 || length(var2) == 0) {
          showNotification("Tidak ada data valid untuk salah satu atau kedua daerah", type = "error")
          return()
        }
        
        # Calculate proportions based on threshold
        threshold_percentile <- input$threshold_percentile / 100
        all_values <- c(var1, var2)
        threshold_value <- quantile(all_values, threshold_percentile, na.rm = TRUE)
        
        successes1 <- sum(var1 > threshold_value)
        successes2 <- sum(var2 > threshold_value)
        trials1 <- length(var1)
        trials2 <- length(var2)
        
        test_result <- prop.test(c(successes1, successes2), c(trials1, trials2),
                                 alternative = input$alternative,
                                 conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot perbandingan
        comparison_data <- data.frame(
          Daerah = c(input$city1, input$city2),
          Proporsi = c(successes1/trials1, successes2/trials2),
          N = c(trials1, trials2),
          Sukses = c(successes1, successes2)
        )
        
        test_results$plot <- ggplot(comparison_data, aes(x = Daerah, y = Proporsi)) +
          geom_col(fill = c("lightblue", "lightcoral"), alpha = 0.7) +
          geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(", Sukses, "/", N, ")")), 
                    vjust = -0.5) +
          labs(title = paste("Perbandingan Proporsi", input$variable),
               subtitle = paste("Threshold:", round(threshold_value, 3), 
                                "(Persentil", input$threshold_percentile, "%)"),
               y = "Proporsi di Atas Threshold") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Interpretasi
        prop1 <- successes1/trials1
        prop2 <- successes2/trials2
        prop_diff <- prop1 - prop2
        
        test_results$interpretation <- paste(
          "INTERPRETASI UJI PROPORSI DUA DAERAH:\n",
          "====================================\n\n",
          "DAERAH YANG DIBANDINGKAN:\n",
          "- Daerah 1:", input$city1, "\n",
          "  * Jumlah observasi (n₁) =", trials1, "\n",
          "  * Proporsi (p̂₁) =", round(prop1, 4), "\n",
          "  * Jumlah sukses =", successes1, "\n",
          "- Daerah 2:", input$city2, "\n",
          "  * Jumlah observasi (n₂) =", trials2, "\n",
          "  * Proporsi (p̂₂) =", round(prop2, 4), "\n",
          "  * Jumlah sukses =", successes2, "\n",
          "- Perbedaan proporsi (p̂₁ - p̂₂) =", round(prop_diff, 4), "\n\n",
          "METODE ANALISIS:\n",
          "- Variabel:", input$variable, "\n",
          "- Threshold (Persentil", input$threshold_percentile, "%) =", round(threshold_value, 3), "\n",
          "- Kriteria sukses: Nilai di atas threshold\n\n",
          "HIPOTESIS:\n",
          "- H₀: p₁ = p₂ (proporsi sama)\n",
          "- H₁: p₁", switch(input$alternative, 
                             "two.sided" = "≠", 
                             "greater" = ">", 
                             "less" = "<"), "p₂\n\n",
          "HASIL UJI STATISTIK:\n",
          "- Chi-square statistik =", round(test_result$statistic, 4), "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) untuk perbedaan = [", 
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀. Ada perbedaan proporsi yang signifikan antara", 
                  input$city1, "dan", input$city2,
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak ada perbedaan proporsi yang signifikan antara", 
                  input$city1, "dan", input$city2,
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (test_result$p.value < input$alpha) {
            paste("Kabupaten/Kota", input$city1, "dan", input$city2, 
                  "memiliki proporsi", input$variable, "yang berbeda secara signifikan.",
                  ifelse(prop_diff > 0, 
                         paste(input$city1, "memiliki proporsi yang lebih tinggi."),
                         paste(input$city2, "memiliki proporsi yang lebih tinggi.")))
          } else {
            paste("Tidak ada bukti yang cukup untuk menyimpulkan bahwa kedua daerah memiliki proporsi", 
                  input$variable, "yang berbeda.")
          }
        )
        
      } else if (input$test_type == "two_provinces") {
        req(input$province1, input$province2)
        
        if (input$province1 == input$province2) {
          showNotification("Pilih dua provinsi yang berbeda", type = "error")
          return()
        }
        
        # Data untuk kedua provinsi
        prov1_data <- data[data$PROVINCE_NAME == input$province1, ]
        prov2_data <- data[data$PROVINCE_NAME == input$province2, ]
        
        if (nrow(prov1_data) == 0 || nrow(prov2_data) == 0) {
          showNotification("Salah satu atau kedua provinsi tidak memiliki data", type = "error")
          return()
        }
        
        var1 <- na.omit(prov1_data[[input$variable]])
        var2 <- na.omit(prov2_data[[input$variable]])
        
        if (length(var1) == 0 || length(var2) == 0) {
          showNotification("Tidak ada data valid untuk salah satu atau kedua provinsi", type = "error")
          return()
        }
        
        # Calculate proportions based on threshold
        threshold_percentile <- input$threshold_percentile / 100
        all_values <- c(var1, var2)
        threshold_value <- quantile(all_values, threshold_percentile, na.rm = TRUE)
        
        successes1 <- sum(var1 > threshold_value)
        successes2 <- sum(var2 > threshold_value)
        trials1 <- length(var1)
        trials2 <- length(var2)
        
        test_result <- prop.test(c(successes1, successes2), c(trials1, trials2),
                                 alternative = input$alternative,
                                 conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot perbandingan provinsi
        comparison_data <- data.frame(
          Provinsi = c(input$province1, input$province2),
          Proporsi = c(successes1/trials1, successes2/trials2),
          N_Kabupaten = c(trials1, trials2),
          Sukses = c(successes1, successes2)
        )
        
        test_results$plot <- ggplot(comparison_data, aes(x = Provinsi, y = Proporsi)) +
          geom_col(fill = c("lightgreen", "orange"), alpha = 0.7) +
          geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(", Sukses, "/", N_Kabupaten, " kab/kota)")), 
                    vjust = -0.5) +
          labs(title = paste("Perbandingan Proporsi", input$variable, "Antar Provinsi"),
               subtitle = paste("Threshold:", round(threshold_value, 3), 
                                "(Persentil", input$threshold_percentile, "%)"),
               y = "Proporsi Kabupaten/Kota di Atas Threshold") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Interpretasi
        prop1 <- successes1/trials1
        prop2 <- successes2/trials2
        prop_diff <- prop1 - prop2
        
        test_results$interpretation <- paste(
          "INTERPRETASI UJI PROPORSI DUA PROVINSI:\n",
          "======================================\n\n",
          "PROVINSI YANG DIBANDINGKAN:\n",
          "- Provinsi 1:", input$province1, "\n",
          "  * Jumlah kabupaten/kota (n₁) =", trials1, "\n",
          "  * Proporsi kabupaten/kota di atas threshold (p̂₁) =", round(prop1, 4), "\n",
          "  * Jumlah kabupaten/kota sukses =", successes1, "\n",
          "- Provinsi 2:", input$province2, "\n",
          "  * Jumlah kabupaten/kota (n₂) =", trials2, "\n",
          "  * Proporsi kabupaten/kota di atas threshold (p̂₂) =", round(prop2, 4), "\n",
          "  * Jumlah kabupaten/kota sukses =", successes2, "\n",
          "- Perbedaan proporsi (p̂₁ - p̂₂) =", round(prop_diff, 4), "\n\n",
          "METODE ANALISIS:\n",
          "- Variabel:", input$variable, "\n",
          "- Threshold (Persentil", input$threshold_percentile, "%) =", round(threshold_value, 3), "\n",
          "- Unit analisis: Kabupaten/Kota dalam setiap provinsi\n",
          "- Kriteria sukses: Kabupaten/Kota dengan nilai di atas threshold\n\n",
          "HIPOTESIS:\n",
          "- H₀: p₁ = p₂ (proporsi kabupaten/kota sama)\n",
          "- H₁: p₁", switch(input$alternative, 
                             "two.sided" = "≠", 
                             "greater" = ">", 
                             "less" = "<"), "p₂\n\n",
          "HASIL UJI STATISTIK:\n",
          "- Chi-square statistik =", round(test_result$statistic, 4), "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) untuk perbedaan = [", 
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀. Ada perbedaan proporsi yang signifikan antara", 
                  input$province1, "dan", input$province2,
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak ada perbedaan proporsi yang signifikan antara", 
                  input$province1, "dan", input$province2,
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (test_result$p.value < input$alpha) {
            paste("Provinsi", input$province1, "dan", input$province2, 
                  "memiliki perbedaan yang signifikan dalam hal proporsi kabupaten/kota dengan", 
                  input$variable, "tinggi.",
                  ifelse(prop_diff > 0, 
                         paste("Provinsi", input$province1, "memiliki lebih banyak kabupaten/kota dengan nilai tinggi."),
                         paste("Provinsi", input$province2, "memiliki lebih banyak kabupaten/kota dengan nilai tinggi.")))
          } else {
            paste("Tidak ada bukti yang cukup untuk menyimpulkan bahwa kedua provinsi memiliki perbedaan", 
                  "dalam proporsi kabupaten/kota dengan", input$variable, "tinggi.")
          }
        )
      }
    })
    
    # Output
    output$test_results <- renderPrint({
      req(test_results$results)
      test_results$results
    })
    
    output$test_plot <- renderPlot({
      req(test_results$plot)
      test_results$plot
    })
    
    output$interpretation <- renderText({
      req(test_results$interpretation)
      test_results$interpretation
    })
    
    # Download handlers
    output$download_results <- downloadHandler(
      filename = function() paste("uji_t_hasil_", Sys.Date(), ".txt", sep=""),
      content = function(file) {
        if (!is.null(test_results$results)) {
          writeLines(capture.output(print(test_results$results)), file)
        }
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() paste("uji_t_plot_", Sys.Date(), ".png", sep=""),
      content = function(file) {
        if (!is.null(test_results$plot)) {
          ggsave(file, test_results$plot, width = 12, height = 8, dpi = 300)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() paste("uji_t_laporan_", Sys.Date(), ".docx", sep=""),
      content = function(file) {
        if (!is.null(test_results$interpretation)) {
          doc <- officer::read_docx()
          doc <- doc %>%
            officer::body_add_par("Laporan Uji t", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel:", input$variable)) %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Uji:", style = "heading 2") %>%
            officer::body_add_par(capture.output(print(test_results$results))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 10, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par("Visualisasi:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 8, height = 5)
            unlink(temp_file)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}