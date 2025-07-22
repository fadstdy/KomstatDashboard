# Uji t Module
# modules/uji_t_module.R

# UJI t - UI FUNCTION
ujiTUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "Panel Kontrol Uji t",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             
             selectInput(ns("test_type"), 
                         "Jenis Uji t:",
                         choices = list(
                           "Uji t Satu Sampel" = "one_sample",
                           "Uji t Dua Sampel Independen" = "two_sample"
                         )),
             
             selectInput(ns("variable"), 
                         "Variabel Numerik:",
                         choices = NULL),
             
             # Input untuk one sample
             conditionalPanel(
               condition = "input.test_type == 'one_sample'",
               ns = ns,
               numericInput(ns("mu0"), 
                            "Nilai Hipotesis (μ₀):",
                            value = 0),
               helpText("Nilai yang akan dibandingkan dengan rata-rata sampel")
             ),
             
             # Input untuk two sample (checkbox equal_var dihapus)
             conditionalPanel(
               condition = "input.test_type == 'two_sample'",
               ns = ns,
               selectInput(ns("group_variable"), 
                           "Variabel Pengelompokan:",
                           choices = NULL),
               # checkboxInput untuk equal_var dihapus di sini
               helpText("Asumsi: Variansi antar kelompok adalah sama (Student's t-test)")
             ),
             
             selectInput(ns("alternative"), 
                         "Hipotesis Alternatif:",
                         choices = list(
                           "Dua arah (≠)" = "two.sided",
                           "Lebih besar (>)" = "greater",
                           "Lebih kecil (<)" = "less"
                         )),
             
             numericInput(ns("alpha"), 
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             # Tombol Aksi
             br(),
             
             div(style = "width: 90%; text-align: center;",
                 actionButton(ns("run_test"),
                              "Jalankan Uji t",
                              class = "btn-info",
                              style = "width: 90%; margin-bottom: 10px;") # Lebar 90%
             ),
             br(),
             # Perbaikan: Tambahkan div untuk mengatur lebar dan perataan tombol download
             h5("Download Hasil:"),
             div(style = "width: 90%; text-align: center;",
                 downloadButton(ns("download_results"),
                                "Hasil Uji",
                                class = "btn-info",
                                style = "width: 90%; margin-bottom: 5px;"), # Lebar 90%
                 br(),
                 downloadButton(ns("download_plot"),
                                "Plot Asumsi",
                                class = "btn-info",
                                style = "width: 90%; margin-bottom: 5px;"), # Lebar 90%
                 br(),
                 downloadButton(ns("download_report"),
                                "Laporan",
                                class = "btn-info",
                                style = "width: 90%;") # Lebar 90%
             )
           )
    ),
    
    # Hasil
    column(8,
           box(
             title = "Hasil Uji t",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           box(
             title = "Visualisasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("test_plot"), height = "400px"))
           )
    ),
    
    # Interpretasi
    column(12,
           box(
             title = "Interpretasi Uji t",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# UJI t - SERVER FUNCTION
ujiTServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      # Filter categorical vars yang valid (2 kelompok dengan data cukup)
      valid_grouping <- categorical_vars[
        sapply(categorical_vars, function(var) {
          if (!var %in% names(values$current_data)) return(FALSE)
          
          groups <- unique(na.omit(values$current_data[[var]]))
          
          # Baris ini memastikan variabel memiliki TEPAT 2 kelompok.
          if (length(groups) != 2) return(FALSE)
          
          # Cek ukuran masing-masing kelompok
          group_sizes <- table(values$current_data[[var]])
          min_size <- min(group_sizes)
          
          # Baris ini memastikan SETIAP kelompok memiliki minimal 3 observasi.
          return(min_size >= 3)
        })
      ]
      
      updateSelectInput(session, "variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      
      if (length(valid_grouping) > 0) {
        # Tambahkan info jumlah kelompok di label
        var_labels <- sapply(valid_grouping, function(var) {
          group_sizes <- table(values$current_data[[var]])
          paste0(var, " (", paste(names(group_sizes), ":", group_sizes, collapse = " vs "), ")")
        })
        
        updateSelectInput(session, "group_variable", 
                          choices = setNames(c("", valid_grouping), 
                                             c("-- Pilih Variabel --", var_labels)))
      } else {
        updateSelectInput(session, "group_variable", 
                          choices = list("-- Tidak ada variabel dengan 2 kelompok valid --" = ""))
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
      
      if (input$test_type == "one_sample") {
        # One sample t-test
        variable <- data[[input$variable]]
        variable_clean <- na.omit(variable)
        
        if (length(variable_clean) < 3) {
          showNotification("Data tidak cukup untuk uji t (minimal 3 observasi)", type = "error")
          return()
        }
        
        test_result <- t.test(variable_clean, mu = input$mu0, 
                              alternative = input$alternative,
                              conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot
        plot_data <- data.frame(Value = variable_clean)
        test_results$plot <- ggplot(plot_data, aes(x = Value)) +
          geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
          geom_vline(xintercept = mean(variable_clean), 
                     color = "red", linetype = "dashed", linewidth = 1.2) +
          geom_vline(xintercept = input$mu0, 
                     color = "blue", linetype = "dashed", linewidth = 1.2) +
          labs(title = paste("Distribusi", input$variable, "- Uji t Satu Sampel"),
               subtitle = paste("Garis Merah: Rata-rata Sampel =", round(mean(variable_clean), 3), 
                                "| Garis Biru: Hipotesis μ₀ =", input$mu0),
               x = input$variable, y = "Frekuensi") +
          theme_custom() +
          theme(plot.subtitle = element_text(size = 10))
        
        # Interpretasi
        test_results$interpretation <- paste(
          "INTERPRETASI UJI t SATU SAMPEL:\n\n",
          "VARIABEL YANG DIUJI:\n",
          "- Variabel:", input$variable, "\n",
          "- Jumlah observasi:", length(variable_clean), "\n",
          "- Rata-rata sampel:", round(test_result$estimate, 4), "\n\n",
          "HIPOTESIS:\n",
          "- H₀: μ =", input$mu0, "\n",
          "- H₁: μ", switch(input$alternative, 
                            "two.sided" = "≠", 
                            "greater" = ">", 
                            "less" = "<"), input$mu0, "\n\n",
          "HASIL UJI STATISTIK:\n",
          "- t-statistik =", round(test_result$statistic, 4), "\n",
          "- Derajat bebas (df) =", test_result$parameter, "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) = [", 
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀.Terdapat cukup bukti untuk menyatakan bahwa rata-rata populasi",
                  switch(input$alternative,
                         "two.sided" = "berbeda dari",
                         "greater" = "lebih besar dari", 
                         "less" = "lebih kecil dari"), input$mu0, 
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak cukup bukti untuk menyatakan bahwa rata-rata populasi",
                  switch(input$alternative,
                         "two.sided" = "berbeda dari",
                         "greater" = "lebih besar dari",
                         "less" = "lebih kecil dari"), input$mu0,
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n",
          if (test_result$p.value < input$alpha) {
            paste("Rata-rata", input$variable, "dalam sampel berbeda secara signifikan dari nilai hipotesis", input$mu0, ".")
          } else {
            paste("Rata-rata", input$variable, "dalam sampel tidak berbeda secara signifikan dari nilai hipotesis", input$mu0, ".")
          }
        )
        
      } else if (input$test_type == "two_sample") {
        req(input$group_variable)
        
        if (input$group_variable == "") {
          showNotification("Silakan pilih variabel pengelompokan", type = "warning")
          return()
        }
        
        # Validasi data
        complete_data <- data[complete.cases(data[[input$variable]], data[[input$group_variable]]), ]
        
        if (nrow(complete_data) < 6) {
          showNotification("Data tidak cukup untuk uji t dua sampel (minimal 6 observasi)", type = "error")
          return()
        }
        
        # Cek apakah ada tepat 2 kelompok
        groups <- unique(complete_data[[input$group_variable]])
        if (length(groups) != 2) {
          showNotification(paste("Variabel pengelompokan harus memiliki tepat 2 kelompok. Ditemukan", 
                                 length(groups), "kelompok."), type = "error")
          return()
        }
        
        # Two sample t-test (selalu dengan var.equal = TRUE)
        formula_str <- paste(input$variable, "~", input$group_variable)
        test_result <- t.test(as.formula(formula_str), data = complete_data,
                              var.equal = TRUE, # Variansi selalu diasumsikan sama
                              alternative = input$alternative,
                              conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot perbandingan
        test_results$plot <- ggplot(complete_data, aes(x = .data[[input$group_variable]], 
                                                       y = .data[[input$variable]])) +
          geom_boxplot(aes(fill = .data[[input$group_variable]]), alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
          stat_summary(fun = mean, geom = "point", color = "red", size = 4, shape = 18) +
          stat_summary(fun = mean, geom = "text", 
                       aes(label = paste("μ =", round(..y.., 2))), 
                       vjust = -0.5, color = "red", size = 3) +
          scale_fill_brewer(palette = "Set2") +
          labs(title = paste("Perbandingan", input$variable, "berdasarkan", input$group_variable),
               subtitle = "Boxplot + Titik Individual | Titik Merah = Rata-rata Kelompok",
               x = input$group_variable, y = input$variable) +
          theme_custom() +
          theme(legend.position = "none")
        
        # Interpretasi
        group_names <- names(test_result$estimate)
        group_means <- as.numeric(test_result$estimate)
        group_diff <- diff(group_means)
        
        # Hitung ukuran kelompok
        group_sizes <- table(complete_data[[input$group_variable]])
        
        test_results$interpretation <- paste(
          "INTERPRETASI UJI t DUA SAMPEL (STUDENT'S T-TEST):\n\n", 
          "KELOMPOK YANG DIBANDINGKAN:\n",
          "- Kelompok 1:", group_names[1], "\n",
          "  * Ukuran sampel (n₁) =", group_sizes[1], "\n",
          "  * Rata-rata (μ₁) =", round(group_means[1], 4), "\n",
          "- Kelompok 2:", group_names[2], "\n",
          "  * Ukuran sampel (n₂) =", group_sizes[2], "\n", 
          "  * Rata-rata (μ₂) =", round(group_means[2], 4), "\n",
          "- Perbedaan rata-rata (μ₁ - μ₂) =", round(group_diff, 4), "\n\n",
          "ASUMSI UJI:\n",
          "- Variansi diasumsikan sama (Student's t-test).\n\n", # Baris asumsi diubah
          "HIPOTESIS:\n",
          "- H₀: μ₁ = μ₂ (tidak ada perbedaan rata-rata)\n",
          "- H₁: μ₁", switch(input$alternative, 
                             "two.sided" = "≠", 
                             "greater" = ">", 
                             "less" = "<"), "μ₂\n\n",
          "HASIL UJI STATISTIK:\n",
          "- t-statistik =", round(test_result$statistic, 4), "\n",
          "- Derajat bebas (df) =", round(test_result$parameter, 2), "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) untuk perbedaan = [", 
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀. Ada perbedaan yang signifikan antara rata-rata", 
                  group_names[1], "dan", group_names[2], 
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak ada perbedaan yang signifikan antara rata-rata", 
                  group_names[1], "dan", group_names[2],
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n",
          if (test_result$p.value < input$alpha) {
            paste("Kelompok", group_names[1], "memiliki rata-rata", input$variable, 
                  "yang berbeda secara signifikan dibandingkan kelompok", group_names[2], ".",
                  ifelse(group_diff > 0, 
                         paste("Kelompok", group_names[1], "memiliki nilai yang lebih tinggi."),
                         paste("Kelompok", group_names[2], "memiliki nilai yang lebih tinggi.")))
          } else {
            paste("Tidak ada bukti yang cukup untuk menyimpulkan bahwa kedua kelompok memiliki rata-rata", 
                  input$variable, "yang berbeda. Perbedaan yang diamati bisa disebabkan oleh variasi sampling.")
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
        tryCatch({ # Tambah tryCatch
          req(test_results$results,
              test_results$interpretation,
              input$test_type,
              input$variable)
          # test_results$plot tidak di-req di sini karena bisa null, dicheck terpisah
          
          doc <- officer::read_docx()
          doc <- doc %>%
            officer::body_add_par("Laporan Uji t", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel:", input$variable)) %>%
            officer::body_add_par(paste("Jenis Uji:", switch(input$test_type,
                                                             "one_sample" = "Uji t Satu Sampel",
                                                             "two_sample" = "Uji t Dua Sampel Independen"))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Uji:", style = "heading 2") %>%
            # PERBAIKAN: Gunakan paste(..., collapse = "\n")
            officer::body_add_par(paste(capture.output(print(test_results$results)), collapse = "\n")) %>%
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
          show_notification("Laporan Word berhasil dibuat!", type = "success") # Menggunakan show_notification
        }, error = function(e) {
          show_notification(paste("Error saat membuat laporan Word:", e$message), type = "error") # Menggunakan show_notification
        })
      }
    )
  })
}