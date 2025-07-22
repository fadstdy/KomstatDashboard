# Uji Varians (F-test & Chi-squared) Module
# modules/uji_chi_square_module.R

# UJI VARIANSI - UI FUNCTION
ujiVariansUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "Panel Kontrol Uji Varians", # Judul diubah
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             
             selectInput(ns("test_type"),
                         "Pilih Jenis Uji Varians:", # Label diubah
                         choices = list(
                           "Uji Varians Satu Sampel (Chi-Square)" = "one_sample_varians", # Nama diubah
                           "Uji Varians Dua Sampel (F-test)" = "two_sample_varians" # Nama diubah
                         )),
             
             # Input untuk Uji Varians Satu Sampel
             conditionalPanel(
               condition = "input.test_type == 'one_sample_varians'",
               ns = ns,
               selectInput(ns("numerical_variable_one_sample"), # Variabel numerik untuk satu sampel
                           "Variabel Numerik:",
                           choices = NULL),
               numericInput(ns("hypothesized_variance"), # Varians hipotesis
                            "Varians Populasi Hipotesis (σ₀²):",
                            value = 1, min = 0, step = 0.1),
               helpText("Menguji varians sampel terhadap nilai hipotesis.")
             ),
             
             # Input untuk Uji Varians Dua Sampel
             conditionalPanel(
               condition = "input.test_type == 'two_sample_varians'",
               ns = ns,
               selectInput(ns("numerical_variable_two_sample"), # Variabel numerik untuk dua sampel
                           "Variabel Numerik:",
                           choices = NULL),
               selectInput(ns("grouping_variable_two_sample"), # Variabel pengelompokan (kategorikal 2 level)
                           "Variabel Pengelompokan (2 Kelompok):",
                           choices = NULL),
               helpText("Membandingkan varians dua kelompok.")
             ),
             
             selectInput(ns("alternative"), # Hipotesis alternatif (sama untuk keduanya)
                         "Hipotesis Alternatif:",
                         choices = list(
                           "Dua arah (≠)" = "two.sided",
                           "Lebih besar (>)" = "greater",
                           "Lebih kecil (<)" = "less"
                         )),
             
             numericInput(ns("alpha"),
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             br(),
             # Tombol "Jalankan Uji Varians"
             actionButton(ns("run_test"),
                          "Jalankan Uji Varians",
                          class = "btn-danger",
                          style = "width: 90%; display: block; margin: auto; margin-bottom: 15px; font-size:18px;"),
             
             hr(),
             h5("Download:"),
             # Tombol Download agar rapi
             downloadButton(ns("download_results"), "Hasil Uji",
                            class = "btn-danger",
                            style = "width: 90%; display: block; margin: auto; margin-bottom: 7px;"),
             br(),
             downloadButton(ns("download_plot"), "Plot Asumsi",
                            class = "btn-danger",
                            style = "width: 90%; display: block; margin: auto; margin-bottom: 7px;"),
             br(),
             downloadButton(ns("download_report"), "Laporan",
                            class = "btn-danger",
                            style = "width: 90%; display: block; margin: auto;")
           )
    ),
    
    # Hasil
    column(8,
           box(
             title = "Hasil Uji Varians", # Judul diubah
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           box(
             title = "Visualisasi Distribusi & Varians", # Judul diubah
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("test_plot"), height = "400px"))
           )
    ),
    
    # Interpretasi
    column(12,
           box(
             title = "Interpretasi Uji Varians", # Judul diubah
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# UJI VARIANSI - SERVER FUNCTION
ujiVariansServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      # Filter categorical vars yang valid untuk pengelompokan 2 kelompok
      valid_grouping_vars <- categorical_vars[
        sapply(categorical_vars, function(var) {
          if (!var %in% names(values$current_data)) return(FALSE)
          groups <- unique(na.omit(values$current_data[[var]]))
          length(groups) == 2 # Pastikan tepat 2 kelompok
        })
      ]
      
      updateSelectInput(session, "numerical_variable_one_sample",
                        choices = setNames(c("", numeric_vars), c("-- Pilih Variabel Numerik --", numeric_vars)))
      updateSelectInput(session, "numerical_variable_two_sample",
                        choices = setNames(c("", numeric_vars), c("-- Pilih Variabel Numerik --", numeric_vars)))
      
      
      if (length(valid_grouping_vars) > 0) {
        group_labels <- sapply(valid_grouping_vars, function(var) {
          groups <- unique(na.omit(values$current_data[[var]]))
          paste0(var, " (", groups[1], " vs ", groups[2], ")")
        })
        updateSelectInput(session, "grouping_variable_two_sample",
                          choices = setNames(c("", valid_grouping_vars), c("-- Pilih Variabel Pengelompokan --", group_labels)))
      } else {
        updateSelectInput(session, "grouping_variable_two_sample",
                          choices = list("-- Tidak ada variabel pengelompokan yang valid --" = ""))
      }
    })
    
    # Nilai reaktif untuk hasil
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      summary_data = NULL, # Untuk menyimpan data ringkasan untuk interpretasi
      test_type_run = NULL # Menyimpan jenis uji yang terakhir dijalankan
    )
    
    # Jalankan uji
    observeEvent(input$run_test, {
      test_results$results <- NULL # Clear previous results
      test_results$plot <- NULL
      test_results$interpretation <- NULL
      test_results$summary_data <- NULL
      test_results$test_type_run <- NULL # Clear previous test type
      
      data <- values$current_data
      alpha <- input$alpha
      
      if (input$test_type == "one_sample_varians") {
        req(input$numerical_variable_one_sample, input$hypothesized_variance)
        if (input$numerical_variable_one_sample == "") {
          show_notification("Silakan pilih variabel numerik.", type = "warning")
          return()
        }
        if (input$hypothesized_variance <= 0) {
          show_notification("Varians hipotesis harus lebih besar dari 0.", type = "error")
          return()
        }
        
        variable_data <- na.omit(data[[input$numerical_variable_one_sample]])
        if (length(variable_data) < 2) { # Minimal 2 observasi untuk menghitung varians
          show_notification("Data tidak cukup untuk Uji Varians Satu Sampel (minimal 2 observasi).", type = "error")
          return()
        }
        
        sample_variance <- var(variable_data)
        n <- length(variable_data)
        hyp_variance <- input$hypothesized_variance
        
        # Uji Chi-Square untuk varians satu sampel
        # H0: sigma^2 = sigma0^2
        # Chi-squared statistic: (n-1) * s^2 / sigma0^2
        chi_sq_statistic <- (n - 1) * sample_variance / hyp_variance
        df <- n - 1
        
        # Calculate p-value based on alternative hypothesis
        if (input$alternative == "two.sided") {
          p_value <- 2 * min(pchisq(chi_sq_statistic, df, lower.tail = TRUE),
                             pchisq(chi_sq_statistic, df, lower.tail = FALSE))
        } else if (input$alternative == "greater") {
          p_value <- pchisq(chi_sq_statistic, df, lower.tail = FALSE)
        } else { # less
          p_value <- pchisq(chi_sq_statistic, df, lower.tail = TRUE)
        }
        
        # Simpan hasil dalam format yang mirip dengan htest
        test_results$results <- list(
          statistic = c("X-squared" = chi_sq_statistic),
          parameter = c("df" = df),
          p.value = p_value,
          alternative = input$alternative,
          method = "Chi-squared Test for One Sample Variance",
          data.name = input$numerical_variable_one_sample,
          estimate = c("sample variance" = sample_variance),
          null.value = c("variance" = hyp_variance)
        )
        class(test_results$results) <- "htest" # Memberikan class htest agar bisa diprint standar
        
        test_results$summary_data <- list(
          test_type = "one_sample",
          variable = input$numerical_variable_one_sample,
          n = n,
          sample_variance = sample_variance,
          hyp_variance = hyp_variance
        )
        
        # Plot untuk satu sampel (histogram dengan garis varians hipotesis)
        test_results$plot <- ggplot(data.frame(Value = variable_data), aes(x = Value)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
          geom_density(color = "blue", linewidth = 1) +
          labs(title = paste("Distribusi", input$numerical_variable_one_sample),
               subtitle = paste("Varians Sampel:", round(sample_variance, 3), "| Varians Hipotesis (σ₀²):", hyp_variance),
               x = input$numerical_variable_one_sample, y = "Densitas") +
          theme_custom()
        
        # Interpretasi
        interpretation_text <- paste(
          "INTERPRETASI UJI VARIANSI SATU SAMPEL (CHI-SQUARE):\n",
          "================================================\n\n",
          "VARIABEL YANG DIUJI:\n",
          "- Variabel Numerik:", input$numerical_variable_one_sample, "\n",
          "- Ukuran Sampel (n):", n, "\n",
          "- Varians Sampel (s²):", round(sample_variance, 4), "\n",
          "- Varians Populasi Hipotesis (σ₀²):", hyp_variance, "\n\n",
          "HIPOTESIS:\n",
          "- H₀: σ² =", hyp_variance, "\n",
          "- H₁: σ²", switch(input$alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"), hyp_variance, "\n\n",
          "HASIL UJI CHI-SQUARE:\n",
          "- Statistik Chi-squared (χ²) =", round(chi_sq_statistic, 4), "\n",
          "- Derajat Bebas (df) =", df, "\n",
          "- p-value =", format_p_value(p_value), "\n\n",
          "KESIMPULAN (α =", alpha, "):\n",
          if (p_value < alpha) {
            paste("TOLAK H₀. Terdapat bukti signifikan bahwa varians populasi", input$numerical_variable_one_sample,
                  switch(input$alternative, "two.sided" = " berbeda dari", "greater" = " lebih besar dari", "less" = " lebih kecil dari"),
                  "nilai hipotesis", hyp_variance,
                  "(p-value =", format_p_value(p_value), "< α =", alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak ada bukti signifikan bahwa varians populasi", input$numerical_variable_one_sample,
                  "berbeda dari nilai hipotesis", hyp_variance,
                  "(p-value =", format_p_value(p_value), "≥ α =", alpha, ")")
          }
        )
        test_results$interpretation <- interpretation_text
        test_results$test_type_run <- "one_sample_varians"
        show_notification("Uji Varians Satu Sampel (Chi-Square) berhasil!", type = "success")
        
      } else if (input$test_type == "two_sample_varians") {
        req(input$numerical_variable_two_sample, input$grouping_variable_two_sample)
        if (input$numerical_variable_two_sample == "" || input$grouping_variable_two_sample == "") {
          show_notification("Silakan pilih variabel numerik dan variabel pengelompokan.", type = "warning")
          return()
        }
        
        variable_data <- data[[input$numerical_variable_two_sample]]
        group_data <- data[[input$grouping_variable_two_sample]]
        
        complete_data <- data.frame(Value = variable_data, Group = group_data)
        complete_data <- na.omit(complete_data)
        
        if (nrow(complete_data) < 4) { # Minimal 2 observasi per kelompok
          show_notification("Data tidak cukup untuk Uji Varians Dua Sampel (minimal 4 observasi total).", type = "error")
          return()
        }
        
        group_counts <- table(complete_data$Group)
        if (any(group_counts < 2)) {
          show_notification("Setiap kelompok harus memiliki minimal 2 observasi.", type = "error")
          return()
        }
        if (length(unique(complete_data$Group)) != 2) {
          show_notification("Variabel pengelompokan harus memiliki tepat 2 kelompok.", type = "error")
          return()
        }
        
        # Lakukan Uji Varians (F-test)
        tryCatch({
          formula_str <- paste("Value", "~", "Group")
          test_result <- var.test(as.formula(formula_str), data = complete_data,
                                  alternative = input$alternative)
          test_results$results <- test_result
          
          # Ekstrak data untuk plot dan interpretasi
          group_names <- names(group_counts)
          mean1 <- mean(complete_data$Value[complete_data$Group == group_names[1]], na.rm = TRUE)
          mean2 <- mean(complete_data$Value[complete_data$Group == group_names[2]], na.rm = TRUE)
          var1 <- var(complete_data$Value[complete_data$Group == group_names[1]], na.rm = TRUE)
          var2 <- var(complete_data$Value[complete_data$Group == group_names[2]], na.rm = TRUE)
          n1 <- group_counts[1]
          n2 <- group_counts[2]
          
          test_results$summary_data <- list(
            test_type = "two_sample",
            variable = input$numerical_variable_two_sample,
            grouping_variable = input$grouping_variable_two_sample,
            group_names = group_names,
            mean1 = mean1, mean2 = mean2,
            var1 = var1, var2 = var2,
            n1 = n1, n2 = n2
          )
          
          # Plot Density atau Boxplot untuk perbandingan varians
          test_results$plot <- ggplot(complete_data, aes(x = Value, fill = Group)) +
            geom_density(alpha = 0.6) +
            geom_vline(aes(xintercept = mean(Value[Group == group_names[1]], na.rm = TRUE), color = group_names[1]),
                       linetype = "dashed", linewidth = 1) +
            geom_vline(aes(xintercept = mean(Value[Group == group_names[2]], na.rm = TRUE), color = group_names[2]),
                       linetype = "dashed", linewidth = 1) +
            scale_fill_brewer(palette = "Pastel1", name = input$grouping_variable_two_sample) +
            scale_color_manual(values = c("red", "blue"), name = "Rata-rata") +
            labs(title = paste("Distribusi", input$numerical_variable_two_sample, "berdasarkan", input$grouping_variable_two_sample),
                 subtitle = "Density Plot dengan Rata-rata Kelompok",
                 x = input$numerical_variable_two_sample, y = "Densitas") +
            theme_custom()
          
          # Interpretasi
          interpretation_text <- paste(
            "INTERPRETASI UJI VARIANSI DUA SAMPEL (F-TEST):\n",
            "============================================\n\n",
            "VARIABEL YANG DIUJI:\n",
            "- Variabel Numerik:", input$numerical_variable_two_sample, "\n",
            "- Variabel Pengelompokan:", input$grouping_variable_two_sample, "\n\n",
            "STATISTIK DESKRIPTIF KELOMPOK:\n",
            "- Kelompok 1 (", group_names[1], "):\n",
            "  * n =", n1, "\n",
            "  * Rata-rata =", round(mean1, 4), "\n",
            "  * Varians =", round(var1, 4), "\n",
            "- Kelompok 2 (", group_names[2], "):\n",
            "  * n =", n2, "\n",
            "  * Rata-rata =", round(mean2, 4), "\n",
            "  * Varians =", round(var2, 4), "\n\n",
            "HIPOTESIS:\n",
            "- H₀: Varians kelompok ", group_names[1], " = Varians kelompok ", group_names[2], " (rasio varians = 1)\n",
            "- H₁: Varians kelompok ", group_names[1], switch(input$alternative, "two.sided" = " ≠ ", "greater" = " > ", "less" = " < "), " Varians kelompok ", group_names[2], "\n\n",
            "HASIL UJI F-STATISTIK:\n",
            "- F-statistik =", round(test_result$statistic, 4), "\n",
            "- Derajat bebas (df1, df2) =", test_result$parameter[1], ", ", test_result$parameter[2], "\n",
            "- p-value =", format_p_value(test_result$p.value), "\n",
            "- Rasio Varians yang Diobservasi =", round(test_result$estimate, 4), "\n\n",
            "KESIMPULAN (α =", alpha, "):\n",
            if (test_result$p.value < alpha) {
              paste("TOLAK H₀. Terdapat bukti signifikan bahwa varians", input$numerical_variable_two_sample,
                    "untuk kelompok", group_names[1],
                    switch(input$alternative, "two.sided" = " berbeda secara signifikan dari", "greater" = " secara signifikan lebih besar dari", "less" = " secara signifikan lebih kecil dari"),
                    "varians untuk kelompok", group_names[2],
                    "(p-value =", format_p_value(test_result$p.value), "< α =", alpha, ")")
            } else {
              paste("GAGAL TOLAK H₀. Tidak ada bukti signifikan bahwa varians", input$numerical_variable_two_sample,
                    "untuk kelompok", group_names[1],
                    "berbeda secara signifikan dari varians untuk kelompok", group_names[2],
                    "(p-value =", format_p_value(test_result$p.value), "≥ α =", alpha, ")")
            }, "\n\n",
            "IMPLIKASI:\n",
            if (test_result$p.value < alpha) {
              paste("Karena varians tidak homogen, ini penting untuk uji statistik lain (misalnya uji t independen).",
                    "Jika Anda melakukan uji t, pertimbangkan menggunakan Welch's t-test (yang tidak mengasumsikan varians sama).")
            } else {
              paste("Varians antar kelompok dapat diasumsikan homogen. Ini mendukung penggunaan uji statistik yang mengasumsikan homogenitas varians (misalnya Student's t-test).")
            }
          )
          test_results$interpretation <- interpretation_text
          test_results$test_type_run <- "two_sample_varians"
          show_notification("Uji Varians Dua Sampel (F-test) berhasil!", type = "success")
          
        }, error = function(e) {
          show_notification(paste("Error dalam Uji Varians Dua Sampel (F-test):", e$message), type = "error")
          test_results$results <- paste("Error:", e$message)
          test_results$plot <- NULL
          test_results$interpretation <- "Uji Varians Dua Sampel gagal dilakukan. Pastikan variabel numerik dan variabel pengelompokan (dengan tepat 2 kategori) sudah terpilih dan memiliki data yang cukup."
        })
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
      filename = function() {
        paste("uji_varians_hasil_", test_results$test_type_run, "_", Sys.Date(), ".txt", sep="")
      },
      content = function(file) {
        tryCatch({
          req(test_results$results)
          sink(file)
          cat("HASIL UJI VARIANSI\n")
          cat("===================\n\n")
          cat("Tanggal Analisis:", as.character(Sys.Date()), "\n")
          cat("Waktu Analisis:", format(Sys.time(), "%H:%M:%S"), "\n\n")
          
          if (test_results$test_type_run == "one_sample_varians" && !is.null(test_results$summary_data)) {
            sd <- test_results$summary_data
            cat("JENIS UJI: Uji Varians Satu Sampel (Chi-Square)\n")
            cat("Variabel Numerik:", sd$variable, "\n")
            cat("Ukuran Sampel (n):", sd$n, "\n")
            cat("Varians Sampel (s²):", round(sd$sample_variance, 4), "\n")
            cat("Varians Hipotesis (σ₀²):", sd$hyp_variance, "\n\n")
          } else if (test_results$test_type_run == "two_sample_varians" && !is.null(test_results$summary_data)) {
            sd <- test_results$summary_data
            cat("JENIS UJI: Uji Varians Dua Sampel (F-test)\n")
            cat("Variabel Numerik:", sd$variable, "\n")
            cat("Variabel Pengelompokan:", sd$grouping_variable, "\n")
            cat("Kelompok 1 (", sd$group_names[1], "): n=", sd$n1, ", Rata-rata=", round(sd$mean1, 4), ", Varians=", round(sd$var1, 4), "\n")
            cat("Kelompok 2 (", sd$group_names[2], "): n=", sd$n2, ", Rata-rata=", round(sd$mean2, 4), ", Varians=", round(sd$var2, 4), "\n\n")
          }
          
          cat("STATISTIK UJI:\n")
          cat("--------------\n")
          print(test_results$results)
          cat("\n\n")
          cat("INTERPRETASI:\n")
          cat("-------------\n")
          cat(test_results$interpretation)
          
          sink()
          show_notification("Hasil uji berhasil diunduh!", type = "success")
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh hasil uji:", e$message), type = "error")
        })
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("uji_varians_plot_", test_results$test_type_run, "_", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        tryCatch({
          req(test_results$plot)
          ggsave(file, test_results$plot, width = 12, height = 8, dpi = 300)
          show_notification("Plot berhasil diunduh!", type = "success")
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh plot:", e$message), type = "error")
        })
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("uji_varians_laporan_", test_results$test_type_run, "_", Sys.Date(), ".docx", sep="")
      },
      content = function(file) {
        tryCatch({
          req(test_results$interpretation, test_results$results)
          
          doc <- officer::read_docx()
          doc <- doc %>%
            officer::body_add_par("Laporan Uji Varians", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Waktu:", format(Sys.time(), "%H:%M:%S")))
          
          if (test_results$test_type_run == "one_sample_varians") {
            doc <- doc %>%
              officer::body_add_par(paste("Jenis Uji: Uji Varians Satu Sampel (Chi-Square)")) %>%
              officer::body_add_par(paste("Variabel Numerik:", input$numerical_variable_one_sample)) %>%
              officer::body_add_par(paste("Varians Populasi Hipotesis (σ₀²):", input$hypothesized_variance))
          } else if (test_results$test_type_run == "two_sample_varians") {
            doc <- doc %>%
              officer::body_add_par(paste("Jenis Uji: Uji Varians Dua Sampel (F-test)")) %>%
              officer::body_add_par(paste("Variabel Numerik:", input$numerical_variable_two_sample)) %>%
              officer::body_add_par(paste("Variabel Pengelompokan:", input$grouping_variable_two_sample))
          }
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Statistik Uji:", style = "heading 2") %>%
            officer::body_add_par(paste(capture.output(print(test_results$results)), collapse = "\n"))
          
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 10, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par(" ") %>%
              officer::body_add_par("Visualisasi Distribusi & Varians:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 8, height = 5)
            unlink(temp_file)
          }
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("---") %>%
            officer::body_add_par("Laporan dibuat secara otomatis oleh sistem analisis statistik") %>%
            officer::body_add_par(paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          
          print(doc, target = file)
          show_notification("Laporan Word berhasil dibuat!", type = "success")
        }, error = function(e) {
          show_notification(paste("Error saat membuat laporan Word:", e$message), type = "error")
        })
      }
    )
  })
}