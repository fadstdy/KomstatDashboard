# Uji Chi-Square Module
# modules/uji_chi_square_module.R

# ==========================================
# UJI CHI-SQUARE - UI FUNCTION
# ==========================================

ujiChiSquareUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "🔗 Panel Kontrol Uji Chi-Square",
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             
             selectInput(ns("test_type"), 
                         "Jenis Uji Chi-Square:",
                         choices = list(
                           "Uji Independensi (Asosiasi)" = "independence",
                           "Uji Goodness of Fit" = "goodness_of_fit"
                         )),
             
             # Input untuk uji independensi
             conditionalPanel(
               condition = "input.test_type == 'independence'",
               ns = ns,
               selectInput(ns("var1"), 
                           "Variabel 1 (Baris):",
                           choices = NULL),
               selectInput(ns("var2"), 
                           "Variabel 2 (Kolom):",
                           choices = NULL),
               helpText("Menguji apakah ada asosiasi antara dua variabel kategorikal")
             ),
             
             # Input untuk goodness of fit
             conditionalPanel(
               condition = "input.test_type == 'goodness_of_fit'",
               ns = ns,
               selectInput(ns("variable"), 
                           "Variabel Kategorikal:",
                           choices = NULL),
               selectInput(ns("expected_type"), 
                           "Distribusi yang Diharapkan:",
                           choices = list(
                             "Distribusi Seragam (Sama Rata)" = "uniform",
                             "Distribusi Kustom" = "custom"
                           )),
               
               # Input untuk distribusi kustom
               conditionalPanel(
                 condition = "input.expected_type == 'custom'",
                 ns = ns,
                 textAreaInput(ns("expected_proportions"), 
                               "Proporsi yang Diharapkan (pisahkan dengan koma):",
                               value = "",
                               placeholder = "Contoh: 0.3, 0.4, 0.3"),
                 helpText("Jumlah proporsi harus = 1.0")
               ),
               
               helpText("Menguji apakah distribusi observasi sesuai dengan distribusi yang diharapkan")
             ),
             
             # Opsi analisis tambahan
             checkboxInput(ns("show_expected"), 
                           "Tampilkan Frekuensi yang Diharapkan", 
                           value = TRUE),
             
             checkboxInput(ns("show_residuals"), 
                           "Tampilkan Standardized Residuals", 
                           value = FALSE),
             
             numericInput(ns("alpha"), 
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             br(),
             actionButton(ns("run_test"), 
                          "Jalankan Uji Chi-Square", 
                          class = "btn-danger btn-lg"),
             
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
             title = "Hasil Uji Chi-Square",
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           box(
             title = "Tabel Kontingensi / Frekuensi",
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             withSpinner(tableOutput(ns("contingency_table")))
           ),
           
           box(
             title = "Visualisasi",
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("test_plot"), height = "400px"))
           ),
           
           # Tabel tambahan (conditional)
           conditionalPanel(
             condition = "output.show_additional_tables == true",
             ns = ns,
             box(
               title = "Analisis Tambahan",
               status = "info",
               solidHeader = TRUE,
               width = 12,
               collapsible = TRUE,
               tabsetPanel(
                 tabPanel("Frekuensi yang Diharapkan",
                          br(),
                          withSpinner(tableOutput(ns("expected_table")))),
                 tabPanel("Standardized Residuals",
                          br(),
                          withSpinner(tableOutput(ns("residuals_table"))))
               )
             )
           )
    ),
    
    # Interpretasi
    column(12,
           box(
             title = "Interpretasi Uji Chi-Square",
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
# UJI CHI-SQUARE - SERVER FUNCTION
# ==========================================

ujiChiSquareServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update pilihan variabel
    observe({
      categorical_vars <- get_categorical_vars(values$current_data)
      
      # Filter categorical vars yang valid (2-15 kategori)
      valid_categorical <- categorical_vars[
        sapply(categorical_vars, function(var) {
          if (!var %in% names(values$current_data)) return(FALSE)
          
          categories <- unique(na.omit(values$current_data[[var]]))
          category_counts <- table(values$current_data[[var]])
          min_count <- min(category_counts)
          
          return(length(categories) >= 2 && length(categories) <= 15 && min_count >= 1)
        })
      ]
      
      if (length(valid_categorical) > 0) {
        # Tambahkan info jumlah kategori di label
        var_labels <- sapply(valid_categorical, function(var) {
          n_categories <- length(unique(values$current_data[[var]]))
          paste0(var, " (", n_categories, " kategori)")
        })
        
        updateSelectInput(session, "var1", 
                          choices = setNames(c("", valid_categorical), 
                                             c("-- Pilih Variabel 1 --", var_labels)))
        updateSelectInput(session, "var2", 
                          choices = setNames(c("", valid_categorical), 
                                             c("-- Pilih Variabel 2 --", var_labels)))
        updateSelectInput(session, "variable", 
                          choices = setNames(c("", valid_categorical), 
                                             c("-- Pilih Variabel --", var_labels)))
      } else {
        updateSelectInput(session, "var1", 
                          choices = list("-- Tidak ada variabel kategorikal yang valid --" = ""))
        updateSelectInput(session, "var2", 
                          choices = list("-- Tidak ada variabel kategorikal yang valid --" = ""))
        updateSelectInput(session, "variable", 
                          choices = list("-- Tidak ada variabel kategorikal yang valid --" = ""))
      }
    })
    
    # Nilai reaktif untuk hasil
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      contingency_table = NULL,
      expected_table = NULL,
      residuals_table = NULL,
      show_additional = FALSE
    )
    
    # Control untuk menampilkan tabel tambahan
    output$show_additional_tables <- reactive({
      test_results$show_additional
    })
    outputOptions(output, "show_additional_tables", suspendWhenHidden = FALSE)
    
    # Jalankan uji
    observeEvent(input$run_test, {
      
      data <- values$current_data
      
      if (input$test_type == "independence") {
        req(input$var1, input$var2)
        
        if (input$var1 == "" || input$var2 == "") {
          showNotification("Silakan pilih kedua variabel", type = "warning")
          return()
        }
        
        if (input$var1 == input$var2) {
          showNotification("Pilih dua variabel yang berbeda", type = "warning")
          return()
        }
        
        # Buat tabel kontingensi
        complete_data <- data[complete.cases(data[[input$var1]], data[[input$var2]]), ]
        
        if (nrow(complete_data) < 5) {
          showNotification("Data tidak cukup untuk uji Chi-Square (minimal 5 observasi)", type = "error")
          return()
        }
        
        cont_table <- table(complete_data[[input$var1]], complete_data[[input$var2]])
        
        # Cek apakah ada sel dengan frekuensi yang terlalu kecil
        if (any(cont_table < 5)) {
          warning_msg <- "Peringatan: Beberapa sel memiliki frekuensi < 5. Hasil uji mungkin tidak akurat."
          showNotification(warning_msg, type = "warning")
        }
        
        # Uji Chi-Square untuk independensi
        test_result <- chisq.test(cont_table)
        test_results$results <- test_result
        test_results$contingency_table <- addmargins(cont_table)
        
        # Tabel tambahan
        if (input$show_expected) {
          test_results$expected_table <- round(test_result$expected, 2)
          test_results$show_additional <- TRUE
        }
        
        if (input$show_residuals) {
          test_results$residuals_table <- round(test_result$stdres, 2)
          test_results$show_additional <- TRUE
        }
        
        # Plot heatmap
        cont_df <- as.data.frame(cont_table)
        names(cont_df) <- c("Var1", "Var2", "Freq")
        
        test_results$plot <- ggplot(cont_df, aes(x = Var1, y = Var2, fill = Freq)) +
          geom_tile(color = "white", linewidth = 0.5) +
          geom_text(aes(label = Freq), color = "white", size = 4, fontface = "bold") +
          scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Frekuensi") +
          labs(title = paste("Tabel Kontingensi:", input$var1, "vs", input$var2),
               subtitle = "Heatmap Frekuensi Observasi",
               x = input$var1, y = input$var2) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Interpretasi
        chi_sq <- test_result$statistic
        df <- test_result$parameter
        p_value <- test_result$p.value
        
        # Hitung Cramér's V sebagai ukuran kekuatan asosiasi
        n <- sum(cont_table)
        cramers_v <- sqrt(chi_sq / (n * (min(nrow(cont_table), ncol(cont_table)) - 1)))
        
        test_results$interpretation <- paste(
          "INTERPRETASI UJI CHI-SQUARE INDEPENDENSI:\n",
          "========================================\n\n",
          "VARIABEL YANG DIUJI:\n",
          "- Variabel 1 (Baris):", input$var1, "\n",
          "- Variabel 2 (Kolom):", input$var2, "\n",
          "- Total observasi:", n, "\n",
          "- Dimensi tabel:", nrow(cont_table), "x", ncol(cont_table), "\n\n",
          "HIPOTESIS:\n",
          "- H₀: Tidak ada asosiasi antara", input$var1, "dan", input$var2, "(variabel independen)\n",
          "- H₁: Ada asosiasi antara", input$var1, "dan", input$var2, "(variabel tidak independen)\n\n",
          "HASIL UJI CHI-SQUARE:\n",
          "- Chi-square statistik (χ²) =", round(chi_sq, 4), "\n",
          "- Derajat bebas (df) =", df, "\n",
          "- p-value =", format_p_value(p_value), "\n",
          "- Cramér's V =", round(cramers_v, 3), "(ukuran kekuatan asosiasi)\n\n",
          "INTERPRETASI CRAMÉR'S V:\n",
          if (cramers_v < 0.1) "- Asosiasi sangat lemah (< 0.1)\n"
          else if (cramers_v < 0.3) "- Asosiasi lemah (0.1 - 0.3)\n"
          else if (cramers_v < 0.5) "- Asosiasi sedang (0.3 - 0.5)\n"
          else "- Asosiasi kuat (> 0.5)\n", "\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (p_value < input$alpha) {
            paste("TOLAK H₀. Ada asosiasi yang signifikan antara", input$var1, "dan", input$var2,
                  "(p-value =", format_p_value(p_value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak ada asosiasi yang signifikan antara", input$var1, "dan", input$var2,
                  "(p-value =", format_p_value(p_value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (p_value < input$alpha) {
            paste("Variabel", input$var1, "dan", input$var2, "saling terkait secara statistik.",
                  "Distribusi salah satu variabel bergantung pada nilai variabel lainnya.",
                  "Kekuatan asosiasi:", 
                  if (cramers_v < 0.1) "sangat lemah"
                  else if (cramers_v < 0.3) "lemah"
                  else if (cramers_v < 0.5) "sedang"
                  else "kuat")
          } else {
            paste("Variabel", input$var1, "dan", input$var2, "dapat dianggap independen.",
                  "Tidak ada bukti bahwa distribusi salah satu variabel dipengaruhi oleh variabel lainnya.")
          }, "\n\n",
          if (any(test_result$expected < 5)) {
            "CATATAN: Beberapa sel memiliki frekuensi yang diharapkan < 5. Pertimbangkan:\n- Menggabungkan kategori dengan frekuensi rendah\n- Menggunakan uji Fisher's Exact Test untuk tabel 2x2\n- Menggunakan uji alternatif seperti G-test\n"
          } else {
            "VALIDITAS UJI: Semua sel memiliki frekuensi yang diharapkan ≥ 5. Hasil uji dapat diandalkan.\n"
          }
        )
        
      } else if (input$test_type == "goodness_of_fit") {
        req(input$variable)
        
        if (input$variable == "") {
          showNotification("Silakan pilih variabel", type = "warning")
          return()
        }
        
        # Hitung frekuensi observasi
        complete_data <- na.omit(data[[input$variable]])
        
        if (length(complete_data) < 5) {
          showNotification("Data tidak cukup untuk uji Chi-Square (minimal 5 observasi)", type = "error")
          return()
        }
        
        observed_freq <- table(complete_data)
        categories <- names(observed_freq)
        n_categories <- length(categories)
        total_n <- sum(observed_freq)
        
        # Tentukan frekuensi yang diharapkan
        if (input$expected_type == "uniform") {
          expected_prop <- rep(1/n_categories, n_categories)
        } else {
          # Kustom proporsi
          if (input$expected_proportions == "") {
            showNotification("Silakan masukkan proporsi yang diharapkan", type = "warning")
            return()
          }
          
          prop_input <- trimws(unlist(strsplit(input$expected_proportions, ",")))
          expected_prop <- as.numeric(prop_input)
          
          if (any(is.na(expected_prop))) {
            showNotification("Proporsi yang diharapkan harus berupa angka", type = "error")
            return()
          }
          
          if (length(expected_prop) != n_categories) {
            showNotification(paste("Jumlah proporsi (", length(expected_prop), 
                                   ") harus sama dengan jumlah kategori (", n_categories, ")"), type = "error")
            return()
          }
          
          if (abs(sum(expected_prop) - 1) > 0.001) {
            showNotification("Jumlah proporsi harus = 1.0", type = "error")
            return()
          }
        }
        
        expected_freq <- expected_prop * total_n
        
        # Cek validitas uji
        if (any(expected_freq < 5)) {
          showNotification("Peringatan: Beberapa kategori memiliki frekuensi yang diharapkan < 5", type = "warning")
        }
        
        # Uji Chi-Square goodness of fit
        test_result <- chisq.test(observed_freq, p = expected_prop)
        test_results$results <- test_result
        
        # Tabel frekuensi
        freq_table <- data.frame(
          Kategori = categories,
          Observasi = as.numeric(observed_freq),
          Diharapkan = round(expected_freq, 2),
          Proporsi_Obs = round(as.numeric(observed_freq)/total_n, 3),
          Proporsi_Diharapkan = round(expected_prop, 3)
        )
        test_results$contingency_table <- freq_table
        
        if (input$show_expected) {
          test_results$expected_table <- freq_table[, c("Kategori", "Diharapkan")]
          test_results$show_additional <- TRUE
        }
        
        if (input$show_residuals) {
          residuals_data <- data.frame(
            Kategori = categories,
            Standardized_Residuals = round(test_result$stdres, 3)
          )
          test_results$residuals_table <- residuals_data
          test_results$show_additional <- TRUE
        }
        
        # Plot perbandingan
        plot_data <- data.frame(
          Kategori = rep(categories, 2),
          Frekuensi = c(as.numeric(observed_freq), expected_freq),
          Tipe = rep(c("Observasi", "Diharapkan"), each = n_categories)
        )
        
        test_results$plot <- ggplot(plot_data, aes(x = Kategori, y = Frekuensi, fill = Tipe)) +
          geom_col(position = "dodge", alpha = 0.7) +
          geom_text(aes(label = round(Frekuensi, 1)), 
                    position = position_dodge(width = 0.9), 
                    vjust = -0.3, size = 3) +
          scale_fill_manual(values = c("Observasi" = "steelblue", "Diharapkan" = "coral")) +
          labs(title = paste("Uji Goodness of Fit:", input$variable),
               subtitle = "Perbandingan Frekuensi Observasi vs Diharapkan",
               x = input$variable, y = "Frekuensi") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Interpretasi
        chi_sq <- test_result$statistic
        df <- test_result$parameter
        p_value <- test_result$p.value
        
        test_results$interpretation <- paste(
          "INTERPRETASI UJI CHI-SQUARE GOODNESS OF FIT:\n",
          "============================================\n\n",
          "VARIABEL YANG DIUJI:\n",
          "- Variabel:", input$variable, "\n",
          "- Jumlah kategori:", n_categories, "\n",
          "- Total observasi:", total_n, "\n",
          "- Distribusi yang diharapkan:", 
          ifelse(input$expected_type == "uniform", "Distribusi Seragam", "Distribusi Kustom"), "\n\n",
          "DISTRIBUSI YANG DIHARAPKAN:\n",
          paste(capture.output(print(data.frame(Kategori = categories, 
                                                Proporsi = round(expected_prop, 3)), 
                                     row.names = FALSE)), collapse = "\n"), "\n\n",
          "HIPOTESIS:\n",
          "- H₀: Data mengikuti distribusi yang diharapkan\n",
          "- H₁: Data tidak mengikuti distribusi yang diharapkan\n\n",
          "HASIL UJI CHI-SQUARE:\n",
          "- Chi-square statistik (χ²) =", round(chi_sq, 4), "\n",
          "- Derajat bebas (df) =", df, "\n",
          "- p-value =", format_p_value(p_value), "\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (p_value < input$alpha) {
            paste("TOLAK H₀. Data TIDAK mengikuti distribusi yang diharapkan",
                  "(p-value =", format_p_value(p_value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Data mengikuti distribusi yang diharapkan",
                  "(p-value =", format_p_value(p_value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (p_value < input$alpha) {
            paste("Distribusi observasi dari variabel", input$variable, 
                  "berbeda secara signifikan dari distribusi yang diharapkan.",
                  "Ada pola atau bias tertentu dalam data yang tidak sesuai dengan asumsi distribusi.")
          } else {
            paste("Distribusi observasi dari variabel", input$variable, 
                  "sesuai dengan distribusi yang diharapkan.",
                  "Tidak ada bukti adanya penyimpangan dari pola distribusi yang diasumsikan.")
          }, "\n\n",
          if (any(expected_freq < 5)) {
            "CATATAN: Beberapa kategori memiliki frekuensi yang diharapkan < 5.\nPertimbangkan menggabungkan kategori dengan frekuensi rendah.\n"
          } else {
            "VALIDITAS UJI: Semua kategori memiliki frekuensi yang diharapkan ≥ 5. Hasil uji dapat diandalkan.\n"
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
    
    output$contingency_table <- renderTable({
      req(test_results$contingency_table)
      test_results$contingency_table
    }, rownames = TRUE)
    
    output$expected_table <- renderTable({
      req(test_results$expected_table)
      test_results$expected_table
    })
    
    output$residuals_table <- renderTable({
      req(test_results$residuals_table)
      test_results$residuals_table
    })
    
    # Download handlers
    output$download_results <- downloadHandler(
      filename = function() paste("uji_chi_square_hasil_", Sys.Date(), ".txt", sep=""),
      content = function(file) {
        if (!is.null(test_results$results)) {
          writeLines(capture.output(print(test_results$results)), file)
        }
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() paste("uji_chi_square_plot_", Sys.Date(), ".png", sep=""),
      content = function(file) {
        if (!is.null(test_results$plot)) {
          ggsave(file, test_results$plot, width = 12, height = 8, dpi = 300)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() paste("uji_chi_square_laporan_", Sys.Date(), ".docx", sep=""),
      content = function(file) {
        if (!is.null(test_results$interpretation)) {
          doc <- officer::read_docx()
          doc <- doc %>%
            officer::body_add_par("Laporan Uji Chi-Square", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Uji:", style = "heading 2") %>%
            officer::body_add_par(capture.output(print(test_results$results))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Tabel Kontingensi/Frekuensi:", style = "heading 2")
          
          if (!is.null(test_results$contingency_table)) {
            doc <- doc %>% 
              officer::body_add_table(test_results$contingency_table, style = "Table Grid")
          }
          
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 10, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par("Visualisasi:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 8, height = 5)
            unlink(temp_file)
          }
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          if (!is.null(test_results$expected_table)) {
            doc <- doc %>%
              officer::body_add_par("Frekuensi yang Diharapkan:", style = "heading 3") %>%
              officer::body_add_table(test_results$expected_table, style = "Table Grid")
          }
          
          if (!is.null(test_results$residuals_table)) {
            doc <- doc %>%
              officer::body_add_par("Standardized Residuals:", style = "heading 3") %>%
              officer::body_add_table(test_results$residuals_table, style = "Table Grid")
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}