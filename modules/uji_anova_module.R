# Uji ANOVA Module
# modules/uji_anova_module.R

# UJI ANOVA - UI FUNCTION
ujiAnovaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "📈 Panel Kontrol Uji ANOVA",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             
             selectInput(ns("test_type"),
                         "Jenis Uji ANOVA:",
                         choices = list(
                           "ANOVA Satu Arah" = "one_way",
                           "ANOVA Dua Arah" = "two_way"
                         )),
             
             selectInput(ns("dependent_variable"),
                         "Variabel Dependen (Numerik):",
                         choices = NULL),
             
             # Input untuk ANOVA satu arah
             conditionalPanel(
               condition = "input.test_type == 'one_way'",
               ns = ns,
               selectInput(ns("factor_variable"),
                           "Variabel Faktor (Pengelompokan):",
                           choices = NULL),
               helpText("Minimal 3 kelompok untuk analisis yang bermakna")
             ),
             
             # Input untuk ANOVA dua arah
             conditionalPanel(
               condition = "input.test_type == 'two_way'",
               ns = ns,
               selectInput(ns("factor1"),
                           "Faktor 1:",
                           choices = NULL),
               selectInput(ns("factor2"),
                           "Faktor 2:",
                           choices = NULL),
               checkboxInput(ns("include_interaction"),
                             "Sertakan Efek Interaksi",
                             value = TRUE),
               helpText("Efek interaksi menguji apakah efek faktor 1 bergantung pada faktor 2")
             ),
             
             numericInput(ns("alpha"),
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             # Opsi Post-hoc (disederhanakan, tidak ada lagi selectInput)
             conditionalPanel(
               condition = "input.test_type == 'one_way'",
               ns = ns,
               helpText("Uji Post-hoc (Tukey HSD) akan dilakukan otomatis jika ANOVA signifikan.")
             ),
             
             br(),
             # Tombol "Jalankan Uji ANOVA" agar rapi
             actionButton(ns("run_test"),
                          "Jalankan Uji ANOVA",
                          class = "btn-warning",
                          style = "width: 90%; display: block; margin: auto; margin-bottom: 15px; font-size:18px; text-align: center;"),
             
             hr(),
             h5("Download:"),
             downloadButton(ns("download_results"), "Hasil Uji",
                            class = "btn-warning",
                            style = "width: 90%; display: block; margin: auto; margin-bottom: 7px; text-align: center;"),
             br(),
             downloadButton(ns("download_plot"), "Plot Asumsi",
                            class = "btn-warning",
                            style = "width: 90%; display: block; margin: auto; margin-bottom: 7px; text-align: center;"),
             br(),
             downloadButton(ns("download_report"), "Laporan",
                            class = "btn-warning",
                            style = "width: 90%; display: block; margin: auto; text-align: center;")
           )
    ),
    
    # Hasil
    column(8,
           box(
             title = "Hasil Uji ANOVA",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           box(
             title = "Visualisasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("test_plot"), height = "400px"))
           ),
           
           # Post-hoc results (conditional)
           conditionalPanel(
             condition = "output.show_posthoc == true",
             ns = ns,
             box(
               title = "Uji Post-hoc (Tukey HSD)", # Judul post-hoc diperbarui
               status = "info",
               solidHeader = TRUE,
               width = 12,
               collapsible = TRUE,
               withSpinner(tableOutput(ns("posthoc_results")))
             )
           )
    ),
    
    # Interpretasi
    column(12,
           box(
             title = "Interpretasi Uji ANOVA",
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
# UJI ANOVA - SERVER FUNCTION
# ==========================================

ujiAnovaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      # Filter categorical vars yang valid untuk ANOVA (minimal 2 kelompok, maksimal 20)
      valid_factors <- categorical_vars[
        sapply(categorical_vars, function(var) {
          if (!var %in% names(values$current_data)) return(FALSE)
          
          groups <- unique(na.omit(values$current_data[[var]]))
          group_sizes <- table(values$current_data[[var]])
          min_size <- min(group_sizes)
          
          return(length(groups) >= 2 && length(groups) <= 20 && min_size >= 2)
        })
      ]
      
      updateSelectInput(session, "dependent_variable",
                        choices = setNames(numeric_vars, numeric_vars))
      
      if (length(valid_factors) > 0) {
        # Tambahkan info jumlah kelompok di label
        factor_labels <- sapply(valid_factors, function(var) {
          n_groups <- length(unique(values$current_data[[var]]))
          paste0(var, " (", n_groups, " kelompok)")
        })
        
        updateSelectInput(session, "factor_variable",
                          choices = setNames(c("", valid_factors),
                                             c("-- Pilih Faktor --", factor_labels)))
        updateSelectInput(session, "factor1",
                          choices = setNames(c("", valid_factors),
                                             c("-- Pilih Faktor 1 --", factor_labels)))
        updateSelectInput(session, "factor2",
                          choices = setNames(c("", valid_factors),
                                             c("-- Pilih Faktor 2 --", factor_labels)))
      } else {
        updateSelectInput(session, "factor_variable",
                          choices = list("-- Tidak ada variabel faktor yang valid --" = ""))
        updateSelectInput(session, "factor1",
                          choices = list("-- Tidak ada variabel faktor yang valid --" = ""))
        updateSelectInput(session, "factor2",
                          choices = list("-- Tidak ada variabel faktor yang valid --" = ""))
      }
    })
    
    # Nilai reaktif untuk hasil
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      posthoc = NULL,
      show_posthoc = FALSE
    )
    
    # Control untuk menampilkan post-hoc
    output$show_posthoc <- reactive({
      test_results$show_posthoc
    })
    outputOptions(output, "show_posthoc", suspendWhenHidden = FALSE)
    
    # Jalankan uji
    observeEvent(input$run_test, {
      req(input$dependent_variable)
      
      data <- values$current_data
      
      if (input$test_type == "one_way") {
        req(input$factor_variable)
        
        if (input$factor_variable == "") {
          show_notification("Silakan pilih variabel faktor", type = "warning")
          return()
        }
        
        # Validasi data
        complete_data <- data[complete.cases(data[[input$dependent_variable]],
                                             data[[input$factor_variable]]), ]
        
        if (nrow(complete_data) < 6) {
          show_notification("Data tidak cukup untuk ANOVA (minimal 6 observasi)", type = "error")
          return()
        }
        
        groups <- unique(complete_data[[input$factor_variable]])
        if (length(groups) < 2) {
          show_notification("Minimal harus ada 2 kelompok untuk ANOVA", type = "error")
          return()
        }
        
        # Cek ukuran setiap kelompok
        group_sizes <- table(complete_data[[input$factor_variable]])
        if (any(group_sizes < 2)) {
          show_notification("Setiap kelompok harus memiliki minimal 2 observasi", type = "error")
          return()
        }
        
        # ANOVA Satu Arah
        formula_str <- paste(input$dependent_variable, "~", input$factor_variable)
        anova_model <- aov(as.formula(formula_str), data = complete_data)
        anova_summary <- summary(anova_model)
        test_results$results <- anova_summary
        
        # Plot ANOVA
        test_results$plot <- ggplot(complete_data, aes(x = .data[[input$factor_variable]],
                                                       y = .data[[input$dependent_variable]])) +
          geom_boxplot(aes(fill = .data[[input$factor_variable]]), alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
          stat_summary(fun = mean, geom = "point", color = "red", size = 3, shape = 18) +
          stat_summary(fun = mean, geom = "text",
                       aes(label = paste("μ =", round(..y.., 2))),
                       vjust = -0.5, color = "red", size = 3) +
          scale_fill_brewer(palette = "Set3") +
          labs(title = paste("ANOVA Satu Arah:", input$dependent_variable, "berdasarkan", input$factor_variable),
               subtitle = paste("Jumlah kelompok:", length(groups), "| Titik merah = rata-rata kelompok"),
               x = input$factor_variable, y = input$dependent_variable) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none")
        
        # Post-hoc test jika signifikan (hanya Tukey HSD)
        test_results$show_posthoc <- FALSE # Reset status post-hoc
        test_results$posthoc <- NULL # Clear previous posthoc results
        
        if (length(anova_summary) > 0 && "Pr(>F)" %in% names(anova_summary[[1]])) {
          p_value <- anova_summary[[1]][["Pr(>F)"]][1]
          if (!is.na(p_value) && p_value < input$alpha) {
            test_results$show_posthoc <- TRUE
            
            # Langsung panggil TukeyHSD
            posthoc_result <- TukeyHSD(anova_model)
            posthoc_table <- as.data.frame(posthoc_result[[input$factor_variable]])
            posthoc_table$Comparison <- rownames(posthoc_table)
            posthoc_table <- posthoc_table[, c("Comparison", "diff", "lwr", "upr", "p adj")]
            names(posthoc_table) <- c("Perbandingan", "Perbedaan", "Batas_Bawah", "Batas_Atas", "P_adjusted")
            
            # Tambahkan kolom signifikansi
            posthoc_table$Signifikan <- ifelse(posthoc_table[["P_adjusted"]] < input$alpha, "Ya", "Tidak")
            posthoc_table[["P_adjusted"]] <- round(posthoc_table[["P_adjusted"]], 4)
            
            test_results$posthoc <- posthoc_table
          }
        }
        
        # Interpretasi ANOVA Satu Arah
        if (length(anova_summary) > 0 && all(c("F value", "Df", "Pr(>F)") %in% names(anova_summary[[1]]))) {
          f_value <- anova_summary[[1]][["F value"]][1]
          df1 <- anova_summary[[1]][["Df"]][1]
          df2 <- anova_summary[[1]][["Df"]][2]
          p_value <- anova_summary[[1]][["Pr(>F)"]][1]
          
          # Hitung statistik deskriptif per kelompok
          group_stats <- complete_data %>%
            group_by(.data[[input$factor_variable]]) %>%
            summarise(
              n = n(),
              mean = round(mean(.data[[input$dependent_variable]], na.rm = TRUE), 3),
              sd = round(sd(.data[[input$dependent_variable]], na.rm = TRUE), 3),
              .groups = 'drop'
            )
          
          test_results$interpretation <- paste(
            "INTERPRETASI ANOVA SATU ARAH:\n\n",
            "VARIABEL YANG DIANALISIS:\n",
            "- Variabel dependen:", input$dependent_variable, "\n",
            "- Variabel faktor:", input$factor_variable, "\n",
            "- Jumlah kelompok:", length(groups), "\n",
            "- Total observasi:", nrow(complete_data), "\n\n",
            "STATISTIK DESKRIPTIF PER KELOMPOK:\n",
            paste(capture.output(print(group_stats, row.names = FALSE)), collapse = "\n"), "\n\n",
            "HIPOTESIS:\n",
            "- H₀: μ₁ = μ₂ = μ₃ = ... (semua rata-rata kelompok sama)\n",
            "- H₁: Minimal ada satu rata-rata kelompok yang berbeda\n\n",
            "HASIL UJI ANOVA:\n",
            "- F-statistik =", round(f_value, 4), "\n",
            "- df₁ (between) =", df1, "\n",
            "- df₂ (within) =", df2, "\n",
            "- p-value =", format_p_value(p_value), "\n\n",
            "KESIMPULAN (α =", input$alpha, "):\n",
            if (p_value < input$alpha) {
              paste("TOLAK H₀. Ada perbedaan yang signifikan antar kelompok",
                    "(p-value =", format_p_value(p_value), "< α =", input$alpha, ")")
            } else {
              paste("GAGAL TOLAK H₀. Tidak ada perbedaan yang signifikan antar kelompok",
                    "(p-value =", format_p_value(p_value), ">= α =", input$alpha, ")")
            }, "\n\n",
            "INTERPRETASI PRAKTIS:\n",
            if (p_value < input$alpha) {
              paste("Terdapat perbedaan yang bermakna pada", input$dependent_variable,
                    "berdasarkan", input$factor_variable, ".",
                    "Lihat hasil uji Post-hoc untuk mengetahui kelompok mana yang berbeda secara spesifik.")
            } else {
              paste("Semua kelompok", input$factor_variable, "memiliki rata-rata", input$dependent_variable,
                    "yang secara statistik tidak berbeda. Variasi yang diamati bisa disebabkan oleh faktor acak.")
            }, "\n\n",
            if (test_results$show_posthoc) {
              paste("UJI POST-HOC:\n",
                    "Karena ANOVA signifikan, dilakukan uji post-hoc (Tukey HSD) untuk mengetahui kelompok mana yang berbeda secara spesifik.")
            } else {
              "UJI POST-HOC:\nTidak dilakukan karena ANOVA tidak signifikan."
            }
          )
        }
        
      } else if (input$test_type == "two_way") {
        req(input$factor1, input$factor2)
        
        if (input$factor1 == "" || input$factor2 == "") {
          show_notification("Silakan pilih kedua faktor", type = "warning")
          return()
        }
        
        if (input$factor1 == input$factor2) {
          show_notification("Pilih dua faktor yang berbeda", type = "warning")
          return()
        }
        
        # Validasi data
        complete_data <- data[complete.cases(data[[input$dependent_variable]],
                                             data[[input$factor1]],
                                             data[[input$factor2]]), ]
        
        if (nrow(complete_data) < 12) {
          show_notification("Data tidak cukup untuk ANOVA dua arah (minimal 12 observasi)", type = "error")
          return()
        }
        
        # Cek kombinasi faktor
        factor_combinations <- table(complete_data[[input$factor1]], complete_data[[input$factor2]])
        if (any(factor_combinations == 0)) {
          show_notification("Beberapa kombinasi faktor tidak memiliki data. ANOVA dua arah memerlukan data di semua kombinasi.",
                            type = "warning")
        }
        
        # ANOVA Dua Arah
        if (input$include_interaction) {
          formula_str <- paste(input$dependent_variable, "~", input$factor1, "*", input$factor2)
        } else {
          formula_str <- paste(input$dependent_variable, "~", input$factor1, "+", input$factor2)
        }
        
        anova_model <- aov(as.formula(formula_str), data = complete_data)
        anova_summary <- summary(anova_model)
        test_results$results <- anova_summary
        
        # Plot ANOVA Dua Arah (Interaction Plot)
        if (input$include_interaction) {
          # Hitung rata-rata per kombinasi
          interaction_data <- complete_data %>%
            group_by(.data[[input$factor1]], .data[[input$factor2]]) %>%
            summarise(
              mean_val = mean(.data[[input$dependent_variable]], na.rm = TRUE),
              se = sd(.data[[input$dependent_variable]], na.rm = TRUE) / sqrt(n()),
              .groups = 'drop'
            )
          
          test_results$plot <- ggplot(interaction_data, aes(x = .data[[input$factor1]],
                                                            y = mean_val,
                                                            color = .data[[input$factor2]],
                                                            group = .data[[input$factor2]])) +
            geom_line(linewidth = 1.2) +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                          width = 0.1, alpha = 0.7) +
            scale_color_brewer(palette = "Set2") +
            labs(title = paste("Plot Interaksi ANOVA Dua Arah:", input$dependent_variable),
                 subtitle = paste("Faktor 1:", input$factor1, "| Faktor 2:", input$factor2),
                 x = input$factor1, y = paste("Rata-rata", input$dependent_variable),
                 color = input$factor2) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
          # Boxplot untuk main effects
          test_results$plot <- ggplot(complete_data, aes(x = .data[[input$factor1]],
                                                         y = .data[[input$dependent_variable]])) +
            geom_boxplot(aes(fill = .data[[input$factor2]]), alpha = 0.7) +
            scale_fill_brewer(palette = "Set3") +
            labs(title = paste("ANOVA Dua Arah:", input$dependent_variable),
                 subtitle = paste("Faktor 1:", input$factor1, "| Faktor 2:", input$factor2),
                 x = input$factor1, y = input$dependent_variable,
                 fill = input$factor2) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
        # Interpretasi ANOVA Dua Arah
        if (length(anova_summary) > 0) {
          anova_table <- anova_summary[[1]]
          
          # Extract hasil untuk setiap efek
          effects <- rownames(anova_table)[1:(nrow(anova_table)-1)] # exclude Residuals
          
          interpretation_text <- paste(
            "INTERPRETASI ANOVA DUA ARAH:\n\n",
            "VARIABEL YANG DIANALISIS:\n",
            "- Variabel dependen:", input$dependent_variable, "\n",
            "- Faktor 1:", input$factor1, "\n",
            "- Faktor 2:", input$factor2, "\n",
            "- Interaksi:", ifelse(input$include_interaction, "Ya", "Tidak"), "\n",
            "- Total observasi:", nrow(complete_data), "\n\n",
            "HIPOTESIS:\n"
          )
          
          for (i in 1:length(effects)) {
            effect_name <- effects[i]
            f_val <- anova_table[["F value"]][i]
            p_val <- anova_table[["Pr(>F)"]][i]
            
            if (grepl(":", effect_name)) {
              interpretation_text <- paste(interpretation_text,
                                           "- H₀ (Interaksi): Tidak ada efek interaksi antara faktor\n",
                                           "- H₁ (Interaksi): Ada efek interaksi antara faktor\n"
              )
            } else {
              interpretation_text <- paste(interpretation_text,
                                           "- H₀ (", effect_name, "): Tidak ada efek utama dari", effect_name, "\n",
                                           "- H₁ (", effect_name, "): Ada efek utama dari", effect_name, "\n"
              )
            }
          }
          
          interpretation_text <- paste(interpretation_text, "\nHASIL UJI ANOVA:\n")
          
          for (i in 1:length(effects)) {
            effect_name <- effects[i]
            f_val <- anova_table[["F value"]][i]
            df1 <- anova_table[["Df"]][i]
            p_val <- anova_table[["Pr(>F)"]][i]
            
            interpretation_text <- paste(interpretation_text,
                                         "- ", effect_name, ":\n",
                                         "  * F =", round(f_val, 4), ", df =", df1,
                                         ", p-value =", format_p_value(p_val), "\n",
                                         "  * Kesimpulan:",
                                         ifelse(p_val < input$alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), "\n"
            )
          }
          
          interpretation_text <- paste(interpretation_text,
                                       "\nINTERPRETASI PRAKTIS:\n"
          )
          
          for (i in 1:length(effects)) {
            effect_name <- effects[i]
            p_val <- anova_table[["Pr(>F)"]][i]
            
            if (grepl(":", effect_name)) {
              if (p_val < input$alpha) {
                interpretation_text <- paste(interpretation_text,
                                             "- Efek interaksi SIGNIFIKAN: Efek salah satu faktor bergantung pada level faktor lainnya.\n"
                )
              } else {
                interpretation_text <- paste(interpretation_text,
                                             "- Efek interaksi TIDAK SIGNIFIKAN: Kedua faktor bekerja secara independen.\n"
                )
              }
            } else {
              if (p_val < input$alpha) {
                interpretation_text <- paste(interpretation_text,
                                             "- Efek utama", effect_name, "SIGNIFIKAN: Ada perbedaan rata-rata antar level", effect_name, ".\n"
                )
              } else {
                interpretation_text <- paste(interpretation_text,
                                             "- Efek utama", effect_name, "TIDAK SIGNIFIKAN: Tidak ada perbedaan rata-rata antar level", effect_name, ".\n"
                )
              }
            }
          }
          
          test_results$interpretation <- interpretation_text
        }
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
    
    output$posthoc_results <- renderTable({
      req(test_results$posthoc)
      test_results$posthoc
    })
    
    # Download handlers
    output$download_results <- downloadHandler(
      filename = function() paste("uji_anova_hasil_", Sys.Date(), ".txt", sep=""),
      content = function(file) {
        tryCatch({ # Tambah tryCatch
          req(test_results$results)
          writeLines(capture.output(print(test_results$results)), file)
          show_notification("Hasil uji berhasil diunduh!", type = "success") # Menggunakan show_notification
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh hasil uji:", e$message), type = "error") # Menggunakan show_notification
        })
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() paste("uji_anova_plot_", Sys.Date(), ".png", sep=""),
      content = function(file) {
        tryCatch({ # Tambah tryCatch
          req(test_results$plot)
          ggsave(file, test_results$plot, width = 12, height = 8, dpi = 300)
          show_notification("Plot berhasil diunduh!", type = "success") # Menggunakan show_notification
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh plot:", e$message), type = "error") # Menggunakan show_notification
        })
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() paste("uji_anova_laporan_", Sys.Date(), ".docx", sep=""),
      content = function(file) {
        tryCatch({ # Tambah tryCatch
          req(test_results$interpretation, test_results$results)
          # test_results$plot dan test_results$posthoc tidak di-req di sini karena bisa null, dicheck terpisah
          
          doc <- officer::read_docx()
          doc <- doc %>%
            officer::body_add_par("Laporan Uji ANOVA", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Waktu:", format(Sys.time(), "%H:%M:%S"))) %>% # Format waktu
            officer::body_add_par(paste("Variabel Dependen:", input$dependent_variable)) %>%
            officer::body_add_par(paste("Jenis ANOVA:", switch(input$test_type,
                                                               "one_way" = "ANOVA Satu Arah",
                                                               "two_way" = "ANOVA Dua Arah")))
          
          if (input$test_type == "one_way") {
            doc <- doc %>% officer::body_add_par(paste("Variabel Faktor:", input$factor_variable))
          } else { # two_way
            doc <- doc %>%
              officer::body_add_par(paste("Faktor 1:", input$factor1)) %>%
              officer::body_add_par(paste("Faktor 2:", input$factor2)) %>%
              officer::body_add_par(paste("Efek Interaksi:", ifelse(input$include_interaction, "Ya", "Tidak")))
          }
          
          doc <- doc %>%
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
              officer::body_add_par(" ") %>%
              officer::body_add_par("Visualisasi:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 8, height = 5)
            unlink(temp_file)
          }
          
          # Tambahkan hasil Post-hoc jika ada
          if (!is.null(test_results$posthoc) && test_results$show_posthoc) {
            doc <- doc %>%
              officer::body_add_par(" ") %>%
              officer::body_add_par("Uji Post-hoc:", style = "heading 2") %>%
              # Pastikan Officer menerima data.frame dengan nama baris sebagai kolom pertama jika ada
              officer::body_add_table(test_results$posthoc, style = "Table Grid")
          }
          
          # Footer
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