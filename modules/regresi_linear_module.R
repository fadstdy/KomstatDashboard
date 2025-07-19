# Regresi Linear Module 
# modules/regresi_linear_module.R

# UI function for Regresi Linear
regresiLinearUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol Regresi Linear
    column(3,
           box(
             title = "Panel Kontrol Regresi Linear",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Pemilihan Variabel
             selectInput(ns("dependent_variable"), 
                         "Variabel Dependen (Y):",
                         choices = NULL),
             
             selectInput(ns("independent_variables"), 
                         "Variabel Independen (X):",
                         choices = NULL,
                         multiple = TRUE),
             helpText("Pilih minimal 1 variabel independen. Untuk model yang baik, disarankan 2-5 variabel."),
             
             # Opsi Model
             hr(),
             h5("Opsi Model:"),
             checkboxInput(ns("include_interactions"), 
                           "Sertakan Interaksi Antar Variabel", 
                           value = FALSE),
             helpText("Menambahkan efek interaksi untuk variabel independen (hanya jika ≥2 variabel)"),
             
             checkboxInput(ns("standardize"), 
                           "Standardisasi Variabel Independen", 
                           value = FALSE),
             helpText("Mengubah variabel ke skala yang sama (z-score) untuk interpretasi koefisien"),
             
             # Tingkat Kepercayaan
             numericInput(ns("conf_level"), 
                          "Tingkat Kepercayaan Interval (%):",
                          value = 95,
                          min = 80,
                          max = 99,
                          step = 1),
             
             # Tombol Aksi
             br(),
             actionButton(ns("run_regression"), 
                          "Jalankan Analisis Regresi", 
                          class = "btn-primary btn-lg"),
             
             hr(),
             h5("Download Hasil:"),
             downloadButton(ns("download_summary"), 
                            "Summary Model", 
                            class = "btn-sm btn-info"),
             br(), br(),
             downloadButton(ns("download_plots"), 
                            "Plot Diagnostik", 
                            class = "btn-sm btn-info"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Laporan Lengkap", 
                            class = "btn-sm btn-success")
           )
    ),
    
    # Konten Utama
    column(9,
           # Info Panel (sebelum analisis)
           conditionalPanel(
             condition = "output.show_initial_info == true",
             ns = ns,
             box(
               title = "Petunjuk Analisis Regresi Linear Berganda",
               status = "info",
               solidHeader = TRUE,
               width = 12,
               div(
                 style = "padding: 20px; text-align: center;",
                 h4("Siap untuk Analisis Regresi Linear"),
                 p("Pilih variabel dependen dan independen di panel kiri, lalu klik tombol 'Jalankan Analisis Regresi' untuk memulai."),
                 hr(),
                 h5("Yang akan dihasilkan:"),
                 tags$ul(
                   style = "text-align: left; display: inline-block;",
                   tags$li("Ringkasan model dengan R-squared dan F-statistik"),
                   tags$li("Koefisien regresi dengan interval kepercayaan"),
                   tags$li("Uji asumsi regresi (normalitas, multikolinearitas, heteroskedastisitas)"),
                   tags$li("Plot diagnostik untuk validasi model"),
                   tags$li("Interpretasi lengkap hasil analisis")
                 )
               )
             )
           ),
           
           # Ringkasan Model Regresi (setelah analisis)
           conditionalPanel(
             condition = "output.show_results == true",
             ns = ns,
             box(
               title = "Ringkasan Model Regresi Linear",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               withSpinner(verbatimTextOutput(ns("regression_summary")))
             )
           ),
           
           # Uji Asumsi (setelah analisis)
           conditionalPanel(
             condition = "output.show_results == true",
             ns = ns,
             box(
               title = "Hasil Uji Asumsi Regresi",
               status = "warning",
               solidHeader = TRUE,
               width = 12,
               collapsible = TRUE,
               tabsetPanel(
                 tabPanel("Normalitas Residual",
                          br(),
                          fluidRow(
                            column(6, 
                                   h5("Histogram Residual"),
                                   withSpinner(plotOutput(ns("residual_histogram"), height = "300px"))),
                            column(6,
                                   h5("Q-Q Plot Residual"),
                                   withSpinner(plotOutput(ns("residual_qqplot"), height = "300px")))
                          ),
                          br(),
                          h5("Uji Shapiro-Wilk Residual"),
                          withSpinner(verbatimTextOutput(ns("shapiro_test_result")))
                 ),
                 tabPanel("Multikolinearitas (VIF)",
                          br(),
                          h5("Variance Inflation Factor (VIF)"),
                          withSpinner(DT::dataTableOutput(ns("vif_table"))),
                          br(),
                          div(
                            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                            h6("Interpretasi VIF:"),
                            tags$ul(
                              tags$li("VIF < 5: Tidak ada multikolinearitas yang signifikan"),
                              tags$li("VIF 5-10: Multikolinearitas moderat, perlu perhatian"),
                              tags$li("VIF > 10: Multikolinearitas tinggi, pertimbangkan menghapus variabel")
                            )
                          )
                 ),
                 tabPanel("Heteroskedastisitas", 
                          br(),
                          h5("Uji Breusch-Pagan"),
                          withSpinner(verbatimTextOutput(ns("bp_test_result"))),
                          br(),
                          p("Tip: Periksa juga 'Plot Residual vs Fitted' di tab Diagnostik Visual untuk inspeksi visual heteroskedastisitas.")
                 )
               )
             )
           ),
           
           # Plot Diagnostik Visual (setelah analisis)
           conditionalPanel(
             condition = "output.show_results == true",
             ns = ns,
             box(
               title = "Diagnostik Visual Model", 
               status = "success",
               solidHeader = TRUE,
               width = 12,
               collapsible = TRUE,
               tabsetPanel(
                 tabPanel("Residual vs Fitted", 
                          br(),
                          withSpinner(plotOutput(ns("residuals_vs_fitted"), height = "400px")),
                          p("Plot ini menguji homoskedastisitas. Pola acak menunjukkan asumsi terpenuhi.")),
                 tabPanel("Scale-Location Plot",
                          br(),
                          withSpinner(plotOutput(ns("scale_location"), height = "400px")),
                          p("Plot ini menguji homoskedastisitas. Garis horizontal menunjukkan varians konstan.")),
                 tabPanel("Cook's Distance",
                          br(),
                          withSpinner(plotOutput(ns("cooks_distance"), height = "400px")),
                          p("Mengidentifikasi observasi berpengaruh. Nilai > 0.5 perlu diperhatikan.")),
                 tabPanel("Leverage vs Residuals",
                          br(),
                          withSpinner(plotOutput(ns("leverage_plot"), height = "400px")),
                          p("Mengidentifikasi outlier dan observasi berpengaruh tinggi."))
               )
             )
           )
    ),
    
    # Bagian Interpretasi Model (setelah analisis)
    conditionalPanel(
      condition = "output.show_results == true",
      ns = ns,
      column(12,
             box(
               title = "Interpretasi Hasil Regresi",
               status = "warning",
               solidHeader = TRUE,
               width = 12,
               collapsible = TRUE,
               withSpinner(verbatimTextOutput(ns("regression_interpretation")))
             )
      )
    ),
    
    # Bagian Interpretasi Uji Asumsi (setelah analisis)
    conditionalPanel(
      condition = "output.show_results == true",
      ns = ns,
      column(12,
             box(
               title = "Interpretasi Uji Asumsi",
               status = "warning",
               solidHeader = TRUE,
               width = 12,
               collapsible = TRUE,
               withSpinner(verbatimTextOutput(ns("assumption_interpretation")))
             )
      )
    )
  )
}

# Fungsi Server untuk Regresi Linear
regresiLinearServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Nilai Reaktif untuk Hasil Regresi
    regression_results <- reactiveValues(
      model = NULL,
      summary = NULL,
      interpretation = NULL,
      residuals = NULL,
      shapiro_test = NULL,
      vif = NULL,
      bptest_result = NULL, 
      assumption_interpretation = NULL,
      residual_hist_plot = NULL,
      residual_qq_plot = NULL
    )
    
    # Perbarui Pilihan Variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      
      updateSelectInput(session, "dependent_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "independent_variables", 
                        choices = setNames(numeric_vars, numeric_vars))
    })
    
    # Control untuk conditional panels
    output$show_initial_info <- reactive({
      is.null(regression_results$model)
    })
    
    output$show_results <- reactive({
      !is.null(regression_results$model)
    })
    
    outputOptions(output, "show_initial_info", suspendWhenHidden = FALSE)
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    
    # Jalankan Regresi
    observeEvent(input$run_regression, {
      req(input$dependent_variable)
      req(input$independent_variables)
      
      if (length(input$independent_variables) == 0) {
        showNotification("Silakan pilih minimal 1 variabel independen", type = "warning")
        return()
      }
      
      if (input$dependent_variable %in% input$independent_variables) {
        showNotification("Variabel dependen tidak boleh sama dengan variabel independen", type = "error")
        return()
      }
      
      data <- values$current_data
      
      # Cek data yang lengkap
      vars_used <- c(input$dependent_variable, input$independent_variables)
      complete_data <- data[complete.cases(data[vars_used]), ]
      
      if (nrow(complete_data) < (length(input$independent_variables) + 5)) {
        showNotification(paste("Data tidak cukup untuk analisis regresi. Diperlukan minimal", 
                               length(input$independent_variables) + 5, "observasi lengkap."), 
                         type = "error")
        return()
      }
      
      # Standardisasi data jika dicentang
      if (input$standardize) {
        data_to_standardize <- complete_data %>%
          select(all_of(input$independent_variables)) %>%
          mutate_all(~scale(.)[,1]) 
        
        data_model <- complete_data %>%
          select(-all_of(input$independent_variables)) %>% 
          bind_cols(data_to_standardize)
        
      } else {
        data_model <- complete_data
      }
      
      # Buat formula regresi
      if (input$include_interactions && length(input$independent_variables) >= 2) {
        # Tambahkan interaksi untuk semua pasangan variabel
        interaction_terms <- combn(input$independent_variables, 2, simplify = FALSE)
        interaction_formula_parts <- sapply(interaction_terms, function(pair) paste(pair, collapse = "*"))
        formula_str <- paste(input$dependent_variable, "~", 
                             paste(c(input$independent_variables, interaction_formula_parts), collapse = "+"))
      } else {
        formula_str <- paste(input$dependent_variable, "~", 
                             paste(input$independent_variables, collapse = "+"))
      }
      
      # Fit model regresi
      tryCatch({
        # Suppress warnings untuk menghindari notifikasi error yang tidak perlu
        options(warn = -1)
        model_results <- lm(as.formula(formula_str), data = data_model)
        regression_results$model <- model_results
        regression_results$summary <- summary(model_results)
        
        # Hitung residual
        residuals_val <- residuals(model_results)
        regression_results$residuals <- residuals_val
        
        # --- Uji Asumsi ---
        
        # 1. Normalitas Residual (Shapiro-Wilk)
        if (length(residuals_val) > 3 && length(residuals_val) <= 5000 && 
            length(unique(na.omit(residuals_val))) > 1 && !any(is.na(residuals_val))) { 
          regression_results$shapiro_test <- shapiro.test(residuals_val)
        } else {
          regression_results$shapiro_test <- "Tidak dapat menjalankan uji Shapiro-Wilk (ukuran sampel tidak sesuai atau data residual tidak bervariasi)."
        }
        
        # 2. Multikolinearitas (VIF)
        if (length(input$independent_variables) > 1) {
          if (requireNamespace("car", quietly = TRUE)) {
            tryCatch({
              vif_values <- car::vif(model_results)
              if (is.matrix(vif_values)) {
                # Untuk model dengan interaksi, VIF bisa berupa matrix
                vif_df <- data.frame(
                  Variabel = rownames(vif_values),
                  VIF = vif_values[, 1]
                )
              } else {
                vif_df <- data.frame(
                  Variabel = names(vif_values),
                  VIF = as.numeric(vif_values)
                )
              }
              vif_df$VIF <- round(vif_df$VIF, 3)
              vif_df$Status <- ifelse(vif_df$VIF > 10, "Tinggi",
                                      ifelse(vif_df$VIF > 5, "Moderat", "Rendah"))
              regression_results$vif <- vif_df
            }, error = function(e) {
              regression_results$vif <- paste("Error menghitung VIF:", e$message)
            })
          } else {
            regression_results$vif <- "Paket 'car' diperlukan untuk VIF. Mohon instal: install.packages('car')"
          }
        } else {
          regression_results$vif <- data.frame(
            Info = "VIF hanya relevan untuk model dengan lebih dari satu variabel independen."
          )
        }
        
        # 3. Heteroskedastisitas (Breusch-Pagan Test only)
        if (requireNamespace("lmtest", quietly = TRUE)) {
          tryCatch({
            regression_results$bptest_result <- lmtest::bptest(model_results)
          }, error = function(e) {
            regression_results$bptest_result <- paste("Error menghitung Uji Breusch-Pagan:", e$message)
          })
        } else {
          regression_results$bptest_result <- "Paket 'lmtest' diperlukan untuk Uji Breusch-Pagan."
        }
        
        # --- Plot untuk Uji Asumsi ---
        # Histogram Residual
        residual_hist_data <- data.frame(Residuals = residuals_val)
        regression_results$residual_hist_plot <- ggplot(residual_hist_data, aes(x = Residuals)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
          geom_density(color = "red", linewidth = 1) +
          labs(title = "Histogram Residual", 
               subtitle = "Garis merah = kurva densitas",
               x = "Residual", y = "Densitas") +
          theme_custom()
        
        # Q-Q Plot Residual
        qq_data <- as.data.frame(qqnorm(residuals_val, plot.it = FALSE))
        regression_results$residual_qq_plot <- ggplot(qq_data, aes(x = x, y = y)) +
          geom_point(alpha = 0.6) +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) + 
          labs(title = "Q-Q Plot Residual", 
               subtitle = "Garis merah = distribusi normal teoritis",
               x = "Kuartil Teoritis", y = "Kuartil Sampel") +
          theme_custom()
        
        # --- Interpretasi Hasil Regresi ---
        regression_results$interpretation <- interpret_regression_summary(regression_results$summary, input$standardize)
        
        # --- Interpretasi Uji Asumsi ---
        assumption_interpret_text <- create_assumption_interpretation(
          regression_results$shapiro_test,
          regression_results$vif,
          regression_results$bptest_result
        )
        
        regression_results$assumption_interpretation <- assumption_interpret_text
        
        # Restore warning options
        options(warn = 0)
        
        showNotification("Analisis regresi berhasil!", type = "success")
        
      }, error = function(e) {
        # Restore warning options even if error occurs
        options(warn = 0)
        showNotification(paste("Error dalam analisis regresi:", e$message), type = "error")
      })
    })
    
    # Tampilkan output
    output$regression_summary <- renderPrint({
      req(regression_results$summary)
      regression_results$summary
    })
    
    # Output plot uji asumsi
    output$residual_histogram <- renderPlot({
      req(regression_results$residual_hist_plot)
      regression_results$residual_hist_plot
    })
    
    output$residual_qqplot <- renderPlot({
      req(regression_results$residual_qq_plot)
      regression_results$residual_qq_plot
    })
    
    output$shapiro_test_result <- renderPrint({
      req(regression_results$shapiro_test)
      regression_results$shapiro_test
    })
    
    output$vif_table <- DT::renderDataTable({
      req(regression_results$vif)
      if (is.data.frame(regression_results$vif)) {
        DT::datatable(
          regression_results$vif,
          options = list(
            pageLength = 10,
            dom = 't'
          ),
          rownames = FALSE
        )
      } else {
        data.frame(Info = regression_results$vif)
      }
    })
    
    # Output uji heteroskedastisitas
    output$bp_test_result <- renderPrint({
      req(regression_results$bptest_result)
      regression_results$bptest_result
    })
    
    # Tampilkan plot diagnostik model (plot dasar R)
    output$residuals_vs_fitted <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 1, 
           main = "Residuals vs Fitted Values",
           sub = "Pola acak menunjukkan homoskedastisitas") 
    })
    
    output$scale_location <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 3,
           main = "Scale-Location Plot",
           sub = "Garis horizontal menunjukkan varians konstan") 
    })
    
    output$cooks_distance <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 4,
           main = "Cook's Distance",
           sub = "Nilai > 0.5 menunjukkan observasi berpengaruh") 
    })
    
    output$leverage_plot <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 5,
           main = "Residuals vs Leverage",
           sub = "Identifikasi outlier dan observasi berpengaruh") 
    })
    
    # Tampilkan interpretasi
    output$regression_interpretation <- renderText({
      req(regression_results$interpretation)
      regression_results$interpretation
    })
    
    # Tampilkan interpretasi asumsi
    output$assumption_interpretation <- renderText({
      req(regression_results$assumption_interpretation)
      regression_results$assumption_interpretation
    })
    
    # Handler Download
    output$download_summary <- downloadHandler(
      filename = function() { paste("regresi_ringkasan_", Sys.Date(), ".txt", sep="") },
      content = function(file) {
        if (!is.null(regression_results$summary)) {
          writeLines(capture.output(print(regression_results$summary)), file)
        }
      }
    )
    
    output$download_plots <- downloadHandler(
      filename = function() { paste("regresi_plot_diagnostik_", Sys.Date(), ".png", sep="") },
      content = function(file) {
        req(regression_results$residual_hist_plot, regression_results$residual_qq_plot)
        
        # Gabungkan plot residual
        combined_plot <- gridExtra::grid.arrange(
          regression_results$residual_hist_plot,
          regression_results$residual_qq_plot,
          ncol = 2
        )
        ggsave(file, combined_plot, width = 12, height = 6, dpi = 300)
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() { paste("regresi_laporan_lengkap_", Sys.Date(), ".docx", sep="") },
      content = function(file) {
        if (!is.null(regression_results$interpretation) && !is.null(regression_results$assumption_interpretation)) {
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Analisis Regresi Linear Berganda", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel Dependen:", input$dependent_variable)) %>%
            officer::body_add_par(paste("Variabel Independen:", paste(input$independent_variables, collapse = ", "))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Ringkasan Model:", style = "heading 2") %>%
            officer::body_add_par(capture.output(print(regression_results$summary))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi Model:", style = "heading 2") %>%
            officer::body_add_par(regression_results$interpretation) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil dan Interpretasi Uji Asumsi:", style = "heading 2") %>%
            officer::body_add_par(regression_results$assumption_interpretation)
          
          # Tambah plot residual
          if (!is.null(regression_results$residual_hist_plot) && !is.null(regression_results$residual_qq_plot)) {
            temp_hist_file <- tempfile(fileext = ".png")
            ggsave(temp_hist_file, regression_results$residual_hist_plot, width = 6, height = 4, dpi = 300)
            
            temp_qq_file <- tempfile(fileext = ".png")
            ggsave(temp_qq_file, regression_results$residual_qq_plot, width = 6, height = 4, dpi = 300)
            
            doc <- doc %>%
              officer::body_add_par("Plot Diagnostik Residual:", style = "heading 3") %>%
              officer::body_add_img(temp_hist_file, width = 5, height = 3) %>%
              officer::body_add_img(temp_qq_file, width = 5, height = 3)
            
            unlink(temp_hist_file)
            unlink(temp_qq_file)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}

# Helper function untuk interpretasi regression summary
interpret_regression_summary <- function(summary_obj, is_standardized = FALSE) {
  if (is.null(summary_obj)) {
    return("Tidak ada ringkasan model untuk diinterpretasi.")
  }
  
  # Ekstrak informasi kunci
  r_squared <- summary_obj$r.squared
  adj_r_squared <- summary_obj$adj.r.squared
  f_statistic <- summary_obj$fstatistic[1]
  f_p_value <- pf(summary_obj$fstatistic[1], 
                  summary_obj$fstatistic[2], 
                  summary_obj$fstatistic[3], 
                  lower.tail = FALSE)
  
  coefficients <- summary_obj$coefficients
  
  interpretation <- paste(
    "INTERPRETASI HASIL REGRESI LINEAR BERGANDA:\n",
    "==========================================\n\n",
    "1. KUALITAS MODEL (GOODNESS OF FIT):\n",
    "- R-squared =", round(r_squared, 4), 
    paste0("(", round(r_squared * 100, 2), "% variabilitas Y dijelaskan oleh model)\n"),
    "- Adjusted R-squared =", round(adj_r_squared, 4), "\n",
    "- Interpretasi kualitas:", interpret_r_squared(r_squared), "\n\n",
    
    "2. UJI SIGNIFIKANSI MODEL KESELURUHAN (F-test):\n",
    "- F-statistik =", round(f_statistic, 4), "\n",
    "- p-value =", format_p_value(f_p_value), "\n",
    "- Kesimpulan:", 
    if (f_p_value < 0.05) {
      "Model secara keseluruhan SIGNIFIKAN (p < 0.05)"
    } else {
      "Model secara keseluruhan TIDAK SIGNIFIKAN (p >= 0.05)"
    }, "\n\n",
    
    "3. INTERPRETASI KOEFISIEN INDIVIDUAL:\n"
  )
  
  # Interpretasi koefisien individual
  for (i in 1:nrow(coefficients)) {
    var_name <- rownames(coefficients)[i]
    coef_value <- coefficients[i, "Estimate"]
    p_value <- coefficients[i, "Pr(>|t|)"]
    
    interpretation <- paste(interpretation,
                            "- ", var_name, ":\n",
                            "  Koefisien =", round(coef_value, 4), "\n",
                            "  p-value =", format_p_value(p_value), "\n",
                            "  Signifikansi:", 
                            if (p_value < 0.001) "*** (sangat signifikan)" 
                            else if (p_value < 0.01) "** (signifikan)"
                            else if (p_value < 0.05) "* (signifikan)"
                            else if (p_value < 0.1) ". (marginal)"
                            else "tidak signifikan", "\n",
                            "  Interpretasi:", 
                            if (var_name == "(Intercept)") {
                              paste("Nilai prediksi Y ketika semua variabel X = 0")
                            } else {
                              if (is_standardized) {
                                paste("Setiap peningkatan 1 standar deviasi", var_name, 
                                      ifelse(coef_value > 0, "meningkatkan", "menurunkan"),
                                      "Y sebesar", abs(round(coef_value, 4)), "standar deviasi")
                              } else {
                                paste("Setiap peningkatan 1 unit", var_name, 
                                      ifelse(coef_value > 0, "meningkatkan", "menurunkan"),
                                      "Y sebesar", abs(round(coef_value, 4)), "unit")
                              }
                            }, "\n\n"
    )
  }
  
  interpretation <- paste(interpretation,
                          "4. REKOMENDASI PRAKTIS:\n",
                          if (r_squared < 0.3) {
                            "- Model memiliki daya prediksi rendah. Pertimbangkan menambah variabel atau transformasi.\n"
                          } else if (r_squared > 0.8) {
                            "- Model memiliki daya prediksi tinggi. Periksa kemungkinan overfitting.\n"
                          } else {
                            "- Model memiliki daya prediksi yang wajar.\n"
                          },
                          "- Pastikan semua asumsi regresi terpenuhi (normalitas, homoskedastisitas, multikolinearitas).\n",
                          "- Lakukan validasi model dengan data terpisah jika memungkinkan.\n",
                          if (is_standardized) {
                            "- Koefisien dalam skala standar memudahkan perbandingan pengaruh relatif antar variabel.\n"
                          } else {
                            "- Untuk membandingkan pengaruh relatif antar variabel, pertimbangkan standardisasi.\n"
                          }
  )
  
  return(interpretation)
}

# Helper function untuk interpretasi asumsi - SIMPLIFIED (removed ncv_test)
create_assumption_interpretation <- function(shapiro_test, vif_results, bp_test) {
  interpretation <- "INTERPRETASI UJI ASUMSI REGRESI LINEAR:\n======================================\n\n"
  
  # 1. Normalitas Residual
  interpretation <- paste(interpretation, "1. NORMALITAS RESIDUAL:\n")
  if (is.character(shapiro_test)) {
    interpretation <- paste(interpretation, shapiro_test, "\n")
  } else if (!is.null(shapiro_test)) {
    shapiro_p_value <- shapiro_test$p.value
    interpretation <- paste(interpretation, 
                            "- Uji Shapiro-Wilk: W =", round(shapiro_test$statistic, 4), 
                            ", p-value =", format_p_value(shapiro_p_value), "\n",
                            "- Kesimpulan:", 
                            if (shapiro_p_value < 0.05) {
                              "RESIDUAL TIDAK NORMAL (p < 0.05)"
                            } else {
                              "RESIDUAL TERDISTRIBUSI NORMAL (p >= 0.05)"
                            }, "\n",
                            "- Rekomendasi:", 
                            if (shapiro_p_value < 0.05) {
                              "Periksa Q-Q plot. Jika sampel besar (>50), CLT mungkin berlaku. Jika tidak, pertimbangkan transformasi Y."
                            } else {
                              "Asumsi normalitas terpenuhi. Interval kepercayaan dan uji hipotesis valid."
                            }, "\n\n"
    )
  }
  
  # 2. Multikolinearitas
  interpretation <- paste(interpretation, "2. MULTIKOLINEARITAS (VIF):\n")
  if (is.data.frame(vif_results) && "VIF" %in% names(vif_results)) {
    max_vif <- max(vif_results$VIF, na.rm = TRUE)
    interpretation <- paste(interpretation,
                            "- VIF maksimum =", round(max_vif, 2), "\n",
                            "- Status multikolinearitas:", 
                            if (max_vif >= 10) {
                              "TINGGI (VIF >= 10) - Ada multikolinearitas serius"
                            } else if (max_vif >= 5) {
                              "MODERAT (5 <= VIF < 10) - Perlu perhatian"
                            } else {
                              "RENDAH (VIF < 5) - Tidak ada masalah multikolinearitas"
                            }, "\n",
                            "- Rekomendasi:", 
                            if (max_vif >= 10) {
                              "Hapus salah satu variabel berkorelasi tinggi atau gunakan regresi regularisasi (Ridge/Lasso)."
                            } else if (max_vif >= 5) {
                              "Monitor koefisien yang tidak stabil. Pertimbangkan menggabungkan variabel serupa."
                            } else {
                              "Asumsi tidak ada multikolinearitas terpenuhi."
                            }, "\n\n"
    )
  } else {
    interpretation <- paste(interpretation, "- ", vif_results, "\n\n")
  }
  
  # 3. Heteroskedastisitas (Breusch-Pagan Test only)
  interpretation <- paste(interpretation, "3. HOMOSKEDASTISITAS (VARIANS KONSTAN):\n")
  
  if (is.character(bp_test)) {
    interpretation <- paste(interpretation, "- Uji Breusch-Pagan: ", bp_test, "\n")
  } else if (!is.null(bp_test) && "p.value" %in% names(bp_test)) {
    bp_p_value <- bp_test$p.value
    interpretation <- paste(interpretation,
                            "- Uji Breusch-Pagan: BP =", round(bp_test$statistic, 4),
                            ", p-value =", format_p_value(bp_p_value), "\n",
                            "- Kesimpulan:", 
                            if (bp_p_value < 0.05) {
                              "ADA HETEROSKEDASTISITAS (p < 0.05)"
                            } else {
                              "HOMOSKEDASTISITAS TERPENUHI (p >= 0.05)"
                            }, "\n"
    )
  }
  
  # Rekomendasi untuk heteroskedastisitas
  interpretation <- paste(interpretation,
                          "- Rekomendasi: Periksa plot 'Residuals vs Fitted' secara visual.\n",
                          "  Jika ada heteroskedastisitas, pertimbangkan:\n",
                          "  * Transformasi variabel dependen (log, akar kuadrat)\n",
                          "  * Menggunakan robust standard errors\n",
                          "  * Weighted Least Squares (WLS)\n\n"
  )
  
  # 4. Kesimpulan Umum
  interpretation <- paste(interpretation,
                          "4. KESIMPULAN VALIDITAS MODEL:\n",
                          "- Periksa semua asumsi di atas sebelum menginterpretasi hasil.\n",
                          "- Jika asumsi dilanggar, hasil inferensi (p-value, interval kepercayaan) mungkin tidak valid.\n",
                          "- Untuk prediksi, model masih bisa berguna meski asumsi tidak sepenuhnya terpenuhi.\n",
                          "- Gunakan plot diagnostik untuk validasi visual tambahan."
  )
  
  return(interpretation)
}