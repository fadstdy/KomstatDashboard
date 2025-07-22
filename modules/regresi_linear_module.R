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
             width = 12, # Ubah lebar box untuk kerapian, mengisi penuh kolom 3
             
             # Pemilihan Variabel
             selectInput(ns("dependent_variable"),
                         "Variabel Dependen (Y):",
                         choices = NULL),
             
             selectInput(ns("independent_variables"),
                         "Variabel Independen (X):",
                         choices = NULL,
                         multiple = TRUE),
             
             # Opsi Model
             hr(),
             h5("Opsi Model:"),
             # checkboxInput(ns("include_interactions"), # <--- Ini dihapus
             #               "Interaksi Antar Variabel \n(jika ≥2 variabel)",
             #               value = FALSE),
             
             # Tingkat Kepercayaan
             numericInput(ns("conf_level"),
                          "Tingkat Kepercayaan Interval (%):",
                          value = 95,
                          min = 80,
                          max = 99,
                          step = 1),
             
             # Tombol Aksi
             br(),
             # Menggunakan div dengan style untuk memberikan lebar tetap pada tombol
             div(style = "width: 100%;", # Membuat div selebar 100% dan teks di tengah
                 actionButton(ns("run_regression"),
                              "Jalankan Regresi",
                              class = "btn-primary",
                              style = "width: 90%; margin-bottom: 10px;") # 
             ),
             
             hr(),
             h5("Download Hasil:"),
             # Mengatur lebar tombol download agar rapi
             div(style = "width: 100%;", # Mengatur div selebar 100% 
                 downloadButton(ns("download_summary"),
                                "Summary Model",
                                class = "btn-info",
                                style = "width: 90%; margin-bottom: 5px;"), # 
                 br(),
                 downloadButton(ns("download_plots"),
                                "Plot Diagnostik",
                                class = "btn-info",
                                style = "width: 90%; margin-bottom: 5px;"), 
                 br(),
                 downloadButton(ns("download_report"),
                                "Laporan Lengkap",
                                class = "btn-success",
                                style = "width: 90%;") # 
             )
           )
    ),
    
    # Konten Utama
    column(9,
           # Info Panel (sebelum analisis)
           conditionalPanel(
             condition = "output.show_initial_info == true",
             ns = ns,
             box(
               title = "Petunjuk Analisis Regresi Linear",
               status = "info",
               solidHeader = TRUE,
               width = 12,
               div(
                 style = "padding: 20px; text-align: center;",
                 p("Pilih variabel dependen dan independen di panel kiri, \nlalu klik tombol 'Jalankan Analisis Regresi' untuk memulai."),
                 hr(),
                 h5("Output:"),
                 tags$ul(
                   style = "text-align: left; display: inline-block;",
                   tags$li("Ringkasan model dengan R-squared dan F-statistik"),
                   tags$li("Koefisien regresi dengan interval kepercayaan"),
                   tags$li("Uji asumsi regresi (normalitas, multikolinearitas, heteroskedastisitas, autokorelasi)"),
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
                 ),
                 tabPanel("Autokorelasi", # New Tab for Autocorrelation
                          br(),
                          h5("Uji Durbin-Watson"),
                          withSpinner(verbatimTextOutput(ns("dw_test_result"))),
                          br(),
                          p("Uji Durbin-Watson memeriksa keberadaan autokorelasi pada residual. Hipotesis nol adalah tidak ada autokorelasi.")
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
      dwtest_result = NULL, # New: Durbin-Watson test result
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
        show_notification("Silakan pilih minimal 1 variabel independen", type = "warning")
        return()
      }
      
      if (input$dependent_variable %in% input$independent_variables) {
        show_notification("Variabel dependen tidak boleh sama dengan variabel independen", type = "error")
        return()
      }
      
      data <- values$current_data
      
      # Cek data yang lengkap
      vars_used <- c(input$dependent_variable, input$independent_variables)
      complete_data <- data[complete.cases(data[vars_used]), ]
      
      if (nrow(complete_data) < (length(input$independent_variables) + 5)) {
        show_notification(paste("Data tidak cukup untuk analisis regresi. Diperlukan minimal",
                               length(input$independent_variables) + 5, "observasi lengkap."),
                         type = "error")
        return()
      }
      
      # Data model tidak perlu distandardisasi lagi
      data_model <- complete_data
      
      # Buat formula regresi (logika interaksi TETAP ada di sini di server meskipun checkbox dihapus)
      # Jika Anda ingin interaksi DITAMBAH SECARA OTOMATIS tanpa checkbox, maka biarkan kode ini.
      # Jika Anda ingin interaksi DIHAPUS SEPENUHNYA, hapus blok 'if (input$include_interactions ...)' ini
      # dan biarkan hanya 'formula_str <- paste(... independent_variables ...)'
      # Berdasarkan permintaan "opsi model interaksi hapus saja", saya akan hapus logika ini
      # dan asumsikan model tanpa interaksi.
      formula_str <- paste(input$dependent_variable, "~",
                           paste(input$independent_variables, collapse = "+"))
      
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
        
        # 4. Autokorelasi (Durbin-Watson Test)
        if (requireNamespace("lmtest", quietly = TRUE)) {
          tryCatch({
            regression_results$dwtest_result <- lmtest::dwtest(model_results)
          }, error = function(e) {
            regression_results$dwtest_result <- paste("Error menghitung Uji Durbin-Watson:", e$message)
          })
        } else {
          regression_results$dwtest_result <- "Paket 'lmtest' diperlukan untuk Uji Durbin-Watson."
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
        regression_results$interpretation <- interpret_regression_summary(regression_results$summary)
        
        # --- Interpretasi Uji Asumsi ---
        assumption_interpret_text <- create_assumption_interpretation(
          regression_results$shapiro_test,
          regression_results$vif,
          regression_results$bptest_result,
          regression_results$dwtest_result # Pass Durbin-Watson result
        )
        
        regression_results$assumption_interpretation <- assumption_interpret_text
        
        # Restore warning options
        options(warn = 0)
        
        show_notification("Analisis regresi berhasil!", type = "success")
        
      }, error = function(e) {
        # Restore warning options even if error occurs
        options(warn = 0)
        show_notification(paste("Error dalam analisis regresi:", e$message), type = "error")
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
    
    # Output uji autokorelasi
    output$dw_test_result <- renderPrint({
      req(regression_results$dwtest_result)
      regression_results$dwtest_result
    })
    
    # Tampilkan plot diagnostik model (plot dasar R)
    output$residuals_vs_fitted <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 1,
           main = "Residuals vs Fitted Values",
           sub = "Pola acak menunjukkan homoskedastisitas")
    })
    
    # MENAMBAHKAN KEMBALI scale_location karena ada di UI tapi tidak di server
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
           sub = "Nilai > 0.5 perlu diperhatikan")
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
        tryCatch({
          req(regression_results$model, # Model itu sendiri
              regression_results$summary, # Ringkasan model
              regression_results$interpretation, # Interpretasi hasil
              regression_results$assumption_interpretation, # Interpretasi asumsi
              input$dependent_variable, # Variabel input
              input$independent_variables # Variabel input
          )
          
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Analisis Regresi Linear Berganda", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel Dependen:", input$dependent_variable)) %>%
            officer::body_add_par(paste("Variabel Independen:", paste(input$independent_variables, collapse = ", "))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Ringkasan Model:", style = "heading 2") %>%
            officer::body_add_par(paste(capture.output(print(regression_results$summary)), collapse = "\n")) %>%
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
          show_notification("Laporan Word berhasil dibuat!", type = "success")
        }, error = function(e) {
          show_notification(paste("Error saat membuat laporan Word:", e$message), type = "error")
        })
      }
    )
  })
}