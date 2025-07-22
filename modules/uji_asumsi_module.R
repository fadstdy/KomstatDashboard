# Uji Asumsi Module 
# modules/uji_asumsi_module.R

# UI function for Uji Asumsi
ujiAsumsiUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "Panel Kontrol Uji Asumsi",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Pemilihan Tipe Uji
             selectInput(ns("test_type"),
                         "Pilih Jenis Uji:",
                         choices = list(
                           "Uji Normalitas" = "normality",
                           "Uji Homogenitas Variansi" = "homogeneity"
                         )),
             
             # Pemilihan Variabel Uji
             selectInput(ns("test_variable"),
                         "Variabel Uji:",
                         choices = NULL),
             
             # Input Kondisional untuk Uji Homogenitas Variansi
             conditionalPanel(
               condition = "input.test_type == 'homogeneity'",
               ns = ns,
               selectInput(ns("group_variable_homogeneity"),
                           "Variabel Pengelompokan:",
                           choices = NULL),
               
               # TAMBAHAN: Checkbox untuk filter kelompok
               conditionalPanel(
                 condition = "output.show_group_filter == true",
                 ns = ns,
                 checkboxInput(ns("filter_small_groups"),
                               "Filter kelompok dengan data < 3",
                               value = TRUE),
                 numericInput(ns("min_group_size"),
                              "Minimal data per kelompok:",
                              value = 3, min = 2, max = 10)
               )
             ),
             
             # Tingkat Signifikansi
             numericInput(ns("alpha_level"),
                          "Tingkat Signifikansi (α):",
                          value = 0.05,
                          min = 0.01,
                          max = 0.1,
                          step = 0.01),
             
             # Tombol Aksi
             br(),
             # Perbaikan: Tambahkan div untuk mengatur lebar dan perataan tombol
             div(style = "width: 90%; text-align: center;",
                 actionButton(ns("run_assumption_test"),
                              "Jalankan Uji Asumsi",
                              class = "btn-success",
                              style = "width: 90%; margin-bottom: 10px;") # Lebar 90%
             ),
             br(),
             # Perbaikan: Tambahkan div untuk mengatur lebar dan perataan tombol download
             h5("Download Hasil:"),
             div(style = "width: 90%; text-align: center;",
                 downloadButton(ns("download_test_results"),
                                "Download Hasil Uji",
                                class = "btn-primary",
                                style = "width: 90%; margin-bottom: 5px;"), # Lebar 90%
                 br(),
                 downloadButton(ns("download_plot_assumption"),
                                "Download Plot Asumsi",
                                class = "btn-primary",
                                style = "width: 90%; margin-bottom: 5px;"), # Lebar 90%
                 br(),
                 downloadButton(ns("download_report_assumption"),
                                "Download Laporan",
                                class = "btn-info",
                                style = "width: 90%;") # Lebar 90%
             )
           )
    ),
    
    # Konten Utama
    column(8,
           # Hasil Uji
           box(
             title = "Hasil Uji Asumsi",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("assumption_test_output")))
           ),
           
           # Visualisasi Uji
           box(
             title = "Visualisasi Uji Asumsi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("assumption_test_plot"), height = "400px"))
           )
    ),
    
    # Bagian Interpretasi
    column(12,
           box(
             title = "Interpretasi dan Kesimpulan Uji Asumsi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("assumption_interpretation_output")))
           )
    )
  )
}

# Fungsi Server untuk Uji Asumsi
ujiAsumsiServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # TAMBAHAN: Fungsi untuk validasi kelompok
    validate_group_variable <- function(data, group_var) {
      if (is.null(group_var) || group_var == "") return(NULL)
      
      # Hitung ukuran kelompok
      group_sizes <- table(data[[group_var]])
      
      return(list(
        n_groups = length(group_sizes),
        min_size = min(group_sizes),
        max_size = max(group_sizes),
        mean_size = round(mean(group_sizes), 1),
        small_groups = sum(group_sizes < 3),
        group_sizes = group_sizes
      ))
    }
    
    # TAMBAHAN: Filter variabel pengelompokan yang valid
    get_valid_grouping_vars <- function(data) {
      categorical_vars <- get_categorical_vars(data)
      
      valid_vars <- sapply(categorical_vars, function(var) {
        if (!var %in% names(data)) return(FALSE)
        
        group_counts <- table(data[[var]])
        n_groups <- length(group_counts)
        min_group_size <- min(group_counts)
        
        # Kriteria: 2-50 kelompok, minimal 2 observasi per kelompok
        return(n_groups >= 2 && n_groups <= 50 && min_group_size >= 2)
      })
      
      return(categorical_vars[valid_vars])
    }
    
    # Perbarui pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      valid_grouping_vars <- get_valid_grouping_vars(values$current_data)
      
      updateSelectInput(session, "test_variable",
                        choices = setNames(numeric_vars, numeric_vars))
      
      # PERBAIKAN: Hanya tampilkan variabel pengelompokan yang valid
      if (length(valid_grouping_vars) > 0) {
        # Tambahkan info jumlah kelompok di label
        var_labels <- sapply(valid_grouping_vars, function(var) {
          n_groups <- length(unique(values$current_data[[var]]))
          paste0(var, " (", n_groups, " kelompok)")
        })
        
        updateSelectInput(session, "group_variable_homogeneity",
                          choices = setNames(c("", valid_grouping_vars),
                                             c("-- Pilih Variabel --", var_labels)))
      } else {
        updateSelectInput(session, "group_variable_homogeneity",
                          choices = list("-- Tidak ada variabel pengelompokan yang valid --" = ""))
      }
    })
    
    # TAMBAHAN: Validasi real-time untuk variabel pengelompokan
    output$show_group_filter <- reactive({
      if (is.null(input$group_variable_homogeneity) || input$group_variable_homogeneity == "") {
        return(FALSE)
      }
      
      validation <- validate_group_variable(values$current_data, input$group_variable_homogeneity)
      return(!is.null(validation) && validation$small_groups > 0)
    })
    outputOptions(output, "show_group_filter", suspendWhenHidden = FALSE)
    
    
    # Nilai reaktif untuk menyimpan hasil uji
    assumption_results <- reactiveValues(
      test_output = NULL,
      test_plot = NULL,
      interpretation = NULL
    )
    
    # Jalankan Uji Asumsi
    observeEvent(input$run_assumption_test, {
      req(input$test_variable)
      
      data <- values$current_data
      alpha <- input$alpha_level
      
      if (input$test_type == "normality") {
        # Uji Normalitas (Shapiro-Wilk)
        variable_to_test <- data[[input$test_variable]]
        
        # Hapus NA dan periksa ukuran data untuk Shapiro-Wilk (min 3 data unik)
        variable_to_test <- na.omit(variable_to_test)
        if (length(variable_to_test) < 3 || length(unique(variable_to_test)) < 3) {
          show_notification("Data tidak cukup atau tidak bervariasi untuk Uji Shapiro-Wilk (min. 3 nilai unik).", type = "warning")
          assumption_results$test_output <- "Data tidak cukup untuk Uji Shapiro-Wilk."
          assumption_results$test_plot <- NULL
          assumption_results$interpretation <- "Tidak dapat menjalankan uji normalitas karena data tidak memenuhi persyaratan."
          return()
        }
        
        shapiro_test <- shapiro.test(variable_to_test)
        assumption_results$test_output <- shapiro_test
        
        # Plot Q-Q
        qq_data <- as.data.frame(qqnorm(variable_to_test, plot.it = FALSE))
        assumption_results$test_plot <- ggplot(qq_data, aes(x = x, y = y)) +
          geom_point() +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          labs(title = paste("Q-Q Plot:", input$test_variable),
               x = "Kuartil Teoritis", y = "Kuartil Sampel") +
          theme_custom()
        
        assumption_results$interpretation <- paste(
          "INTERPRETASI UJI NORMALITAS (SHAPIRO-WILK):\n\n",
          "Hipotesis:\n",
          "H0: Data terdistribusi normal\n",
          "H1: Data tidak terdistribusi normal\n\n",
          "Hasil Uji Shapiro-Wilk:\n",
          "- W statistik:", round(shapiro_test$statistic, 4), "\n",
          "- p-value:", format_p_value(shapiro_test$p.value), "\n\n",
          "Kesimpulan:\n",
          interpret_normality(shapiro_test$p.value, alpha)
        )
        
      } else if (input$test_type == "homogeneity") {
        # Uji Homogenitas Variansi (Levene's Test)
        req(input$group_variable_homogeneity)
        
        if (input$group_variable_homogeneity == "") {
          show_notification("Silakan pilih variabel pengelompokan untuk uji homogenitas.", type = "warning")
          return()
        }
        
        variable_to_test <- data[[input$test_variable]]
        group_variable <- data[[input$group_variable_homogeneity]]
        
        # Hapus NA
        complete_data <- data.frame(Value = variable_to_test, Group = group_variable)
        complete_data <- na.omit(complete_data)
        
        if (nrow(complete_data) == 0) {
          show_notification("Tidak ada data lengkap untuk uji homogenitas.", type = "warning")
          assumption_results$test_output <- "Tidak ada data lengkap untuk uji homogenitas."
          assumption_results$test_plot <- NULL
          assumption_results$interpretation <- "Tidak dapat menjalankan uji homogenitas karena tidak ada data lengkap."
          return()
        }
        
        # PERBAIKAN: Validasi kelompok
        group_counts <- table(complete_data$Group)
        
        # Filter kelompok kecil jika diperlukan
        if (input$filter_small_groups && any(group_counts < input$min_group_size)) {
          valid_groups <- names(group_counts)[group_counts >= input$min_group_size]
          complete_data <- complete_data[complete_data$Group %in% valid_groups, ]
          complete_data$Group <- droplevels(complete_data$Group)
          
          n_removed <- length(group_counts) - length(valid_groups)
          show_notification(paste("Kelompok dengan data <", input$min_group_size, "telah difilter.", n_removed, "kelompok dihapus."), type = "message")
        }
        
        # Periksa ulang setelah filtering
        group_counts_final <- table(complete_data$Group)
        if (length(group_counts_final) < 2) {
          show_notification("Setelah filtering, tidak tersisa cukup kelompok untuk uji homogenitas (minimal 2 kelompok).", type = "error")
          return()
        }
        
        if (any(group_counts_final < 2)) {
          show_notification("Masih ada kelompok dengan data < 2. Uji homogenitas tidak dapat dilakukan.", type = "error")
          return()
        }
        
        # Menggunakan Levene's Test dari paket 'car'
        tryCatch({
          levene_test <- car::leveneTest(Value ~ Group, data = complete_data)
          assumption_results$test_output <- levene_test
          
          # Visualisasi (Boxplot per kelompok)
          assumption_results$test_plot <- ggplot(complete_data, aes(x = Group, y = Value)) +
            geom_boxplot(fill = "lightgreen", alpha = 0.7) +
            labs(title = paste("Boxplot Variansi:", input$test_variable, "berdasarkan", input$group_variable_homogeneity),
                 subtitle = paste("Jumlah kelompok:", length(group_counts_final)),
                 x = input$group_variable_homogeneity, y = input$test_variable) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          assumption_results$interpretation <- paste(
            "INTERPRETASI UJI HOMOGENITAS VARIANSI (LEVENES TEST):\n\n",
            "Variabel Pengelompokan:", input$group_variable_homogeneity, "\n",
            "Jumlah Kelompok yang Dianalisis:", length(group_counts_final), "\n",
            "Ukuran Kelompok: ", min(group_counts_final), "-", max(group_counts_final), " observasi\n\n",
            "Hipotesis:\n",
            "H0: Variansi antar kelompok adalah sama (homogen)\n",
            "H1: Minimal ada satu variansi kelompok yang berbeda (heterogen)\n\n",
            "Hasil Levene's Test:\n",
            "- F-statistik:", round(levene_test$`F value`[1], 4), "\n",
            "- df1:", levene_test$Df[1], "\n",
            "- df2:", levene_test$Df[2], "\n",
            "- p-value:", format_p_value(levene_test$`Pr(>F)`[1]), "\n\n",
            "Kesimpulan:\n",
            interpret_homogeneity(levene_test$`Pr(>F)`[1], alpha)
          )
          
        }, error = function(e) {
          showNotification(paste("Error dalam uji Levene:", e$message), type = "error")
          assumption_results$test_output <- paste("Error:", e$message)
          assumption_results$test_plot <- NULL
          assumption_results$interpretation <- "Uji homogenitas gagal dilakukan. Periksa data dan variabel pengelompokan."
        })
      }
    })
    
    # Tampilkan Hasil Uji
    output$assumption_test_output <- renderPrint({
      req(assumption_results$test_output)
      assumption_results$test_output
    })
    
    # Tampilkan Plot Uji
    output$assumption_test_plot <- renderPlot({
      req(assumption_results$test_plot)
      assumption_results$test_plot
    })
    
    # Tampilkan Interpretasi
    output$assumption_interpretation_output <- renderText({
      req(assumption_results$interpretation)
      assumption_results$interpretation
    })
    
    # Handler Download Hasil Uji
    output$download_test_results <- downloadHandler(
      filename = function() {
        paste("hasil_uji_asumsi_", input$test_type, "_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        tryCatch({ # <--- Tambahkan tryCatch
          req(assumption_results$test_output)
          writeLines(capture.output(print(assumption_results$test_output)), file)
          show_notification("Hasil uji berhasil diunduh!", type = "success")
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh hasil uji:", e$message), type = "error")
        })
      }
    )
    
    # Handler Download Plot Asumsi
    output$download_plot_assumption <- downloadHandler(
      filename = function() {
        paste("plot_uji_asumsi_", input$test_type, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        tryCatch({ # <--- Tambahkan tryCatch
          req(assumption_results$test_plot)
          ggsave(file, assumption_results$test_plot, width = 10, height = 6, dpi = 300)
          show_notification("Plot asumsi berhasil diunduh!", type = "success")
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh plot asumsi:", e$message), type = "error")
        })
      }
    )
    
    # Handler Download Laporan
    output$download_report_assumption <- downloadHandler(
      filename = function() {
        paste("laporan_uji_asumsi_", input$test_type, "_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        tryCatch({ # <--- Tambahkan tryCatch untuk debug error pembuatan dokumen
          req(assumption_results$test_output,
              assumption_results$test_plot,
              assumption_results$interpretation,
              input$test_type,
              input$test_variable)
          
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Uji Asumsi", style = "heading 1") %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(paste("Variabel Uji:", input$test_variable)) %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Uji:", style = "heading 2")
          
          if (!is.null(assumption_results$test_output)) {
            # PERBAIKI INI: Gabungkan output print menjadi satu string
            doc <- doc %>% officer::body_add_par(paste(capture.output(print(assumption_results$test_output)), collapse = "\n"))
          }
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Visualisasi Uji:", style = "heading 2")
          
          if (!is.null(assumption_results$test_plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, assumption_results$test_plot, width = 8, height = 6, dpi = 300)
            doc <- doc %>% officer::body_add_img(temp_file, width = 6, height = 4)
            unlink(temp_file)
          }
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(assumption_results$interpretation)
          
          print(doc, target = file)
          show_notification("Laporan Word berhasil dibuat!", type = "success") # Notifikasi sukses
        }, error = function(e) {
          show_notification(paste("Error saat membuat laporan Word:", e$message), type = "error")
        })
      }
    )
  })
}