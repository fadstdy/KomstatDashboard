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
                           choices = NULL)
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
             actionButton(ns("run_assumption_test"), 
                          "Jalankan Uji Asumsi", 
                          class = "btn-success"),
             br(), br(),
             
             # Opsi Download
             h5("Download Hasil:"),
             downloadButton(ns("download_test_results"), 
                            "Download Hasil Uji", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_plot_assumption"), 
                            "Download Plot Asumsi", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report_assumption"), 
                            "Download Laporan", 
                            class = "btn-info")
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
    
    # Perbarui pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      updateSelectInput(session, "test_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "group_variable_homogeneity", 
                        choices = setNames(categorical_vars, categorical_vars))
    })
    
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
          showNotification("Data tidak cukup atau tidak bervariasi untuk Uji Shapiro-Wilk (min. 3 nilai unik).", type = "warning")
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
          "INTERPRETASI UJI NORMALITAS (SHAPIRO-WILK):\n",
          "=========================================\n\n",
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
        
        variable_to_test <- data[[input$test_variable]]
        group_variable <- data[[input$group_variable_homogeneity]]
        
        # Hapus NA
        complete_data <- data.frame(Value = variable_to_test, Group = group_variable)
        complete_data <- na.omit(complete_data)
        
        if (nrow(complete_data) == 0) {
          showNotification("Tidak ada data lengkap untuk uji homogenitas.", type = "warning")
          assumption_results$test_output <- "Tidak ada data lengkap untuk uji homogenitas."
          assumption_results$test_plot <- NULL
          assumption_results$interpretation <- "Tidak dapat menjalankan uji homogenitas karena tidak ada data lengkap."
          return()
        }
        
        # Pastikan ada setidaknya 2 kelompok yang berbeda
        if (length(unique(complete_data$Group)) < 2) {
          showNotification("Variabel pengelompokan harus memiliki minimal dua kategori untuk uji homogenitas.", type = "warning")
          assumption_results$test_output <- "Variabel pengelompokan tidak cukup."
          assumption_results$test_plot <- NULL
          assumption_results$interpretation <- "Tidak dapat menjalankan uji homogenitas karena variabel pengelompokan memiliki kurang dari 2 kategori."
          return()
        }
        
        # Menggunakan Levene's Test dari paket 'car'
        levene_test <- car::leveneTest(Value ~ Group, data = complete_data)
        assumption_results$test_output <- levene_test
        
        # Visualisasi (Boxplot per kelompok)
        assumption_results$test_plot <- ggplot(complete_data, aes(x = Group, y = Value)) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7) +
          labs(title = paste("Boxplot Variansi:", input$test_variable, "berdasarkan", input$group_variable_homogeneity),
               x = input$group_variable_homogeneity, y = input$test_variable) +
          theme_custom()
        
        assumption_results$interpretation <- paste(
          "INTERPRETASI UJI HOMOGENITAS VARIANSI (LEVENES TEST):\n",
          "=================================================\n\n",
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
        if (!is.null(assumption_results$test_output)) {
          writeLines(capture.output(print(assumption_results$test_output)), file)
        }
      }
    )
    
    # Handler Download Plot Asumsi
    output$download_plot_assumption <- downloadHandler(
      filename = function() {
        paste("plot_uji_asumsi_", input$test_type, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        if (!is.null(assumption_results$test_plot)) {
          ggsave(file, assumption_results$test_plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    # Handler Download Laporan
    output$download_report_assumption <- downloadHandler(
      filename = function() {
        paste("laporan_uji_asumsi_", input$test_type, "_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(assumption_results$interpretation)) {
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Uji Asumsi", style = "heading 1") %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(paste("Variabel Uji:", input$test_variable)) %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Uji:", style = "heading 2")
          
          if (!is.null(assumption_results$test_output)) {
            doc <- doc %>% officer::body_add_par(capture.output(print(assumption_results$test_output)))
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
        }
      }
    )
  })
}