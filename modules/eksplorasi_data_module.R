# Eksplorasi Data Module
# modules/eksplorasi_data_module.R

# UI function for Eksplorasi Data
eksplorasiDataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "Panel Kontrol Eksplorasi Data",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Pemilihan Variabel
             selectInput(ns("select_variable"), 
                         "Pilih Variabel Utama:",
                         choices = NULL),
             
             selectInput(ns("group_variable_plot"), 
                         "Pilih Variabel Pengelompokan (Opsional):",
                         choices = NULL,
                         multiple = FALSE), # Hanya satu variabel pengelompokan untuk kesederhanaan
             
             # Tombol Aksi
             br(),
             actionButton(ns("generate_plots"), 
                          "Tampilkan Visualisasi", 
                          class = "btn-success"),
             br(), br(),
             
             # Opsi Download
             h5("Download Hasil:"),
             downloadButton(ns("download_descriptive_stats"), 
                            "Download Statistik Deskriptif", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_main_plot"), 
                            "Download Plot Utama", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Konten Utama
    column(8,
           # Statistik Deskriptif
           box(
             title = "Statistik Deskriptif",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(tableOutput(ns("descriptive_stats")))
           ),
           
           # Visualisasi
           box(
             title = "Visualisasi Data",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel( 
               tabPanel("Distribusi Variabel Utama", # Satu tab, output plot di dalamnya yang akan muncul/kosong
                        br(),
                        # Plot untuk numerik
                        plotOutput(ns("plot_histogram_numeric"), height = "300px"), 
                        plotOutput(ns("plot_boxplot_numeric"), height = "300px"),   
                        # Plot untuk kategorikal
                        plotOutput(ns("plot_barplot_categorical"), height = "300px") 
               ),
               
               tabPanel("Boxplot per Kategori (Jika Ada Pengelompokan)", 
                        br(),
                        withSpinner(plotOutput(ns("plot_boxplot_by_category")))),
               
               tabPanel("Tabel Frekuensi (Untuk Kategorikal)", 
                        br(),
                        withSpinner(tableOutput(ns("plot_frequency_table")))),
               
               tabPanel("Matriks Korelasi (Numerik)", 
                        br(),
                        withSpinner(plotOutput(ns("plot_correlation"), height = "auto"))
               )
             )
           )
    ),
    
    # Tabel Ringkasan Data
    column(12,
           box(
             title = "Ringkasan Data",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(DT::dataTableOutput(ns("data_summary_table")))
           )
    ),
    
    # Bagian Interpretasi
    column(12,
           box(
             title = "Interpretasi Hasil Eksplorasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("exploration_interpretation")))
           )
    )
  )
}

# Fungsi Server untuk Eksplorasi Data
eksplorasiDataServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Perbarui Pilihan Variabel
    observe({
      all_vars <- names(values$current_data)
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      updateSelectInput(session, "select_variable", 
                        choices = setNames(all_vars, all_vars)) 
      updateSelectInput(session, "group_variable_plot", 
                        choices = setNames(c("", categorical_vars), c("", categorical_vars))) 
    })
    
    # Nilai Reaktif untuk Hasil Eksplorasi
    exploration_results <- reactiveValues(
      descriptive_stats = NULL,
      hist_plot = NULL,            
      boxplot_single_plot = NULL,  
      barplot_plot = NULL,          
      plot_grouped_boxplot = NULL,
      plot_frequency_table_data = NULL,
      plot_correlation_matrix = NULL,
      interpretation = NULL
    )
    
    # Hasilkan plot dan statistik saat tombol diklik
    observeEvent(input$generate_plots, {
      req(input$select_variable)
      
      data <- values$current_data
      var_name <- input$select_variable
      
      # Reset semua nilai reaktif plot
      exploration_results$hist_plot <- NULL
      exploration_results$boxplot_single_plot <- NULL
      exploration_results$barplot_plot <- NULL
      exploration_results$plot_grouped_boxplot <- NULL
      exploration_results$plot_frequency_table_data <- NULL
      exploration_results$plot_correlation_matrix <- NULL
      
      
      # --- Statistik Deskriptif ---
      if (is.numeric(data[[var_name]])) {
        exploration_results$descriptive_stats <- create_summary_table(data, var_name)
      } else {
        freq_table <- as.data.frame(table(data[[var_name]], useNA = "ifany"))
        names(freq_table) <- c("Kategori", "Frekuensi")
        freq_table$Persentase <- round((freq_table$Frekuensi / sum(freq_table$Frekuensi)) * 100, 2)
        exploration_results$descriptive_stats <- freq_table
      }
      
      # --- Visualisasi: Plot Distribusi Variabel Utama (Plot individual) ---
      if (is.numeric(data[[var_name]])) {
        # Histogram untuk numerik
        exploration_results$hist_plot <- ggplot(data, aes(x = .data[[var_name]])) +
          geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
          labs(title = paste("Histogram", var_name), x = var_name, y = "Frekuensi") +
          theme_custom()
        
        # Boxplot untuk numerik (tunggal)
        exploration_results$boxplot_single_plot <- ggplot(data, aes(x = "", y = .data[[var_name]])) +
          geom_boxplot(fill = "lightcoral", alpha = 0.7) +
          labs(title = paste("Boxplot", var_name), x = "", y = var_name) +
          theme_custom()
        
      } else {
        # Barplot untuk kategorikal
        exploration_results$barplot_plot <- ggplot(data, aes(x = .data[[var_name]])) +
          geom_bar(fill = "lightgreen", alpha = 0.7, color = "black") +
          labs(title = paste("Barplot", var_name), x = var_name, y = "Frekuensi") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      # --- Visualisasi: Boxplot per Kategori (Jika Ada Pengelompokan) ---
      if (!is.null(input$group_variable_plot) && input$group_variable_plot != "" && is.numeric(data[[var_name]])) {
        group_var_name <- input$group_variable_plot
        if (group_var_name %in% names(data) && !is.numeric(data[[group_var_name]])) { 
          exploration_results$plot_grouped_boxplot <- ggplot(data, aes(x = .data[[group_var_name]], y = .data[[var_name]])) +
            geom_boxplot(fill = "lightcoral", alpha = 0.7) +
            labs(title = paste("Boxplot", var_name, "berdasarkan", group_var_name),
                 x = group_var_name, y = var_name) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      }
      
      # --- Visualisasi: Tabel Frekuensi (Untuk Kategorikal) ---
      if (!is.numeric(data[[var_name]])) { 
        freq_table <- as.data.frame(table(data[[var_name]], useNA = "ifany"))
        names(freq_table) <- c("Kategori", "Frekuensi")
        freq_table$Persentase <- round((freq_table$Frekuensi / sum(freq_table$Frekuensi)) * 100, 2)
        exploration_results$plot_frequency_table_data <- freq_table
      }
      
      # --- Visualisasi: Matriks Korelasi (Numerik) ---
      numeric_only_data <- data %>% select(where(is.numeric)) %>% na.omit()
      if (ncol(numeric_only_data) >= 2) {
        cor_matrix <- cor(numeric_only_data)
        cor_df <- as.data.frame(as.table(cor_matrix))
        names(cor_df) <- c("Variabel1", "Variabel2", "Korelasi")
        
        exploration_results$plot_correlation_matrix <- ggplot(cor_df, aes(x = Variabel1, y = Variabel2, fill = Korelasi)) +
          geom_tile(color = "white") +
          scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Korelasi") +
          geom_text(aes(label = round(Korelasi, 2)), color = "black", size = 3) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
          coord_fixed() +
          labs(title = "Matriks Korelasi Variabel Numerik")
      }
      
      # --- Interpretasi Hasil Eksplorasi ---
      interpretation_text <- "INTERPRETASI HASIL EKSPLORASI DATA:\n==================================\n\n"
      
      if (is.numeric(data[[var_name]])) {
        stats_table <- exploration_results$descriptive_stats 
        if (!is.null(stats_table) && "Mean" %in% stats_table$Statistik) { 
          mean_val <- stats_table$Nilai[stats_table$Statistik == "Mean"]
          median_val <- stats_table$Nilai[stats_table$Statistik == "Median"]
          sd_val <- stats_table$Nilai[stats_table$Statistik == "Std Dev"]
          
          interpretation_text <- paste0(interpretation_text, 
                                        "Variabel '", var_name, "' adalah variabel numerik.\n",
                                        "Rata-rata: ", round(mean_val, 3), "\n",
                                        "Median: ", round(median_val, 3), "\n",
                                        "Standar Deviasi: ", round(sd_val, 3), "\n",
                                        "Distribusi divisualisasikan dengan histogram dan boxplot tunggal.\n", 
                                        "Histogram menunjukkan bentuk distribusi (menceng/simetris) dan penyebaran data.\n",
                                        "Boxplot menunjukkan nilai tengah, penyebaran, dan potensi outlier.\n"
          )
        } else {
          interpretation_text <- paste0(interpretation_text, 
                                        "Variabel '", var_name, "' adalah variabel numerik, tetapi statistik deskriptif tidak tersedia atau tidak lengkap.\n")
        }
      } else {
        freq_table_data <- exploration_results$descriptive_stats 
        interpretation_text <- paste0(interpretation_text,
                                      "Variabel '", var_name, "' adalah variabel kategorikal.\n",
                                      "Distribusi ditampilkan dengan barplot yang menunjukkan frekuensi setiap kategori.\n",
                                      "Tabel frekuensi memberikan hitungan dan persentase setiap kategori.\n"
        )
      }
      
      if (!is.null(input$group_variable_plot) && input$group_variable_plot != "" && is.numeric(data[[var_name]])) {
        interpretation_text <- paste0(interpretation_text, "\nBoxplot per kategori menampilkan perbandingan distribusi variabel numerik '", var_name, "' di antara kelompok-kelompok dari variabel '", input$group_variable_plot, "'. Ini berguna untuk mengidentifikasi perbedaan rata-rata dan penyebaran antar kelompok.")
      }
      
      if (!is.null(exploration_results$plot_correlation_matrix)) {
        interpretation_text <- paste0(interpretation_text, "\n\nMatriks korelasi menunjukkan hubungan linear antar variabel numerik. Nilai mendekati 1 menunjukkan korelasi positif kuat, -1 menunjukkan korelasi negatif kuat, dan 0 menunjukkan tidak ada korelasi linear.")
      }
      
      exploration_results$interpretation <- interpretation_text
    })
    
    # Tampilkan output
    output$descriptive_stats <- renderTable({
      req(exploration_results$descriptive_stats)
      exploration_results$descriptive_stats
    })
    
    # Output renderPlot individual untuk plot distribusi
    output$plot_histogram_numeric <- renderPlot({
      req(exploration_results$hist_plot)
      exploration_results$hist_plot
    })
    
    output$plot_boxplot_numeric <- renderPlot({
      req(exploration_results$boxplot_single_plot)
      exploration_results$boxplot_single_plot
    })
    
    output$plot_barplot_categorical <- renderPlot({
      req(exploration_results$barplot_plot)
      exploration_results$barplot_plot
    })
    
    output$plot_boxplot_by_category <- renderPlot({
      req(exploration_results$plot_grouped_boxplot)
      exploration_results$plot_grouped_boxplot
    })
    
    output$plot_frequency_table <- renderTable({
      req(exploration_results$plot_frequency_table_data)
      exploration_results$plot_frequency_table_data
    })
    
    output$plot_correlation <- renderPlot({
      req(exploration_results$plot_correlation_matrix)
      exploration_results$plot_correlation_matrix
    })
    
    output$data_summary_table <- DT::renderDataTable({
      req(values$current_data)
      DT::datatable(
        values$current_data,
        options = list(scrollX = TRUE, pageLength = 10),
        class = 'cell-border stripe',
        rownames = FALSE
      )
    })
    
    output$exploration_interpretation <- renderText({
      req(exploration_results$interpretation)
      exploration_results$interpretation
    })
    
    # Handler Download
    output$download_descriptive_stats <- downloadHandler(
      filename = function() { paste("statistik_deskriptif_", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        write.csv(exploration_results$descriptive_stats, file, row.names=FALSE)
      }
    )
    
    output$download_main_plot <- downloadHandler(
      filename = function() { 
        # Pilih plot mana yang akan diunduh berdasarkan yang aktif/tidak NULL
        if (!is.null(exploration_results$hist_plot)) {
          paste("histogram_", input$select_variable, "_", Sys.Date(), ".png", sep="")
        } else if (!is.null(exploration_results$boxplot_single_plot)) {
          paste("boxplot_single_", input$select_variable, "_", Sys.Date(), ".png", sep="")
        } else if (!is.null(exploration_results$barplot_plot)) {
          paste("barplot_", input$select_variable, "_", Sys.Date(), ".png", sep="")
        } else {
          paste("plot_eksplorasi_", Sys.Date(), ".png", sep="") # Fallback
        }
      },
      content = function(file) {
        # Simpan plot yang sesuai
        if (!is.null(exploration_results$hist_plot)) {
          ggsave(file, exploration_results$hist_plot, width = 10, height = 6, dpi = 300)
        } else if (!is.null(exploration_results$boxplot_single_plot)) {
          ggsave(file, exploration_results$boxplot_single_plot, width = 10, height = 6, dpi = 300)
        } else if (!is.null(exploration_results$barplot_plot)) {
          ggsave(file, exploration_results$barplot_plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() { paste("laporan_eksplorasi_", Sys.Date(), ".docx", sep="") },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- doc %>%
          officer::body_add_par("Laporan Eksplorasi Data", style = "heading 1") %>%
          officer::body_add_par(paste("Variabel Utama:", input$select_variable)) %>%
          officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
          officer::body_add_par(" ") %>%
          
          officer::body_add_par("Statistik Deskriptif:", style = "heading 2")
        if (!is.null(exploration_results$descriptive_stats)) {
          doc <- doc %>% officer::body_add_table(exploration_results$descriptive_stats, style = "Table Grid")
        }
        
        doc <- doc %>% officer::body_add_par("Visualisasi Distribusi:", style = "heading 2")
        # Tambah Histogram jika numerik
        if (!is.null(exploration_results$hist_plot)) {
          temp_plot_file <- tempfile(fileext = ".png")
          ggsave(temp_plot_file, exploration_results$hist_plot, width = 8, height = 6, dpi = 300)
          doc <- doc %>% 
            officer::body_add_par("Visualisasi Histogram:", style = "heading 2") %>%
            officer::body_add_img(temp_plot_file, width = 6, height = 4)
          unlink(temp_plot_file)
        }
        
        # Tambah Boxplot Tunggal jika numerik
        if (!is.null(exploration_results$boxplot_single_plot)) {
          temp_plot_file <- tempfile(fileext = ".png")
          ggsave(temp_plot_file, exploration_results$boxplot_single_plot, width = 8, height = 6, dpi = 300)
          doc <- doc %>% 
            officer::body_add_par("Visualisasi Boxplot Tunggal:", style = "heading 2") %>%
            officer::body_add_img(temp_plot_file, width = 6, height = 4)
          unlink(temp_plot_file)
        }
        
        # Tambah Barplot jika kategorikal
        if (!is.null(exploration_results$barplot_plot)) {
          temp_plot_file <- tempfile(fileext = ".png")
          ggsave(temp_plot_file, exploration_results$barplot_plot, width = 8, height = 6, dpi = 300)
          doc <- doc %>% 
            officer::body_add_par("Visualisasi Barplot:", style = "heading 2") %>%
            officer::body_add_img(temp_plot_file, width = 6, height = 4)
          unlink(temp_plot_file)
        }
        
        if (!is.null(exploration_results$plot_grouped_boxplot)) {
          temp_plot_file_grouped <- tempfile(fileext = ".png")
          ggsave(temp_plot_file_grouped, exploration_results$plot_grouped_boxplot, width = 8, height = 6, dpi = 300)
          doc <- doc %>% 
            officer::body_add_par("Visualisasi Boxplot per Kategori:", style = "heading 2") %>%
            officer::body_add_img(temp_plot_file_grouped, width = 6, height = 4)
          unlink(temp_plot_file_grouped)
        }
        
        if (!is.null(exploration_results$plot_correlation_matrix)) {
          temp_plot_file_corr <- tempfile(fileext = ".png")
          ggsave(temp_plot_file_corr, exploration_results$plot_correlation_matrix, width = 8, height = 6, dpi = 300)
          doc <- doc %>% 
            officer::body_add_par("Matriks Korelasi:", style = "heading 2") %>%
            officer::body_add_img(temp_plot_file_corr, width = 6, height = 4)
          unlink(temp_plot_file_corr)
        }
        
        doc <- doc %>% 
          officer::body_add_par("Interpretasi:", style = "heading 2") %>%
          officer::body_add_par(exploration_results$interpretation)
        
        print(doc, target = file)
      }
    )
  })
}