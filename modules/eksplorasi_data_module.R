# Eksplorasi Data Module - IMPROVED VERSION
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
                         multiple = FALSE),
             
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
           # Box Utama dengan Tab System
           box(
             title = "Hasil Eksplorasi Data",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             
             # TAB SYSTEM UNTUK SEMUA KONTEN
             tabsetPanel(
               # Tab 1: Statistik Deskriptif (untuk numerik) atau Tabel Frekuensi (untuk kategorikal)
               tabPanel("Statistik & Frekuensi",
                        br(),
                        conditionalPanel(
                          condition = "output.show_numeric_stats == true",
                          ns = ns,
                          h4("Statistik Deskriptif"),
                          withSpinner(tableOutput(ns("descriptive_stats")))
                        ),
                        conditionalPanel(
                          condition = "output.show_categorical_stats == true", 
                          ns = ns,
                          h4("Tabel Frekuensi"),
                          withSpinner(DT::dataTableOutput(ns("plot_frequency_table")))
                        )
               ),
               
               # Tab 2: Visualisasi Distribusi
               tabPanel("Visualisasi Distribusi",
                        br(),
                        # Variabel numerik tanpa pengelompokan
                        conditionalPanel(
                          condition = "output.show_numeric_single == true",
                          ns = ns,
                          h4("Distribusi Variabel Numerik"),
                          fluidRow(
                            column(6, withSpinner(plotOutput(ns("plot_histogram_numeric"), height = "350px"))),
                            column(6, withSpinner(plotOutput(ns("plot_boxplot_numeric"), height = "350px")))
                          )
                        ),
                        
                        # Variabel kategorikal
                        conditionalPanel(
                          condition = "output.show_categorical == true",
                          ns = ns,
                          h4("Distribusi Variabel Kategorikal"),
                          withSpinner(plotOutput(ns("plot_barplot_categorical"), height = "400px"))
                        )
               ),
               
               # Tab 3: Perbandingan Kelompok (hanya muncul jika ada pengelompokan)
               conditionalPanel(
                 condition = "output.show_numeric_grouped == true",
                 ns = ns,
                 tabPanel("Perbandingan per Kelompok",
                          br(),
                          tabsetPanel(
                            tabPanel("Boxplot Perbandingan",
                                     br(),
                                     withSpinner(plotOutput(ns("plot_boxplot_by_category"), height = "400px"))),
                            tabPanel("Histogram per Kelompok", 
                                     br(),
                                     withSpinner(plotOutput(ns("plot_histogram_by_category"), height = "400px")))
                          )
                 )
               ),
               
               # Tab 4: Matriks Korelasi (hanya muncul jika ada >= 2 variabel numerik)
               conditionalPanel(
                 condition = "output.show_correlation == true",
                 ns = ns,
                 tabPanel("Matriks Korelasi",
                          br(),
                          h4("Hubungan Antar Variabel Numerik"),
                          withSpinner(plotOutput(ns("plot_correlation"), height = "500px"))
                 )
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
      plot_histogram_by_category = NULL,
      plot_frequency_table_data = NULL,
      plot_correlation_matrix = NULL,
      interpretation = NULL,
      variable_type = NULL,  # Track variable type
      has_grouping = FALSE   # Track if grouping is used
    )
    
    # CONDITIONAL PANEL CONTROLS
    output$show_numeric_single <- reactive({
      !is.null(exploration_results$variable_type) && 
        exploration_results$variable_type == "numeric" && 
        !exploration_results$has_grouping
    })
    
    output$show_numeric_grouped <- reactive({
      !is.null(exploration_results$variable_type) && 
        exploration_results$variable_type == "numeric" && 
        exploration_results$has_grouping
    })
    
    output$show_categorical <- reactive({
      !is.null(exploration_results$variable_type) && 
        exploration_results$variable_type == "categorical"
    })
    
    # NEW: Show stats table for numeric variables
    output$show_numeric_stats <- reactive({
      !is.null(exploration_results$variable_type) && 
        exploration_results$variable_type == "numeric"
    })
    
    # NEW: Show frequency table for categorical variables  
    output$show_categorical_stats <- reactive({
      !is.null(exploration_results$variable_type) && 
        exploration_results$variable_type == "categorical"
    })
    
    # NEW: Show correlation matrix only if there are >= 2 numeric variables
    output$show_correlation <- reactive({
      numeric_vars <- get_numeric_vars(values$current_data)
      length(numeric_vars) >= 2 && !is.null(exploration_results$plot_correlation_matrix)
    })
    
    # Make these reactive outputs available to conditionalPanel
    outputOptions(output, "show_numeric_single", suspendWhenHidden = FALSE)
    outputOptions(output, "show_numeric_grouped", suspendWhenHidden = FALSE)
    outputOptions(output, "show_categorical", suspendWhenHidden = FALSE)
    outputOptions(output, "show_numeric_stats", suspendWhenHidden = FALSE)
    outputOptions(output, "show_categorical_stats", suspendWhenHidden = FALSE)
    outputOptions(output, "show_correlation", suspendWhenHidden = FALSE)
    
    # Hasilkan plot dan statistik saat tombol diklik
    observeEvent(input$generate_plots, {
      req(input$select_variable)
      
      data <- values$current_data
      var_name <- input$select_variable
      has_grouping <- !is.null(input$group_variable_plot) && input$group_variable_plot != ""
      
      # Reset semua nilai reaktif plot
      exploration_results$hist_plot <- NULL
      exploration_results$boxplot_single_plot <- NULL
      exploration_results$barplot_plot <- NULL
      exploration_results$plot_grouped_boxplot <- NULL
      exploration_results$plot_histogram_by_category <- NULL
      exploration_results$plot_frequency_table_data <- NULL
      exploration_results$plot_correlation_matrix <- NULL
      
      # Determine variable type and grouping
      exploration_results$variable_type <- if(is.numeric(data[[var_name]])) "numeric" else "categorical"
      exploration_results$has_grouping <- has_grouping
      
      # --- Statistik Deskriptif ---
      exploration_results$descriptive_stats <- create_summary_table(data, var_name)
      
      # --- Visualisasi berdasarkan tipe variabel ---
      if (exploration_results$variable_type == "numeric") {
        
        if (!has_grouping) {
          # NUMERIC WITHOUT GROUPING - Single plots
          exploration_results$hist_plot <- ggplot(data, aes(x = .data[[var_name]])) +
            geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
            labs(title = paste("Histogram", var_name), x = var_name, y = "Frekuensi") +
            theme_custom()
          
          exploration_results$boxplot_single_plot <- ggplot(data, aes(x = "", y = .data[[var_name]])) +
            geom_boxplot(fill = "lightcoral", alpha = 0.7) +
            labs(title = paste("Boxplot", var_name), x = "", y = var_name) +
            theme_custom()
          
        } else {
          # NUMERIC WITH GROUPING - Multiple comparison plots
          group_var_name <- input$group_variable_plot
          
          # Boxplot per kategori
          exploration_results$plot_grouped_boxplot <- ggplot(data, aes(x = .data[[group_var_name]], y = .data[[var_name]])) +
            geom_boxplot(fill = "lightcoral", alpha = 0.7) +
            labs(title = paste("Boxplot", var_name, "berdasarkan", group_var_name),
                 x = group_var_name, y = var_name) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          # NEW: Histogram per kategori (faceted)
          exploration_results$plot_histogram_by_category <- ggplot(data, aes(x = .data[[var_name]])) +
            geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "black") +
            facet_wrap(as.formula(paste("~", group_var_name)), scales = "free_y") +
            labs(title = paste("Histogram", var_name, "per", group_var_name),
                 x = var_name, y = "Frekuensi") +
            theme_custom() +
            theme(strip.text = element_text(size = 10))
        }
        
      } else {
        # CATEGORICAL VARIABLE
        exploration_results$barplot_plot <- ggplot(data, aes(x = .data[[var_name]])) +
          geom_bar(fill = "lightgreen", alpha = 0.7, color = "black") +
          labs(title = paste("Barplot", var_name), x = var_name, y = "Frekuensi") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Frequency table
        freq_table <- as.data.frame(table(data[[var_name]], useNA = "ifany"))
        names(freq_table) <- c("Kategori", "Frekuensi")
        freq_table$Persentase <- round((freq_table$Frekuensi / sum(freq_table$Frekuensi)) * 100, 2)
        exploration_results$plot_frequency_table_data <- freq_table
      }
      
      # --- Matriks Korelasi (selalu ada) ---
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
      
      if (exploration_results$variable_type == "numeric") {
        stats_table <- exploration_results$descriptive_stats 
        if (!is.null(stats_table) && "Mean" %in% stats_table$Statistik) { 
          mean_val <- stats_table$Nilai[stats_table$Statistik == "Mean"]
          median_val <- stats_table$Nilai[stats_table$Statistik == "Median"]
          sd_val <- stats_table$Nilai[stats_table$Statistik == "Std Dev"]
          
          interpretation_text <- paste0(interpretation_text, 
                                        "Variabel '", var_name, "' adalah variabel numerik.\n",
                                        "Rata-rata: ", round(mean_val, 3), "\n",
                                        "Median: ", round(median_val, 3), "\n",
                                        "Standar Deviasi: ", round(sd_val, 3), "\n\n")
          
          if (!has_grouping) {
            interpretation_text <- paste0(interpretation_text,
                                          "ANALISIS DISTRIBUSI TUNGGAL:\n",
                                          "- Histogram menunjukkan bentuk distribusi data\n",
                                          "- Boxplot menampilkan median, kuartil, dan outlier potensial\n",
                                          "- Gunakan informasi ini untuk memahami karakteristik dasar variabel\n\n")
          } else {
            interpretation_text <- paste0(interpretation_text,
                                          "ANALISIS PERBANDINGAN ANTAR KELOMPOK:\n",
                                          "- Boxplot per kategori: membandingkan distribusi antar kelompok\n",
                                          "- Histogram per kategori: melihat bentuk distribusi setiap kelompok\n",
                                          "- Perhatikan perbedaan rata-rata, median, dan variabilitas antar kelompok\n\n")
          }
        }
      } else {
        interpretation_text <- paste0(interpretation_text,
                                      "Variabel '", var_name, "' adalah variabel kategorikal.\n",
                                      "ANALISIS DISTRIBUSI KATEGORIKAL:\n",
                                      "- Barplot menunjukkan frekuensi setiap kategori\n",
                                      "- Tabel frekuensi memberikan hitungan dan persentase detil\n",
                                      "- Identifikasi kategori yang dominan dan yang jarang muncul\n\n")
      }
      
      if (!is.null(exploration_results$plot_correlation_matrix)) {
        interpretation_text <- paste0(interpretation_text, 
                                      "MATRIKS KORELASI:\n",
                                      "- Menunjukkan hubungan linear antar variabel numerik\n",
                                      "- Nilai mendekati 1: korelasi positif kuat\n",
                                      "- Nilai mendekati -1: korelasi negatif kuat\n",
                                      "- Nilai mendekati 0: tidak ada korelasi linear\n",
                                      "- Gunakan untuk mengidentifikasi variabel yang saling terkait\n\n")
      }
      
      exploration_results$interpretation <- interpretation_text
    })
    
    # Tampilkan output
    output$descriptive_stats <- renderTable({
      req(exploration_results$descriptive_stats, exploration_results$variable_type == "numeric")
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
    
    # NEW: Histogram per kategori
    output$plot_histogram_by_category <- renderPlot({
      req(exploration_results$plot_histogram_by_category)
      exploration_results$plot_histogram_by_category
    })
    
    output$plot_frequency_table <- DT::renderDataTable({
      req(exploration_results$plot_frequency_table_data)
      DT::datatable(
        exploration_results$plot_frequency_table_data,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 25, 50),
          searching = TRUE,
          ordering = TRUE,
          info = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE
        ),
        class = 'cell-border stripe hover',
        rownames = FALSE,
        caption = "Distribusi Frekuensi dan Persentase"
      ) %>%
        DT::formatRound(columns = "Persentase", digits = 2)
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
    
    # Handler Download - Updated
    output$download_descriptive_stats <- downloadHandler(
      filename = function() { paste("statistik_deskriptif_", Sys.Date(), ".csv", sep="") },
      content = function(file) {
        write.csv(exploration_results$descriptive_stats, file, row.names=FALSE)
      }
    )
    
    output$download_main_plot <- downloadHandler(
      filename = function() { 
        plot_type <- if(!is.null(exploration_results$hist_plot)) "histogram" else
          if(!is.null(exploration_results$boxplot_single_plot)) "boxplot" else
            if(!is.null(exploration_results$barplot_plot)) "barplot" else
              if(!is.null(exploration_results$plot_grouped_boxplot)) "boxplot_grouped" else "plot"
        paste(plot_type, "_", input$select_variable, "_", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        # Choose the most appropriate plot to download
        main_plot <- exploration_results$hist_plot %||% 
          exploration_results$boxplot_single_plot %||%
          exploration_results$barplot_plot %||%
          exploration_results$plot_grouped_boxplot
        
        if (!is.null(main_plot)) {
          ggsave(file, main_plot, width = 12, height = 8, dpi = 300)
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
          officer::body_add_par(paste("Pengelompokan:", ifelse(exploration_results$has_grouping, input$group_variable_plot, "Tidak ada"))) %>%
          officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
          officer::body_add_par(" ") %>%
          officer::body_add_par("Statistik Deskriptif:", style = "heading 2")
        
        if (!is.null(exploration_results$descriptive_stats)) {
          doc <- doc %>% officer::body_add_table(exploration_results$descriptive_stats, style = "Table Grid")
        }
        
        # Add all relevant plots
        all_plots <- list(
          list(plot = exploration_results$hist_plot, title = "Histogram"),
          list(plot = exploration_results$boxplot_single_plot, title = "Boxplot Tunggal"),
          list(plot = exploration_results$barplot_plot, title = "Barplot"),
          list(plot = exploration_results$plot_grouped_boxplot, title = "Boxplot per Kategori"),
          list(plot = exploration_results$plot_histogram_by_category, title = "Histogram per Kategori"),
          list(plot = exploration_results$plot_correlation_matrix, title = "Matriks Korelasi")
        )
        
        for (plot_info in all_plots) {
          if (!is.null(plot_info$plot)) {
            temp_plot_file <- tempfile(fileext = ".png")
            ggsave(temp_plot_file, plot_info$plot, width = 10, height = 6, dpi = 300)
            doc <- doc %>% 
              officer::body_add_par(plot_info$title, style = "heading 3") %>%
              officer::body_add_img(temp_plot_file, width = 8, height = 5)
            unlink(temp_plot_file)
          }
        }
        
        doc <- doc %>% 
          officer::body_add_par("Interpretasi:", style = "heading 2") %>%
          officer::body_add_par(exploration_results$interpretation)
        
        print(doc, target = file)
      }
    )
  })
}