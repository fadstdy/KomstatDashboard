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
      updateSelectInput(session, "select_variable2",
                        choices = setNames(c("", numeric_vars), c("", numeric_vars)))
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
      scatter_plot = NULL,
      ggplot_scatter = NULL, # Ini akan menyimpan objek ggplot asli dari scatter
      interpretation = NULL,
      variable_type = NULL,
      has_grouping = FALSE,
      show_results = FALSE # Kontrol visibilitas hasil
    )
    
    # CONDITIONAL PANEL CONTROLS
    output$show_numeric_single <- reactive({
      !is.null(exploration_results$variable_type) &&
        exploration_results$variable_type == "numeric" &&
        !exploration_results$has_grouping && exploration_results$show_results
    })
    
    output$show_numeric_grouped <- reactive({
      !is.null(exploration_results$variable_type) &&
        exploration_results$variable_type == "numeric" &&
        exploration_results$has_grouping && exploration_results$show_results
    })
    
    output$show_categorical <- reactive({
      !is.null(exploration_results$variable_type) &&
        exploration_results$variable_type == "categorical" && exploration_results$show_results
    })
    
    output$show_numeric_stats <- reactive({
      !is.null(exploration_results$variable_type) &&
        exploration_results$variable_type == "numeric" && exploration_results$show_results
    })
    
    output$show_categorical_stats <- reactive({
      !is.null(exploration_results$variable_type) &&
        exploration_results$variable_type == "categorical" && exploration_results$show_results
    })
    
    output$show_correlation <- reactive({
      numeric_vars <- get_numeric_vars(values$current_data)
      length(numeric_vars) >= 2 && !is.null(exploration_results$plot_correlation_matrix) && exploration_results$show_results
    })
    
    outputOptions(output, "show_numeric_single", suspendWhenHidden = FALSE)
    outputOptions(output, "show_numeric_grouped", suspendWhenHidden = FALSE)
    outputOptions(output, "show_categorical", suspendWhenHidden = FALSE)
    outputOptions(output, "show_numeric_stats", suspendWhenHidden = FALSE)
    outputOptions(output, "show_categorical_stats", suspendWhenHidden = FALSE)
    outputOptions(output, "show_correlation", suspendWhenHidden = FALSE)
    
    # Kontrol visibilitas utama box hasil
    output$show_results <- reactive({
      exploration_results$show_results
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    
    output$show_scatter_plot <- reactive({
      !is.null(input$select_variable) && input$select_variable != "" &&
        !is.null(input$select_variable2) && input$select_variable2 != "" &&
        input$select_variable != input$select_variable2 &&
        (is.numeric(values$current_data[[input$select_variable]]) %||% FALSE) &&
        (is.numeric(values$current_data[[input$select_variable2]]) %||% FALSE) &&
        exploration_results$show_results
    })
    outputOptions(output, "show_scatter_plot", suspendWhenHidden = FALSE)
    
    # Observer untuk tombol "Reset Pilihan"
    observeEvent(input$reset_choices, {
      # Reset semua selectInput ke nilai awal (NULL atau "")
      all_vars <- names(values$current_data)
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      updateSelectInput(session, "select_variable",
                        choices = setNames(all_vars, all_vars), selected = NULL)
      updateSelectInput(session, "select_variable2",
                        choices = setNames(c("", numeric_vars), c("", numeric_vars)), selected = NULL)
      updateSelectInput(session, "group_variable_plot",
                        choices = setNames(c("", categorical_vars), c("", categorical_vars)), selected = NULL)
      
      # Reset semua hasil eksplorasi menggunakan fungsi helper
      clear_reactive_values(exploration_results)
      exploration_results$show_results <- FALSE # Sembunyikan hasil
      exploration_results$interpretation <- NULL # Pastikan interpretasi juga direset
      
      showNotification("Pilihan dan hasil eksplorasi telah direset!", type = "message")
    })
    
    # Hasilkan plot dan statistik saat tombol diklik
    observeEvent(input$generate_plots, {
      req(input$select_variable)
      
      data <- values$current_data
      var_name <- input$select_variable
      has_grouping <- !is.null(input$group_variable_plot) && input$group_variable_plot != ""
      
      # Reset semua nilai reaktif plot sebelum menghasilkan yang baru
      # Kecuali untuk show_results yang akan diatur di bawah
      exploration_results$descriptive_stats = NULL
      exploration_results$hist_plot = NULL
      exploration_results$boxplot_single_plot = NULL
      exploration_results$barplot_plot = NULL
      exploration_results$plot_grouped_boxplot = NULL
      exploration_results$plot_histogram_by_category = NULL
      exploration_results$plot_frequency_table_data = NULL
      exploration_results$plot_correlation_matrix = NULL
      exploration_results$scatter_plot = NULL
      exploration_results$ggplot_scatter = NULL
      exploration_results$interpretation = NULL
      exploration_results$variable_type = NULL
      exploration_results$has_grouping = FALSE
      
      exploration_results$show_results <- TRUE # Tampilkan hasil setelah generate
      
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
          
          # Histogram per kategori (faceted)
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
      
      # --- Scatter Plot ---
      if (!is.null(input$select_variable2) && input$select_variable2 != "" &&
          input$select_variable != input$select_variable2 &&
          is.numeric(data[[input$select_variable]]) &&
          is.numeric(data[[input$select_variable2]])) {
        
        x_var <- input$select_variable
        y_var <- input$select_variable2
        
        p_scatter <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]],
                                      text = paste0("<b>", x_var, ":</b> ", round(.data[[x_var]], 2), "<br>",
                                                    "<b>", y_var, ":</b> ", round(.data[[y_var]], 2), "<br>",
                                                    # Periksa apakah CITY_NAME ada sebelum mencoba mengaksesnya
                                                    if ("CITY_NAME" %in% names(data)) paste0("<b>Wilayah:</b> ", data$CITY_NAME) else NULL))) +
          geom_point(alpha = 0.6, color = "steelblue") +
          geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
          labs(title = paste("Scatter Plot:", x_var, "vs", y_var),
               x = x_var, y = y_var) +
          theme_custom()
        
        exploration_results$ggplot_scatter <- p_scatter
        exploration_results$scatter_plot <- plotly::ggplotly(p_scatter, tooltip = "text")
      }
      
      # Matriks Korelasi
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
      interpretation_text <- "INTERPRETASI HASIL EKSPLORASI DATA:\n\n"
      
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
      
      if (!is.null(exploration_results$ggplot_scatter)) { # Cek ggplot_scatter, bukan scatter_plot
        interpretation_text <- paste0(interpretation_text,
                                      "SCATTER PLOT:\n",
                                      "- Menunjukkan hubungan antara ", input$select_variable, " dan ", input$select_variable2, ".\n",
                                      "- Amati arah (positif/negatif) dan kekuatan (rapat/menyebar) sebaran titik-titik data.\n",
                                      "- Garis putus-putus merah menunjukkan tren linear (garis regresi) antara kedua variabel.\n",
                                      "- Pola titik yang mengumpul di sekitar garis menunjukkan hubungan yang kuat, sementara sebaran acak menunjukkan hubungan yang lemah atau tidak ada hubungan linear.\n\n")
      }
      
      exploration_results$interpretation <- interpretation_text
    })
    
    # Tampilkan output
    output$descriptive_stats <- renderTable({
      req(exploration_results$descriptive_stats, exploration_results$variable_type == "numeric")
      exploration_results$descriptive_stats
    })
    
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
    
    output$plot_scatter <- renderPlotly({
      req(exploration_results$scatter_plot)
      exploration_results$scatter_plot
    })
    
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
              if(!is.null(exploration_results$plot_grouped_boxplot)) "boxplot_grouped" else
                if(!is.null(exploration_results$ggplot_scatter)) "scatter" else "plot"
        paste(plot_type, "_", input$select_variable, "_", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        main_plot <- exploration_results$hist_plot %||%
          exploration_results$boxplot_single_plot %||%
          exploration_results$barplot_plot %||%
          exploration_results$plot_grouped_boxplot %||%
          exploration_results$ggplot_scatter
        
        if (!is.null(main_plot)) {
          # Gunakan ggsave untuk menyimpan plot
          ggsave(file, main_plot, width = 12, height = 8, dpi = 300)
        } else {
          showNotification("Tidak ada plot utama yang tersedia untuk diunduh.", type = "warning")
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() { paste("laporan_eksplorasi_", Sys.Date(), ".docx", sep="") },
      content = function(file) {
        # Pastikan package officer sudah terinstal
        if (!requireNamespace("officer", quietly = TRUE)) {
          stop("Paket 'officer' diperlukan untuk mengunduh laporan Word. Mohon instal: install.packages('officer')")
        }
        # Pastikan package flextable sudah terinstal untuk tabel yang lebih baik (opsional)
        if (!requireNamespace("flextable", quietly = TRUE)) {
          message("Paket 'flextable' tidak terinstal. Tabel akan ditambahkan sebagai tabel dasar.")
        }
        
        req(exploration_results$interpretation) # Pastikan ada interpretasi untuk laporan
        
        doc <- officer::read_docx()
        doc <- doc %>%
          officer::body_add_par("Laporan Eksplorasi Data", style = "heading 1") %>%
          officer::body_add_par(paste("Variabel Utama:", input$select_variable)) %>%
          officer::body_add_par(paste("Variabel Kedua (Scatter):", ifelse(!is.null(input$select_variable2) && input$select_variable2 != "", input$select_variable2, "Tidak ada"))) %>%
          officer::body_add_par(paste("Pengelompokan:", ifelse(exploration_results$has_grouping, input$group_variable_plot, "Tidak ada"))) %>%
          officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
          officer::body_add_par(" ")
        
        # --- Tambahkan Statistik Deskriptif / Tabel Frekuensi ---
        doc <- doc %>% officer::body_add_par("Statistik & Frekuensi:", style = "heading 2")
        if (!is.null(exploration_results$descriptive_stats) && exploration_results$variable_type == "numeric") {
          doc <- doc %>%
            officer::body_add_par("Statistik Deskriptif Variabel Numerik:", style = "heading 3")
          if (requireNamespace("flextable", quietly = TRUE)) {
            doc <- doc %>% flextable::body_add_flextable(flextable::regulartable(exploration_results$descriptive_stats) %>% flextable::autofit())
          } else {
            doc <- doc %>% officer::body_add_table(exploration_results$descriptive_stats, style = "Table Grid")
          }
        }
        if (!is.null(exploration_results$plot_frequency_table_data) && exploration_results$variable_type == "categorical") {
          doc <- doc %>%
            officer::body_add_par("Tabel Frekuensi Variabel Kategorikal:", style = "heading 3")
          if (requireNamespace("flextable", quietly = TRUE)) {
            doc <- doc %>% flextable::body_add_flextable(flextable::regulartable(exploration_results$plot_frequency_table_data) %>% flextable::autofit())
          } else {
            doc <- doc %>% officer::body_add_table(exploration_results$plot_frequency_table_data, style = "Table Grid")
          }
        }
        doc <- doc %>% officer::body_add_par(" ") # Spasi antar bagian
        
        # --- Tambahkan Visualisasi ---
        doc <- doc %>% officer::body_add_par("Visualisasi Data:", style = "heading 2")
        
        all_plots <- list()
        
        if (!is.null(exploration_results$hist_plot)) {
          all_plots <- c(all_plots, list(list(plot = exploration_results$hist_plot, title = "Histogram Distribusi Variabel")))
        }
        if (!is.null(exploration_results$boxplot_single_plot)) {
          all_plots <- c(all_plots, list(list(plot = exploration_results$boxplot_single_plot, title = "Boxplot Distribusi Variabel")))
        }
        if (!is.null(exploration_results$barplot_plot)) {
          all_plots <- c(all_plots, list(list(plot = exploration_results$barplot_plot, title = "Barplot Distribusi Variabel")))
        }
        if (!is.null(exploration_results$plot_grouped_boxplot)) {
          all_plots <- c(all_plots, list(list(plot = exploration_results$plot_grouped_boxplot, title = "Boxplot Perbandingan per Kategori")))
        }
        if (!is.null(exploration_results$plot_histogram_by_category)) {
          all_plots <- c(all_plots, list(list(plot = exploration_results$plot_histogram_by_category, title = "Histogram Perbandingan per Kategori")))
        }
        if (!is.null(exploration_results$plot_correlation_matrix)) {
          all_plots <- c(all_plots, list(list(plot = exploration_results$plot_correlation_matrix, title = "Matriks Korelasi Variabel Numerik")))
        }
        if (!is.null(exploration_results$ggplot_scatter)) { # Gunakan ggplot_scatter
          all_plots <- c(all_plots, list(list(plot = exploration_results$ggplot_scatter, title = "Scatter Plot Hubungan Dua Variabel Numerik")))
        }
        
        for (plot_info in all_plots) {
          temp_plot_file <- tempfile(fileext = ".png")
          ggsave(temp_plot_file, plot_info$plot, width = 10, height = 6, dpi = 300) # Pastikan ukuran dan resolusi sesuai
          doc <- doc %>%
            officer::body_add_par(plot_info$title, style = "heading 3") %>%
            officer::body_add_img(temp_plot_file, width = 8, height = 5) %>% # Hapus alt_text di sini
            officer::body_add_par(paste("Gambar:", plot_info$title), style = "Normal") # Tambahkan caption secara terpisah
          
          doc <- doc %>% officer::body_add_par(" ") # Spasi setelah gambar
          unlink(temp_plot_file)
        }
        
        # Tambahkan Interpretasi 
        doc <- doc %>%
          officer::body_add_par("Interpretasi Hasil Eksplorasi:", style = "heading 2") %>%
          officer::body_add_par(exploration_results$interpretation)
        
        print(doc, target = file)
      }
    )
  })
}