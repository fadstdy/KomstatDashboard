# Manajemen Data Module
# modules/manajemen_data_module.R

# UI function for Manajemen Data
manajemenDataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "Panel Kontrol Manajemen Data",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Pemilihan Variabel
             selectInput(ns("select_variable"), 
                         "Pilih Variabel:",
                         choices = NULL),
             
             # Tipe Transformasi
             radioButtons(ns("transformation_type"), 
                          "Jenis Transformasi:",
                          choices = list(
                            "Kategorisasi Manual" = "manual",
                            "Kategorisasi Otomatis" = "auto",
                            "Transformasi Logaritma" = "log",
                            "Transformasi Akar Kuadrat" = "sqrt",
                            "Standardisasi (Z-score)" = "zscore"
                          )),
             
             # Input Kondisional berdasarkan Tipe Transformasi
             conditionalPanel(
               condition = "input.transformation_type == 'manual'",
               ns = ns,
               textInput(ns("custom_breaks"), 
                         "Titik Potong (pisahkan dengan koma):",
                         value = ""),
               textInput(ns("custom_labels"), 
                         "Label Kategori (pisahkan dengan koma):",
                         value = "")
             ),
             
             conditionalPanel(
               condition = "input.transformation_type == 'auto'",
               ns = ns,
               numericInput(ns("n_categories"), 
                            "Jumlah Kategori:", 
                            value = 3, 
                            min = 2, 
                            max = 10),
               selectInput(ns("auto_method"), 
                           "Metode Kategorisasi:",
                           choices = list(
                             "Kuartil" = "quartile",
                             "Interval Sama" = "equal",
                             "Jenks Natural Breaks" = "jenks"
                           ))
             ),
             
             # Tombol Aksi
             br(),
             actionButton(ns("apply_transformation"), 
                          "Terapkan Transformasi", 
                          class = "btn-success"),
             br(), br(),
             actionButton(ns("reset_data"), 
                          "Reset ke Data Asli", 
                          class = "btn-warning"),
             
             # Opsi Download
             hr(),
             h5("Download Hasil:"),
             downloadButton(ns("download_transformed"), 
                            "Download Data Transformasi", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Area Konten Utama
    column(8,
           # Statistik Deskriptif Sebelum Transformasi
           box(
             title = "Statistik Deskriptif Sebelum Transformasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(tableOutput(ns("before_stats")))
           ),
           
           # Visualisasi Sebelum Transformasi
           box(
             title = "Visualisasi Sebelum Transformasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(plotOutput(ns("before_plot")))
           )
    ),
    
    # Hasil Setelah Transformasi
    column(12,
           box(
             title = "Hasil Transformasi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             # Tabs untuk Berbagai Tampilan
             tabsetPanel(
               tabPanel("Statistik Deskriptif",
                        br(),
                        withSpinner(tableOutput(ns("after_stats")))),
               
               tabPanel("Visualisasi",
                        br(),
                        withSpinner(plotOutput(ns("after_plot")))),
               
               
               tabPanel("Data Preview",
                        br(),
                        withSpinner(DT::dataTableOutput(ns("data_preview"))))
             )
           )
    ),
    
    # Bagian Interpretasi
    column(12,
           box(
             title = "Interpretasi Hasil Transformasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Fungsi Server untuk Manajemen Data
manajemenDataServer <- function(id, values) { 
  moduleServer(id, function(input, output, session) {
    
    # Inisialisasi original_data saat modul dimulai, menggunakan isolate()
    # original_data kini adalah salinan dari data gabungan saat startup
    values$original_data <- isolate(values$current_data) 
    
    # Perbarui Pilihan Variabel
    observe({
      current_selected_var <- input$select_variable 
      numeric_vars <- get_numeric_vars(values$current_data)
      
      if (!is.null(current_selected_var) && current_selected_var %in% numeric_vars) {
        updateSelectInput(session, "select_variable", 
                          choices = setNames(numeric_vars, numeric_vars),
                          selected = current_selected_var) 
      } else {
        updateSelectInput(session, "select_variable", 
                          choices = setNames(numeric_vars, numeric_vars),
                          selected = NULL) 
      }
    })
    
    # Nilai Reaktif untuk Hasil Transformasi
    transformation_results <- reactiveValues(
      before_data = NULL,
      after_data = NULL,
      transformation_info = NULL
    )
    
    # Tampilkan Statistik Sebelum Transformasi
    output$before_stats <- renderTable({
      req(input$select_variable)
      if (input$select_variable %in% names(values$current_data)) {
        create_summary_table(values$current_data, input$select_variable)
      }
    })
    
    # Tampilkan Plot Sebelum Transformasi
    output$before_plot <- renderPlot({
      req(input$select_variable)
      if (input$select_variable %in% names(values$current_data)) {
        data <- values$current_data
        var_name <- input$select_variable
        
        p1 <- ggplot(data, aes(x = .data[[var_name]])) +
          geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
          labs(title = paste("Histogram", var_name),
               x = var_name, y = "Frekuensi") +
          theme_custom()
        
        p2 <- ggplot(data, aes(x = "", y = .data[[var_name]])) +
          geom_boxplot(fill = "lightcoral", alpha = 0.7) +
          labs(title = paste("Boxplot", var_name),
               x = "", y = var_name) +
          theme_custom()
        
        gridExtra::grid.arrange(p1, p2, ncol = 2)
      }
    })
    
    # Terapkan Transformasi
    observeEvent(input$apply_transformation, {
      req(input$select_variable) 
      
      data <- values$current_data 
      var_name <- input$select_variable 
      
      # Simpan data asli (hanya kolom untuk visual perbandingan sebelum/sesudah)
      transformation_results$before_data <- data[[var_name]]
      
      # Terapkan transformasi berdasarkan tipe
      if (input$transformation_type == "manual") {
        req(input$custom_breaks, input$custom_labels)
        
        breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
        labels <- trimws(unlist(strsplit(input$custom_labels, ",")))
        
        if (length(breaks) == length(labels) + 1) {
          if (!is.numeric(data[[var_name]])) {
            showNotification("Variabel yang dipilih bukan numerik, tidak bisa dikategorikan.", type = "error")
            return()
          }
          data <- create_categorical_var(data, var_name, breaks, labels)
          transformation_results$transformation_info <- list(
            type = "Kategorisasi Manual",
            breaks = breaks,
            labels = labels
          )
        } else {
          showNotification("Jumlah titik potong (breaks) harus sama dengan jumlah label kategori ditambah 1.", 
                           type = "error")
          return()
        }
        
      } else if (input$transformation_type == "auto") {
        req(input$n_categories, input$auto_method)
        
        n_cat <- input$n_categories
        method <- input$auto_method
        
        if (!is.numeric(data[[var_name]])) {
          showNotification("Variabel yang dipilih bukan numerik, tidak bisa dikategorikan secara otomatis.", type = "error")
          return()
        }
        
        if (method == "quartile") {
          breaks <- quantile(data[[var_name]], 
                             probs = seq(0, 1, length.out = n_cat + 1), 
                             na.rm = TRUE)
          labels <- paste("Kuartil", 1:n_cat, sep = "")
        } else if (method == "equal") {
          breaks <- seq(min(data[[var_name]], na.rm = TRUE), 
                        max(data[[var_name]], na.rm = TRUE), 
                        length.out = n_cat + 1)
          labels <- paste("Kategori", 1:n_cat, sep = "")
        } else if (method == "jenks") {
          if (requireNamespace("classInt", quietly = TRUE)) {
            breaks <- classInt::classIntervals(data[[var_name]], n = n_cat, style = "jenks")$brks
            labels <- paste("Kelompok Jenks", 1:n_cat, sep = "")
          } else {
            showNotification("Paket 'classInt' diperlukan untuk metode Jenks. Mohon instal: install.packages('classInt')", type = "error")
            return()
          }
        }
        
        data <- create_categorical_var(data, var_name, breaks, labels)
        transformation_results$transformation_info <- list(
          type = paste("Kategorisasi Otomatis -", method),
          breaks = breaks,
          labels = labels,
          n_categories = n_cat
        )
        
      } else if (input$transformation_type == "log") {
        if (!is.numeric(data[[var_name]])) {
          showNotification("Variabel yang dipilih bukan numerik, tidak bisa ditransformasi logaritma.", type = "error")
          return()
        }
        new_var <- paste0(var_name, "_log")
        data[[new_var]] <- log(data[[var_name]] + 1)  # Menambahkan 1 untuk menghindari log(0)
        transformation_results$transformation_info <- list(
          type = "Transformasi Logaritma",
          new_variable = new_var
        )
        
      } else if (input$transformation_type == "sqrt") {
        if (!is.numeric(data[[var_name]])) {
          showNotification("Variabel yang dipilih bukan numerik, tidak bisa ditransformasi akar kuadrat.", type = "error")
          return()
        }
        new_var <- paste0(var_name, "_sqrt")
        data[[new_var]] <- sqrt(data[[var_name]])
        transformation_results$transformation_info <- list(
          type = "Transformasi Akar Kuadrat",
          new_variable = new_var
        )
        
      } else if (input$transformation_type == "zscore") {
        if (!is.numeric(data[[var_name]])) {
          showNotification("Variabel yang dipilih bukan numerik, tidak bisa distandardisasi Z-score.", type = "error")
          return()
        }
        new_var <- paste0(var_name, "_zscore")
        data[[new_var]] <- scale(data[[var_name]])[,1]
        transformation_results$transformation_info <- list(
          type = "Standardisasi (Z-score)",
          new_variable = new_var
        )
      }
      
      # Perbarui current_data untuk seluruh aplikasi
      values$current_data <- data 
      transformation_results$after_data <- data
      
      showNotification("Transformasi berhasil diterapkan!", type = "message")
    })
    
    # Reset data
    observeEvent(input$reset_data, {
      values$current_data <- values$original_data 
      transformation_results$before_data <- NULL
      transformation_results$after_data <- NULL
      transformation_results$transformation_info <- NULL
      showNotification("Data berhasil direset ke data asli!", type = "message")
    })
    
    # Tampilkan Statistik Setelah Transformasi
    output$after_stats <- renderTable({
      req(transformation_results$after_data)
      req(input$select_variable)
      
      data <- transformation_results$after_data
      
      if (input$transformation_type %in% c("manual", "auto")) {
        cat_var <- paste0(input$select_variable, "_CAT")
        if (cat_var %in% names(data)) {
          table_data <- table(data[[cat_var]])
          prop_data <- prop.table(table_data) * 100
          
          result <- data.frame(
            Kategori = names(table_data),
            Frekuensi = as.numeric(table_data),
            Persentase = round(as.numeric(prop_data), 2)
          )
          return(result)
        }
      } else {
        new_var <- transformation_results$transformation_info$new_variable
        if (new_var %in% names(data)) {
          return(create_summary_table(data, new_var)) 
        }
      }
      
      return(data.frame(Info = "Tidak ada data transformasi untuk ditampilkan"))
    })
    
    # Tampilkan Plot Setelah Transformasi
    output$after_plot <- renderPlot({
      req(transformation_results$after_data)
      req(input$select_variable)
      
      data <- transformation_results$after_data
      
      if (input$transformation_type %in% c("manual", "auto")) {
        cat_var <- paste0(input$select_variable, "_CAT")
        if (cat_var %in% names(data)) {
          p1 <- ggplot(data, aes(x = .data[[cat_var]])) + 
            geom_bar(fill = "lightgreen", alpha = 0.7, color = "black") +
            labs(title = paste("Distribusi", cat_var),
                 x = cat_var, y = "Frekuensi") +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          p2 <- ggplot(data, aes(x = .data[[cat_var]], y = .data[[input$select_variable]])) + 
            geom_boxplot(fill = "lightyellow", alpha = 0.7) +
            labs(title = paste("Boxplot per Kategori"),
                 x = cat_var, y = input$select_variable) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          gridExtra::grid.arrange(p1, p2, ncol = 2)
        }
      } else {
        new_var <- transformation_results$transformation_info$new_variable
        if (new_var %in% names(data)) {
          p1 <- ggplot(data, aes(x = .data[[new_var]])) + 
            geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
            labs(title = paste("Histogram", new_var),
                 x = new_var, y = "Frekuensi") +
            theme_custom()
          
          p2 <- ggplot(data, aes(x = "", y = .data[[new_var]])) + 
            geom_boxplot(fill = "lightcoral", alpha = 0.7) +
            labs(title = paste("Boxplot", new_var),
                 x = "", y = new_var) +
            theme_custom()
          
          gridExtra::grid.arrange(p1, p2, ncol = 2)
        }
      }
    })
    
    # Tampilkan Preview Data
    output$data_preview <- DT::renderDataTable({
      req(transformation_results$after_data)
      
      DT::datatable(
        transformation_results$after_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(10, 25, 50)
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      )
    })
    
    # Hasilkan Interpretasi
    output$interpretation <- renderText({
      req(transformation_results$transformation_info)
      
      info <- transformation_results$transformation_info
      interpretation <- ""
      
      if (info$type == "Kategorisasi Manual") {
        interpretation <- paste(
          "INTERPRETASI KATEGORISASI MANUAL:\n\n",
          "Variabel", input$select_variable, "telah dikategorikan secara manual dengan:\n",
          "- Titik potong:", paste(info$breaks, collapse = ", "), "\n",
          "- Label kategori:", paste(info$labels, collapse = ", "), "\n\n",
          "Kategorisasi ini digunakan untuk analisis pada kelompok-kelompok tertentu",
          "berdasarkan rentang nilai yang telah ditentukan.\n\n",
          "Analisis Lanjutan:\n",
          "- Uji chi-square untuk asosiasi antar kategori\n",
          "- Analisis deskriptif per kategori\n",
          "- Visualisasi distribusi kategori"
        )
      } else if (grepl("Kategorisasi Otomatis", info$type)) {
        interpretation <- paste(
          "INTERPRETASI KATEGORISASI OTOMATIS:\n\n",
          "Variabel", input$select_variable, "telah dikategorikan secara otomatis dengan:\n",
          "- Metode:", info$type, "\n",
          "- Jumlah kategori:", info$n_categories, "\n",
          "- Titik potong:", paste(round(info$breaks, 3), collapse = ", "), "\n\n",
          "Metode ini memberikan pembagian yang objektif berdasarkan distribusi data.\n\n",
          "Analisis Lanjutan:\n",
          "- Validasi hasil kategori dengan pengetahuan\n",
          "- Analisis perbedaan antar kategori\n",
          "- Uji statistik untuk validasi pembagian kategori"
        )
      } else if (info$type == "Transformasi Logaritma") {
        interpretation <- paste(
          "INTERPRETASI TRANSFORMASI LOGARITMA:\n",
          "Variabel baru:", info$new_variable, "\n\n",
          "Setelah dilakukan transformasi logaritma pada variabel", input$select_variable, ", distribusi data menjadi lebih normal.\n",
          "Hal ini terlihat dari penyebaran data yang lebih simetris dan varians yang lebih seragam dibanding sebelum transformasi.\n",
          "Transformasi ini juga membantu mengurangi pengaruh nilai ekstrem atau outlier sehingga model analisis menjadi lebih akurat.\n",
          "Namun, karena data telah diubah ke skala logaritma maka setiap perubahan satu unit pada variabel logaritmik mencerminkan\n",
          "perubahan relatif (persentase) pada skala aslinya."
        )
      } else if (info$type == "Transformasi Akar Kuadrat") {
        interpretation <- paste(
          "INTERPRETASI TRANSFORMASI AKAR KUADRAT:\n",
          "Variabel baru:", info$new_variable, "\n\n",
          "Variabel", input$select_variable, "telah ditransformasi menggunakan akar kuadrat.\n",
          "Transformasi digunakan untuk mengurangi kemencengan (skewness) pada distribusi data.\n",
          "Selain itu, digunakan untuk mengurangi perbedaan varians dan mendekatkan bentuk ke distribusi normal.\n",
          "Dibandingkan dengan transformasi logaritma, pendekatan ini lebih mudah diinterpretasikan karena berlaku untuk nilai non-negatif"
        )
      } else if (info$type == "Standardisasi (Z-score)") {
        interpretation <- paste(
          "INTERPRETASI STANDARDISASI Z-SCORE:\n",
          "Variabel baru:", info$new_variable, "\n\n",
          "Variabel", input$select_variable, "telah distandarisasi menggunakan z-score.\n",
          "Karakteristik hasil:\n",
          "- Mean = 0\n",
          "- Standar deviasi = 1\n",
          "- Skala yang seragam\n\n",
          "Tujuan standardisasi adalah untuk menyamakan skala antar variabel, terutama saat digunakan dalam analisis multivariat\n",
          "seperti regresi berganda.\n",
          "Interpretasi z-score:\n",
          "- z = 0: nilai sama dengan rata-rata\n",
          "- z = 1: nilai 1 SD di atas rata-rata\n",
          "- z = -1: nilai 1 SD di bawah rata-rata\n",
          "- $|z| > 2$: nilai ekstrem (outlier)"
        )
      }
      
      return(interpretation)
    })
    
    # Handler Download
    output$download_transformed <- downloadHandler(
      filename = function() {
        paste("data_transformasi_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (!is.null(transformation_results$after_data)) {
          write.csv(transformation_results$after_data, file, row.names = FALSE)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_transformasi_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(transformation_results$transformation_info)) {
          # Buat dokumen Word dengan laporan transformasi
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Transformasi Data", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel:", input$select_variable)) %>%
            officer::body_add_par(paste("Jenis Transformasi:", transformation_results$transformation_info$type)) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(capture.output(cat(renderText({
              req(transformation_results$transformation_info)
              # Kembalikan teks interpretasi di sini
            })()))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Statistik Deskriptif:", style = "heading 2")
          
          print(doc, target = file)
        }
      }
    )
  })
}
