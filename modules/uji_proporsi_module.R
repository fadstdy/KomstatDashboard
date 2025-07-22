# Uji Proporsi Module 
# modules/uji_proporsi_module.R

# UI function for Uji Proporsi
ujiProporsiUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "Panel Kontrol Uji Proporsi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             # Pilih Jenis Uji
             selectInput(ns("test_type"),
                         "Jenis Uji:",
                         choices = list(
                           "Proporsi Unit Administratif" = "administrative",
                           "Proporsi Kategori" = "categorical",
                           "Perbandingan Dua Proporsi" = "two_proportion"
                         )),
             
             # UJI PROPORSI ADMINISTRATIF
             conditionalPanel(
               condition = "input.test_type == 'administrative'",
               ns = ns,
               h5(" Uji Proporsi Unit Administratif:"),
               p("Menguji proporsi kabupaten/provinsi yang memenuhi kriteria tertentu"),
               
               # Pilih Variabel Numerik
               selectInput(ns("admin_variable"),
                           "Variabel Numerik:",
                           choices = NULL),
               
               # Info Variabel
               conditionalPanel(
                 condition = "input.admin_variable != ''",
                 ns = ns,
                 div(
                   style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;",
                   verbatimTextOutput(ns("admin_variable_info"))
                 )
               ),
               
               # Level Analisis
               selectInput(ns("admin_level"),
                           "Level Analisis:",
                           choices = list(
                             "Kabupaten/Kota" = "city",
                             "Provinsi" = "province"
                           )),
               
               # Threshold
               numericInput(ns("admin_threshold"),
                            "Nilai Threshold:",
                            value = 70),
               
               p("Contoh: Berapa proporsi kabupaten dengan tingkat kemiskinan > 30?"),
               
               # Proporsi Hipotesis
               numericInput(ns("admin_p0"),
                            "Proporsi Hipotesis (p₀):",
                            value = 0.5, min = 0, max = 1, step = 0.01),
               
               # Hipotesis Alternatif
               selectInput(ns("admin_alternative"),
                           "Hipotesis Alternatif:",
                           choices = list(
                             "Dua arah (≠)" = "two.sided",
                             "Lebih besar (>)" = "greater",
                             "Lebih kecil (<)" = "less"
                           ))
             ),
             
             # UJI PROPORSI KATEGORI
             conditionalPanel(
               condition = "input.test_type == 'categorical'",
               ns = ns,
               h5("Uji Proporsi Kategori:"),
               p("Menguji proporsi observasi dalam kategori tertentu"),
               
               # Pilih Variabel Kategorikal
               selectInput(ns("cat_variable"),
                           "Variabel Kategorikal:",
                           choices = NULL),
               
               # Info Variabel
               conditionalPanel(
                 condition = "input.cat_variable != ''",
                 ns = ns,
                 div(
                   style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; margin: 10px 0;",
                   verbatimTextOutput(ns("cat_variable_info"))
                 )
               ),
               
               # Pilih Kategori Sukses
               selectInput(ns("success_category"),
                           "Kategori 'Sukses':",
                           choices = NULL),
               
               # Proporsi Hipotesis
               numericInput(ns("cat_p0"),
                            "Proporsi Hipotesis (p₀):",
                            value = 0.5, min = 0, max = 1, step = 0.01),
               
               # Hipotesis Alternatif
               selectInput(ns("cat_alternative"),
                           "Hipotesis Alternatif:",
                           choices = list(
                             "Dua arah (≠)" = "two.sided",
                             "Lebih besar (>)" = "greater",
                             "Lebih kecil (<)" = "less"
                           ))
             ),
             
             # UJI DUA PROPORSI
             conditionalPanel(
               condition = "input.test_type == 'two_proportion'",
               ns = ns,
               h5("Uji Perbandingan Dua Proporsi:"),
               p("Membandingkan proporsi antara dua kelompok"),
               
               # Pilih Jenis Perbandingan
               selectInput(ns("comparison_method"),
                           "Metode Perbandingan:",
                           choices = list(
                             "Dua Provinsi (Prop. Kabupaten)" = "provinces",
                             "Dua Kategori dari Variabel" = "categories"
                           )),
               
               # Untuk perbandingan provinsi
               conditionalPanel(
                 condition = "input.comparison_method == 'provinces'",
                 ns = ns,
                 selectInput(ns("prov_variable"), "Variabel Numerik:", choices = NULL),
                 numericInput(ns("prov_threshold"), "Threshold:", value = 70),
                 selectInput(ns("province1"), "Provinsi Pertama:", choices = NULL),
                 selectInput(ns("province2"), "Provinsi Kedua:", choices = NULL)
               ),
               
               # Untuk perbandingan kategori
               conditionalPanel(
                 condition = "input.comparison_method == 'categories'",
                 ns = ns,
                 selectInput(ns("group_variable"), "Variabel Pengelompokan:", choices = NULL),
                 selectInput(ns("target_variable"), "Variabel Target:", choices = NULL),
                 selectInput(ns("target_category"), "Kategori Target:", choices = NULL)
               ),
               
               # Hipotesis Alternatif
               selectInput(ns("two_alternative"),
                           "Hipotesis Alternatif:",
                           choices = list(
                             "Dua arah (≠)" = "two.sided",
                             "Pertama > Kedua" = "greater",
                             "Pertama < Kedua" = "less"
                           ))
             ),
             
             numericInput(ns("alpha"),
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             br(),
             # Perbaikan: Tombol "Jalankan Uji Proporsi" agar rapi
             actionButton(ns("run_test"),
                          "Jalankan Uji Proporsi",
                          class = "btn-success",
                          style = "width: 90%; display: block; margin: auto; margin-bottom: 15px;"), # Gunakan display:block, margin:auto
             
             hr(),
             h5("Download Hasil:"),
             # Perbaikan: Tombol Download agar rapi
             downloadButton(ns("download_results"), "Hasil Uji",
                            class = "btn-success",
                            style = "width: 90%; display: block; margin: auto; margin-bottom: 7px;"),
             br(),
             downloadButton(ns("download_plot"), "Plot Asumsi",
                            class = "btn-success",
                            style = "width: 90%; display: block; margin: auto; margin-bottom: 7px;"),
             br(),
             downloadButton(ns("download_report"), "Laporan",
                            class = "btn-success",
                            style = "width: 90%; display: block; margin: auto;")
           )
    ),
    
    # Hasil
    column(8,
           box(
             title = "Hasil Uji Proporsi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           box(
             title = "Visualisasi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("test_plot"), height = "400px"))
           )
    ),
    
    # Interpretasi
    column(12,
           box(
             title = "📋 Interpretasi dan Kesimpulan",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Fungsi server untuk uji proporsi
ujiProporsiServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    # UPDATE PILIHAN VARIABEL 
    observe({
      req(values$current_data)
      
      tryCatch({
        all_names <- names(values$current_data)
        
        # Variabel numerik
        numeric_vars <- all_names[sapply(values$current_data, is.numeric)]
        if (length(numeric_vars) > 0) {
          updateSelectInput(session, "admin_variable", choices = setNames(numeric_vars, numeric_vars))
          updateSelectInput(session, "prov_variable", choices = setNames(numeric_vars, numeric_vars))
        }
        
        # Variabel kategorikal
        categorical_vars <- all_names[sapply(values$current_data, function(x) is.factor(x) || is.character(x))]
        excluded_vars <- c("DISTRICTCODE", "PROVINCE_NAME", "CITY_NAME")
        clean_categorical <- setdiff(categorical_vars, excluded_vars)
        
        if (length(clean_categorical) > 0) {
          updateSelectInput(session, "cat_variable",
                            choices = setNames(c("", clean_categorical), c("-- Pilih Variabel --", clean_categorical)))
          updateSelectInput(session, "group_variable",
                            choices = setNames(c("", clean_categorical), c("-- Pilih Variabel --", clean_categorical)))
          updateSelectInput(session, "target_variable",
                            choices = setNames(c("", clean_categorical), c("-- Pilih Variabel --", clean_categorical)))
        }
        
        # Provinsi
        if ("PROVINCE_NAME" %in% all_names) {
          provinces <- sort(unique(values$current_data$PROVINCE_NAME))
          provinces <- provinces[!is.na(provinces)]
          if (length(provinces) > 1) {
            updateSelectInput(session, "province1", choices = setNames(provinces, provinces))
            updateSelectInput(session, "province2", choices = setNames(provinces, provinces))
          }
        }
        
      }, error = function(e) {
        show_notification("Error loading variables", type = "error") # Menggunakan show_notification
      })
    })
    
    # Update kategori untuk variabel kategorikal
    observeEvent(input$cat_variable, {
      if (!is.null(input$cat_variable) && input$cat_variable != "") {
        categories <- sort(unique(na.omit(values$current_data[[input$cat_variable]])))
        updateSelectInput(session, "success_category",
                          choices = setNames(c("", categories), c("-- Pilih Kategori --", categories)))
      }
    })
    
    observeEvent(input$target_variable, {
      if (!is.null(input$target_variable) && input$target_variable != "") {
        categories <- sort(unique(na.omit(values$current_data[[input$target_variable]])))
        updateSelectInput(session, "target_category",
                          choices = setNames(c("", categories), c("-- Pilih Kategori --", categories)))
      }
    })
    
    # INFO VARIABEL 
    output$admin_variable_info <- renderText({
      if (is.null(input$admin_variable) || input$admin_variable == "") {
        return("Pilih variabel terlebih dahulu")
      }
      
      var_data <- na.omit(values$current_data[[input$admin_variable]])
      if (length(var_data) == 0) return("Tidak ada data valid")
      
      paste0("STATISTIK VARIABEL:\n",
             "Range: ", round(min(var_data), 2), " - ", round(max(var_data), 2), "\n",
             "Mean: ", round(mean(var_data), 2), " | Median: ", round(median(var_data), 2), "\n",
             "Total observasi: ", length(var_data))
    })
    
    output$cat_variable_info <- renderText({
      if (is.null(input$cat_variable) || input$cat_variable == "") {
        return("Pilih variabel terlebih dahulu")
      }
      
      var_data <- na.omit(values$current_data[[input$cat_variable]])
      if (length(var_data) == 0) return("Tidak ada data valid")
      
      freq_table <- table(var_data)
      freq_text <- paste(names(freq_table), ":", freq_table, collapse = " | ")
      
      paste0("DISTRIBUSI KATEGORI:\n",
             freq_text, "\n",
             "Total observasi: ", length(var_data))
    })
    
    # HASIL UJI 
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      summary = NULL
    )
    
    # Jalankan uji
    observeEvent(input$run_test, {
      
      if (input$test_type == "administrative") {
        # UJI PROPORSI ADMINISTRATIF 
        
        req(input$admin_variable, input$admin_threshold, input$admin_p0)
        
        if (input$admin_level == "city") {
          # Analisis per kabupaten/kota
          admin_data <- values$current_data %>%
            dplyr::group_by(CITY_NAME) %>%
            dplyr::summarise(
              value = mean(.data[[input$admin_variable]], na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::filter(!is.na(value))
          
          unit_name <- "kabupaten/kota"
          
        } else if (input$admin_level == "province") {
          # Analisis per provinsi
          admin_data <- values$current_data %>%
            dplyr::group_by(PROVINCE_NAME) %>%
            dplyr::summarise(
              value = mean(.data[[input$admin_variable]], na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::filter(!is.na(value)) %>%
            dplyr::rename(CITY_NAME = PROVINCE_NAME) # Untuk konsistensi
          
          unit_name <- "provinsi"
        }
        
        if (nrow(admin_data) < 5) {
          show_notification("Data tidak mencukupi untuk uji proporsi (minimal 5 unit)", type = "error") # Menggunakan show_notification
          return()
        }
        
        # Hitung proporsi
        successes <- sum(admin_data$value > input$admin_threshold)
        total <- nrow(admin_data)
        proportion <- successes / total
        
        # Cek syarat uji proporsi
        if (successes < 5 || (total - successes) < 5) {
          show_notification("Warning: Syarat np ≥ 5 dan n(1-p) ≥ 5 tidak terpenuhi. Hasil mungkin tidak akurat.", type = "warning") # Menggunakan show_notification
        }
        
        # Uji proporsi
        test_result <- prop.test(successes, total, p = input$admin_p0,
                                 alternative = input$admin_alternative, correct = TRUE)
        
        test_results$results <- test_result
        test_results$summary <- list(
          type = "administrative",
          successes = successes,
          total = total,
          proportion = proportion,
          threshold = input$admin_threshold,
          variable = input$admin_variable,
          unit = unit_name
        )
        
        # Plot
        plot_data <- data.frame(
          Status = c(paste("≤", input$admin_threshold), paste(">", input$admin_threshold)),
          Jumlah = c(total - successes, successes),
          Proporsi = c(1 - proportion, proportion)
        )
        
        test_results$plot <- ggplot(plot_data, aes(x = Status, y = Proporsi)) +
          geom_col(fill = c("lightcoral", "lightblue"), alpha = 0.7, color = "black") +
          geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(n=", Jumlah, ")")),
                    vjust = -0.5, size = 4) +
          labs(title = paste("Proporsi", unit_name, "dengan", input$admin_variable, ">", input$admin_threshold),
               subtitle = paste("Total", unit_name, ":", total, "| Proporsi sukses:", round(proportion, 3)),
               x = paste(input$admin_variable), y = "Proporsi") +
          ylim(0, max(plot_data$Proporsi) * 1.15) +
          theme_custom()
        
        # Interpretasi
        test_results$interpretation <- paste0(
          "INTERPRETASI UJI PROPORSI ADMINISTRATIF\n\n",
          "ANALISIS: Proporsi ", unit_name, " dengan ", input$admin_variable, " > ", input$admin_threshold, "\n",
          "UNIT ANALISIS: ", total, " ", unit_name, "\n",
          "KRITERIA SUKSES: ", input$admin_variable, " > ", input$admin_threshold, "\n\n",
          "HASIL:\n",
          "- Jumlah ", unit_name, " sukses: ", successes, "/", total, "\n",
          "- Proporsi sampel: ", round(proportion, 4), "\n",
          "- Proporsi hipotesis (p₀): ", input$admin_p0, "\n\n",
          "HIPOTESIS:\n",
          "- H₀: p = ", input$admin_p0, "\n",
          "- H₁: p ", switch(input$admin_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"), " ", input$admin_p0, "\n\n",
          "STATISTIK UJI:\n",
          "- Chi-square = ", round(test_result$statistic, 4), "\n",
          "- p-value ", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan 95%: [", paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α = ", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀: Proporsi berbeda signifikan dari", input$admin_p0, "(p < α)")
          } else {
            paste("GAGAL TOLAK H₀: Proporsi tidak berbeda signifikan dari", input$admin_p0, "(p ≥ α)")
          }
        )
        
      } else if (input$test_type == "categorical") {
        # UJI PROPORSI KATEGORI
        
        req(input$cat_variable, input$success_category, input$cat_p0)
        
        # Filter data valid
        valid_data <- values$current_data %>%
          dplyr::filter(!is.na(.data[[input$cat_variable]]))
        
        if (nrow(valid_data) < 30) {
          show_notification("Data tidak mencukupi untuk uji proporsi (minimal 30 observasi)", type = "error") # Menggunakan show_notification
          return()
        }
        
        # Hitung proporsi
        successes <- sum(valid_data[[input$cat_variable]] == input$success_category)
        total <- nrow(valid_data)
        proportion <- successes / total
        
        # Uji proporsi
        test_result <- prop.test(successes, total, p = input$cat_p0,
                                 alternative = input$cat_alternative, correct = TRUE)
        
        test_results$results <- test_result
        test_results$summary <- list(
          type = "categorical",
          successes = successes,
          total = total,
          proportion = proportion,
          success_category = input$success_category,
          variable = input$cat_variable
        )
        
        # Frekuensi semua kategori
        freq_table <- table(valid_data[[input$cat_variable]])
        
        # Plot
        plot_data <- data.frame(
          Kategori = names(freq_table),
          Jumlah = as.numeric(freq_table),
          Proporsi = as.numeric(freq_table) / total,
          Status = ifelse(names(freq_table) == input$success_category, "Sukses", "Lainnya")
        )
        
        test_results$plot <- ggplot(plot_data, aes(x = Kategori, y = Proporsi, fill = Status)) +
          geom_col(alpha = 0.7, color = "black") +
          geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(n=", Jumlah, ")")),
                    vjust = -0.5, size = 3.5) +
          scale_fill_manual(values = c("Sukses" = "lightblue", "Lainnya" = "lightgray")) +
          labs(title = paste("Distribusi Kategori:", input$cat_variable),
               subtitle = paste("Kategori Sukses:", input$success_category, "| Proporsi:", round(proportion, 3)),
               x = input$cat_variable, y = "Proporsi") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Interpretasi
        test_results$interpretation <- paste0(
          "INTERPRETASI UJI PROPORSI KATEGORI\n\n",
          "VARIABEL: ", input$cat_variable, "\n",
          "KATEGORI SUKSES: ", input$success_category, "\n",
          "TOTAL OBSERVASI: ", total, "\n\n",
          "DISTRIBUSI KATEGORI:\n",
          paste(names(freq_table), ":", freq_table, collapse = "\n"), "\n\n",
          "HASIL:\n",
          "- Jumlah sukses: ", successes, "/", total, "\n",
          "- Proporsi sampel: ", round(proportion, 4), "\n",
          "- Proporsi hipotesis (p₀): ", input$cat_p0, "\n\n",
          "HIPOTESIS:\n",
          "- H₀: p = ", input$cat_p0, "\n",
          "- H₁: p ", switch(input$cat_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"), " ", input$cat_p0, "\n\n",
          "STATISTIK UJI:\n",
          "- Chi-square = ", round(test_result$statistic, 4), "\n",
          "- p-value ", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan 95%: [", paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α = ", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("✅ TOLAK H₀: Proporsi berbeda signifikan dari", input$cat_p0, "(p < α)")
          } else {
            paste("❌ GAGAL TOLAK H₀: Proporsi tidak berbeda signifikan dari", input$cat_p0, "(p ≥ α)")
          }
        )
        
      } else if (input$test_type == "two_proportion") {
        # UJI PERBANDINGAN DUA PROPORSI 
        
        if (input$comparison_method == "provinces") {
          req(input$prov_variable, input$prov_threshold, input$province1, input$province2)
          
          if (input$province1 == input$province2) {
            show_notification("Pilih dua provinsi yang berbeda", type = "error") # Menggunakan show_notification
            return()
          }
          
          # Data provinsi 1
          prov1_data <- values$current_data %>%
            dplyr::filter(PROVINCE_NAME == input$province1) %>%
            dplyr::group_by(CITY_NAME) %>%
            dplyr::summarise(value = mean(.data[[input$prov_variable]], na.rm = TRUE), .groups = 'drop') %>%
            dplyr::filter(!is.na(value))
          
          unit_name <- "kabupaten/kota"
          
          # Data provinsi 2
          prov2_data <- values$current_data %>%
            dplyr::filter(PROVINCE_NAME == input$province2) %>%
            dplyr::group_by(CITY_NAME) %>%
            dplyr::summarise(value = mean(.data[[input$prov_variable]], na.rm = TRUE), .groups = 'drop') %>%
            dplyr::filter(!is.na(value))
          
          if (nrow(prov1_data) < 3 || nrow(prov2_data) < 3) {
            show_notification("Setiap provinsi harus memiliki minimal 3 kabupaten/kota", type = "error") # Menggunakan show_notification
            return()
          }
          
          # Hitung proporsi
          successes1 <- sum(prov1_data$value > input$prov_threshold)
          total1 <- nrow(prov1_data)
          prop1 <- successes1 / total1
          
          successes2 <- sum(prov2_data$value > input$prov_threshold)
          total2 <- nrow(prov2_data)
          prop2 <- successes2 / total2
          
          # Uji proporsi
          test_result <- prop.test(c(successes1, successes2), c(total1, total2),
                                   alternative = input$two_alternative)
          
          test_results$results <- test_result
          test_results$summary <- list(
            type = "two_proportion_provinces",
            group1 = input$province1,
            group2 = input$province2,
            successes1 = successes1,
            total1 = total1,
            prop1 = prop1,
            successes2 = successes2,
            total2 = total2,
            prop2 = prop2,
            threshold = input$prov_threshold,
            variable = input$prov_variable
          )
          
          # Plot
          comparison_data <- data.frame(
            Provinsi = c(input$province1, input$province2),
            Proporsi = c(prop1, prop2),
            Jumlah_Sukses = c(successes1, successes2),
            Total = c(total1, total2)
          )
          
          test_results$plot <- ggplot(comparison_data, aes(x = Provinsi, y = Proporsi)) +
            geom_col(fill = c("lightblue", "lightcoral"), alpha = 0.7, color = "black") +
            geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(", Jumlah_Sukses, "/", Total, ")")),
                      vjust = -0.5, size = 4) +
            labs(title = paste("Perbandingan Proporsi Kabupaten dengan", input$prov_variable, ">", input$prov_threshold),
                 subtitle = paste("Perbedaan proporsi:", round(prop1 - prop2, 3)),
                 x = "Provinsi", y = "Proporsi") +
            ylim(0, max(comparison_data$Proporsi) * 1.15) +
            theme_custom()
          
          # Interpretasi
          test_results$interpretation <- paste0(
            "INTERPRETASI UJI PERBANDINGAN DUA PROPORSI (PROVINSI)\n\n",
            "ANALISIS: Proporsi kabupaten dengan ", input$prov_variable, " > ", input$prov_threshold, "\n\n",
            "PROVINSI 1: ", input$province1, "\n",
            "- Jumlah kabupaten: ", total1, "\n",
            "- Kabupaten sukses: ", successes1, "\n",
            "- Proporsi: ", round(prop1, 4), "\n\n",
            "PROVINSI 2: ", input$province2, "\n",
            "- Jumlah kabupaten: ", total2, "\n",
            "- Kabupaten sukses: ", successes2, "\n",
            "- Proporsi: ", round(prop2, 4), "\n\n",
            "PERBEDAAN PROPORSI: ", round(prop1 - prop2, 4), "\n\n",
            "HIPOTESIS:\n",
            "- H₀: p₁ = p₂\n",
            "- H₁: p₁ ", switch(input$two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"), " p₂\n\n",
            "STATISTIK UJI:\n",
            "- Chi-square = ", round(test_result$statistic, 4), "\n",
            "- p-value ", format_p_value(test_result$p.value), "\n",
            "- Interval Kepercayaan 95%: [", paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
            "KESIMPULAN (α = ", input$alpha, "):\n",
            if (test_result$p.value < input$alpha) {
              paste("✅ TOLAK H₀: Ada perbedaan proporsi yang signifikan antara kedua provinsi (p < α)")
            } else {
              paste("❌ GAGAL TOLAK H₀: Tidak ada perbedaan proporsi yang signifikan antara kedua provinsi (p ≥ α)")
            }
          )
          
        } else if (input$comparison_method == "categories") {
          req(input$group_variable, input$target_variable, input$target_category)
          
          # Filter data dengan kedua variabel valid
          valid_data <- values$current_data %>%
            dplyr::filter(!is.na(.data[[input$group_variable]]) & !is.na(.data[[input$target_variable]]))
          
          # Cek jumlah kelompok
          groups <- unique(valid_data[[input$group_variable]])
          if (length(groups) != 2) {
            show_notification("Variabel pengelompokan harus memiliki tepat 2 kategori", type = "error") # Menggunakan show_notification
            return()
          }
          
          # Data untuk setiap kelompok
          group1_data <- valid_data[valid_data[[input$group_variable]] == groups[1], ]
          group2_data <- valid_data[valid_data[[input$group_variable]] == groups[2], ]
          
          if (nrow(group1_data) < 10 || nrow(group2_data) < 10) {
            show_notification("Setiap kelompok harus memiliki minimal 10 observasi", type = "error") # Menggunakan show_notification
            return()
          }
          
          # Hitung proporsi
          successes1 <- sum(group1_data[[input$target_variable]] == input$target_category)
          total1 <- nrow(group1_data)
          prop1 <- successes1 / total1
          
          successes2 <- sum(group2_data[[input$target_variable]] == input$target_category)
          total2 <- nrow(group2_data)
          prop2 <- successes2 / total2
          
          # Uji proporsi
          test_result <- prop.test(c(successes1, successes2), c(total1, total2),
                                   alternative = input$two_alternative)
          
          test_results$results <- test_result
          test_results$summary <- list(
            type = "two_proportion_categories",
            group1 = as.character(groups[1]),
            group2 = as.character(groups[2]),
            successes1 = successes1,
            total1 = total1,
            prop1 = prop1,
            successes2 = successes2,
            total2 = total2,
            prop2 = prop2,
            target_category = input$target_category,
            group_variable = input$group_variable,
            target_variable = input$target_variable
          )
          
          # Plot
          comparison_data <- data.frame(
            Kelompok = c(as.character(groups[1]), as.character(groups[2])),
            Proporsi = c(prop1, prop2),
            Jumlah_Sukses = c(successes1, successes2),
            Total = c(total1, total2)
          )
          
          test_results$plot <- ggplot(comparison_data, aes(x = Kelompok, y = Proporsi)) +
            geom_col(fill = c("lightblue", "lightcoral"), alpha = 0.7, color = "black") +
            geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(", Jumlah_Sukses, "/", Total, ")")),
                      vjust = -0.5, size = 4) +
            labs(title = paste("Perbandingan Proporsi", input$target_category, "pada", input$target_variable),
                 subtitle = paste("Berdasarkan", input$group_variable, "| Perbedaan proporsi:", round(prop1 - prop2, 3)),
                 x = input$group_variable, y = "Proporsi") +
            ylim(0, max(comparison_data$Proporsi) * 1.15) +
            theme_custom()
          
          # Interpretasi
          test_results$interpretation <- paste0(
            "INTERPRETASI UJI PERBANDINGAN DUA PROPORSI (KATEGORI)\n\n",
            "ANALISIS: Proporsi ", input$target_category, " pada ", input$target_variable, "\n",
            "PENGELOMPOKAN: ", input$group_variable, "\n\n",
            "KELOMPOK 1: ", groups[1], "\n",
            "- Total observasi: ", total1, "\n",
            "- Observasi sukses: ", successes1, "\n",
            "- Proporsi: ", round(prop1, 4), "\n\n",
            "KELOMPOK 2: ", groups[2], "\n",
            "- Total observasi: ", total2, "\n",
            "- Observasi sukses: ", successes2, "\n",
            "- Proporsi: ", round(prop2, 4), "\n\n",
            "PERBEDAAN PROPORSI: ", round(prop1 - prop2, 4), "\n\n",
            "HIPOTESIS:\n",
            "- H₀: p₁ = p₂\n",
            "- H₁: p₁ ", switch(input$two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"), " p₂\n\n",
            "STATISTIK UJI:\n",
            "- Chi-square = ", round(test_result$statistic, 4), "\n",
            "- p-value ", format_p_value(test_result$p.value), "\n",
            "- Interval Kepercayaan 95%: [", paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
            "KESIMPULAN (α = ", input$alpha, "):\n",
            if (test_result$p.value < input$alpha) {
              paste("✅ TOLAK H₀: Ada perbedaan proporsi yang signifikan antara kedua kelompok (p < α)")
            } else {
              paste("❌ GAGAL TOLAK H₀: Tidak ada perbedaan proporsi yang signifikan antara kedua kelompok (p ≥ α)")
            }
          )
        }
      }
    })
    
    # OUTPUT 
    output$test_results <- renderPrint({
      req(test_results$results)
      
      cat("HASIL UJI PROPORSI\n")
      
      if (!is.null(test_results$summary)) {
        summary <- test_results$summary
        
        if (summary$type == "administrative") {
          cat("Jenis Uji: Proporsi Unit Administratif\n")
          cat("Variabel:", summary$variable, "\n")
          cat("Threshold:", summary$threshold, "\n")
          cat("Unit Analisis:", summary$unit, "\n")
          cat("Total Unit:", summary$total, "\n")
          cat("Unit Sukses:", summary$successes, "\n")
          cat("Proporsi Sampel:", round(summary$proportion, 4), "\n\n")
          
        } else if (summary$type == "categorical") {
          cat("Jenis Uji: Proporsi Kategori\n")
          cat("Variabel:", summary$variable, "\n")
          cat("Kategori Sukses:", summary$success_category, "\n")
          cat("Total Observasi:", summary$total, "\n")
          cat("Observasi Sukses:", summary$successes, "\n")
          cat("Proporsi Sampel:", round(summary$proportion, 4), "\n\n")
          
        } else if (summary$type %in% c("two_proportion_provinces", "two_proportion_categories")) {
          cat("Jenis Uji: Perbandingan Dua Proporsi\n")
          cat("Kelompok 1:", summary$group1, "- Proporsi:", round(summary$prop1, 4),
              "(", summary$successes1, "/", summary$total1, ")\n")
          cat("Kelompok 2:", summary$group2, "- Proporsi:", round(summary$prop2, 4),
              "(", summary$successes2, "/", summary$total2, ")\n")
          cat("Perbedaan Proporsi:", round(summary$prop1 - summary$prop2, 4), "\n\n")
        }
      }
      
      cat("STATISTIK UJI:\n")
      print(test_results$results)
    })
    
    output$test_plot <- renderPlot({
      req(test_results$plot)
      test_results$plot
    })
    
    output$interpretation <- renderText({
      req(test_results$interpretation)
      test_results$interpretation
    })
    
    # DOWNLOAD HANDLERS 
    output$download_results <- downloadHandler(
      filename = function() paste("uji_proporsi_hasil_", Sys.Date(), ".txt", sep=""),
      content = function(file) {
        tryCatch({ # Tambah tryCatch
          req(test_results$results)
          sink(file)
          cat("HASIL UJI PROPORSI\n\n")
          cat("Tanggal Analisis:", as.character(Sys.Date()), "\n")
          cat("Waktu Analisis:", as.character(Sys.time()), "\n\n")
          
          if (!is.null(test_results$summary)) {
            summary <- test_results$summary
            cat("RINGKASAN ANALISIS:\n")
            
            if (summary$type == "administrative") {
              cat("Jenis Uji: Proporsi Unit Administratif\n")
              cat("Variabel:", summary$variable, "\n")
              cat("Threshold:", summary$threshold, "\n")
              cat("Unit Analisis:", summary$unit, "\n")
              cat("Total Unit:", summary$total, "\n")
              cat("Unit Sukses:", summary$successes, "\n")
              cat("Proporsi Sampel:", round(summary$proportion, 4), "\n\n")
            } else if (summary$type == "categorical") { # Tambahkan ini untuk ringkasan kategori
              cat("Jenis Uji: Proporsi Kategori\n")
              cat("Variabel:", summary$variable, "\n")
              cat("Kategori Sukses:", summary$success_category, "\n")
              cat("Total Observasi:", summary$total, "\n")
              cat("Observasi Sukses:", summary$successes, "\n")
              cat("Proporsi Sampel:", round(summary$proportion, 4), "\n\n")
            } else if (summary$type %in% c("two_proportion_provinces", "two_proportion_categories")) { # Tambahkan ini untuk ringkasan dua proporsi
              cat("Jenis Uji: Perbandingan Dua Proporsi\n")
              cat("Kelompok 1:", summary$group1, "- Proporsi:", round(summary$prop1, 4),
                  "(", summary$successes1, "/", summary$total1, ")\n")
              cat("Kelompok 2:", summary$group2, "- Proporsi:", round(summary$prop2, 4),
                  "(", summary$successes2, "/", summary$total2, ")\n")
              cat("Perbedaan Proporsi:", round(summary$prop1 - summary$prop2, 4), "\n\n")
            }
          }
          
          cat("HASIL STATISTIK UJI:\n\n")
          print(test_results$results)
          
          cat("\n\nINTERPRETASI:\n\n")
          cat(test_results$interpretation)
          
          sink()
          show_notification("Hasil uji berhasil diunduh!", type = "success") # Menggunakan show_notification
        }, error = function(e) {
          show_notification(paste("Gagal mengunduh hasil uji:", e$message), type = "error") # Menggunakan show_notification
        })
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() paste("uji_proporsi_plot_", Sys.Date(), ".png", sep=""),
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
      filename = function() paste("laporan_uji_proporsi_", Sys.Date(), ".docx", sep=""),
      content = function(file) {
        tryCatch({ # Tambah tryCatch
          req(test_results$interpretation, test_results$results)
          # test_results$plot tidak di-req di sini karena bisa null, dicheck terpisah
          
          doc <- officer::read_docx()
          
          # Header
          doc <- doc %>%
            officer::body_add_par("LAPORAN UJI PROPORSI", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Waktu:", format(Sys.time(), "%H:%M:%S"))) %>% # Format waktu
            officer::body_add_par(" ")
          
          # Ringkasan Analisis
          doc <- doc %>%
            officer::body_add_par("Ringkasan Analisis", style = "heading 2")
          
          if (!is.null(test_results$summary)) {
            summary <- test_results$summary
            if (summary$type == "administrative") {
              doc <- doc %>%
                officer::body_add_par(paste("Jenis Uji: Proporsi Unit Administratif")) %>%
                officer::body_add_par(paste("Variabel:", summary$variable)) %>%
                officer::body_add_par(paste("Threshold:", summary$threshold)) %>%
                officer::body_add_par(paste("Unit Analisis:", summary$unit)) %>%
                officer::body_add_par(paste("Total Unit:", summary$total)) %>%
                officer::body_add_par(paste("Unit Sukses:", summary$successes)) %>%
                officer::body_add_par(paste("Proporsi Sampel:", round(summary$proportion, 4)))
            } else if (summary$type == "categorical") {
              doc <- doc %>%
                officer::body_add_par(paste("Jenis Uji: Proporsi Kategori")) %>%
                officer::body_add_par(paste("Variabel:", summary$variable)) %>%
                officer::body_add_par(paste("Kategori Sukses:", summary$success_category)) %>%
                officer::body_add_par(paste("Total Observasi:", summary$total)) %>%
                officer::body_add_par(paste("Observasi Sukses:", summary$successes)) %>%
                officer::body_add_par(paste("Proporsi Sampel:", round(summary$proportion, 4)))
            } else if (summary$type %in% c("two_proportion_provinces", "two_proportion_categories")) {
              doc <- doc %>%
                officer::body_add_par(paste("Jenis Uji: Perbandingan Dua Proporsi")) %>%
                officer::body_add_par(paste("Kelompok 1:", summary$group1, "- Proporsi:", round(summary$prop1, 4), "(", summary$successes1, "/", summary$total1, ")")) %>%
                officer::body_add_par(paste("Kelompok 2:", summary$group2, "- Proporsi:", round(summary$prop2, 4), "(", summary$successes2, "/", summary$total2, ")")) %>%
                officer::body_add_par(paste("Perbedaan Proporsi:", round(summary$prop1 - summary$prop2, 4)))
              if (summary$type == "two_proportion_provinces") {
                doc <- doc %>%
                  officer::body_add_par(paste("Variabel Numerik:", summary$variable)) %>%
                  officer::body_add_par(paste("Threshold:", summary$threshold))
              } else { # two_proportion_categories
                doc <- doc %>%
                  officer::body_add_par(paste("Variabel Pengelompokan:", summary$group_variable)) %>%
                  officer::body_add_par(paste("Variabel Target:", summary$target_variable)) %>%
                  officer::body_add_par(paste("Kategori Target:", summary$target_category))
              }
            }
          }
          
          # Hasil Statistik Uji
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Statistik Uji", style = "heading 2") %>%
            # PERBAIKAN: Gunakan paste(..., collapse = "\n")
            officer::body_add_par(paste(capture.output(print(test_results$results)), collapse = "\n"))
          
          # Visualisasi
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 10, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par(" ") %>%
              officer::body_add_par("Visualisasi", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 8, height = 5)
            unlink(temp_file)
          }
          
          # Interpretasi
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi dan Kesimpulan", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          # Footer
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("---") %>%
            officer::body_add_par("Laporan dibuat secara otomatis oleh sistem analisis statistik") %>%
            officer::body_add_par(paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))) # Format waktu lengkap
          
          print(doc, target = file)
          show_notification("Laporan Word berhasil dibuat!", type = "success") # Menggunakan show_notification
        }, error = function(e) {
          show_notification(paste("Error saat membuat laporan Word:", e$message), type = "error") # Menggunakan show_notification
        })
      }
    )
  })
}