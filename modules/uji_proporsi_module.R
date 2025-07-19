# Uji Proporsi Module
# modules/uji_proporsi_module.R

# ==========================================
# UJI PROPORSI - UI FUNCTION
# ==========================================

ujiProporsiUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol
    column(4,
           box(
             title = "📊 Panel Kontrol Uji Proporsi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             # Level 1: Pilih Jenis Uji (1 Sampel vs 2 Sampel)
             selectInput(ns("test_type_main"), 
                         "Jenis Uji:",
                         choices = list(
                           "Uji Proporsi Satu Sampel" = "one_sample_prop_test",
                           "Uji Proporsi Dua Sampel" = "two_sample_prop_test"
                         )),
             
             # Sub-pilihan untuk Uji Proporsi Satu Sampel
             conditionalPanel(
               condition = "input.test_type_main == 'one_sample_prop_test'",
               ns = ns,
               selectInput(ns("sample_type_1_s"), 
                           "Jenis Sampel (Satu):",
                           choices = list(
                             "Satu Kabupaten/Kota" = "one_region_1s",
                             "Satu Provinsi" = "one_province_1s", # Opsi baru
                             "Satu Kelompok Kustom" = "one_custom_group_1s" # Opsi baru
                           ))
             ),
             
             # Sub-pilihan untuk Uji Proporsi Dua Sampel
             conditionalPanel(
               condition = "input.test_type_main == 'two_sample_prop_test'",
               ns = ns,
               selectInput(ns("sample_type_2_s"), 
                           "Jenis Sampel (Dua):",
                           choices = list(
                             "Dua Kabupaten/Kota" = "two_regions_2s",
                             "Dua Provinsi" = "two_provinces_2s",
                             "Dua Kelompok Kustom" = "two_custom_groups_2s"
                           ))
             ),
             
             selectInput(ns("variable"), 
                         "Variabel untuk Analisis Proporsi:",
                         choices = NULL),
             
             # Input khusus untuk 'Proporsi Hipotesis (p0)' yang hanya relevan untuk uji 1 sampel
             conditionalPanel(
               condition = "input.test_type_main == 'one_sample_prop_test'",
               ns = ns,
               numericInput(ns("p0"), 
                            "Proporsi Hipotesis (p₀):",
                            value = 0.5, min = 0, max = 1, step = 0.01),
               helpText("Proporsi yang akan dibandingkan dengan data observasi")
             ),
             
             # Input untuk Satu Kabupaten/Kota (sekarang tergantung sample_type_1_s)
             conditionalPanel(
               condition = "input.test_type_main == 'one_sample_prop_test' && input.sample_type_1_s == 'one_region_1s'",
               ns = ns,
               selectInput(ns("selected_city_1s"), # Nama ID diubah
                           "Pilih Kabupaten/Kota:",
                           choices = NULL)
             ),
             
             # Input untuk Satu Provinsi (Opsi baru, tergantung sample_type_1_s)
             conditionalPanel(
               condition = "input.test_type_main == 'one_sample_prop_test' && input.sample_type_1_s == 'one_province_1s'",
               ns = ns,
               selectInput(ns("selected_province_1s"), # Nama ID baru
                           "Pilih Provinsi:",
                           choices = NULL)
             ),
             
             # Input untuk Satu Kelompok Kustom (Opsi baru, tergantung sample_type_1_s)
             conditionalPanel(
               condition = "input.test_type_main == 'one_sample_prop_test' && input.sample_type_1_s == 'one_custom_group_1s'",
               ns = ns,
               selectInput(ns("selected_custom_group_var_1s"), # Variabel kategorikal
                           "Pilih Variabel Kustom:",
                           choices = NULL),
               selectInput(ns("selected_custom_category_1s"), # Kategori spesifik dalam variabel kustom
                           "Pilih Kategori:",
                           choices = NULL)
             ),
             
             # Input untuk Dua Kabupaten/Kota (sekarang tergantung sample_type_2_s)
             conditionalPanel(
               condition = "input.test_type_main == 'two_sample_prop_test' && input.sample_type_2_s == 'two_regions_2s'",
               ns = ns,
               selectInput(ns("city1_2s"), # Nama ID diubah
                           "Kabupaten/Kota Pertama:",
                           choices = NULL),
               selectInput(ns("city2_2s"), # Nama ID diubah
                           "Kabupaten/Kota Kedua:",
                           choices = NULL)
             ),
             
             # Input untuk Dua Provinsi (sekarang tergantung sample_type_2_s)
             conditionalPanel(
               condition = "input.test_type_main == 'two_sample_prop_test' && input.sample_type_2_s == 'two_provinces_2s'",
               ns = ns,
               selectInput(ns("province1_2s"), # Nama ID diubah
                           "Provinsi Pertama:",
                           choices = NULL),
               selectInput(ns("province2_2s"), # Nama ID diubah
                           "Provinsi Kedua:",
                           choices = NULL)
             ),
             
             # Input untuk Dua Kelompok Kustom (sekarang tergantung sample_type_2_s)
             conditionalPanel(
               condition = "input.test_type_main == 'two_sample_prop_test' && input.sample_type_2_s == 'two_custom_groups_2s'",
               ns = ns,
               selectInput(ns("custom_group_variable_2s"), # Nama ID diubah
                           "Variabel Pengelompokan Kustom:",
                           choices = NULL)
             ),
             
             # Threshold untuk analisis proporsi (untuk data kontinu)
             # Sekarang muncul untuk semua uji proporsi (baik 1 atau 2 sampel) yang melibatkan threshold,
             # dan tidak hanya jika test_type != 'one_region'
             conditionalPanel(
               condition = "(input.test_type_main == 'one_sample_prop_test' && input.sample_type_1_s != 'one_custom_group_1s') ||
                            input.test_type_main == 'two_sample_prop_test'", # Semua 2 sampel perlu ini
               ns = ns,
               # Checkbox untuk custom threshold
               checkboxInput(ns("use_custom_threshold"), 
                             "Gunakan Threshold Kustom", 
                             value = FALSE),
               # Input untuk nilai custom threshold
               conditionalPanel(
                 condition = "input.use_custom_threshold == true",
                 ns = ns,
                 numericInput(ns("custom_threshold_value"), 
                              "Nilai Threshold:",
                              value = 0),
                 helpText("Hitung proporsi di atas nilai threshold ini")
               ),
               # Threshold persentil hanya untuk perbandingan 2 sampel dan 1 sampel provinsi
               conditionalPanel(
                 condition = "input.test_type_main == 'two_sample_prop_test' || (input.test_type_main == 'one_sample_prop_test' && input.sample_type_1_s == 'one_province_1s')",
                 ns = ns,
                 numericInput(ns("threshold_percentil_shared"), # Nama diubah untuk menghindari konflik
                              "Threshold Persentil (%):",
                              value = 50, min = 0, max = 100, step = 5),
                 helpText("Contoh: 50% = di atas median, 75% = di atas kuartil 3")
               )
             ),
             
             # Pilihan Hipotesis Alternatif umum untuk 2 sampel
             conditionalPanel(
               condition = "input.test_type_main == 'two_sample_prop_test'",
               ns = ns,
               selectInput(ns("alternative"), 
                           "Hipotesis Alternatif (2 Sampel):",
                           choices = list(
                             "Dua arah (≠)" = "two.sided",
                             "Pertama > Kedua" = "greater",
                             "Pertama < Kedua" = "less"
                           ))
             ),
             # Pilihan Hipotesis Alternatif khusus untuk 1 sampel
             conditionalPanel(
               condition = "input.test_type_main == 'one_sample_prop_test'",
               ns = ns,
               selectInput(ns("alternative_one_sample"),
                           "Hipotesis Alternatif (1 Sampel):",
                           choices = list(
                             "Dua arah (≠)" = "two.sided",
                             "Lebih besar (>)" = "greater",
                             "Lebih kecil (<)" = "less"
                           ))
             ),
             
             numericInput(ns("alpha"), 
                          "Tingkat Signifikansi:",
                          value = 0.05, min = 0.01, max = 0.1, step = 0.01),
             
             br(),
             actionButton(ns("run_test"), 
                          "Jalankan Uji Proporsi", 
                          class = "btn-success btn-lg"),
             
             hr(),
             h5("📥 Download:"),
             downloadButton(ns("download_results"), "Hasil", class = "btn-sm btn-primary"),
             br(), br(),
             downloadButton(ns("download_plot"), "Plot", class = "btn-sm btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), "Laporan", class = "btn-sm btn-info")
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
             title = "Interpretasi Uji Proporsi",
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
# UJI PROPORSI - SERVER FUNCTION
# ==========================================

ujiProporsiServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update pilihan variabel
    observe({
      all_data_names <- names(values$current_data)
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data) 
      
      updateSelectInput(session, "variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      
      # Update pilihan kabupaten/kota
      if ("CITY_NAME" %in% all_data_names) {
        cities <- sort(unique(values$current_data$CITY_NAME))
        updateSelectInput(session, "selected_city_1s", # Untuk one_sample_prop_test
                          choices = setNames(cities, cities))
        updateSelectInput(session, "city1_2s", # Untuk two_sample_prop_test
                          choices = setNames(cities, cities))
        updateSelectInput(session, "city2_2s", # Untuk two_sample_prop_test
                          choices = setNames(cities, cities))
      }
      
      # Update pilihan provinsi
      if ("PROVINCE_NAME" %in% all_data_names) {
        provinces <- sort(unique(values$current_data$PROVINCE_NAME))
        updateSelectInput(session, "selected_province_1s", # Untuk one_sample_prop_test
                          choices = setNames(provinces, provinces))
        updateSelectInput(session, "province1_2s", # Untuk two_sample_prop_test
                          choices = setNames(provinces, provinces))
        updateSelectInput(session, "province2_2s", # Untuk two_sample_prop_test
                          choices = setNames(provinces, provinces))
      }
      
      # Filter variabel kategorikal yang valid untuk pengelompokan 2 kelompok kustom
      valid_2_group_categorical <- categorical_vars[
        sapply(categorical_vars, function(var) {
          if (!var %in% all_data_names) return(FALSE)
          
          groups <- unique(na.omit(values$current_data[[var]]))
          if (length(groups) != 2) return(FALSE) # Harus tepat 2 kelompok
          
          group_sizes <- table(values$current_data[[var]])
          min_size <- min(group_sizes)
          
          return(min_size >= 3) # Minimal 3 observasi per kelompok
        })
      ]
      
      if (length(valid_2_group_categorical) > 0) {
        group_labels_2_s <- sapply(valid_2_group_categorical, function(var) {
          group_sizes <- table(values$current_data[[var]])
          paste0(var, " (", paste(names(group_sizes), ":", group_sizes, collapse = " vs "), ")")
        })
        updateSelectInput(session, "custom_group_variable_2s", # Untuk two_sample_prop_test
                          choices = setNames(c("", valid_2_group_categorical), 
                                             c("-- Pilih Variabel Pengelompokan --", group_labels_2_s)))
      } else {
        updateSelectInput(session, "custom_group_variable_2s", 
                          choices = list("-- Tidak ada variabel dengan 2 kelompok valid --" = ""))
      }
      
      # Filter variabel kategorikal untuk satu kelompok kustom (bisa >2 kelompok, tapi pilih 1 kategori)
      valid_any_group_categorical <- categorical_vars[
        sapply(categorical_vars, function(var) {
          if (!var %in% all_data_names) return(FALSE)
          
          groups <- unique(na.omit(values$current_data[[var]]))
          # Untuk 1-sample custom group, variabel bisa punya lebih dari 2 kelompok,
          # karena nanti akan pilih 1 kategori saja. Tapi harus ada setidaknya 1 kelompok.
          return(length(groups) >= 1 && length(na.omit(values$current_data[[var]])) > 0)
        })
      ]
      
      if (length(valid_any_group_categorical) > 0) {
        updateSelectInput(session, "selected_custom_group_var_1s", # Untuk one_sample_prop_test
                          choices = setNames(c("", valid_any_group_categorical), 
                                             c("-- Pilih Variabel Kustom --", valid_any_group_categorical)))
      } else {
        updateSelectInput(session, "selected_custom_group_var_1s", 
                          choices = list("-- Tidak ada variabel kategorikal valid --" = ""))
      }
    })
    
    # Update pilihan kategori untuk "Satu Kelompok Kustom"
    observeEvent(input$selected_custom_group_var_1s, {
      req(input$selected_custom_group_var_1s)
      req(input$selected_custom_group_var_1s != "")
      
      selected_var <- input$selected_custom_group_var_1s
      categories <- sort(unique(na.omit(values$current_data[[selected_var]])))
      
      updateSelectInput(session, "selected_custom_category_1s",
                        choices = setNames(c("", categories), 
                                           c("-- Pilih Kategori --", categories)))
    })
    
    # Nilai reaktif untuk hasil
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL
    )
    
    # Fungsi pembantu untuk menentukan threshold (dipakai di beberapa tempat)
    calculate_threshold_and_successes <- function(values_vec, use_custom, custom_val, percentile, p0_val = NULL) {
      threshold_value <- NULL
      threshold_type <- "none"
      successes <- NULL
      
      if (use_custom) {
        threshold_value <- custom_val
        successes <- sum(values_vec > threshold_value, na.rm = TRUE)
        threshold_type <- "kustom"
      } else {
        # Cek apakah data biner (0/1)
        if (all(values_vec %in% c(0, 1), na.rm = TRUE)) {
          successes <- sum(values_vec == 1, na.rm = TRUE)
          threshold_value <- NA
          threshold_type <- "biner"
        } else {
          # Jika bukan biner, gunakan persentil atau p0 sebagai persentil
          if (!is.null(percentile)) { # Untuk 2-sampel atau 1-sampel provinsi/global
            threshold_value <- quantile(values_vec, percentile / 100, na.rm = TRUE)
            successes <- sum(values_vec > threshold_value, na.rm = TRUE)
            threshold_type <- "persentil"
          } else if (!is.null(p0_val)) { # Untuk 1-sampel kab/kota yang bukan biner
            # Ini mengasumsikan p0_val sudah dalam skala 0-1 dan digunakan sebagai persentil
            threshold_value <- quantile(values_vec, p0_val, na.rm = TRUE)
            successes <- sum(values_vec > threshold_value, na.rm = TRUE)
            threshold_type <- "persentil_p0"
          } else {
            stop("Tidak ada metode threshold yang valid terdeteksi.")
          }
        }
      }
      list(successes = successes, threshold_value = threshold_value, threshold_type = threshold_type)
    }
    
    
    # Jalankan uji
    observeEvent(input$run_test, {
      req(input$variable)
      
      data <- values$current_data
      selected_var_name <- input$variable
      alpha <- input$alpha
      
      # Logika utama berdasarkan Jenis Uji (1 Sampel atau 2 Sampel)
      if (input$test_type_main == "one_sample_prop_test") {
        req(input$sample_type_1_s)
        req(input$p0) # Proporsi Hipotesis
        
        if (input$sample_type_1_s == "one_region_1s") {
          req(input$selected_city_1s)
          
          sample_name <- input$selected_city_1s
          unit_type <- "observasi"
          filtered_data <- data[data$CITY_NAME == sample_name, ]
          
          if (nrow(filtered_data) == 0) { showNotification("Tidak ada data untuk kabupaten/kota terpilih", type = "error"); return() }
          variable_values <- na.omit(filtered_data[[selected_var_name]])
          if (length(variable_values) == 0) { showNotification("Tidak ada data valid untuk variabel terpilih", type = "error"); return() }
          
          # Tentukan threshold dan hitung proporsi
          threshold_info <- calculate_threshold_and_successes(
            variable_values,
            input$use_custom_threshold,
            input$custom_threshold_value,
            percentile = NULL, # Tidak pakai percentile dari input threshold_percentil_shared untuk ini
            p0_val = input$p0 # Gunakan p0 sebagai persentil jika bukan biner dan tidak custom
          )
          
        } else if (input$sample_type_1_s == "one_province_1s") {
          req(input$selected_province_1s)
          
          sample_name <- input$selected_province_1s
          unit_type <- "kabupaten/kota" # Unit observasi adalah kabupaten/kota
          
          # Agregasi data kabupaten/kota di provinsi ini
          prov_data <- data[data$PROVINCE_NAME == sample_name, ]
          if (nrow(prov_data) == 0) { showNotification("Tidak ada data untuk provinsi terpilih", type = "error"); return() }
          
          # Untuk proporsi, kita butuh nilai variabel untuk setiap kabupaten/kota
          # Ambang batas harus dihitung dari semua data yang relevan terlebih dahulu.
          # Proporsi sukses di sini adalah proporsi KAB/KOTA di provinsi tsb yang memenuhi kriteria
          
          # Hitung threshold dari semua data (dari seluruh dataset)
          all_data_var_values <- na.omit(data[[selected_var_name]]) # Ambil semua data variabel dari dataset
          if (length(all_data_var_values) == 0) { showNotification("Tidak ada data valid secara keseluruhan untuk menentukan threshold.", type = "error"); return(); }
          
          threshold_info <- calculate_threshold_and_successes(
            all_data_var_values,
            input$use_custom_threshold,
            input$custom_threshold_value,
            percentile = input$threshold_percentil_shared # Pakai persentil yang umum
          )
          
          # Sekarang, tentukan "sukses" untuk setiap kabupaten/kota
          # Sebuah kabupaten/kota dianggap "sukses" jika rata-rata nilai variabelnya di atas threshold
          # Ini memerlukan perhitungan rata-rata per kabupaten/kota di provinsi tsb
          # dan membandingkannya dengan threshold_info$threshold_value
          
          agg_data <- prov_data %>%
            dplyr::group_by(CITY_NAME) %>%
            dplyr::summarise(mean_var = mean(.data[[selected_var_name]], na.rm = TRUE), .groups = 'drop') %>%
            na.omit()
          
          if (nrow(agg_data) == 0) { showNotification("Tidak ada data agregat valid untuk provinsi ini", type = "error"); return() }
          
          # Observasi adalah jumlah kabupaten/kota di provinsi ini
          variable_values <- agg_data$mean_var
          
          # Override threshold_info untuk kasus ini
          threshold_info$successes <- sum(agg_data$mean_var > threshold_info$threshold_value, na.rm = TRUE)
          threshold_info$trials <- nrow(agg_data) # Jumlah kabupaten/kota
          
        } else if (input$sample_type_1_s == "one_custom_group_1s") {
          req(input$selected_custom_group_var_1s, input$selected_custom_category_1s)
          
          group_var_name <- input$selected_custom_group_var_1s
          category_name <- input$selected_custom_category_1s
          
          if (category_name == "") {showNotification("Pilih kategori untuk analisis", type="error"); return();}
          
          sample_name <- paste0(group_var_name, ": ", category_name)
          unit_type <- "observasi"
          
          # Filter data untuk kategori terpilih dari variabel kustom
          filtered_data <- data[data[[group_var_name]] == category_name, ]
          if (nrow(filtered_data) == 0) { showNotification("Tidak ada data untuk kategori terpilih", type = "error"); return() }
          
          variable_values <- na.omit(filtered_data[[selected_var_name]])
          if (length(variable_values) == 0) { showNotification("Tidak ada data valid untuk variabel terpilih di kategori ini", type = "error"); return() }
          
          # Tentukan threshold dan hitung proporsi
          # Untuk 1-sample custom group, kita tidak menggunakan threshold_percentile dari input UI ini
          # Kita pakai logika asli one_region untuk thresholding (biner, p0 sebagai persentil, atau kustom)
          threshold_info <- calculate_threshold_and_successes(
            variable_values,
            input$use_custom_threshold,
            input$custom_threshold_value,
            percentile = NULL, # Tidak pakai percentile dari input threshold_percentil_shared
            p0_val = input$p0 # Gunakan p0 sebagai persentil default jika bukan biner
          )
          
        } else {
          showNotification("Pilih jenis sampel yang valid untuk uji 1 sampel.", type = "error"); return()
        }
        
        # Jika proporsi tidak ditentukan secara eksplisit dari calculate_threshold_and_successes (misal biner)
        if (is.null(threshold_info$successes) && threshold_info$threshold_type == "biner") {
          if (all(variable_values %in% c(0, 1), na.rm = TRUE)) {
            threshold_info$successes <- sum(variable_values == 1, na.rm = TRUE)
            threshold_info$trials <- length(variable_values)
            threshold_info$threshold_value <- NA # Tidak ada nilai threshold untuk biner
          } else {
            showNotification("Variabel bukan biner dan metode threshold tidak ditentukan.", type = "error"); return()
          }
        } else if (is.null(threshold_info$successes) && threshold_info$threshold_type != "biner") {
          # Ini untuk kasus dimana threshold_info.successes belum dihitung karena bukan biner
          threshold_info$trials <- length(variable_values)
        }
        
        
        successes <- threshold_info$successes
        trials <- threshold_info$trials # Ini mungkin sudah dihitung di one_province_agg
        
        if (is.null(successes) || is.null(trials) || trials == 0) {
          showNotification("Gagal menghitung jumlah sukses/trials. Periksa data dan pengaturan threshold.", type = "error"); return()
        }
        
        test_result <- prop.test(successes, trials, p = input$p0,
                                 alternative = input$alternative_one_sample, # Gunakan alt_hyp dari 1 sampel
                                 conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot untuk satu sampel (histogram atau barplot)
        plot_subtitle_text <- paste0("Proporsi 'Sukses' (nilai > ",
                                     ifelse(is.na(threshold_info$threshold_value), "N/A", round(threshold_info$threshold_value, 3)),
                                     ") = ", round(successes/trials, 3), " (", successes, "/", trials, " ", unit_type, ")")
        
        if (threshold_info$threshold_type == "biner") {
          plot_data <- data.frame(
            Kategori = c("0", "1"),
            Jumlah = c(trials - successes, successes),
            Proporsi = c((trials - successes)/trials, successes/trials)
          )
          test_results$plot <- ggplot(plot_data, aes(x = Kategori, y = Proporsi)) +
            geom_col(fill = c("lightcoral", "lightblue"), alpha = 0.7) +
            geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(n=", Jumlah, ")")),
                      vjust = -0.5) +
            labs(title = paste("Proporsi", selected_var_name, "di", sample_name),
                 subtitle = plot_subtitle_text,
                 y = "Proporsi") +
            theme_custom()
        } else {
          test_results$plot <- ggplot(data.frame(Value = variable_values), aes(x = Value)) +
            geom_histogram(bins = 20, fill = "lightgreen", alpha = 0.7, color = "black") +
            geom_vline(xintercept = threshold_info$threshold_value, color = "red", linetype = "dashed", linewidth = 1.2) +
            labs(title = paste("Distribusi", selected_var_name, "di", sample_name),
                 subtitle = plot_subtitle_text,
                 x = selected_var_name, y = "Frekuensi") +
            theme_custom()
        }
        
        
        # Interpretasi 1 Sampel
        test_results$interpretation <- paste(
          "INTERPRETASI UJI PROPORSI SATU SAMPEL:\n",
          "=====================================\n\n",
          "SAMPEL YANG DIANALISIS:\n",
          "- Lokasi/Kelompok:", sample_name, "\n",
          "- Variabel:", selected_var_name, "\n",
          "- Jumlah unit observasi:", trials, " (tipe: ", unit_type, ")\n\n",
          "METODE ANALISIS PROPORSI:\n",
          "- Kriteria 'sukses': Nilai variabel > threshold\n",
          "- Threshold:", ifelse(is.na(threshold_info$threshold_value), "N/A (Data Biner)", round(threshold_info$threshold_value, 3)),
          ifelse(threshold_info$threshold_type == "persentil", paste0(" (Persentil ", input$p0*100, "%)"), ""), "\n",
          "- Proporsi sampel (p̂):", round(successes/trials, 4), " (", successes, " dari ", trials, ")\n\n",
          "HIPOTESIS:\n",
          "- H₀: p =", input$p0, "\n",
          "- H₁: p", switch(input$alternative_one_sample, # Gunakan alt_hyp dari 1 sampel
                            "two.sided" = "≠",
                            "greater" = ">",
                            "less" = "<"), input$p0, "\n\n",
          "HASIL UJI STATISTIK:\n",
          "- Chi-square statistik =", round(test_result$statistic, 4), "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) = [",
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀. Ada bukti statistik yang signifikan bahwa proporsi di", sample_name, "berbeda signifikan dari", input$p0,
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Proporsi di", sample_name, "tidak berbeda signifikan dari", input$p0,
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (test_result$p.value < input$alpha) {
            paste("Proporsi", selected_var_name, "pada", sample_name, "berbeda secara nyata dari nilai hipotesis", input$p0, ".")
          } else {
            paste("Proporsi", selected_var_name, "pada", sample_name, "tidak berbeda secara nyata dari nilai hipotesis", input$p0, ".")
          }
        )
        
      } else if (input$test_type_main == "two_sample_prop_test") {
        req(input$sample_type_2_s)
        
        group1_name <- ""
        group2_name <- ""
        var1_values <- NULL
        var2_values <- NULL
        unit_type <- "" # Observasi atau kabupaten/kota
        
        # Logika perbandingan 2 sampel berdasarkan jenis pengelompokan
        if (input$sample_type_2_s == "two_regions_2s") {
          req(input$city1_2s, input$city2_2s)
          if (input$city1_2s == input$city2_2s) { showNotification("Pilih dua kabupaten/kota yang berbeda", type = "error"); return() }
          
          group1_name <- input$city1_2s
          group2_name <- input$city2_2s
          unit_type <- "observasi"
          
          filtered_data1 <- data[data$CITY_NAME == group1_name, ]
          filtered_data2 <- data[data$CITY_NAME == group2_name, ]
          
          if (nrow(filtered_data1) == 0 || nrow(filtered_data2) == 0) { showNotification("Salah satu atau kedua kabupaten/kota tidak memiliki data", type = "error"); return() }
          
          var1_values <- na.omit(filtered_data1[[selected_var_name]])
          var2_values <- na.omit(filtered_data2[[selected_var_name]])
          
        } else if (input$sample_type_2_s == "two_provinces_2s") {
          req(input$province1_2s, input$province2_2s)
          if (input$province1_2s == input$province2_2s) { showNotification("Pilih dua provinsi yang berbeda", type = "error"); return() }
          
          group1_name <- input$province1_2s
          group2_name <- input$province2_2s
          unit_type <- "kabupaten/kota" # Unit analisis adalah kabupaten/kota
          
          prov1_data <- data[data$PROVINCE_NAME == group1_name, ]
          prov2_data <- data[data$PROVINCE_NAME == group2_name, ]
          
          if (nrow(prov1_data) == 0 || nrow(prov2_data) == 0) { showNotification("Salah satu atau kedua provinsi tidak memiliki data", type = "error"); return() }
          
          # Di sini, var1/var2 akan menjadi nilai agregat (misal rata-rata) per kab/kota
          # dan proporsi sukses adalah proporsi KAB/KOTA yang memenuhi kriteria
          
          agg_data1 <- prov1_data %>%
            dplyr::group_by(CITY_NAME) %>%
            dplyr::summarise(mean_var = mean(.data[[selected_var_name]], na.rm = TRUE), .groups = 'drop') %>%
            na.omit()
          
          agg_data2 <- prov2_data %>%
            dplyr::group_by(CITY_NAME) %>%
            dplyr::summarise(mean_var = mean(.data[[selected_var_name]], na.rm = TRUE), .groups = 'drop') %>%
            na.omit()
          
          if (nrow(agg_data1) == 0 || nrow(agg_data2) == 0) { showNotification("Tidak ada data agregat valid untuk salah satu atau kedua provinsi", type = "error"); return() }
          
          var1_values <- agg_data1$mean_var
          var2_values <- agg_data2$mean_var
          
        } else if (input$sample_type_2_s == "two_custom_groups_2s") {
          req(input$custom_group_variable_2s)
          
          group_var_name <- input$custom_group_variable_2s
          unit_type <- "observasi"
          
          # Validasi variabel pengelompokan kustom (sama seperti sebelumnya, tapi sekarang di sini)
          complete_data_custom <- data[complete.cases(data[[selected_var_name]], data[[group_var_name]]), ]
          if (nrow(complete_data_custom) < 6) { showNotification("Data tidak cukup untuk uji proporsi dua kelompok kustom (minimal 6 observasi total).", type = "error"); return() }
          
          groups_custom <- unique(complete_data_custom[[group_var_name]])
          if (length(groups_custom) != 2) { showNotification(paste("Variabel pengelompokan kustom harus memiliki tepat 2 kelompok. Ditemukan", length(groups_custom), "kelompok."), type = "error"); return() }
          
          group_sizes_custom <- table(complete_data_custom[[group_var_name]])
          if (any(group_sizes_custom < 3)) { showNotification("Setiap kelompok dalam variabel pengelompokan kustom harus memiliki minimal 3 observasi.", type = "error"); return() }
          
          group1_name <- as.character(groups_custom[1])
          group2_name <- as.character(groups_custom[2])
          
          var1_values <- na.omit(complete_data_custom[complete_data_custom[[group_var_name]] == group1_name, selected_var_name])
          var2_values <- na.omit(complete_data_custom[complete_data_custom[[group_var_name]] == group2_name, selected_var_name])
        } else {
          showNotification("Pilih jenis perbandingan yang valid untuk uji 2 sampel.", type = "error"); return()
        }
        
        # Lanjutkan dengan perhitungan proporsi setelah mendapatkan var1_values & var2_values
        if (length(var1_values) == 0 || length(var2_values) == 0) {
          showNotification("Tidak ada data valid untuk salah satu atau kedua kelompok/daerah.", type = "error")
          return()
        }
        
        # Hitung threshold dari gabungan semua nilai
        all_values_combined <- c(var1_values, var2_values)
        threshold_info <- calculate_threshold_and_successes(
          all_values_combined,
          input$use_custom_threshold,
          input$custom_threshold_value,
          percentile = input$threshold_percentil_shared # Gunakan input percentile dari UI
        )
        
        successes1 <- sum(var1_values > threshold_info$threshold_value, na.rm = TRUE)
        successes2 <- sum(var2_values > threshold_info$threshold_value, na.rm = TRUE)
        trials1 <- length(var1_values)
        trials2 <- length(var2_values)
        
        test_result <- prop.test(c(successes1, successes2), c(trials1, trials2),
                                 alternative = input$alternative, # Gunakan alt_hyp dari 2 sampel
                                 conf.level = 1 - input$alpha)
        test_results$results <- test_result
        
        # Plot perbandingan 2 sampel
        comparison_data <- data.frame(
          Kelompok = c(group1_name, group2_name),
          Proporsi = c(successes1/trials1, successes2/trials2),
          N = c(trials1, trials2),
          Sukses = c(successes1, successes2)
        )
        
        plot_title_suffix <- ""
        if (input$sample_type_2_s == "two_regions_2s") plot_title_suffix <- paste0("Antar Kabupaten/Kota: ", group1_name, " vs ", group2_name)
        if (input$sample_type_2_s == "two_provinces_2s") plot_title_suffix <- paste0("Antar Provinsi: ", group1_name, " vs ", group2_name)
        if (input$sample_type_2_s == "two_custom_groups_2s") plot_title_suffix <- paste0("Berdasarkan ", group_var_name, ": ", group1_name, " vs ", group2_name)
        
        
        test_results$plot <- ggplot(comparison_data, aes(x = Kelompok, y = Proporsi)) +
          geom_col(fill = c("lightblue", "lightcoral"), alpha = 0.7) +
          geom_text(aes(label = paste0(round(Proporsi*100, 1), "%\n(", Sukses, "/", N, " ", unit_type, ")")),
                    vjust = -0.5) +
          labs(title = paste("Perbandingan Proporsi", selected_var_name, plot_title_suffix),
               subtitle = paste("Threshold:", round(threshold_info$threshold_value, 3),
                                "(Persentil", input$threshold_percentil_shared, "%)"),
               y = paste0("Proporsi ", unit_type, " di Atas Threshold")) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Interpretasi 2 Sampel
        prop1_val <- successes1/trials1
        prop2_val <- successes2/trials2
        prop_diff <- prop1_val - prop2_val
        
        test_results$interpretation <- paste(
          "INTERPRETASI UJI PROPORSI DUA SAMPEL:\n",
          "====================================\n\n",
          "KELOMPOK YANG DIBANDINGKAN (Unit analisis: ", unit_type, "):\n",
          "- Kelompok 1 (", group1_name, "):\n",
          "  * Jumlah unit observasi (n₁) =", trials1, "\n",
          "  * Proporsi (p̂₁) =", round(prop1_val, 4), "\n",
          "  * Jumlah sukses =", successes1, "\n",
          "- Kelompok 2 (", group2_name, "):\n",
          "  * Jumlah unit observasi (n₂) =", trials2, "\n",
          "  * Proporsi (p̂₂) =", round(prop2_val, 4), "\n",
          "  * Jumlah sukses =", successes2, "\n",
          "- Perbedaan proporsi (p̂₁ - p̂₂) =", round(prop_diff, 4), "\n\n",
          "METODE ANALISIS PROPORSI:\n",
          "- Variabel:", selected_var_name, "\n",
          "- Threshold (Persentil", input$threshold_percentil_shared, "%) =", round(threshold_info$threshold_value, 3), "\n",
          "- Kriteria sukses: Nilai di atas threshold\n\n",
          "HIPOTESIS:\n",
          "- H₀: p₁ = p₂ (proporsi sama)\n",
          "- H₁: p₁", switch(input$alternative,
                             "two.sided" = "≠",
                             "greater" = ">",
                             "less" = "<"), "p₂\n\n",
          "HASIL UJI STATISTIK:\n",
          "- Chi-square statistik =", round(test_result$statistic, 4), "\n",
          "- p-value =", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", round((1-input$alpha)*100, 1), "%) untuk perbedaan = [",
          paste(round(test_result$conf.int, 3), collapse = ", "), "]\n\n",
          "KESIMPULAN (α =", input$alpha, "):\n",
          if (test_result$p.value < input$alpha) {
            paste("TOLAK H₀. Ada perbedaan proporsi yang signifikan antara",
                  group1_name, "dan", group2_name,
                  "(p-value =", format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("GAGAL TOLAK H₀. Tidak ada perbedaan proporsi yang signifikan antara",
                  group1_name, "dan", group2_name,
                  "(p-value =", format_p_value(test_result$p.value), ">= α =", input$alpha, ")")
          }, "\n\n",
          "INTERPRETASI PRAKTIS:\n",
          if (test_result$p.value < input$alpha) {
            paste("Kelompok", group1_name, "dan", group2_name,
                  "memiliki proporsi", selected_var_name, "yang berbeda secara signifikan.",
                  ifelse(prop_diff > 0,
                         paste("Kelompok", group1_name, "memiliki proporsi yang lebih tinggi."),
                         paste("Kelompok", group2_name, "memiliki proporsi yang lebih tinggi.")))
          } else {
            paste("Tidak ada bukti yang cukup untuk menyimpulkan bahwa kedua kelompok memiliki proporsi",
                  selected_var_name, "yang berbeda.")
          }
        )
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
    
    # Download handlers (sesuaikan nama file dari uji_t_ menjadi uji_proporsi_)
    output$download_results <- downloadHandler(
      filename = function() paste("uji_proporsi_hasil_", Sys.Date(), ".txt", sep=""),
      content = function(file) {
        if (!is.null(test_results$results)) {
          writeLines(capture.output(print(test_results$results)), file)
        }
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() paste("uji_proporsi_plot_", Sys.Date(), ".png", sep=""),
      content = function(file) {
        if (!is.null(test_results$plot)) {
          ggsave(file, test_results$plot, width = 12, height = 8, dpi = 300)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() paste("uji_proporsi_laporan_", Sys.Date(), ".docx", sep=""),
      content = function(file) {
        if (!is.null(test_results$interpretation)) {
          doc <- officer::read_docx()
          doc <- doc %>%
            officer::body_add_par("Laporan Uji Proporsi", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel:", input$variable)) %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type_main)) # Menggunakan input$test_type_main
          
          # Tambahkan detail jenis sampel
          if (input$test_type_main == "one_sample_prop_test") {
            doc <- doc %>% officer::body_add_par(paste("Jenis Sampel (Satu):", input$sample_type_1_s))
          } else if (input$test_type_main == "two_sample_prop_test") {
            doc <- doc %>% officer::body_add_par(paste("Jenis Sampel (Dua):", input$sample_type_2_s))
          }
          
          doc <- doc %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil Uji:", style = "heading 2") %>%
            officer::body_add_par(capture.output(print(test_results$results))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 10, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par("Visualisasi:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 8, height = 5)
            unlink(temp_file)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}