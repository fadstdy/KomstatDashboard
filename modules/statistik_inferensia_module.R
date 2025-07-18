# Statistik Inferensia Module
# modules/statistik_inferensia_module.R

# UI function for Statistik Inferensia
statistikInferensiaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Panel Kontrol Uji Statistik
    column(3,
           box(
             title = "Panel Kontrol Uji Statistik",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Pemilihan Tipe Uji
             selectInput(ns("test_type"), 
                         "Jenis Uji Statistik:",
                         choices = list(
                           "Uji t Satu Sampel" = "one_sample_t",
                           "Uji t Dua Sampel" = "two_sample_t",
                           "Uji Proporsi Satu Sampel" = "one_sample_prop",
                           "Uji Proporsi Dua Sampel" = "two_sample_prop",
                           "Uji Variansi" = "variance_test",
                           "ANOVA Satu Arah" = "anova_one_way",
                           "ANOVA Dua Arah" = "anova_two_way",
                           "Uji Chi-Square" = "chi_square"
                         )),
             
             # Pemilihan Variabel
             selectInput(ns("test_variable"), 
                         "Variabel Uji:",
                         choices = NULL),
             
             # Input Kondisional untuk Uji yang Berbeda
             conditionalPanel(
               condition = "input.test_type == 'one_sample_t'",
               ns = ns,
               numericInput(ns("mu0"), 
                            "Nilai Hipotesis (μ₀):",
                            value = 0)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'two_sample_t'",
               ns = ns,
               selectInput(ns("group_variable"), 
                           "Variabel Pengelompokan:",
                           choices = NULL),
               checkboxInput(ns("equal_var"), 
                             "Asumsi Variansi Sama", 
                             value = TRUE)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'one_sample_prop'",
               ns = ns,
               numericInput(ns("p0"), 
                            "Proporsi Hipotesis (p₀):",
                            value = 0.5,
                            min = 0,
                            max = 1,
                            step = 0.01)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'two_sample_prop'",
               ns = ns,
               selectInput(ns("group_variable_prop"), 
                           "Variabel Pengelompokan:",
                           choices = NULL)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'variance_test'",
               ns = ns,
               selectInput(ns("group_variable_var"), 
                           "Variabel Pengelompokan:",
                           choices = NULL)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'anova_one_way'",
               ns = ns,
               selectInput(ns("group_variable_anova"), 
                           "Variabel Pengelompokan:",
                           choices = NULL)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'anova_two_way'",
               ns = ns,
               selectInput(ns("factor1"), 
                           "Faktor 1:",
                           choices = NULL),
               selectInput(ns("factor2"), 
                           "Faktor 2:",
                           choices = NULL),
               checkboxInput(ns("interaction"), 
                             "Sertakan Interaksi", 
                             value = TRUE)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'chi_square'",
               ns = ns,
               selectInput(ns("var1_chi"), 
                           "Variabel 1:",
                           choices = NULL),
               selectInput(ns("var2_chi"), 
                           "Variabel 2:",
                           choices = NULL)
             ),
             
             # Hipotesis Alternatif
             conditionalPanel(
               condition = "input.test_type == 'one_sample_t' || input.test_type == 'two_sample_t' || input.test_type == 'one_sample_prop' || input.test_type == 'two_sample_prop'",
               ns = ns,
               selectInput(ns("alternative"), 
                           "Hipotesis Alternatif:",
                           choices = list(
                             "Dua arah (≠)" = "two.sided",
                             "Lebih besar (>)" = "greater",
                             "Lebih kecil (<)" = "less"
                           ))
             ),
             
             # Tingkat Signifikansi
             numericInput(ns("alpha"), 
                          "Tingkat Signifikansi (α):",
                          value = 0.05,
                          min = 0.01,
                          max = 0.1,
                          step = 0.01),
             
             # Tingkat Kepercayaan
             numericInput(ns("conf_level"), 
                          "Tingkat Kepercayaan:",
                          value = 0.95,
                          min = 0.8,
                          max = 0.99,
                          step = 0.01),
             
             # Tombol Aksi
             br(),
             actionButton(ns("run_test"), 
                          "Jalankan Uji", 
                          class = "btn-success"),
             br(), br(),
             
             # Opsi Download
             h5("Download Hasil:"),
             downloadButton(ns("download_plot"), 
                            "Download Plot", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_results"), 
                            "Download Hasil", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Konten Utama
    column(9,
           # Hasil Uji Statistik
           box(
             title = "Hasil Uji Statistik",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           # Visualisasi
           box(
             title = "Visualisasi Hasil",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Plot Utama",
                        br(),
                        withSpinner(plotOutput(ns("main_plot"), height = "400px"))),
               
               tabPanel("Distribusi",
                        br(),
                        withSpinner(plotOutput(ns("distribution_plot"), height = "400px"))),
               
               tabPanel("Confidence Interval",
                        br(),
                        withSpinner(plotOutput(ns("ci_plot"), height = "400px")))
             )
           ),
           
           # Statistik Deskriptif
           box(
             title = "Statistik Deskriptif",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(tableOutput(ns("descriptive_stats")))
           ),
           
           # Analisis Tambahan
           box(
             title = "Analisis Tambahan",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel(
               tabPanel("Effect Size",
                        br(),
                        withSpinner(tableOutput(ns("effect_size")))),
               
               tabPanel("Power Analysis",
                        br(),
                        withSpinner(verbatimTextOutput(ns("power_analysis")))),
               
               tabPanel("Post-hoc Tests",
                        br(),
                        withSpinner(tableOutput(ns("posthoc_tests"))))
             )
           )
    ),
    
    # Bagian Interpretasi
    column(12,
           box(
             title = "Interpretasi dan Kesimpulan",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Fungsi Server untuk Statistik Inferensia
statistikInferensiaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Perbarui pilihan variabel
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      updateSelectInput(session, "test_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "group_variable", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "group_variable_prop", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "group_variable_var", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "group_variable_anova", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "factor1", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "factor2", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "var1_chi", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "var2_chi", 
                        choices = setNames(categorical_vars, categorical_vars))
    })
    
    # Nilai reaktif untuk hasil uji
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      descriptive_stats = NULL,
      effect_size = NULL,
      power_analysis = NULL,
      posthoc = NULL
    )
    
    # Jalankan Uji Statistik
    observeEvent(input$run_test, {
      req(input$test_variable)
      
      data <- values$current_data
      
      if (input$test_type == "one_sample_t") {
        # Uji t Satu Sampel
        variable <- data[[input$test_variable]]
        test_result <- t.test(variable, mu = input$mu0, 
                              alternative = input$alternative,
                              conf.level = input$conf_level)
        test_results$results <- test_result
        
        # Buat visualisasi
        test_results$plot <- ggplot(data, aes(x = .data[[input$test_variable]])) +
          geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
          geom_vline(xintercept = mean(variable, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = input$mu0, color = "blue", linetype = "dashed", linewidth = 1) +
          labs(title = paste("One-Sample t-test:", input$test_variable),
               subtitle = paste("Merah: Rata-rata Sampel, Biru: Rata-rata Hipotesis =", input$mu0),
               x = input$test_variable, y = "Frekuensi") +
          theme_custom()
        
        # Hitung ukuran efek (Cohen's d)
        effect_size <- abs(mean(variable, na.rm = TRUE) - input$mu0) / sd(variable, na.rm = TRUE)
        test_results$effect_size <- data.frame(
          Measure = "Cohen's d",
          Value = round(effect_size, 3),
          Interpretation = case_when(
            effect_size < 0.2 ~ "Efek kecil",
            effect_size < 0.5 ~ "Efek sedang",
            effect_size < 0.8 ~ "Efek besar",
            TRUE ~ "Efek sangat besar"
          )
        )
        
        # Hasilkan interpretasi
        interpretation <- paste(
          "INTERPRETASI UJI t SATU SAMPEL:\n",
          "===============================\n\n",
          "Hipotesis:\n",
          "H0: μ =", input$mu0, "\n",
          "H1: μ", switch(input$alternative, 
                          "two.sided" = "≠", 
                          "greater" = ">", 
                          "less" = "<"), input$mu0, "\n\n",
          "Hasil:\n",
          "- t-statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", input$conf_level * 100, "%):", 
          paste(round(test_result$conf.int, 3), collapse = " - "), "\n",
          "- Rata-rata Sampel:", round(test_result$estimate, 3), "\n\n",
          "Kesimpulan:\n",
          interpret_t_test(test_result$p.value, input$alternative, input$alpha), "\n\n",
          "Ukuran Efek:\n",
          "- Cohen's d:", round(effect_size, 3), "\n",
          "- Interpretasi:", test_results$effect_size$Interpretation
        )
        
      } else if (input$test_type == "two_sample_t") {
        req(input$group_variable)
        
        # Uji t Dua Sampel
        formula_str <- paste(input$test_variable, "~", input$group_variable)
        test_result <- t.test(as.formula(formula_str), data = data,
                              var.equal = input$equal_var,
                              alternative = input$alternative,
                              conf.level = input$conf_level)
        test_results$results <- test_result
        
        # Buat visualisasi
        test_results$plot <- ggplot(data, aes(x = .data[[input$group_variable]], 
                                              y = .data[[input$test_variable]])) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
          labs(title = paste("Two-Sample t-test:", input$test_variable, "berdasarkan", input$group_variable),
               x = input$group_variable, y = input$test_variable) +
          theme_custom()
        
        # Hitung ukuran efek (Cohen's d)
        group_stats <- data %>%
          group_by(.data[[input$group_variable]]) %>%
          summarise(mean = mean(.data[[input$test_variable]], na.rm = TRUE),
                    sd = sd(.data[[input$test_variable]], na.rm = TRUE),
                    n = n(),
                    .groups = 'drop')
        
        if (nrow(group_stats) == 2) {
          pooled_sd <- sqrt(((group_stats$n[1] - 1) * group_stats$sd[1]^2 + 
                               (group_stats$n[2] - 1) * group_stats$sd[2]^2) / 
                              (group_stats$n[1] + group_stats$n[2] - 2))
          effect_size <- abs(group_stats$mean[1] - group_stats$mean[2]) / pooled_sd
          
          test_results$effect_size <- data.frame(
            Measure = "Cohen's d",
            Value = round(effect_size, 3),
            Interpretation = case_when(
              effect_size < 0.2 ~ "Efek kecil",
              effect_size < 0.5 ~ "Efek sedang",
              effect_size < 0.8 ~ "Efek besar",
              TRUE ~ "Efek sangat besar"
            )
          )
        }
        
        # Hasilkan interpretasi
        interpretation <- paste(
          "INTERPRETASI UJI t DUA SAMPEL:\n",
          "==============================\n\n",
          "Hipotesis:\n",
          "H0: μ₁ = μ₂\n",
          "H1: μ₁", switch(input$alternative, 
                           "two.sided" = "≠", 
                           "greater" = ">", 
                           "less" = "<"), "μ₂\n\n",
          "Hasil:\n",
          "- t-statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", input$conf_level * 100, "%):", 
          paste(round(test_result$conf.int, 3), collapse = " - "), "\n",
          "- Perbedaan Rata-rata:", round(diff(test_result$estimate), 3), "\n\n",
          "Kesimpulan:\n",
          interpret_t_test(test_result$p.value, input$alternative, input$alpha), "\n\n",
          "Ukuran Efek:\n",
          if (!is.null(test_results$effect_size)) {
            paste("- Cohen's d:", test_results$effect_size$Value, "\n",
                  "- Interpretasi:", test_results$effect_size$Interpretation)
          } else {
            "- Tidak dapat dihitung"
          }
        )
        
      } else if (input$test_type == "one_sample_prop") {
        req(input$test_variable)
        
        variable <- data[[input$test_variable]]
        
        # Validasi variabel untuk uji proporsi (harus biner atau logis)
        if (!is.numeric(variable) || length(unique(na.omit(variable))) > 2) {
          showNotification("Variabel uji untuk uji proporsi satu sampel harus biner (misal: 0/1 atau TRUE/FALSE).", type = "error")
          return()
        }
        if (sum(variable == 0 | variable == 1, na.rm = TRUE) != length(na.omit(variable))) {
          showNotification("Variabel uji untuk uji proporsi satu sampel harus berupa 0 dan 1.", type = "error")
          return()
        }
        
        # Hitung sukses (1s) dan total observasi
        x <- sum(variable == 1, na.rm = TRUE)
        n <- sum(!is.na(variable))
        
        if (n == 0) {
          showNotification("Tidak ada observasi yang valid untuk variabel uji.", type = "error")
          return()
        }
        
        # Uji Proporsi Satu Sampel
        test_result <- prop.test(x, n, p = input$p0, 
                                 alternative = input$alternative, 
                                 conf.level = input$conf_level)
        test_results$results <- test_result
        
        # Buat visualisasi (Bar plot proporsi)
        prop_df <- data.frame(
          Category = c("Sukses", "Gagal"),
          Proportion = c(x/n, 1 - x/n)
        )
        
        test_results$plot <- ggplot(prop_df, aes(x = Category, y = Proportion, fill = Category)) +
          geom_bar(stat = "identity", alpha = 0.7, color = "black") +
          labs(title = paste("Proporsi:", input$test_variable),
               subtitle = paste("Proporsi Sampel =", round(x/n, 3), ", Proporsi Hipotesis =", input$p0),
               x = "Kategori", y = "Proporsi") +
          scale_fill_manual(values = c("Sukses" = "steelblue", "Gagal" = "lightcoral")) +
          theme_custom()
        
        # Hitung ukuran efek (h)
        prop_sample <- x / n
        effect_size_h <- 2 * asin(sqrt(prop_sample)) - 2 * asin(sqrt(input$p0))
        test_results$effect_size <- data.frame(
          Measure = "Cohen's h",
          Value = round(abs(effect_size_h), 3),
          Interpretation = case_when(
            abs(effect_size_h) < 0.2 ~ "Efek kecil",
            abs(effect_size_h) < 0.5 ~ "Efek sedang",
            abs(effect_size_h) < 0.8 ~ "Efek besar",
            TRUE ~ "Efek sangat besar"
          )
        )
        
        # Hasilkan interpretasi
        interpretation <- paste(
          "INTERPRETASI UJI PROPORSI SATU SAMPEL:\n",
          "=====================================\n\n",
          "Hipotesis:\n",
          "H0: p =", input$p0, "\n",
          "H1: p", switch(input$alternative, 
                          "two.sided" = "≠", 
                          "greater" = ">", 
                          "less" = "<"), input$p0, "\n\n",
          "Hasil:\n",
          "- Chi-square statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", input$conf_level * 100, "%):", 
          paste(round(test_result$conf.int, 3), collapse = " - "), "\n",
          "- Proporsi Sampel:", round(test_result$estimate, 3), "\n\n",
          "Kesimpulan:\n",
          interpret_prop_test(test_result$p.value, input$alternative, input$alpha), "\n\n",
          "Ukuran Efek:\n",
          "- Cohen's h:", test_results$effect_size$Value, "\n",
          "- Interpretasi:", test_results$effect_size$Interpretation
        )
        
      } else if (input$test_type == "two_sample_prop") {
        req(input$test_variable, input$group_variable_prop)
        
        variable <- data[[input$test_variable]]
        group_var <- data[[input$group_variable_prop]]
        
        # Validasi variabel untuk uji proporsi (harus biner atau logis)
        if (!is.numeric(variable) || length(unique(na.omit(variable))) > 2) {
          showNotification("Variabel uji untuk uji proporsi dua sampel harus biner (misal: 0/1 atau TRUE/FALSE).", type = "error")
          return()
        }
        if (sum(variable == 0 | variable == 1, na.rm = TRUE) != length(na.omit(variable))) {
          showNotification("Variabel uji untuk uji proporsi dua sampel harus berupa 0 dan 1.", type = "error")
          return()
        }
        
        # Pastikan variabel kelompok memiliki tepat dua level
        unique_groups <- unique(na.omit(group_var))
        if (length(unique_groups) != 2) {
          showNotification("Variabel pengelompokan harus memiliki tepat dua kategori.", type = "error")
          return()
        }
        
        # Hitung sukses dan percobaan untuk setiap kelompok
        x_group1 <- sum(variable[group_var == unique_groups[1]] == 1, na.rm = TRUE)
        n_group1 <- sum(!is.na(variable[group_var == unique_groups[1]]))
        
        x_group2 <- sum(variable[group_var == unique_groups[2]] == 1, na.rm = TRUE)
        n_group2 <- sum(!is.na(variable[group_var == unique_groups[2]]))
        
        if (n_group1 == 0 || n_group2 == 0) {
          showNotification("Tidak ada observasi yang valid di salah satu kelompok.", type = "error")
          return()
        }
        
        x_vals <- c(x_group1, x_group2)
        n_vals <- c(n_group1, n_group2)
        
        # Uji Proporsi Dua Sampel
        test_result <- prop.test(x_vals, n_vals, 
                                 alternative = input$alternative, 
                                 conf.level = input$conf_level)
        test_results$results <- test_result
        
        # Buat visualisasi (Bar plot Berkelompok)
        prop_df <- data.frame(
          Group = rep(unique_groups, each = 2),
          Category = rep(c("Sukses", "Gagal"), times = 2),
          Count = c(x_group1, n_group1 - x_group1, x_group2, n_group2 - x_group2),
          Proportion = c(x_group1/n_group1, (n_group1 - x_group1)/n_group1, 
                         x_group2/n_group2, (n_group2 - x_group2)/n_group2)
        )
        
        test_results$plot <- ggplot(prop_df, aes(x = Group, y = Proportion, fill = Category)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7, color = "black") +
          labs(title = paste("Perbandingan Proporsi:", input$test_variable, "berdasarkan", input$group_variable_prop),
               x = input$group_variable_prop, y = "Proporsi") +
          scale_fill_manual(values = c("Sukses" = "steelblue", "Gagal" = "lightcoral")) +
          theme_custom()
        
        # Hitung ukuran efek (Cohen's h)
        prop_group1 <- x_group1 / n_group1
        prop_group2 <- x_group2 / n_group2
        effect_size_h <- 2 * asin(sqrt(prop_group1)) - 2 * asin(sqrt(prop_group2))
        test_results$effect_size <- data.frame(
          Measure = "Cohen's h",
          Value = round(abs(effect_size_h), 3),
          Interpretation = case_when(
            abs(effect_size_h) < 0.2 ~ "Efek kecil",
            abs(effect_size_h) < 0.5 ~ "Efek sedang",
            abs(effect_size_h) < 0.8 ~ "Efek besar",
            TRUE ~ "Efek sangat besar"
          )
        )
        
        # Hasilkan interpretasi
        interpretation <- paste(
          "INTERPRETASI UJI PROPORSI DUA SAMPEL:\n",
          "====================================\n\n",
          "Hipotesis:\n",
          "H0: p₁ = p₂\n",
          "H1: p₁", switch(input$alternative, 
                           "two.sided" = "≠", 
                           "greater" = ">", 
                           "less" = "<"), "p₂\n\n",
          "Hasil:\n",
          "- Chi-square statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n",
          "- Interval Kepercayaan (", input$conf_level * 100, "%):", 
          paste(round(test_result$conf.int, 3), collapse = " - "), "\n",
          "- Proporsi Sampel (", unique_groups[1], " vs ", unique_groups[2], "): ", 
          paste(round(test_result$estimate, 3), collapse = " vs "), "\n\n",
          "Kesimpulan:\n",
          interpret_prop_test(test_result$p.value, input$alternative, input$alpha), "\n\n",
          "Ukuran Efek:\n",
          "- Cohen's h:", test_results$effect_size$Value, "\n",
          "- Interpretasi:", test_results$effect_size$Interpretation
        )
        
      } else if (input$test_type == "anova_one_way") {
        req(input$group_variable_anova)
        
        # Validasi data sebelum ANOVA
        if (!input$group_variable_anova %in% names(data)) {
          showNotification("Variabel pengelompokan tidak ditemukan", type = "error")
          return()
        }
        
        # Hapus nilai yang hilang
        complete_data <- data[complete.cases(data[[input$test_variable]], data[[input$group_variable_anova]]), ]
        
        if (nrow(complete_data) < 3) {
          showNotification("Data tidak cukup untuk ANOVA", type = "error")
          return()
        }
        
        # Periksa apakah ada setidaknya 2 kelompok
        groups <- unique(complete_data[[input$group_variable_anova]])
        if (length(groups) < 2) {
          showNotification("Minimal harus ada 2 kelompok untuk ANOVA", type = "error")
          return()
        }
        
        # ANOVA Satu Arah
        formula_str <- paste(input$test_variable, "~", input$group_variable_anova)
        anova_result <- aov(as.formula(formula_str), data = complete_data)
        anova_summary <- summary(anova_result)
        test_results$results <- anova_summary
        
        # Buat visualisasi
        test_results$plot <- ggplot(complete_data, aes(x = .data[[input$group_variable_anova]], 
                                                       y = .data[[input$test_variable]])) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
          labs(title = paste("One-Way ANOVA:", input$test_variable, "berdasarkan", input$group_variable_anova),
               x = input$group_variable_anova, y = input$test_variable) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Hitung eta-squared (ukuran efek) - dengan pemeriksaan error
        tryCatch({
          ss_total <- sum((complete_data[[input$test_variable]] - mean(complete_data[[input$test_variable]], na.rm = TRUE))^2, na.rm = TRUE)
          
          # Periksa apakah hasil ANOVA valid
          if (length(anova_summary) > 0 && "Sum Sq" %in% names(anova_summary[[1]])) {
            ss_between <- anova_summary[[1]][["Sum Sq"]][1]
            eta_squared <- ss_between / ss_total
            
            test_results$effect_size <- data.frame(
              Measure = "Eta-squared",
              Value = round(eta_squared, 3),
              Interpretation = case_when(
                eta_squared < 0.01 ~ "Efek kecil",
                eta_squared < 0.06 ~ "Efek sedang",
                eta_squared < 0.14 ~ "Efek besar",
                TRUE ~ "Efek sangat besar"
              )
            )
          } else {
            test_results$effect_size <- data.frame(
              Measure = "Eta-squared",
              Value = NA,
              Interpretation = "Tidak dapat dihitung"
            )
          }
        }, error = function(e) {
          test_results$effect_size <- data.frame(
            Measure = "Eta-squared",
            Value = NA,
            Interpretation = "Tidak dapat dihitung"
          )
        })
        
        # Uji post-hoc jika signifikan - dengan pemeriksaan error
        tryCatch({
          if (length(anova_summary) > 0 && "Pr(>F)" %in% names(anova_summary[[1]])) {
            p_value <- anova_summary[[1]][["Pr(>F)"]][1]
            if (!is.na(p_value) && length(p_value) > 0 && p_value < input$alpha) {
              posthoc_result <- pairwise.t.test(complete_data[[input$test_variable]], 
                                                complete_data[[input$group_variable_anova]], 
                                                p.adjust.method = "bonferroni")
              test_results$posthoc <- posthoc_result
            }
          }
        }, error = function(e) {
          # Uji post-hoc gagal, lanjutkan tanpa itu
        })
        
        # Hasilkan interpretasi - dengan pemeriksaan error
        tryCatch({
          if (length(anova_summary) > 0 && all(c("F value", "Df", "Pr(>F)") %in% names(anova_summary[[1]]))) {
            f_value <- anova_summary[[1]][["F value"]][1]
            df1 <- anova_summary[[1]][["Df"]][1]
            df2 <- anova_summary[[1]][["Df"]][2]
            p_value <- anova_summary[[1]][["Pr(>F)"]][1]
            
            interpretation <- paste(
              "INTERPRETASI ANOVA SATU ARAH:\n",
              "=============================\n\n",
              "Hipotesis:\n",
              "H0: μ₁ = μ₂ = μ₃ = ... (semua rata-rata sama)\n",
              "H1: Minimal ada satu rata-rata yang berbeda\n\n",
              "Hasil:\n",
              "- F-statistik:", round(f_value, 4), "\n",
              "- df:", df1, ",", df2, "\n",
              "- p-value:", format_p_value(p_value), "\n\n",
              "Kesimpulan:\n",
              interpret_anova(p_value, input$alpha), "\n\n",
              "Ukuran Efek:\n",
              "- Eta-squared:", ifelse(is.na(test_results$effect_size$Value), "Tidak dapat dihitung", test_results$effect_size$Value), "\n",
              "- Interpretasi:", test_results$effect_size$Interpretation, "\n\n",
              if (!is.na(p_value) && p_value < input$alpha) {
                "Rekomendasi:\n- Lakukan uji post-hoc untuk mengetahui kelompok mana yang berbeda\n- Periksa asumsi normalitas dan homogenitas"
              } else {
                "Rekomendasi:\n- Tidak ada perbedaan signifikan antar kelompok\n- Pertimbangkan faktor lain yang mungkin berpengaruh"
              }
            )
          } else {
            interpretation <- "Error: Tidak dapat memproses hasil ANOVA"
          }
        }, error = function(e) {
          interpretation <- paste("Error dalam interpretasi ANOVA:", e$message)
        })
        
      } else if (input$test_type == "chi_square") {
        req(input$var1_chi, input$var2_chi)
        
        # Uji Chi-Square Independensi
        cont_table <- table(data[[input$var1_chi]], data[[input$var2_chi]])
        test_result <- chisq.test(cont_table)
        test_results$results <- test_result
        
        # Buat visualisasi
        cont_df <- as.data.frame(cont_table)
        names(cont_df) <- c("Var1", "Var2", "Freq")
        
        test_results$plot <- ggplot(cont_df, aes(x = Var1, y = Var2, fill = Freq)) +
          geom_tile() +
          geom_text(aes(label = Freq), color = "white", size = 4) +
          scale_fill_gradient(low = "lightblue", high = "darkblue") +
          labs(title = paste("Uji Chi-Square:", input$var1_chi, "vs", input$var2_chi),
               x = input$var1_chi, y = input$var2_chi) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Hitung Cramer's V (ukuran efek)
        n <- sum(cont_table)
        cramers_v <- sqrt(test_result$statistic / (n * (min(nrow(cont_table), ncol(cont_table)) - 1)))
        
        test_results$effect_size <- data.frame(
          Measure = "Cramer's V",
          Value = round(cramers_v, 3),
          Interpretation = case_when(
            cramers_v < 0.1 ~ "Efek kecil",
            cramers_v < 0.3 ~ "Efek sedang",
            cramers_v < 0.5 ~ "Efek besar",
            TRUE ~ "Efek sangat besar"
          )
        )
        
        # Hasilkan interpretasi
        interpretation <- paste(
          "INTERPRETASI UJI CHI-SQUARE:\n",
          "============================\n\n",
          "Hipotesis:\n",
          "H0: Tidak ada asosiasi antara", input$var1_chi, "dan", input$var2_chi, "\n",
          "H1: Ada asosiasi antara", input$var1_chi, "dan", input$var2_chi, "\n\n",
          "Hasil:\n",
          "- Chi-square statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n\n",
          "Kesimpulan:\n",
          if (test_result$p.value < input$alpha) {
            paste("Terdapat asosiasi yang signifikan (p-value =", 
                  format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("Tidak terdapat asosiasi yang signifikan (p-value =", 
                  format_p_value(test_result$p.value), "> α =", input$alpha, ")")
          }, "\n\n",
          "Ukuran Efek:\n",
          "- Cramer's V:", test_results$effect_size$Value, "\n",
          "- Interpretasi:", test_results$effect_size$Interpretation
        )
      } else if (input$test_type == "variance_test") {
        req(input$test_variable, input$group_variable_var)
        
        variable <- data[[input$test_variable]]
        group_var <- data[[input$group_variable_var]]
        
        # Hapus nilai NA
        complete_data <- data.frame(Variable = variable, Group = group_var)
        complete_data <- complete_data[complete.cases(complete_data), ]
        
        if (nrow(complete_data) == 0) {
          showNotification("Tidak ada data lengkap untuk melakukan uji variansi.", type = "error")
          return()
        }
        
        # Pastikan variabel kelompok memiliki setidaknya dua level
        unique_groups <- unique(complete_data$Group)
        if (length(unique_groups) < 2) {
          showNotification("Variabel pengelompokan harus memiliki minimal dua kategori untuk uji variansi.", type = "error")
          return()
        }
        
        # Uji Homogenitas Variansi Levene
        levene_test <- car::leveneTest(Value ~ Group, data = complete_data)
        test_results$results <- levene_test
        
        # Buat visualisasi (Boxplot untuk menunjukkan sebaran variansi)
        test_results$plot <- ggplot(complete_data, aes(x = Group, y = Variable)) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7) +
          labs(title = paste("Visualisasi Variansi:", input$test_variable, "berdasarkan", input$group_variable_var),
               x = input$group_variable_var, y = input$test_variable) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Hasilkan interpretasi
        levene_p_value <- levene_test$`Pr(>F)`[1]
        interpretation <- paste(
          "INTERPRETASI UJI VARIANSI (LEVENES TEST):\n",
          "=======================================\n\n",
          "Hipotesis:\n",
          "H0: Variansi antar kelompok adalah sama (homogen)\n",
          "H1: Minimal ada satu variansi kelompok yang berbeda (heterogen)\n\n",
          "Hasil Levene's Test:\n",
          "- F-statistik:", round(levene_test$`F value`[1], 4), "\n",
          "- df1:", levene_test$Df[1], "\n",
          "- df2:", levene_test$Df[2], "\n",
          "- p-value:", format_p_value(levene_p_value), "\n\n",
          "Kesimpulan:\n",
          if (levene_p_value < input$alpha) {
            paste("Variansi antar kelompok tidak homogen (p-value =", format_p_value(levene_p_value), "< α =", input$alpha, "). Asumsi homogenitas variansi dilanggar.")
          } else {
            paste("Variansi antar kelompok adalah homogen (p-value =", format_p_value(levene_p_value), "> α =", input$alpha, "). Asumsi homogenitas variansi terpenuhi.")
          }, "\n\n",
          "Rekomendasi:\n",
          if (levene_p_value < input$alpha) {
            "Jika asumsi homogenitas variansi dilanggar, pertimbangkan untuk menggunakan uji statistik yang tidak mengasumsikan variansi yang sama (misalnya Welch's t-test) atau transformasi data."
          } else {
            "Anda dapat melanjutkan dengan uji statistik yang mengasumsikan homogenitas variansi (misalnya t-test standar atau ANOVA)."
          }
        )
      } else if (input$test_type == "anova_two_way") {
        req(input$test_variable, input$factor1, input$factor2)
        
        factor1_var <- data[[input$factor1]]
        factor2_var <- data[[input$factor2]]
        response_var <- data[[input$test_variable]]
        
        complete_data <- data.frame(Response = response_var, Factor1 = factor1_var, Factor2 = factor2_var)
        complete_data <- complete_data[complete.cases(complete_data), ]
        
        if (nrow(complete_data) == 0) {
          showNotification("Tidak ada data lengkap untuk melakukan ANOVA dua arah.", type = "error")
          return()
        }
        
        # Pastikan faktor memiliki setidaknya dua level masing-masing
        if (length(unique(complete_data$Factor1)) < 2 || length(unique(complete_data$Factor2)) < 2) {
          showNotification("Kedua faktor harus memiliki minimal dua kategori.", type = "error")
          return()
        }
        
        # ANOVA Dua Arah
        if (input$interaction) {
          formula_str <- paste("Response ~ Factor1 * Factor2")
        } else {
          formula_str <- paste("Response ~ Factor1 + Factor2")
        }
        
        anova_result <- aov(as.formula(formula_str), data = complete_data)
        anova_summary <- summary(anova_result)
        test_results$results <- anova_summary
        
        # Buat visualisasi (Plot interaksi atau boxplot berkelompok)
        test_results$plot <- ggplot(complete_data, aes(x = Factor1, y = Response, fill = Factor2)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("ANOVA Dua Arah:", input$test_variable, "berdasarkan", input$factor1, "dan", input$factor2),
               x = input$factor1, y = input$test_variable) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Hasilkan interpretasi
        interpretation <- paste(
          "INTERPRETASI ANOVA DUA ARAH:\n",
          "===========================\n\n",
          "Hipotesis:\n",
          "H0 (Faktor 1): Tidak ada efek utama dari", input$factor1, "pada", input$test_variable, "\n",
          "H0 (Faktor 2): Tidak ada efek utama dari", input$factor2, "pada", input$test_variable, "\n",
          if (input$interaction) paste("H0 (Interaksi): Tidak ada efek interaksi antara", input$factor1, "dan", input$factor2, "\n"),
          "\n",
          "Hasil:\n"
        )
        
        # Ekstrak p-value dan tambahkan ke interpretasi
        p_value_factor1 <- anova_summary[[1]]$`Pr(>F)`[1]
        p_value_factor2 <- anova_summary[[1]]$`Pr(>F)`[2]
        
        interpretation <- paste(interpretation,
                                "- P-value (Efek Utama", input$factor1, "):", format_p_value(p_value_factor1), "\n",
                                "- P-value (Efek Utama", input$factor2, "):", format_p_value(p_value_factor2), "\n"
        )
        
        if (input$interaction) {
          p_value_interaction <- anova_summary[[1]]$`Pr(>F)`[3]
          interpretation <- paste(interpretation,
                                  "- P-value (Efek Interaksi):", format_p_value(p_value_interaction), "\n"
          )
        }
        interpretation <- paste(interpretation, "\nKesimpulan:\n")
        
        if (input$interaction) {
          if (p_value_interaction < input$alpha) {
            interpretation <- paste(interpretation,
                                    "- Terdapat efek interaksi yang signifikan antara", input$factor1, "dan", input$factor2, "pada", input$test_variable, ". Efek dari satu faktor bergantung pada level faktor lainnya.\n"
            )
          } else {
            interpretation <- paste(interpretation,
                                    "- Tidak ada efek interaksi yang signifikan. Efek dari satu faktor tidak bergantung pada level faktor lainnya.\n"
            )
          }
        }
        
        if (p_value_factor1 < input$alpha) {
          interpretation <- paste(interpretation,
                                  "- Terdapat efek utama yang signifikan dari", input$factor1, "pada", input$test_variable, ".\n"
          )
        } else {
          interpretation <- paste(interpretation,
                                  "- Tidak ada efek utama yang signifikan dari", input$factor1, "pada", input$test_variable, ".\n"
          )
        }
        
        if (p_value_factor2 < input$alpha) {
          interpretation <- paste(interpretation,
                                  "- Terdapat efek utama yang signifikan dari", input$factor2, "pada", input$test_variable, ".\n"
          )
        } else {
          interpretation <- paste(interpretation,
                                  "- Tidak ada efek utama yang signifikan dari", input$factor2, "pada", input$test_variable, ".\n"
          )
        }
        
        interpretation <- paste(interpretation, "\n", "Rekomendasi:\n",
                                "Periksa plot interaksi untuk memahami efek antar faktor. Jika efek interaksi signifikan, interpretasi efek utama mungkin tidak relevan. Lakukan uji post-hoc jika ada efek signifikan."
        )
      }
      
      test_results$interpretation <- interpretation
      
      # Hitung statistik deskriptif
      if (input$test_type %in% c("one_sample_t", "one_sample_prop")) {
        test_results$descriptive_stats <- create_summary_table(data, input$test_variable)
      } else if (input$test_type %in% c("two_sample_t", "two_sample_prop", "anova_one_way", "variance_test")) {
        group_var_name <- switch(input$test_type,
                                 "two_sample_t" = input$group_variable,
                                 "two_sample_prop" = input$group_variable_prop,
                                 "anova_one_way" = input$group_variable_anova,
                                 "variance_test" = input$group_variable_var)
        
        test_results$descriptive_stats <- data %>%
          group_by(.data[[group_var_name]]) %>%
          summarise(
            N = n(),
            Mean = round(mean(.data[[input$test_variable]], na.rm = TRUE), 3),
            SD = round(sd(.data[[input$test_variable]], na.rm = TRUE), 3),
            SE = round(sd(.data[[input$test_variable]], na.rm = TRUE) / sqrt(n()), 3),
            Min = round(min(.data[[input$test_variable]], na.rm = TRUE), 3),
            Max = round(max(.data[[input$test_variable]], na.rm = TRUE), 3),
            .groups = 'drop'
          )
      } else if (input$test_type == "chi_square") {
        test_results$descriptive_stats <- as.data.frame(table(data[[input$var1_chi]], data[[input$var2_chi]]))
        colnames(test_results$descriptive_stats) <- c(input$var1_chi, input$var2_chi, "Frekuensi")
      } else if (input$test_type == "anova_two_way") {
        test_results$descriptive_stats <- data %>%
          group_by(.data[[input$factor1]], .data[[input$factor2]]) %>%
          summarise(
            N = n(),
            Mean = round(mean(.data[[input$test_variable]], na.rm = TRUE), 3),
            SD = round(sd(.data[[input$test_variable]], na.rm = TRUE), 3),
            SE = round(sd(.data[[input$test_variable]], na.rm = TRUE) / sqrt(n()), 3),
            .groups = 'drop'
          )
      }
    })
    
    # Tampilkan Hasil Uji
    output$test_results <- renderPrint({ 
      req(test_results$results)
      test_results$results
    })
    
    # Tampilkan plot utama
    output$main_plot <- renderPlot({
      req(test_results$plot)
      test_results$plot
    })
    
    # Tampilkan statistik deskriptif
    output$descriptive_stats <- renderTable({
      req(test_results$descriptive_stats)
      test_results$descriptive_stats
    })
    
    # Tampilkan ukuran efek
    output$effect_size <- renderTable({
      req(test_results$effect_size)
      test_results$effect_size
    })
    
    # Tampilkan interpretasi
    output$interpretation <- renderText({
      req(test_results$interpretation)
      test_results$interpretation
    })
    
    # Tampilkan uji post-hoc
    output$posthoc_tests <- renderTable({
      if (!is.null(test_results$posthoc)) {
        if (input$test_type == "anova_one_way") {
          # Format hasil pairwise t-test
          posthoc_matrix <- test_results$posthoc$p.value
          
          # Konversi matriks ke format panjang untuk tampilan yang lebih baik
          posthoc_df <- as.data.frame(as.table(posthoc_matrix))
          colnames(posthoc_df) <- c("Grup1", "Grup2", "P_value")
          posthoc_df <- posthoc_df[!is.na(posthoc_df$P_value), ] # Hapus perbandingan NA
          
          # Tambah kolom untuk signifikansi
          posthoc_df$Significant <- ifelse(posthoc_df$P_value < input$alpha, "Ya", "Tidak")
          
          return(posthoc_df)
        }
      } else {
        return(data.frame(Info = "Tidak ada uji post-hoc yang dilakukan"))
      }
    })
    
    # Handler Download
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("plot_statistik_inferensia_", input$test_type, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$plot)) {
          ggsave(file, test_results$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste("hasil_statistik_inferensia_", input$test_type, "_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$results)) {
          writeLines(capture.output(print(test_results$results)), file)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_statistik_inferensia_", input$test_type, "_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$interpretation)) {
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Statistik Inferensia", style = "heading 1") %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil dan Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 8, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par("Visualisasi:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 6, height = 4)
            unlink(temp_file)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}