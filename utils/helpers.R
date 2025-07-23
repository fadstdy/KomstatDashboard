# Helper Functions
# utils/helpers.R

# Fungsi untuk menampilkan notifikasi
show_notification <- function(message, type = "default") {
  # Tipe valid untuk showNotification: "default", "message", "warning", "error"
  valid_types <- c("default", "message", "warning", "error")
  
  # Konversi tipe umum ke tipe valid
  if (type == "success") type <- "message"
  if (type == "info") type <- "message"
  if (type == "danger") type <- "error"
  
  # Pastikan tipe valid
  if (!type %in% valid_types) type <- "default"
  
  showNotification(message, type = type)
}

# Fungsi untuk membuat handler download plot
create_plot_download <- function(plot_obj, filename) {
  downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot_obj, width = 10, height = 6, dpi = 300)
    }
  )
}

# Fungsi untuk membuat handler download tabel
create_table_download <- function(table_data, filename) {
  downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(table_data, file, row.names = FALSE)
    }
  )
}

# Fungsi untuk membuat dokumen Word dengan hasil
create_word_report <- function(title, content_list, filename) {
  doc <- officer::read_docx()
  
  # Tambah judul
  doc <- doc %>%
    officer::body_add_par(title, style = "heading 1") %>%
    officer::body_add_par(" ")
  
  # Tambah konten
  for (item in content_list) {
    if (item$type == "text") {
      doc <- doc %>%
        officer::body_add_par(item$content, style = "Normal")
    } else if (item$type == "table") {
      doc <- doc %>%
        officer::body_add_flextable(item$content)
    } else if (item$type == "plot") {
      temp_file <- tempfile(fileext = ".png")
      ggsave(temp_file, item$content, width = 8, height = 6, dpi = 300)
      doc <- doc %>%
        officer::body_add_img(temp_file, width = 6, height = 4)
      unlink(temp_file)
    }
    doc <- doc %>% officer::body_add_par(" ")
  }
  
  print(doc, target = filename)
}

# Fungsi untuk menginterpretasikan koefisien korelasi
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r >= 0.8) {
    strength <- "sangat kuat"
  } else if (abs_r >= 0.6) {
    strength <- "kuat"
  } else if (abs_r >= 0.4) {
    strength <- "sedang"
  } else if (abs_r >= 0.2) {
    strength <- "lemah"
  } else {
    strength <- "sangat lemah"
  }
  
  direction <- ifelse(r > 0, "positif", "negatif")
  return(paste("Korelasi", direction, strength))
}

# Fungsi untuk menginterpretasikan uji normalitas
interpret_normality <- function(p_value, alpha = 0.05) {
  if (p_value > alpha) {
    return(paste0("Data berdistribusi normal (p-value = ",
                  round(p_value, 4), " > ", alpha, ")"))
  } else {
    return(paste0("Data tidak berdistribusi normal (p-value = ",
                  round(p_value, 4), " < ", alpha, ")"))
  }
}

# Fungsi untuk menginterpretasikan uji homogenitas
interpret_homogeneity <- function(p_value, alpha = 0.05) {
  if (p_value > alpha) {
    return(paste0("Variansi homogen (p-value = ",
                  round(p_value, 4), " > ", alpha, ")"))
  } else {
    return(paste0("Variansi tidak homogen (p-value = ",
                  round(p_value, 4), " < ", alpha, ")"))
  }
}

# Fungsi untuk menginterpretasikan uji t
interpret_t_test <- function(p_value, alternative, alpha = 0.05) {
  if (p_value < alpha) {
    if (alternative == "two.sided") {
      return(paste0("Terdapat perbedaan yang signifikan (p-value = ",
                    round(p_value, 4), " < ", alpha, ")"))
    } else if (alternative == "greater") {
      return(paste0("Rata-rata kelompok pertama signifikan lebih besar (p-value = ",
                    round(p_value, 4), " < ", alpha, ")"))
    } else {
      return(paste0("Rata-rata kelompok pertama signifikan lebih kecil (p-value = ",
                    round(p_value, 4), " < ", alpha, ")"))
    }
  } else {
    return(paste0("Tidak terdapat perbedaan yang signifikan (p-value = ",
                  round(p_value, 4), " > ", alpha, ")"))
  }
}

# Fungsi untuk menginterpretasikan uji proporsi
interpret_prop_test <- function(p_value, alternative, alpha = 0.05) {
  if (p_value < alpha) {
    if (alternative == "two.sided") {
      return(paste0("Terdapat perbedaan proporsi yang signifikan (p-value = ",
                    round(p_value, 4), " < ", alpha, ")"))
    } else if (alternative == "greater") {
      return(paste0("Proporsi kelompok pertama signifikan lebih besar (p-value = ",
                    round(p_value, 4), " < ", alpha, ")"))
    } else {
      return(paste0("Proporsi kelompok pertama signifikan lebih kecil (p-value = ",
                    round(p_value, 4), " < ", alpha, ")"))
    }
  } else {
    return(paste0("Tidak terdapat perbedaan proporsi yang signifikan (p-value = ",
                  round(p_value, 4), " > ", alpha, ")"))
  }
}


# Fungsi untuk menginterpretasikan ANOVA
interpret_anova <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    return(paste0("Terdapat perbedaan rata-rata yang signifikan antar kelompok (p-value = ",
                  round(p_value, 4), " < ", alpha, ")"))
  } else {
    return(paste0("Tidak terdapat perbedaan rata-rata yang signifikan antar kelompok (p-value = ",
                  round(p_value, 4), " > ", alpha, ")"))
  }
}

# Fungsi untuk memformat p-value
format_p_value <- function(p_value) {
  if (p_value < 0.001) {
    return("< 0.001")
  } else {
    return(round(p_value, 4)) # Menggunakan round untuk konsistensi
  }
}

# Fungsi untuk menginterpretasikan R-squared (PERTANYAKAN VERSI INI)
interpret_r_squared <- function(r_squared) {
  if (r_squared < 0.3) {
    return("Lemah - Model menjelaskan variabilitas rendah")
  } else if (r_squared < 0.5) {
    return("Sedang - Model memiliki daya prediksi cukup")
  } else if (r_squared < 0.7) {
    return("Baik - Model memiliki daya prediksi yang baik")
  } else if (r_squared < 0.9) {
    return("Sangat Baik - Model memiliki daya prediksi tinggi")
  } else {
    return("Sangat Tinggi - Periksa kemungkinan overfitting")
  }
}


# Fungsi untuk membuat tema kustom untuk plot
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
}

# Fungsi untuk mendapatkan variabel numerik
get_numeric_vars <- function(data) {
  names(data)[sapply(data, is.numeric)]
}

# Fungsi untuk mendapatkan variabel kategorikal
get_categorical_vars <- function(data) {
  categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  return(categorical_cols)
}

# Fungsi untuk menghitung statistik deskriptif dasar
calculate_basic_stats <- function(data, var_name) {
  if (!is.numeric(data[[var_name]])) {
    showNotification(paste0("Variabel '", var_name, "' bukan numerik. Statistik deskriptif tidak dapat dihitung."), type = "warning")
    return(NULL) # Kembalikan NULL jika tidak numerik
  }
  
  vec <- na.omit(data[[var_name]])
  if (length(vec) == 0) {
    return(list(
      Mean = NA, Median = NA, StdDev = NA, Min = NA, Max = NA,
      Q1 = NA, Q3 = NA, IQR = NA, Skewness = NA, Kurtosis = NA, N = 0, Missing = sum(is.na(data[[var_name]]))
    ))
  }
  
  # Pastikan paket 'moments' terinstal untuk skewness/kurtosis
  if (!requireNamespace("moments", quietly = TRUE)) {
    warning("Paket 'moments' diperlukan untuk menghitung skewness dan kurtosis. Mohon instal: install.packages('moments').")
    skew <- NA
    kurt <- NA
  } else {
    skew <- moments::skewness(vec)
    kurt <- moments::kurtosis(vec)
  }
  
  qs <- quantile(vec, probs = c(0.25, 0.75), na.rm = TRUE)
  
  return(list(
    Mean = mean(vec),
    Median = median(vec),
    StdDev = sd(vec),
    Min = min(vec),
    Max = max(vec),
    Q1 = qs[1],
    Q3 = qs[2],
    IQR = qs[2] - qs[1], # Dihitung sebagai Q3 - Q1
    Skewness = skew,
    Kurtosis = kurt,
    N = length(vec),
    Missing = sum(is.na(data[[var_name]]))
  ))
}

# Fungsi untuk membuat variabel kategorikal (Dipindahkan dari data_processing.R)
create_categorical_var <- function(data, var_name, breaks, labels) {
  new_col_name <- paste0(var_name, "_CAT")
  
  if (!is.numeric(data[[var_name]])) {
    stop("Variabel untuk kategorisasi harus numerik.")
  }
  
  data[[new_col_name]] <- cut(data[[var_name]],
                              breaks = breaks,
                              labels = labels,
                              right = TRUE, # Interval (a, b], di mana b termasuk
                              include.lowest = TRUE) # Sertakan nilai terendah dalam interval pertama
  
  if (any(is.na(data[[new_col_name]]))) {
    warning(paste0("Beberapa nilai di '", var_name, "' tidak masuk dalam kategori yang ditentukan oleh titik potong (breaks). Pastikan titik potong mencakup seluruh rentang data Anda, termasuk nilai minimum dan maksimum yang relevan."))
  }
  
  return(data)
}

# Fungsi untuk membuat tabel ringkasan statistik deskriptif
create_summary_table <- function(data, var_name) {
  if (!var_name %in% names(data)) {
    return(data.frame(Error = paste("Variabel", var_name, "tidak ditemukan dalam data")))
  }
  
  if (!is.numeric(data[[var_name]])) {
    # Untuk variabel kategorikal, kembalikan tabel frekuensi
    freq_table <- table(data[[var_name]], useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    result <- data.frame(
      Kategori = names(freq_table),
      Frekuensi = as.numeric(freq_table),
      Persentase = round(as.numeric(prop_table), 2)
    )
    return(result)
  }
  
  # Untuk variabel numerik, gunakan calculate_basic_stats
  stats <- calculate_basic_stats(data, var_name)
  
  if (is.null(stats)) {
    return(data.frame(Error = "Tidak dapat menghitung statistik untuk variabel ini"))
  }
  
  # Format sebagai tabel yang rapi
  result <- data.frame(
    Statistik = c("Mean", "Median", "Std Dev", "Min", "Max", "Q1", "Q3", "IQR",
                  "Skewness", "Kurtosis", "N", "Missing"),
    Nilai = c(
      round(stats$Mean, 3),
      round(stats$Median, 3),
      round(stats$StdDev, 3),
      round(stats$Min, 3),
      round(stats$Max, 3),
      round(stats$Q1, 3),
      round(stats$Q3, 3),
      round(stats$IQR, 3),
      round(stats$Skewness, 3),
      round(stats$Kurtosis, 3),
      stats$N,
      stats$Missing
    )
  )
  
  return(result)
}

# Fungsi untuk menginterpretasikan ringkasan regresi
interpret_regression_summary <- function(summary_obj) {
  if (is.null(summary_obj)) {
    return("Tidak ada ringkasan model untuk diinterpretasi.")
  }
  
  # Ekstrak informasi kunci
  r_squared <- summary_obj$r.squared
  adj_r_squared <- summary_obj$adj.r.squared
  f_statistic <- summary_obj$fstatistic[1]
  f_p_value <- pf(summary_obj$fstatistic[1],
                  summary_obj$fstatistic[2],
                  summary_obj$fstatistic[3],
                  lower.tail = FALSE)
  
  coefficients <- summary_obj$coefficients
  
  interpretation <- paste(
    "INTERPRETASI HASIL REGRESI LINEAR:\n\n",
    "Kebaikan Model:\n",
    "- R-squared:", round(r_squared, 4),
    paste0("(", round(r_squared * 100, 2), "% variabilitas dijelaskan model)\n"),
    "- Adjusted R-squared:", round(adj_r_squared, 4), "\n",
    "- Interpretasi:", interpret_r_squared(r_squared), "\n\n",
    
    "Uji Signifikansi Model Keseluruhan (F-test):\n",
    "- F-statistik:", round(f_statistic, 4), "\n",
    "- p-value:", format_p_value(f_p_value), "\n",
    "- Kesimpulan:",
    if (f_p_value < 0.05) {
      "Model secara keseluruhan signifikan (p < 0.05)"
    } else {
      "Model secara keseluruhan tidak signifikan (p >= 0.05)"
    }, "\n\n",
    
    "Uji Signifikansi Parsial:\n"
  )
  
  # Interpretasi koefisien individual
  for (i in 1:nrow(coefficients)) {
    var_name <- rownames(coefficients)[i]
    coef_value <- coefficients[i, "Estimate"]
    p_value <- coefficients[i, "Pr(>|t|)"]
    
    interpretation <- paste(interpretation,
                            "- ", var_name, ":\n",
                            "  Koefisien:", round(coef_value, 4), "\n",
                            "  p-value:", format_p_value(p_value), "\n",
                            "  Signifikansi:",
                            if (p_value < 0.001) "*** (p < 0.001)"
                            else if (p_value < 0.01) "** (p < 0.01)"
                            else if (p_value < 0.05) "* (p < 0.05)"
                            else if (p_value < 0.1) ". (p < 0.1)"
                            else "tidak signifikan", "\n",
                            "  Interpretasi:",
                            if (var_name == "(Intercept)") {
                              paste("Nilai prediksi ketika semua variabel independen = 0")
                            } else {
                              paste("Setiap peningkatan 1 unit", var_name,
                                    ifelse(coef_value > 0, "akan meningkatkan", "akan menurunkan"),
                                    "variabel dependen sebesar", abs(round(coef_value, 4)), "unit")
                            }, "\n\n"
    )
  }
  return(interpretation)
}

# Helper function untuk interpretasi asumsi
create_assumption_interpretation <- function(shapiro_test, vif_results, bp_test, dw_test = NULL) {
  interpretation <- "Interpretasi Uji Asumsi Regresi Linear:\n\n"
  
  # 1. Normalitas Residual
  interpretation <- paste(interpretation, "Normalitas Residual:\n")
  if (is.character(shapiro_test)) {
    interpretation <- paste(interpretation, shapiro_test, "\n")
  } else if (!is.null(shapiro_test)) {
    shapiro_p_value <- shapiro_test$p.value
    interpretation <- paste(interpretation,
                            "- Uji Shapiro-Wilk: W =", round(shapiro_test$statistic, 4),
                            ", p-value =", format_p_value(shapiro_p_value), "\n",
                            "- Kesimpulan:",
                            if (shapiro_p_value < 0.05) {
                              "Residual tidak berdistribusi normal (p < 0.05)"
                            } else {
                              "Residual berdistribusi normal (p >= 0.05)"
                            }, "\n",
                            "- Rekomendasi:",
                            if (shapiro_p_value < 0.05) {
                              "Transformasi variabel (logaritma, akar kuadrat)."
                            } else {
                              "Asumsi normalitas terpenuhi. Interval kepercayaan dan uji hipotesis valid."
                            }, "\n\n"
    )
  }
  
  # 2. Multikolinearitas
  interpretation <- paste(interpretation, "Multikolinearitas (VIF):\n")
  if (is.data.frame(vif_results) && "VIF" %in% names(vif_results)) {
    max_vif <- max(vif_results$VIF, na.rm = TRUE)
    interpretation <- paste(interpretation,
                            "- VIF maksimum =", round(max_vif, 2), "\n",
                            "- Status multikolinearitas:",
                            if (max_vif >= 10) {
                              "TINGGI (VIF >= 10) - Ada masalah multikolinearitas"
                            } else {
                              "RENDAH (VIF < 10) - Tidak ada masalah multikolinearitas"
                            }, "\n",
                            "- Rekomendasi:",
                            if (max_vif >= 10) {
                              "Hapus salah satu variabel berkorelasi tinggi"
                            } else {
                              "Asumsi tidak ada multikolinearitas terpenuhi."
                            }, "\n\n"
    )
  } else {
    interpretation <- paste(interpretation, "- ", vif_results, "\n\n")
  }
  
  # 3. Heteroskedastisitas (Breusch-Pagan Test only)
  interpretation <- paste(interpretation, "Homoskedastisitas (Varians Konstan):\n")
  
  if (is.character(bp_test)) {
    interpretation <- paste(interpretation, "- Uji Breusch-Pagan: ", bp_test, "\n")
  } else if (!is.null(bp_test) && "p.value" %in% names(bp_test)) {
    bp_p_value <- bp_test$p.value
    interpretation <- paste(interpretation,
                            "- Uji Breusch-Pagan: BP =", round(bp_test$statistic, 4),
                            ", p-value =", format_p_value(bp_p_value), "\n",
                            "- Kesimpulan:",
                            if (bp_p_value < 0.05) {
                              "Ada heteroskedastisitas (p < 0.05)"
                            } else {
                              "Homoskedastisitas terpenuhi (p >= 0.05)"
                            }, "\n"
    )
  }
  
  # Rekomendasi untuk heteroskedastisitas
  interpretation <- paste(interpretation,
                          "- Rekomendasi:\n",
                          "  Jika ada heteroskedastisitas, pertimbangkan:\n",
                          "  Transformasi variabel (log, akar kuadrat)\n\n"
  )
  
  # 4. Autokorelasi (Durbin-Watson Test)
  interpretation <- paste(interpretation, "Autokorelasi (Uji Durbin-Watson):\n")
  if (is.character(dw_test)) {
    interpretation <- paste(interpretation, "- Uji Durbin-Watson: ", dw_test, "\n\n")
  } else if (!is.null(dw_test) && "p.value" %in% names(dw_test)) {
    dw_statistic <- dw_test$statistic
    dw_p_value <- dw_test$p.value
    interpretation <- paste(interpretation,
                            "- Durbin-Watson statistik (DW) =", round(dw_statistic, 4), "\n",
                            "- p-value =", format_p_value(dw_p_value), "\n",
                            "- Kesimpulan:",
                            if (dw_p_value < 0.05) {
                              "Ada autokorelasi (p < 0.05)."
                            } else {
                              "Tidak ada autokorelasi (p >= 0.05)."
                            }, "\n",
                            "- Rekomendasi:",
                            if (dw_p_value < 0.05) {
                              "Pertimbangkan menggunakan metode estimasi yang memperhitungkan autokorelasi (Cochrane-Orcutt, First-Difference)."
                            } else {
                              "Asumsi tidak ada autokorelasi terpenuhi."
                            }, "\n\n"
    )
  }
  return(interpretation)
}

# Fungsi generik untuk membuat grafik batang horizontal top N
create_top_n_bar_plot <- function(data, value_col, name_col, title = "Top 5 Daerah", x_label = "Nilai", y_label = "Kabupaten/Kota", fill_low = "lightblue", fill_high = "darkblue", plot_title_size = 10) {
  
  if (is.null(data) || nrow(data) == 0 || !(value_col %in% names(data)) || !(name_col %in% names(data))) {
    # Mengembalikan plot kosong dengan pesan
    return(ggplot() +
             labs(title = paste0("Data untuk ", title, " tidak tersedia.")) +
             theme_void() +
             theme(plot.title = element_text(hjust = 0.5, size = plot_title_size, color = "gray50")))
  }
  
  # Mengurutkan berdasarkan nilai dan mengubah menjadi faktor
  data[[name_col]] <- factor(data[[name_col]],
                             levels = data[[name_col]][order(data[[value_col]])])
  
  ggplot(data, aes(x = .data[[value_col]], y = .data[[name_col]], fill = .data[[value_col]],
                   text = paste0("Kabupaten/Kota: ", .data[[name_col]], "<br>",
                                 x_label, ": ", round(.data[[value_col]], 2)))) + # Custom tooltip text, round value
    geom_bar(stat = "identity") +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = plot_title_size)) + # Judul di tengah, ukuran judul disesuaikan
    scale_fill_gradient(low = fill_low, high = fill_high)
}

# Fungsi spesifik untuk membuat line chart (misalnya untuk populasi atau buta huruf)
create_top_n_line_chart <- function(data, value_col, name_col, title = "Top 5 Daerah", x_label = "Peringkat", y_label = "Nilai", plot_title_size = 10) {
  
  if (is.null(data) || nrow(data) == 0 || !(value_col %in% names(data)) || !(name_col %in% names(data))) {
    # Mengembalikan plot kosong dengan pesan
    return(ggplot() +
             labs(title = paste0("Data untuk ", title, " tidak tersedia.")) +
             theme_void() +
             theme(plot.title = element_text(hjust = 0.5, size = plot_title_size, color = "gray50")))
  }
  
  # Mengurutkan data dan menambahkan kolom peringkat
  data <- data %>%
    arrange(desc(.data[[value_col]])) %>%
    mutate(rank = 1:n()) # Membuat peringkat untuk sumbu X jika tidak ada variabel waktu
  
  ggplot(data, aes(x = rank, y = .data[[value_col]], group = 1,
                   text = paste0("Kabupaten/Kota: ", .data[[name_col]], "<br>",
                                 y_label, ": ", round(.data[[value_col]], 2)))) +
    geom_line(size = 1, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = plot_title_size),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = 1:nrow(data), labels = data[[name_col]])
}


# Fungsi generik untuk mengosongkan semua elemen di reactiveValues
clear_reactive_values <- function(reactive_values_obj) {
  for (name in names(reactive_values_obj)) {
    reactive_values_obj[[name]] <- NULL
  }
}