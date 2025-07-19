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

# Fungsi untuk menginterpretasikan R-squared
interpret_r_squared <- function(r_squared) {
  if (r_squared >= 0.8) {
    return(paste0("Model sangat baik (R² = ", round(r_squared, 4), ")"))
  } else if (r_squared >= 0.6) {
    return(paste0("Model baik (R² = ", round(r_squared, 4), ")"))
  } else if (r_squared >= 0.4) {
    return(paste0("Model sedang (R² = ", round(r_squared, 4), ")"))
  } else if (r_squared >= 0.2) {
    return(paste0("Model lemah (R² = ", round(r_squared, 4), ")"))
  } else {
    return(paste0("Model sangat lemah (R² = ", round(r_squared, 4), ")"))
  }
}

# Fungsi untuk memformat p-value
format_p_value <- function(p_value) {
  if (p_value < 0.001) {
    return("< 0.001")
  } else {
    return(sprintf("%.3f", p_value))
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
      # Perbaikan: Ganti 'size' dengan 'linewidth'
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

# Tambahkan fungsi ini ke dalam file utils/helpers.R

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
    "INTERPRETASI HASIL REGRESI LINEAR BERGANDA:\n",
    "==========================================\n\n",
    "1. GOODNESS OF FIT:\n",
    "- R-squared:", round(r_squared, 4), 
    paste0("(", round(r_squared * 100, 2), "% variabilitas dijelaskan model)\n"),
    "- Adjusted R-squared:", round(adj_r_squared, 4), "\n",
    "- Interpretasi:", interpret_r_squared(r_squared), "\n\n",
    
    "2. UJI SIGNIFIKANSI MODEL KESELURUHAN (F-test):\n",
    "- F-statistik:", round(f_statistic, 4), "\n",
    "- p-value:", format_p_value(f_p_value), "\n",
    "- Kesimpulan:", 
    if (f_p_value < 0.05) {
      "Model secara keseluruhan signifikan (p < 0.05)"
    } else {
      "Model secara keseluruhan tidak signifikan (p >= 0.05)"
    }, "\n\n",
    
    "3. UJI SIGNIFIKANSI KOEFISIEN INDIVIDUAL:\n"
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
                                    ifelse(coef_value > 0, "meningkatkan", "menurunkan"),
                                    "variabel dependen sebesar", abs(round(coef_value, 4)), "unit")
                            }, "\n\n"
    )
  }
  
  interpretation <- paste(interpretation,
                          "4. REKOMENDASI:\n",
                          if (r_squared < 0.3) {
                            "- Model memiliki daya prediksi rendah. Pertimbangkan menambah variabel atau transformasi.\n"
                          } else if (r_squared > 0.8) {
                            "- Model memiliki daya prediksi tinggi. Periksa kemungkinan overfitting.\n"
                          } else {
                            "- Model memiliki daya prediksi yang wajar.\n"
                          },
                          "- Periksa asumsi regresi (normalitas residual, multikolinearitas, heteroskedastisitas).\n",
                          "- Lakukan validasi model dengan data terpisah jika memungkinkan.\n"
  )
  
  return(interpretation)
}

# Helper function untuk interpretasi R-squared
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

# Helper function untuk format p-value
format_p_value <- function(p_value) {
  if (p_value < 0.001) {
    return("< 0.001")
  } else {
    return(round(p_value, 4))
  }
}