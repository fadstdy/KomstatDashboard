# Helper Functions
# utils/helpers.R

# Fungsi untuk menampilkan notifikasi aman
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

# Fungsi untuk mendapatkan variabel numerik (Dipindahkan dari data_processing.R)
get_numeric_vars <- function(data) {
  names(data)[sapply(data, is.numeric)]
}

# Fungsi untuk mendapatkan variabel kategorikal (Dipindahkan dari data_processing.R)
get_categorical_vars <- function(data) {
  categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  return(categorical_cols)
}

# Fungsi untuk menghitung statistik deskriptif dasar (Dipindahkan dari data_processing.R)
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