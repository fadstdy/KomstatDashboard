# Main App File - app.R
# Dashboard Analisis Statistik
# Author: Dashboard Analytics Team

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(officer)
library(car)       # Untuk VIF dan ncvTest
library(lmtest)    # Untuk bptest (Breusch-Pagan test)
library(gridExtra) # Untuk menggabungkan plot
library(readr)     # Untuk read_csv
library(data.table) # Untuk fread
library(moments)   # Untuk skewness dan kurtosis
library(classInt)  # Untuk Jenks Natural Breaks
library(shinycssloaders) # Untuk withSpinner function

# Sumber fungsi utilitas
source("utils/helpers.R") 

# Sumber fungsi UI dan server modul
source("modules/beranda_module.R")
source("modules/manajemen_data_module.R")
source("modules/eksplorasi_data_module.R")
source("modules/uji_asumsi_module.R")
source("modules/statistik_inferensia_module.R")
source("modules/regresi_linear_module.R")

# --- Pemuatan Data Langsung dari file gabungan ---
# Asumsi data_sovi.csv sudah berisi semua kolom yang dibutuhkan (termasuk CITY_NAME, PROVINCE_NAME)
# Asumsi data_sovi.csv berada di direktori utama proyek, atau di folder data/
data_path <- "data/data_sovi.csv" # <--- Sesuaikan PATH ini jika file Anda di tempat lain
# Contoh jika di folder 'data': data_path <- "data/data_sovi.csv"

# Periksa apakah file ada, jika tidak, berikan peringatan atau hentikan aplikasi
if (!file.exists(data_path)) {
  stop(paste0("File data utama tidak ditemukan di: ", data_path, ". Mohon pastikan file 'data_sovi.csv' ada di lokasi yang benar."))
}

global_combined_data <- readr::read_csv(data_path, show_col_types = FALSE)

# Opsional: Jika DISTRICTCODE, PROVINCE_NAME, CITY_NAME perlu dikonversi ke faktor, lakukan di sini
if ("DISTRICTCODE" %in% names(global_combined_data)) {
  global_combined_data$DISTRICTCODE <- as.character(global_combined_data$DISTRICTCODE)
}
if ("PROVINCE_NAME" %in% names(global_combined_data)) {
  global_combined_data$PROVINCE_NAME <- as.factor(global_combined_data$PROVINCE_NAME)
}
if ("CITY_NAME" %in% names(global_combined_data)) {
  global_combined_data$CITY_NAME <- as.factor(global_combined_data$CITY_NAME)
}

# --- AKHIR Pemuatan Data Langsung ---


# Definisi UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Statistik"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("cogs")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi_data", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "uji_asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "statistik_inferensia", icon = icon("flask")),
      menuItem("Regresi Linear", tabName = "regresi_linear", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    # Tautan ke CSS kustom
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    ),
    
    tabItems(
      tabItem(tabName = "beranda",
              berandaUI("beranda_tab")),
      tabItem(tabName = "manajemen_data",
              manajemenDataUI("manajemen_data_tab")),
      tabItem(tabName = "eksplorasi_data",
              eksplorasiDataUI("eksplorasi_data_tab")),
      tabItem(tabName = "uji_asumsi",
              ujiAsumsiUI("uji_asumsi_tab")),
      tabItem(tabName = "statistik_inferensia",
              statistikInferensiaUI("statistik_inferensia_tab")),
      tabItem(tabName = "regresi_linear",
              regresiLinearUI("regresi_linear_tab"))
    )
  )
)

# Definisi logika server
server <- function(input, output, session) {
  
  # Nilai reaktif untuk menyimpan data saat ini (dimulai dengan data gabungan yang dimuat)
  values <- reactiveValues(current_data = global_combined_data)
  
  # Panggil server modul
  berandaServer("beranda_tab", values)
  manajemenDataServer("manajemen_data_tab", values) 
  eksplorasiDataServer("eksplorasi_data_tab", values)
  ujiAsumsiServer("uji_asumsi_tab", values)
  statistikInferensiaServer("statistik_inferensia_tab", values)
  regresiLinearServer("regresi_linear_tab", values)
  
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)