# Main App File - app.R

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(officer)
library(car)       # Untuk VIF
library(lmtest)    # Untuk Breusch-Pagan test
library(gridExtra) # Untuk menggabungkan plot
library(readr)     # Untuk read_csv
library(data.table) # Untuk fread
library(moments)   # Untuk skewness dan kurtosis
library(classInt)  # Untuk Jenks Natural Breaks
library(shinycssloaders) # Untuk withSpinner function
library(RColorBrewer)
library(htmlwidgets)
library(leaflet)
library(sf)
library(geojsonio)  # Untuk geojson_read()
library(sp)         # Untuk sp::merge() dan sp::proj4string()
library(htmltools)

# Sumber fungsi utilitas
source("utils/helpers.R")

# Sumber fungsi UI dan server modul
source("modules/beranda_module.R")
source("modules/manajemen_data_module.R")
source("modules/eksplorasi_data_module.R")
source("modules/peta_module.R")
source("modules/uji_asumsi_module.R")
source("modules/statistik_inferensia_module.R")
source("modules/regresi_linear_module.R")

# Pemuatan Data
data_path <- "data/data_soviedit.csv"

# Periksa apakah file ada, jika tidak, berikan peringatan atau hentikan aplikasi
if (!file.exists(data_path)) {
  stop(paste0("File data utama tidak ditemukan di: ", data_path, ". Mohon pastikan file 'data_soviedit.csv' ada di lokasi yang benar."))
}

global_combined_data <- readr::read_csv(data_path, show_col_types = FALSE)

# Opsional: Jika DISTRICTCODE, PROVINCE_NAME, CITY_NAME perlu dikonversi ke faktor
if ("DISTRICTCODE" %in% names(global_combined_data)) {
  global_combined_data$DISTRICTCODE <- as.character(global_combined_data$DISTRICTCODE)
}
if ("PROVINCE_NAME" %in% names(global_combined_data)) {
  global_combined_data$PROVINCE_NAME <- as.factor(global_combined_data$PROVINCE_NAME)
}
if ("CITY_NAME" %in% names(global_combined_data)) {
  global_combined_data$CITY_NAME <- as.factor(global_combined_data$CITY_NAME)
}

# AKHIR Pemuatan Data


# Definisi UI
ui <- dashboardPage(
  dashboardHeader(title = "SOVI Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("cogs")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi_data", icon = icon("chart-bar")),
      menuItem("Peta Tematik", tabName = "peta", icon = icon("map")),
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
      tabItem(tabName = "peta",
              petaUI("peta_tab")),
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
  # Penting: Modul lain harus memperbarui nilai-nilai reaktif ini agar laporan lengkap berfungsi.
  values <- reactiveValues(
    current_data = global_combined_data,
    exploration_results = reactiveValues(
      interpretation = NULL,
      descriptive_stats = NULL,
      hist_plot = NULL,
      boxplot_single_plot = NULL,
      barplot_plot = NULL,
      plot_grouped_boxplot = NULL,
      plot_histogram_by_category = NULL,
      plot_correlation_matrix = NULL,
      ggplot_scatter = NULL,
      plot_frequency_table_data = NULL
    ),
    transformation_results = reactiveValues( # Ini akan diisi oleh manajemen_data_module
      after_data = NULL,
      transformation_info = NULL,
      before_data = NULL
    ),
    regression_results = reactiveValues(
      model = NULL,
      summary = NULL,
      interpretation = NULL,
      residuals = NULL,
      shapiro_test = NULL,
      vif = NULL,
      bptest_result = NULL,
      dwtest_result = NULL,
      assumption_interpretation = NULL,
      residual_hist_plot = NULL,
      residual_qq_plot = NULL
    ),
    assumption_results = reactiveValues( # Ini akan diisi oleh uji_asumsi_module
      test_output = NULL,
      test_plot = NULL,
      interpretation = NULL
    ),
    statistik_inferensia_results = reactiveValues( # Ini akan diisi oleh statistik_inferensia_module dan sub-modulnya
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      test_type_run = NULL # Untuk mengetahui jenis uji terakhir yang dijalankan
    ),
    peta_results = reactiveValues( # Ini akan diisi oleh peta_module
      interpretation = NULL,
      merged_data = NULL
    )
  )
  
  # Panggil server modul
  berandaServer("beranda_tab", values)
  manajemenDataServer("manajemen_data_tab", values)
  eksplorasiDataServer("eksplorasi_data_tab", values)
  petaServer("peta_tab", values)
  ujiAsumsiServer("uji_asumsi_tab", values)
  statistikInferensiaServer("statistik_inferensia_tab", values)
  regresiLinearServer("regresi_linear_tab", values)
  
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)