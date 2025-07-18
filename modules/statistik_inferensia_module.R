# Statistik Inferensia Module - MAIN FILE
# modules/statistik_inferensia_module.R

# Source all submenu modules
source("modules/uji_t_module.R")
source("modules/uji_proporsi_module.R") 
source("modules/uji_anova_module.R")
source("modules/uji_chi_square_module.R")

# ==========================================
# MAIN UI FUNCTION - SUBMENU STRUCTURE
# ==========================================

statistikInferensiaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
           box(
             title = "Statistik Inferensia - Pilih Jenis Uji",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Navigation Pills untuk Submenu
             div(
               style = "margin-bottom: 20px;",
               h4("Pilih Jenis Uji Statistik:"),
               fluidRow(
                 column(3,
                        actionButton(ns("show_t_test"), 
                                     HTML("<i class='fa fa-calculator'></i><br/>Uji t<br/><small>Perbandingan Rata-rata</small>"), 
                                     class = "btn-info btn-lg btn-block",
                                     style = "height: 100px; font-size: 16px; white-space: normal;")),
                 column(3,
                        actionButton(ns("show_prop_test"), 
                                     HTML("<i class='fa fa-pie-chart'></i><br/>Uji Proporsi<br/><small>Perbandingan Kabupaten/Provinsi</small>"), 
                                     class = "btn-success btn-lg btn-block",
                                     style = "height: 100px; font-size: 16px; white-space: normal;")),
                 column(3,
                        actionButton(ns("show_anova_test"), 
                                     HTML("<i class='fa fa-bar-chart'></i><br/>Uji ANOVA<br/><small>Perbandingan Multi Kelompok</small>"), 
                                     class = "btn-warning btn-lg btn-block",
                                     style = "height: 100px; font-size: 16px; white-space: normal;")),
                 column(3,
                        actionButton(ns("show_chi_test"), 
                                     HTML("<i class='fa fa-link'></i><br/>Uji Chi-Square<br/><small>Asosiasi Kategorikal</small>"), 
                                     class = "btn-danger btn-lg btn-block",
                                     style = "height: 100px; font-size: 16px; white-space: normal;"))
               ),
               
               # Deskripsi singkat
               hr(),
               div(
                 style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                 h5("ℹ️ Panduan Pemilihan Uji:"),
                 tags$ul(
                   tags$li(strong("Uji t:"), " Untuk membandingkan rata-rata antara satu atau dua kelompok"),
                   tags$li(strong("Uji Proporsi:"), " Untuk membandingkan proporsi antar kabupaten/kota atau provinsi"), 
                   tags$li(strong("Uji ANOVA:"), " Untuk membandingkan rata-rata lebih dari dua kelompok"),
                   tags$li(strong("Uji Chi-Square:"), " Untuk menguji asosiasi antara dua variabel kategorikal")
                 )
               )
             )
           )
    ),
    
    # Konten Dinamis berdasarkan Submenu yang Dipilih
    column(12,
           conditionalPanel(
             condition = "output.show_t_panel == true",
             ns = ns,
             ujiTUI(ns("uji_t"))
           ),
           conditionalPanel(
             condition = "output.show_prop_panel == true",
             ns = ns,
             ujiProporsiUI(ns("uji_proporsi"))
           ),
           conditionalPanel(
             condition = "output.show_anova_panel == true",
             ns = ns,
             ujiAnovaUI(ns("uji_anova"))
           ),
           conditionalPanel(
             condition = "output.show_chi_panel == true",
             ns = ns,
             ujiChiSquareUI(ns("uji_chi"))
           )
    )
  )
}

# ==========================================
# MAIN SERVER FUNCTION
# ==========================================

statistikInferensiaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values untuk mengontrol panel yang tampil
    current_panel <- reactiveValues(
      t_test = FALSE,
      prop_test = FALSE,
      anova_test = FALSE,
      chi_test = FALSE
    )
    
    # Observer untuk tombol navigasi
    observeEvent(input$show_t_test, {
      current_panel$t_test <- TRUE
      current_panel$prop_test <- FALSE
      current_panel$anova_test <- FALSE
      current_panel$chi_test <- FALSE
      
      showNotification("Modul Uji t dimuat", type = "message")
    })
    
    observeEvent(input$show_prop_test, {
      current_panel$t_test <- FALSE
      current_panel$prop_test <- TRUE
      current_panel$anova_test <- FALSE
      current_panel$chi_test <- FALSE
      
      showNotification("Modul Uji Proporsi dimuat", type = "message")
    })
    
    observeEvent(input$show_anova_test, {
      current_panel$t_test <- FALSE
      current_panel$prop_test <- FALSE
      current_panel$anova_test <- TRUE
      current_panel$chi_test <- FALSE
      
      showNotification("Modul Uji ANOVA dimuat", type = "message")
    })
    
    observeEvent(input$show_chi_test, {
      current_panel$t_test <- FALSE
      current_panel$prop_test <- FALSE
      current_panel$anova_test <- FALSE
      current_panel$chi_test <- TRUE
      
      showNotification("Modul Uji Chi-Square dimuat", type = "message")
    })
    
    # Output untuk conditional panels
    output$show_t_panel <- reactive({ current_panel$t_test })
    output$show_prop_panel <- reactive({ current_panel$prop_test })
    output$show_anova_panel <- reactive({ current_panel$anova_test })
    output$show_chi_panel <- reactive({ current_panel$chi_test })
    
    outputOptions(output, "show_t_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "show_prop_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "show_anova_panel", suspendWhenHidden = FALSE)
    outputOptions(output, "show_chi_panel", suspendWhenHidden = FALSE)
    
    # Call submenu servers
    ujiTServer("uji_t", values)
    ujiProporsiServer("uji_proporsi", values)
    ujiAnovaServer("uji_anova", values)
    ujiChiSquareServer("uji_chi", values)
  })
}