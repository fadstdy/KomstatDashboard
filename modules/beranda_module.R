# modules/beranda_module.R

# UI function for Beranda
berandaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Bagian Header
    column(12,
           box(
             title = "Selamat Datang di Dashboard Analisis Statistik",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             h3(HTML("Potret Sosial Ekonomi Indonesia:<br> Analisis Interaktif Berdasarkan Indeks SOVI"),
                style = "text-align: center; margin-bottom: 20px;"),
             
             p("Dashboard ini dirancang untuk mempermudah pemahaman terhadap kondisi sosial-ekonomi di Indonesia secara menyeluruh. Melalui tampilan interaktif yang dibangun dengan R Shiny, data dapat dieksplorasi dengan mudah melalui visualisasi, uji statistik, hingga analisis regresi. Setiap fitur dirancang untuk menampilkan pola dan informasi penting berdasarkan Indeks SOVI (Socioeconomic Vulnerability Index), sehingga mendukung interpretasi data secara lebih mendalam dan bermakna.",
               style = "text-align: justify; font-size: 14px;")
           )
    ),
    
    # Metrik Kunci (Value Boxes)
    column(12,
           fluidRow(
             column(4,
                    valueBoxOutput(ns("n_provinces_valuebox"), width = 12)
             ),
             column(4,
                    valueBoxOutput(ns("n_obs_valuebox"), width = 12)
             ),
             column(4,
                    valueBoxOutput(ns("n_vars_valuebox"), width = 12)
             )
           )
    ),
    
    # Bagian Top 5 Daerah (Lebar Penuh)
    column(12,
           box(
             title = "Fakta Data Utama",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel(
               tabPanel("Tingkat Kemiskinan Tertinggi",
                        br(),
                        fluidRow(
                          column(6,
                                 withSpinner(plotlyOutput(ns("poverty_bar"), height = "300px"))),
                          column(6,
                                 withSpinner(plotlyOutput(ns("poverty_line"), height = "300px")))
                        )
               ),
               tabPanel("Persentase Anak <5 Tahun Tertinggi",
                        br(),
                        fluidRow(
                          column(6,
                                 withSpinner(plotlyOutput(ns("children_bar"), height = "300px"))),
                          column(6,
                                 withSpinner(plotlyOutput(ns("children_line"), height = "300px")))
                        )
               ),
               tabPanel("Tingkat Buta Huruf Tertinggi",
                        br(),
                        fluidRow(
                          column(6,
                                 withSpinner(plotlyOutput(ns("illiterate_bar"), height = "300px"))),
                          column(6,
                                 withSpinner(plotlyOutput(ns("illiterate_line"), height = "300px")))
                        )
               ),
               tabPanel("Persentase Rumah Tanpa Drainase Tertinggi",
                        br(),
                        fluidRow(
                          column(6,
                                 withSpinner(plotlyOutput(ns("nosewer_bar"), height = "300px"))),
                          column(6,
                                 withSpinner(plotlyOutput(ns("nosewer_line"), height = "300px")))
                        )
               )
             )
           )
    ),
    
    # Tab untuk Metadata dan Fitur Dashboard
    column(12,
           box(
             title = "Informasi Dashboard",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel(
               # Tab Metadata
               tabPanel("Metadata Dataset",
                        br(),
                        div(
                          h4("Dataset: Data Kerentanan Sosial di Indonesia"),
                          hr(),
                          fluidRow(
                            column(6,
                                   div(
                                     style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                                     h5("Informasi Umum"),
                                     tags$ul(
                                       tags$li(strong("Sumber Data: "), "Survei Sosial Ekonomi Nasional (SUSENAS) 2017 oleh BPS-Statistics Indonesia"),
                                       tags$li(strong("Periode Data: "), "2017"),
                                       tags$li(strong("Unit Analisis: "), "Kabupaten/Kota di Indonesia"),
                                       tags$li(strong("Cakupan Geografis: "), "511 Kabupaten/Kota di Seluruh Indonesia")
                                     )
                                   )
                            )
                          ),
                          br(),
                          h5("Deskripsi Variabel:"),
                          withSpinner(tableOutput(ns("variable_descriptions_table")))
                        )
               ),
               
               # Tab Fitur Dashboard
               tabPanel("Fitur Dashboard",
                        br(),
                        fluidRow(
                          column(6,
                                 div(
                                   style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                                   h5(icon("tools"), " Fitur Analisis Utama"),
                                   tags$ul(
                                     tags$li(strong("Manajemen Data: "), "Transformasi dan kategorisasi variabel, handling missing values dan outlier"),
                                     tags$li(strong("Eksplorasi Data: "), "Statistik deskriptif, visualisasi distribusi, analisis korelasi"),
                                     tags$li(strong("Peta Tematik: "), "Visualisasi spasial dengan peta interaktif"),
                                     tags$li(strong("Uji Asumsi: "), "Pengujian normalitas, homogenitas, multikolinearitas"),
                                     tags$li(strong("Statistik Inferensia: "), "Uji hipotesis, ANOVA, perbandingan grup"),
                                     tags$li(strong("Regresi Linear: "), "Analisis regresi berganda dengan diagnostik model")
                                   )
                                 )
                          ),
                          column(6,
                                 div(
                                   style = "background: #d1ecf1; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                                   h5(icon("download"), " Fitur Export & Download"),
                                   tags$ul(
                                     tags$li("Download grafik (PNG/PDF)"),
                                     tags$li("Export tabel (CSV/Excel)"),
                                     tags$li("Laporan interpretasi (TXT)"),
                                     tags$li("Download gabungan per menu"),
                                     tags$li("Data hasil transformasi")
                                   )
                                 ),
                                 div(
                                   style = "background: #f8d7da; padding: 15px; border-radius: 8px;",
                                   h5(icon("lightbulb"), " Tips Penggunaan"),
                                   tags$ul(
                                     tags$li("Mulai dari menu Manajemen Data untuk preprocessing"),
                                     tags$li("Lakukan eksplorasi data sebelum analisis inferensia"),
                                     tags$li("Periksa asumsi sebelum uji parametrik"),
                                     tags$li("Gunakan peta untuk analisis pola spasial"),
                                     tags$li("Download interpretasi untuk dokumentasi")
                                   )
                                 )
                          )
                        )
               ),
               
               # Tab Preview Data
               tabPanel(" Preview Data",
                        br(),
                        div(
                          h4("Struktur Data:"),
                          p("Tabel di bawah menampilkan 10 baris pertama dari dataset. Gunakan fitur pencarian dan filter untuk eksplorasi lebih lanjut."),
                          withSpinner(DT::dataTableOutput(ns("data_preview"))),
                          br(),
                          div(style = "text-align: center;",
                              downloadButton(ns("download_data"), "Download Data Lengkap",
                                             class = "btn-primary"))
                        )
               )
             )
           )
    )
  )
}

# Fungsi Server untuk Beranda
berandaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Value Boxes
    output$n_obs_valuebox <- renderValueBox({
      valueBox(
        value = nrow(values$current_data),
        subtitle = "Jumlah Kabupaten/Kota",
        icon = icon("map-marker-alt"),
        color = "light-blue"
      )
    })
    
    output$n_vars_valuebox <- renderValueBox({
      valueBox(
        value = ncol(values$current_data),
        subtitle = "Jumlah Variabel",
        icon = icon("columns"),
        color = "purple"
      )
    })
    
    output$n_provinces_valuebox <- renderValueBox({
      num_provinces <- if ("PROVINCE_NAME" %in% names(values$current_data)) {
        length(unique(values$current_data$PROVINCE_NAME))
      } else {
        NA
      }
      
      valueBox(
        value = num_provinces,
        subtitle = "Jumlah Provinsi",
        icon = icon("globe-asia"),
        color = "purple"
      )
    })
    
    # Fungsi helper untuk mendapatkan top N data
    get_top_n_data <- function(data, var_name, n = 5) {
      if (is.null(data) || nrow(data) == 0) return(NULL)
      if (!var_name %in% names(data)) return(NULL)
      if (!"CITY_NAME" %in% names(data)) return(NULL)
      
      data_clean <- data[!is.na(data[[var_name]]), ]
      if (nrow(data_clean) == 0) return(NULL)
      
      data_clean[order(-data_clean[[var_name]]), ][1:min(n, nrow(data_clean)), ]
    }
    
    # Reactive data dengan error handling 
    top5_poverty_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "POVERTY")
    })
    
    top5_children_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "CHILDREN")
    })
    
    top5_illiterate_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "ILLITERATE")
    })
    
    top5_nosewer_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "NOSEWER")
    })
    
    # Plot untuk Tingkat Kemiskinan Tertinggi - DIPERBAIKI
    output$poverty_bar <- renderPlotly({
      tryCatch({
        data <- top5_poverty_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE) %>%
            plotly::layout(title = "Data Tidak Tersedia")
        } else {
          p <- create_top_n_bar_plot(data,
                                     value_col = "POVERTY",
                                     name_col = "CITY_NAME",
                                     title = "Top 5 Daerah dengan Tingkat Kemiskinan Tertinggi",
                                     x_label = "Tingkat Kemiskinan (%)")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    output$poverty_line <- renderPlotly({
      tryCatch({
        data <- top5_poverty_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_line_chart(data,
                                       value_col = "POVERTY",
                                       name_col = "CITY_NAME",
                                       title = "Trend Tingkat Kemiskinan",
                                       x_label = "Kabupaten/Kota",
                                       y_label = "Tingkat Kemiskinan (%)")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    # Plot untuk Jumlah Penduduk Tertinggi 
    output$children_bar <- renderPlotly({
      tryCatch({
        data <- top5_children_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_bar_plot(data,
                                     value_col = "CHILDREN",
                                     name_col = "CITY_NAME",
                                     title = "Top 5 Daerah dengan Jumlah Penduduk Tertinggi",
                                     x_label = "Jumlah Penduduk",
                                     fill_low = "lightgreen", fill_high = "darkgreen")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    output$children_line <- renderPlotly({
      tryCatch({
        data <- top5_children_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_line_chart(data,
                                       value_col = "CHILDREN",
                                       name_col = "CITY_NAME",
                                       title = "Trend Jumlah Penduduk",
                                       x_label = "Kabupaten/Kota",
                                       y_label = "Jumlah Penduduk")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    # Plot untuk Tingkat Buta Huruf Tertinggi 
    output$illiterate_bar <- renderPlotly({
      tryCatch({
        data <- top5_illiterate_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_bar_plot(data,
                                     value_col = "ILLITERATE",
                                     name_col = "CITY_NAME",
                                     title = "Top 5 Daerah dengan Tingkat Buta Huruf Tertinggi",
                                     x_label = "Tingkat Buta Huruf (%)",
                                     fill_low = "lightcyan", fill_high = "darkcyan")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    output$illiterate_line <- renderPlotly({
      tryCatch({
        data <- top5_illiterate_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_line_chart(data,
                                       value_col = "ILLITERATE",
                                       name_col = "CITY_NAME",
                                       title = "Trend Tingkat Buta Huruf",
                                       x_label = "Kabupaten/Kota",
                                       y_label = "Tingkat Buta Huruf (%)")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    # Plot untuk Persentase Rumah Tanpa Drainase Tertinggi - BARU
    output$nosewer_bar <- renderPlotly({
      tryCatch({
        data <- top5_nosewer_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_bar_plot(data,
                                     value_col = "NOSEWER",
                                     name_col = "CITY_NAME",
                                     title = "Top 5 Daerah dengan Persentase Rumah Tanpa Drainase Tertinggi",
                                     x_label = "Persentase Rumah Tanpa Drainase (%)",
                                     fill_low = "lightcoral", fill_high = "darkred")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    output$nosewer_line <- renderPlotly({
      tryCatch({
        data <- top5_nosewer_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_line_chart(data,
                                       value_col = "NOSEWER",
                                       name_col = "CITY_NAME",
                                       title = "Trend Persentase Rumah Tanpa Drainase",
                                       x_label = "Kabupaten/Kota",
                                       y_label = "Persentase Rumah Tanpa Drainase (%)")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    # Tabel Deskripsi Variabel - DIPERBAIKI dengan variabel NODRAIN
    output$variable_descriptions_table <- renderTable({
      data.frame(
        Label = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
                  "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE",
                  "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION","PROVINCE_NAME","CITY_NAME"),
        Variabel = c("Kode Distrik", "Anak-anak", "Perempuan", "Lansia", "Kepala Rumah Tangga Perempuan", "Anggota Rumah Tangga",
                     "Rumah Tangga Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan Populasi", "Kemiskinan", "Buta Huruf",
                     "Pelatihan Kesiapsiagaan Bencana", "Rentan Bencana", "Kepemilikan Rumah (Sewa)", "Drainase", "Sumber Air", "Populasi","Provinsi","Kabupaten/Kota"),
        Tipe_Data = c("String", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
                      "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
                      "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik","String","String"),
        Deskripsi = c(
          "Kode unik untuk wilayah/distrik di Indonesia",
          "Persentase populasi di bawah lima tahun",
          "Persentase populasi perempuan",
          "Persentase populasi 65 tahun ke atas",
          "Persentase rumah tangga dengan kepala rumah tangga perempuan",
          "Rata-rata jumlah anggota rumah tangga dalam satu distrik",
          "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan",
          "Persentase populasi 15 tahun ke atas dengan pendidikan rendah",
          "Persentase perubahan populasi",
          "Persentase penduduk miskin",
          "Persentase populasi yang tidak bisa membaca dan menulis",
          "Persentase rumah tangga yang tidak mendapatkan pelatihan bencana",
          "Persentase rumah tangga yang tinggal di daerah rawan bencana",
          "Persentase rumah tangga yang menyewa rumah",
          "Persentase rumah tangga yang tidak memiliki sistem drainase",
          "Persentase rumah tangga yang menggunakan air pipa",
          "Jumlah total populasi",
          "Nama provinsi sesuai periode data",
          "Nama kabupaten/kota sesuai periode data"
        )
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")
    
    # Tabel Preview Data
    output$data_preview <- DT::renderDataTable({
      DT::datatable(
        values$current_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      )
    })
    
    # Handler Download Data Lengkap
    output$download_data <- downloadHandler(
      filename = function() {
        paste("data_lengkap_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(values$current_data, file, row.names = FALSE)
      }
    )
  })
}