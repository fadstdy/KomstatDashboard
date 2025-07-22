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
               tabPanel("Jumlah Penduduk Tertinggi",
                        br(),
                        fluidRow(
                          column(6,
                                 withSpinner(plotlyOutput(ns("population_bar"), height = "300px"))),
                          column(6,
                                 withSpinner(plotlyOutput(ns("population_line"), height = "300px")))
                        )
               ),
               tabPanel("Tingkat Buta Huruf Tertinggi",
                        br(),
                        fluidRow(
                          column(6,
                                 withSpinner(plotlyOutput(ns("illiterate_line"), height = "300px"))),
                          column(6,
                                 withSpinner(plotlyOutput(ns("illiterate_bar"), height = "300px")))
                        )
               )
             )
           )
    ),
    
    # Bagian Fitur Dashboard
    column(12,
           box(
             title = "Fitur Dashboard",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             height = "auto",
             div(
               h4("Fitur Analisis Utama:"),
               hr(),
               tags$ul(
                 tags$li(strong("Manajemen Data: "), "Transformasi dan kategorisasi variabel, termasuk interpretasi output."),
                 tags$li(strong("Eksplorasi Data: "), "Statistik deskriptif dan visualisasi (histogram, boxplot, barplot), tabel ringkasan, dan peta (jika memungkinkan), disertai interpretasi."),
                 tags$li(strong("Uji Asumsi: "), "Pengujian normalitas (Shapiro-Wilk, Q-Q plot), homogenitas varians (Levene, Bartlett), multikolinearitas (VIF), dan heteroskedastisitas (ncvTest, Breusch-Pagan) dengan interpretasi."),
                 tags$li(strong("Statistik Inferensia: "), "Uji beda rata-rata (satu/dua kelompok), uji proporsi (satu/dua kelompok), uji variansi, ANOVA (satu/dua arah), dan penjelasan hasil."),
                 tags$li(strong("Regresi Linear Berganda: "), "Analisis regresi berganda lengkap dengan uji asumsi (normalitas residual, multikolinearitas, heteroskedastisitas), serta penjelasan parameter dan kesimpulan model.")
               ),
               br(),
               h4("Fitur Ekspor:"),
               hr(),
               tags$ul(
                 tags$li("Download grafik (PNG/PDF)"),
                 tags$li("Export tabel (CSV/Excel)"),
                 tags$li("Laporan lengkap (Word/PDF)"),
                 tags$li("Tombol unduh gabungan untuk seluruh isi tiap menu.")
               )
             )
           )
    ),
    
    # Bagian Informasi Dataset Utama
    column(12,
           box(
             title = "Informasi Dataset Utama",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             height = "auto",
             div(
               h4("Dataset: Data Kerentanan Sosial di Indonesia"),
               hr(),
               tags$ul(
                 tags$li(strong("Sumber Data Utama: "), "Survei Sosial Ekonomi Nasional (SUSENAS) 2017 oleh BPS-Statistics Indonesia"),
                 tags$li(strong("Periode Data: "), "2017"),
                 tags$li(strong("Unit Analisis: "), "Kabupaten/Kota di Indonesia"),
                 tags$li(strong("Cakupan Geografis: "), "511 Kabupaten/Kota di Seluruh Indonesia")
               ),
               br(),
               h5("Penjelasan Variabel Utama:"),
               withSpinner(tableOutput(ns("variable_descriptions_table"))),
               br()
             )
           )
    ),
    
    # Bagian Preview Data
    column(12,
           box(
             title = "Preview Data",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = FALSE,
             div(
               h4("Struktur Data:"),
               withSpinner(DT::dataTableOutput(ns("data_preview"))),
               br(),
               downloadButton(ns("download_data"), "Download Data Lengkap",
                              class = "btn-primary")
             )
           )
    )
  )
}

# Fungsi Server untuk Beranda - DIPERBAIKI
berandaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Value Boxes
    output$n_obs_valuebox <- renderValueBox({
      valueBox(
        value = nrow(values$current_data),
        subtitle = "Jumlah Kabupaten/Kota",
        icon = icon("map-marker-alt"),
        color = "aqua"
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
        color = "green"
      )
    })
    
    # --- Bagian Reactive untuk Data Top 5 - DIPERBAIKI ---
    get_top_n_data <- function(data, var_name, n = 5) {
      # Tambahan validasi yang lebih ketat
      if (is.null(data) || nrow(data) == 0) return(NULL)
      if (!var_name %in% names(data)) return(NULL)
      if (!"CITY_NAME" %in% names(data)) return(NULL)
      
      # Filter data yang valid (tidak NA)
      data_clean <- data[!is.na(data[[var_name]]), ]
      if (nrow(data_clean) == 0) return(NULL)
      
      # Urutkan dan ambil top n
      data_clean[order(-data_clean[[var_name]]), ][1:min(n, nrow(data_clean)), ]
    }
    
    # Reactive data dengan error handling
    top5_poverty_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "POVERTY")
    })
    
    top5_noelectric_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "NOELECTRIC")
    })
    
    top5_population_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "POPULATION")
    })
    
    top5_illiterate_data <- reactive({
      req(values$current_data)
      get_top_n_data(values$current_data, "ILLITERATE")
    })
    
    # --- Render Plot dengan Error Handling yang Lebih Baik ---
    
    # Plot untuk Tingkat Kemiskinan Tertinggi
    output$poverty_bar <- renderPlotly({
      tryCatch({
        data <- top5_poverty_data()
        if (is.null(data)) {
          # Return empty plotly jika data kosong
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
        # Error fallback
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    output$poverty_line <- renderPlotly({
      tryCatch({
        data <- top5_population_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_line_chart(data,
                                       value_col = "POPULATION",
                                       name_col = "CITY_NAME",
                                       title = "Top 5 Daerah dengan Tingkat Kemiskinan Tertinggi",
                                       x_label = "Kabupaten/Kota",
                                       y_label = "Tingkat Kemiskinan %")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    # Plot untuk Jumlah Penduduk Tertinggi
    output$population_bar <- renderPlotly({
      tryCatch({
        data <- top5_population_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_bar_plot(data,
                                     value_col = "POPULATION",
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
    
    output$population_line <- renderPlotly({
      tryCatch({
        data <- top5_population_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_line_chart(data,
                                       value_col = "POPULATION",
                                       name_col = "CITY_NAME",
                                       title = "Top 5 Daerah dengan Jumlah Penduduk Tertinggi",
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
                                       title = "Top 5 Daerah dengan Tingkat Buta Huruf Tertinggi",
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
    
    output$illiterate_bar <- renderPlotly({
      tryCatch({
        data <- top5_population_data()
        if (is.null(data)) {
          plotly::plot_ly() %>% 
            plotly::add_text(text = "Data tidak tersedia", 
                             x = 0.5, y = 0.5, showlegend = FALSE)
        } else {
          p <- create_top_n_bar_plot(data,
                                     value_col = "POPULATION",
                                     name_col = "CITY_NAME",
                                     title = "Top 5 Daerah dengan Tingkat Buta Huruf Tertinggi",
                                     x_label = "Jumlah Penduduk",
                                     fill_low = "lightcyan", fill_high = "darkcyan")
          ggplotly(p, tooltip = "text")
        }
      }, error = function(e) {
        plotly::plot_ly() %>% 
          plotly::add_text(text = paste("Error:", e$message), 
                           x = 0.5, y = 0.5, showlegend = FALSE)
      })
    })
    
    # Tabel Deskripsi Variabel
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