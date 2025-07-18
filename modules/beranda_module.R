# Beranda Module
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
    column(12, # Memastikan fluidRow value boxes sejajar dengan box lainnya
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
             title = "Fakta Data Utama: Top 5 Daerah",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel(
               tabPanel("Tingkat Kemiskinan Tertinggi",
                        br(),
                        withSpinner(tableOutput(ns("top5_poverty")))),
               tabPanel("Persentase Anak Tertinggi",
                        br(),
                        withSpinner(tableOutput(ns("top5_children")))),
               tabPanel("Rumah Tangga Tanpa Listrik Tertinggi",
                        br(),
                        withSpinner(tableOutput(ns("top5_noelectric")))),
               tabPanel("Jumlah Penduduk Tertinggi",
                        br(),
                        withSpinner(tableOutput(ns("top5_population")))),
               tabPanel("Tingkat Buta Huruf Tertinggi",
                        br(),
                        withSpinner(tableOutput(ns("top5_illiterate"))))
             )
           )
    ),
    
    # Bagian Fitur Dashboard (Lebar Penuh - dipindahkan)
    column(12, 
           box(
             title = "Fitur Dashboard",
             status = "success",
             solidHeader = TRUE,
             width = 12, 
             height = "auto",
             div(
               h4("🔧 Fitur Analisis Utama:"),
               hr(),
               tags$ul(
                 tags$li(strong("Manajemen Data: "), "Transformasi dan kategorisasi variabel, termasuk interpretasi output."),
                 tags$li(strong("Eksplorasi Data: "), "Statistik deskriptif dan visualisasi (histogram, boxplot, barplot), tabel ringkasan, dan peta (jika memungkinkan), disertai interpretasi."),
                 tags$li(strong("Uji Asumsi: "), "Pengujian normalitas (Shapiro-Wilk, Q-Q plot), homogenitas varians (Levene, Bartlett), multikolinearitas (VIF), dan heteroskedastisitas (ncvTest, Breusch-Pagan) dengan interpretasi."),
                 tags$li(strong("Statistik Inferensia: "), "Uji beda rata-rata (satu/dua kelompok), uji proporsi (satu/dua kelompok), uji variansi, ANOVA (satu/dua arah), dan penjelasan hasil."),
                 tags$li(strong("Regresi Linear Berganda: "), "Analisis regresi berganda lengkap dengan uji asumsi (normalitas residual, multikolinearitas, heteroskedastisitas), serta penjelasan parameter dan kesimpulan model.")
               ),
               br(),
               h4("📥 Fitur Ekspor:"), 
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
    
    # Bagian Informasi Dataset Utama (Lebar Penuh - dipindahkan dan dikonsolidasi)
    column(12, 
           box(
             title = "Informasi Dataset Utama",
             status = "info",
             solidHeader = TRUE,
             width = 12, 
             height = "auto",
             div(
               h4("📊 Dataset: Data Kerentanan Sosial di Indonesia"),
               hr(),
               tags$ul(
                 tags$li(strong("Sumber Data Utama: "), "Survei Sosial Ekonomi Nasional (SUSENAS) 2017 oleh BPS-Statistics Indonesia"),
                 tags$li(strong("Periode Data: "), "2017"), 
                 tags$li(strong("Unit Analisis: "), "Kabupaten/Kota di Indonesia"),
                 tags$li(strong("Cakupan Geografis: "), "511 Kabupaten/Kota di Seluruh Indonesia")
               ),
               br(),
               h5("📈 Penjelasan Variabel Utama:"), 
               withSpinner(tableOutput(ns("variable_descriptions_table"))), 
               br(),
               
               # Detail Metadata Artikel yang dikonsolidasi
               h5("📄 Detail Publikasi Artikel Pendukung:"),
               hr(),
               tags$ul(
                 tags$li(strong("Judul Artikel: "), "Revisiting social vulnerability analysis in Indonesia data"),
                 tags$li(strong("Penulis: "), "Robert Kurniawan, Bahrul Ilmi Nasution, Neli Agustina, Budi Yuniarto"),
                 tags$li(strong("Jurnal: "), "Data in Brief 40 (2022) 107743"),
                 tags$li(strong("Tanggal Publikasi: "), "Received: 6 Oktober 2021; Revised: 14 Desember 2021; Accepted: 20 Desember 2021; Available online: 23 Desember 2021"),
                 tags$li(strong("DOI Artikel: "), tags$a(href="https://doi.org/10.1016/j.dib.2021.107743", "10.1016/j.dib.2021.107743", target="_blank"), "")
               ),
               br(),
               h5("📝 Abstrak (Ringkasan Dataset dari Artikel):"),
               hr(),
               p("Makalah ini menyajikan dataset tentang kerentanan sosial di Indonesia. Dataset ini berisi beberapa dimensi yang didasarkan pada studi sebelumnya. Data dikompilasi terutama dari Survei Sosial Ekonomi Nasional (SUSENAS) 2017 yang dilakukan oleh BPS-Statistics Indonesia. Kami menggunakan bobot untuk mendapatkan estimasi berdasarkan sampling multistage. Kami juga menerima informasi tambahan tentang populasi, jumlah, dan pertumbuhan populasi dari proyeksi populasi BPS-Statistics Indonesia tahun 2017. Selanjutnya, kami menyediakan matriks jarak sebagai informasi pelengkap dan jumlah populasi untuk melakukan Fuzzy Geographically Weighted Clustering (FGWC). Data ini dapat digunakan untuk analisis lebih lanjut tentang kerentanan sosial untuk mempromosikan manajemen bencana.", style = "text-align: justify;")
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
               h4("📋 Struktur Data:"),
               withSpinner(DT::dataTableOutput(ns("data_preview"))),
               br(),
               downloadButton(ns("download_data"), "Download Data Lengkap",
                              class = "btn-primary")
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
    
    # Tabel Top 5 (menggunakan CITY_NAME dan PROVINCE_NAME)
    output$top5_poverty <- renderTable({
      req(values$current_data)
      data <- values$current_data
      # Perbaikan: Pastikan kolom CITY_NAME dan PROVINCE_NAME ada di data gabungan
      if (all(c("POVERTY", "CITY_NAME", "PROVINCE_NAME") %in% names(data))) {
        data %>%
          arrange(desc(POVERTY)) %>%
          head(5) %>%
          select(`Nama Kabupaten/Kota` = CITY_NAME, `Nama Provinsi` = PROVINCE_NAME, `Tingkat Kemiskinan (%)` = POVERTY)
      } else {
        data.frame(Info = "Kolom 'CITY_NAME' atau 'PROVINCE_NAME' tidak ditemukan di data utama. Mohon pastikan file 'data_sovi.csv' mengandung kolom-kolom nama lokasi.")
      }
    })
    
    output$top5_children <- renderTable({
      req(values$current_data)
      data <- values$current_data
      if (all(c("CHILDREN", "CITY_NAME", "PROVINCE_NAME") %in% names(data))) {
        data %>%
          arrange(desc(CHILDREN)) %>%
          head(5) %>%
          select(`Nama Kabupaten/Kota` = CITY_NAME, `Nama Provinsi` = PROVINCE_NAME, `Persentase Anak (%)` = CHILDREN)
      } else {
        data.frame(Info = "Kolom 'CITY_NAME' atau 'PROVINCE_NAME' tidak ditemukan di data utama. Mohon pastikan file 'data_sovi.csv' mengandung kolom-kolom nama lokasi.")
      }
    })
    
    output$top5_noelectric <- renderTable({
      req(values$current_data)
      data <- values$current_data
      if (all(c("NOELECTRIC", "CITY_NAME", "PROVINCE_NAME") %in% names(data))) {
        data %>%
          arrange(desc(NOELECTRIC)) %>%
          head(5) %>%
          select(`Nama Kabupaten/Kota` = CITY_NAME, `Nama Provinsi` = PROVINCE_NAME, `Tanpa Listrik (%)` = NOELECTRIC)
      } else {
        data.frame(Info = "Kolom 'CITY_NAME' atau 'PROVINCE_NAME' tidak ditemukan di data utama. Mohon pastikan file 'data_sovi.csv' mengandung kolom-kolom nama lokasi.")
      }
    })
    
    output$top5_population <- renderTable({
      req(values$current_data)
      data <- values$current_data
      if (all(c("POPULATION", "CITY_NAME", "PROVINCE_NAME") %in% names(data))) {
        data %>%
          arrange(desc(POPULATION)) %>%
          head(5) %>%
          select(`Nama Kabupaten/Kota` = CITY_NAME, `Nama Provinsi` = PROVINCE_NAME, `Jumlah Penduduk` = POPULATION)
      } else {
        data.frame(Info = "Kolom 'CITY_NAME' atau 'PROVINCE_NAME' tidak ditemukan di data utama. Mohon pastikan file 'data_sovi.csv' mengandung kolom-kolom nama lokasi.")
      }
    })
    
    output$top5_illiterate <- renderTable({
      req(values$current_data)
      data <- values$current_data
      if (all(c("ILLITERATE", "CITY_NAME", "PROVINCE_NAME") %in% names(data))) {
        data %>%
          arrange(desc(ILLITERATE)) %>%
          head(5) %>%
          select(`Nama Kabupaten/Kota` = CITY_NAME, `Nama Provinsi` = PROVINCE_NAME, `Tingkat Buta Huruf (%)` = ILLITERATE)
      } else {
        data.frame(Info = "Kolom 'CITY_NAME' atau 'PROVINCE_NAME' tidak ditemukan di data utama. Mohon pastikan file 'data_sovi.csv' mengandung kolom-kolom nama lokasi.")
      }
    })
    
    # Tabel Deskripsi Variabel (dalam Bahasa Indonesia)
    output$variable_descriptions_table <- renderTable({
      data.frame(
        Label = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                  "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                  "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
        Variabel = c("Kode Distrik", "Anak-anak", "Perempuan", "Lansia", "Kepala Rumah Tangga Perempuan", "Anggota Rumah Tangga", 
                     "Rumah Tangga Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan Populasi", "Kemiskinan", "Buta Huruf", 
                     "Pelatihan Kesiapsiagaan Bencana", "Rentan Bencana", "Kepemilikan Rumah (Sewa)", "Drainase", "Sumber Air", "Populasi"),
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
          "Jumlah total populasi"
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