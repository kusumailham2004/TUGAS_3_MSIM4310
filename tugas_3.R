library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

# 1. Membaca Dataset Excel sesuai lokasi file di komputer Anda
file_path <- "D:/KULIAH/ANALISIS DAN VISUALISASI DATA/TUGAS 3/Data_set_Tugas_tutorial_3_STSI4204.xlsx"
data_weather <- read_excel(file_path, sheet = "weather")

# Menambahkan kolom indeks urutan observasi (Hari ke-1 s.d. 366) untuk Line Plot
data_weather <- data_weather %>%
  mutate(Hari_Ke = row_number())

# Memisahkan nama variabel numerik dan variabel kategorikal secara otomatis
var_numerik <- names(data_weather)[sapply(data_weather, is.numeric)]
var_numerik_tanpa_idx <- setdiff(var_numerik, "Hari_Ke")
var_kategori <- names(data_weather)[sapply(data_weather, function(x) is.character(x) || is.factor(x))]

# 2. Antarmuka Pengguna (User Interface / UI)
ui <- fluidPage(
  titlePanel("Aplikasi Visualisasi Data Interaktif Cuaca (STSI4204)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Pengaturan Visualisasi"),
      
      # Pilihan 2: Jenis plot / tampilan yang ingin dilihat
      radioButtons(
        inputId = "jenis_plot",
        label = "Pilih Jenis Tampilan / Plot:",
        choices = c(
          "a. Scatter Plot Interaktif" = "scatter",
          "b. Line Plot Interaktif"    = "line",
          "c. Bar Plot Interaktif"     = "bar",
          "d. Tabel Data"              = "tabel"
        ),
        selected = "scatter"
      ),
      
      hr(),
      
      # Pilihan 1: Variabel yang ingin divisualisasikan (menyesuaikan jenis plot)
      conditionalPanel(
        condition = "input.jenis_plot == 'scatter'",
        selectInput("var_x", "Pilih Variabel Sumbu X (Numerik):",
                    choices = var_numerik_tanpa_idx, selected = "MinTemp"),
        selectInput("var_y", "Pilih Variabel Sumbu Y (Numerik):",
                    choices = var_numerik_tanpa_idx, selected = "MaxTemp"),
        selectInput("var_color", "Pilih Variabel Warna (Kategori):",
                    choices = var_kategori, selected = "RainTomorrow")
      ),
      
      conditionalPanel(
        condition = "input.jenis_plot == 'line'",
        selectInput("var_line", "Pilih Variabel Numerik (Tren Harian):",
                    choices = var_numerik_tanpa_idx, selected = "MaxTemp")
      ),
      
      conditionalPanel(
        condition = "input.jenis_plot == 'bar'",
        selectInput("var_cat", "Pilih Variabel Kategori (Sumbu X):",
                    choices = var_kategori, selected = "WindGustDir"),
        selectInput("var_num_bar", "Pilih Variabel Numerik (Rata-rata Sumbu Y):",
                    choices = var_numerik_tanpa_idx, selected = "MaxTemp")
      ),
      
      conditionalPanel(
        condition = "input.jenis_plot == 'tabel'",
        selectInput("var_tabel", "Pilih Variabel yang Ditampilkan di Tabel:",
                    choices = names(data_weather)[names(data_weather) != "Hari_Ke"],
                    selected = c("MinTemp", "MaxTemp", "Rainfall", "Humidity3pm",
                                 "WindGustDir", "WindGustSpeed", "RainToday", "RainTomorrow"),
                    multiple = TRUE)
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.jenis_plot != 'tabel'",
        plotlyOutput("plot_interaktif", height = "520px")
      ),
      conditionalPanel(
        condition = "input.jenis_plot == 'tabel'",
        DTOutput("tabel_interaktif")
      )
    )
  )
)

# 3. Logika Server (Server Logic)
server <- function(input, output, session) {
  
  output$plot_interaktif <- renderPlotly({
    req(input$jenis_plot != "tabel")
    
    # a. Scatter Plot Interaktif
    if (input$jenis_plot == "scatter") {
      req(input$var_x, input$var_y, input$var_color)
      df_plot <- data_weather %>%
        filter(!is.na(.data[[input$var_x]]),
               !is.na(.data[[input$var_y]]),
               !is.na(.data[[input$var_color]]))
      
      p <- ggplot(df_plot, aes(x = .data[[input$var_x]],
                               y = .data[[input$var_y]],
                               color = .data[[input$var_color]])) +
        geom_point(alpha = 0.75, size = 2.5) +
        labs(
          title = paste("Scatter Plot:", input$var_x, "vs", input$var_y),
          x = input$var_x,
          y = input$var_y,
          color = input$var_color
        ) +
        theme_minimal()
      
      ggplotly(p)
      
      # b. Line Plot Interaktif
    } else if (input$jenis_plot == "line") {
      req(input$var_line)
      df_plot <- data_weather %>%
        filter(!is.na(.data[[input$var_line]]))
      
      p <- ggplot(df_plot, aes(x = Hari_Ke, y = .data[[input$var_line]])) +
        geom_line(color = "#1f77b4", linewidth = 0.8) +
        geom_point(color = "#1f77b4", size = 1.2, alpha = 0.6) +
        labs(
          title = paste("Line Plot Tren Harian:", input$var_line),
          x = "Urutan Observasi (Hari ke-)",
          y = input$var_line
        ) +
        theme_minimal()
      
      ggplotly(p)
      
      # c. Bar Plot Interaktif
    } else if (input$jenis_plot == "bar") {
      req(input$var_cat, input$var_num_bar)
      df_bar <- data_weather %>%
        filter(!is.na(.data[[input$var_cat]]),
               !is.na(.data[[input$var_num_bar]])) %>%
        group_by(Kategori = .data[[input$var_cat]]) %>%
        summarise(Rata_Rata = round(mean(.data[[input$var_num_bar]], na.rm = TRUE), 2),
                  .groups = "drop")
      
      p <- ggplot(df_bar, aes(x = reorder(Kategori, -Rata_Rata), y = Rata_Rata)) +
        geom_col(fill = "#2b5c8f", alpha = 0.9) +
        labs(
          title = paste("Bar Plot Rata-rata", input$var_num_bar, "Berdasarkan", input$var_cat),
          x = input$var_cat,
          y = paste("Rata-rata", input$var_num_bar)
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    }
  })
  
  # d. Tabel Data Interaktif
  output$tabel_interaktif <- renderDT({
    req(input$jenis_plot == "tabel")
    kolom_pilih <- if (length(input$var_tabel) > 0) {
      input$var_tabel
    } else {
      names(data_weather)[names(data_weather) != "Hari_Ke"]
    }
    
    datatable(
      data_weather[, kolom_pilih, drop = FALSE],
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# 4. Menjalankan Aplikasi Shiny
shinyApp(ui = ui, server = server)
