
# 1. Memuat Library yang Dibutuhkan
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(bslib)
library(thematic)
library(shinyjs)
library(car)
library(scales)
library(stringr)
library(tibble)
library(multcomp)   # Untuk notasi huruf pada plot
# --- [MODIFIKASI] --- Library rmarkdown dihapus karena fitur laporan PDF dihilangkan.

# 2. Pengaturan Tema & Visual
thematic_shiny()
app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff", fg = "#0a2540", primary = "#007bff", secondary = "#6c757d",
  success = "#20c997", base_font = font_google("Poppins", local = FALSE),
  heading_font = font_google("Quicksand", local = FALSE),
  "card-bg" = "#ffffff", "card-border-width" = "0px"
)

# 3. User Interface (UI) - Tampilan Aplikasi
ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  tags$head(
    tags$style(HTML("
        body {
          background: linear-gradient(180deg, #e0f7fa 0%, #f8f9fa 100%);
          background-attachment: fixed;
        }
        .card { box-shadow: 0 4px 12px 0 rgba(0,0,0,0.07); }
        .btn-lg { font-weight: bold; }
        .table-sm { font-size: 0.875rem; }
        .assumption-card .card-body { padding-top: 1rem; }
        pre { white-space: pre-wrap; }
        /* --- [MODIFIKASI] --- Menghapus styling untuk tombol download */
      "))
  ),
  titlePanel(
    div(style="display: flex; align-items: center; justify-content: space-between;",
        h1("Analisis Rancangan Percobaan Statistik")
        # --- [MODIFIKASI] --- Menghapus tags$img untuk logo UNJ
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4(icon("sliders-h"), " Panel Kontrol"), hr(),
      fileInput("file_input", "Unggah File CSV Anda", accept = c("text/csv", ".csv")),
      radioButtons("design_type", "Pilih Jenis Rancangan:",
                   choices = c("Rancangan Acak Lengkap (RAL)" = "RAL", "Rancangan Acak Kelompok (RAK)" = "RAK")),
      uiOutput("treatment_var_ui"),
      conditionalPanel("input.design_type == 'RAK'", uiOutput("block_var_ui")),
      uiOutput("response_var_ui"),
      
      # FITUR BARU: Opsi Transformasi Data
      selectInput("transformation_type", "Transformasi Data Respons (jika perlu):",
                  choices = c("Tidak Ada (Asli)" = "none",
                              "Logaritma Natural (Log)" = "log",
                              "Akar Kuadrat (Sqrt)" = "sqrt")),
      
      sliderInput("alpha", "Pilih Taraf Nyata (α):", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      div(actionButton("run_analysis", "Jalankan Analisis", icon = icon("play-circle"), class = "btn-success btn-lg w-100"), align = "center")
      
      # --- [MODIFIKASI] --- Menghapus tombol Download Laporan PDF dari UI
      
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Panduan", icon = icon("book-open"), br(),
                 card(card_header(tags$h5(icon("list-ol"), " Langkah Penggunaan")),
                      card_body(tags$ol(
                        tags$li(HTML("<b>Siapkan Data:</b> Pastikan data Anda dalam format file <b>.csv</b>. Lihat contoh format di bawah untuk RAL dan RAK.")),
                        tags$li(HTML("<b>Unggah File:</b> Pada 'Panel Kontrol', klik tombol 'Browse...' atau 'Unggah File' dan pilih file .csv Anda.")),
                        tags$li(HTML("<b>Konfigurasi Analisis:</b> Pilih 'Jenis Rancangan' (RAL/RAK) dan tentukan kolom mana yang berfungsi sebagai 'Perlakuan', 'Kelompok' (jika RAK), dan 'Respons' dari menu dropdown yang muncul.")),
                        tags$li(HTML("<b>(Opsional) Pilih Transformasi:</b> Jika Anda menduga asumsi klasik (normalitas/homogenitas) tidak akan terpenuhi, Anda dapat memilih metode transformasi data untuk variabel respons.")),
                        tags$li(HTML("<b>Jalankan Analisis:</b> Klik tombol hijau 'Jalankan Analisis'. Aplikasi akan memproses data Anda.")),
                        tags$li(HTML("<b>Periksa Asumsi:</b> Setelah analisis berjalan, Anda akan otomatis diarahkan ke tab 'Diagnostik & Uji Asumsi'. Jika asumsi tidak terpenuhi, coba kembali ke 'Panel Kontrol', pilih 'Transformasi Data', dan jalankan ulang analisis.")),
                        # --- [MODIFIKASI] --- Mengubah teks langkah terakhir karena tidak ada lagi tombol download
                        tags$li(HTML("<b>Lihat Hasil:</b> Jika asumsi terpenuhi, lanjutkan ke tab 'Hasil ANOVA' dan 'Uji Lanjut' untuk melihat semua hasil analisis."))
                      ))),
                 card(card_header(tags$h5(icon("file-csv"), " Contoh Format Data RAL")),
                      card_body(
                        tags$p("Data untuk RAL minimal memiliki 2 kolom."),
                        tags$table(class = "table table-bordered table-sm", style = "width: auto;",
                                   tags$thead(tags$tr(tags$th("Perlakuan"), tags$th("Respons"))),
                                   tags$tbody(
                                     tags$tr(tags$td("Pupuk A"), tags$td("25.5")), tags$tr(tags$td("Pupuk A"), tags$td("26.7")),
                                     tags$tr(tags$td("Pupuk A"), tags$td("24.9")), tags$tr(tags$td("Pupuk B"), tags$td("28.1")),
                                     tags$tr(tags$td("Pupuk B"), tags$td("29.0"))
                                   )))),
                 card(card_header(tags$h5(icon("file-csv"), " Contoh Format Data RAK")),
                      card_body(
                        tags$p("Data untuk RAK minimal memiliki 3 kolom."),
                        tags$table(class = "table table-bordered table-sm", style = "width: auto;",
                                   tags$thead(tags$tr(tags$th("Perlakuan"), tags$th("Kelompok"), tags$th("Respons"))),
                                   tags$tbody(
                                     tags$tr(tags$td("Varietas A"), tags$td("Blok 1"), tags$td("5.2")), tags$tr(tags$td("Varietas B"), tags$td("Blok 1"), tags$td("5.9")),
                                     tags$tr(tags$td("Varietas A"), tags$td("Blok 2"), tags$td("5.4")), tags$tr(tags$td("Varietas B"), tags$td("Blok 2"), tags$td("6.1")),
                                     tags$tr(tags$td("Varietas A"), tags$td("Blok 3"), tags$td("5.0"))
                                   ))))
        ),
        tabPanel("Data Anda", icon = icon("table"), DTOutput("data_table")),
        tabPanel("Boxplot", icon = icon("chart-pie"), plotOutput("boxplot")),
        tabPanel("Diagnostik & Uji Asumsi", icon = icon("stethoscope"), br(),
                 uiOutput("transformation_warning_ui"), # Peringatan jika transformasi diterapkan
                 card(class="assumption-card",
                      card_header(tags$h5(icon("chart-line"), "Asumsi Normalitas Galat")),
                      card_body(
                        plotOutput("qq_plot", height = "300px"), hr(),
                        tags$h6("Output Statistik (Uji Shapiro-Wilk)", style="font-weight:bold;"),
                        verbatimTextOutput("shapiro_test_raw_output"), hr(),
                        uiOutput("shapiro_interpretation_output")
                      )
                 ),
                 card(class="assumption-card",
                      card_header(tags$h5(icon("chart-scatter"), "Asumsi Homogenitas Ragam")),
                      card_body(
                        plotOutput("residual_plot", height = "300px"), hr(),
                        tags$h6("Output Statistik (Uji Levene)", style="font-weight:bold;"),
                        verbatimTextOutput("levene_test_raw_output"), hr(),
                        uiOutput("levene_interpretation_output")
                      )
                 ),
                 # --- [MODIFIKASI] --- Menambahkan card baru untuk Uji Independensi Galat
                 card(class="assumption-card",
                      card_header(tags$h5(icon("wave-square"), "Asumsi Independensi Galat")),
                      card_body(
                        tags$h6("Output Statistik (Uji Durbin-Watson)", style="font-weight:bold;"),
                        verbatimTextOutput("durbin_watson_test_raw_output"), hr(),
                        uiOutput("durbin_watson_interpretation_output")
                      )
                 )
        ),
        tabPanel("Hasil ANOVA", icon = icon("calculator"), br(),
                 card(card_header(tags$h5(icon("lightbulb"), " Hipotesis Uji")), 
                      card_body(htmlOutput("hypotheses_output"))),
                 card(card_header(tags$h5(icon("table-list"), " Tabel Analisis Varians (ANOVA)")), 
                      card_body(verbatimTextOutput("anova_result"))),
                 card(card_header(tags$h5(icon("percentage"), " Presisi Percobaan")), 
                      card_body(uiOutput("cv_output"))),
                 card(card_header(tags$h5(icon("comment-dots"), " Interpretasi Hasil ANOVA")), 
                      card_body(htmlOutput("anova_interpretation_output")))
        ),
        tabPanel("Uji Lanjut", icon = icon("vials"), br(),
                 uiOutput("uji_lanjut_ui_output")
        )
      )
    )
  )
)

# 4. Server Logic
server <- function(input, output, session) {
  
  # (Bagian reaktif untuk data input dan seleksi variabel)
  data_input <- reactive({ req(input$file_input); read.csv(input$file_input$datapath, header = TRUE, stringsAsFactors = TRUE) })
  output$treatment_var_ui <- renderUI({ req(data_input()); selectInput("treatment_var", "Pilih Variabel Perlakuan:", choices = names(data_input())) })
  output$block_var_ui <- renderUI({ req(data_input()); selectInput("block_var", "Pilih Variabel Kelompok/Blok:", choices = names(data_input())) })
  output$response_var_ui <- renderUI({ req(data_input()); numeric_cols <- names(data_input())[sapply(data_input(), is.numeric)]; selectInput("response_var", "Pilih Variabel Respons (Numerik):", choices = numeric_cols) })
  
  # FITUR BARU: Reaktif untuk data yang telah ditransformasi
  transformed_data <- reactive({
    req(data_input(), input$response_var, input$transformation_type)
    df <- data_input()
    resp_var <- input$response_var
    
    # Lakukan validasi sebelum transformasi
    if (input$transformation_type == "log" && any(df[[resp_var]] <= 0, na.rm = TRUE)) {
      showNotification("Transformasi Log tidak dapat diterapkan pada nilai non-positif (<= 0).", type = "error", duration = 10)
      return(NULL) # Kembalikan NULL jika transformasi gagal
    }
    if (input$transformation_type == "sqrt" && any(df[[resp_var]] < 0, na.rm = TRUE)) {
      showNotification("Transformasi Akar Kuadrat tidak dapat diterapkan pada nilai negatif.", type = "error", duration = 10)
      return(NULL) # Kembalikan NULL jika transformasi gagal
    }
    
    if (input$transformation_type == "log") {
      df[[resp_var]] <- log(df[[resp_var]])
    } else if (input$transformation_type == "sqrt") {
      df[[resp_var]] <- sqrt(df[[resp_var]])
    }
    
    return(df)
  })
  
  # FITUR BARU: UI untuk peringatan transformasi
  output$transformation_warning_ui <- renderUI({
    req(input$transformation_type)
    if (input$transformation_type != "none") {
      transform_name <- ifelse(input$transformation_type == "log", "Logaritma (Log)", "Akar Kuadrat (Sqrt)")
      card(
        style = "background-color: #fff3cd; border-color: #ffeeba;",
        card_body(
          tags$p(class = "text-dark",
                 icon("exclamation-triangle"), 
                 HTML(sprintf("<b>Peringatan:</b> Anda sedang melihat hasil analisis pada data yang telah ditransformasi menggunakan <b>%s</b>. Interpretasi harus dilakukan pada skala yang telah diubah.", transform_name))
          )
        )
      )
    }
  })
  
  
  analysis_results <- eventReactive(input$run_analysis, {
    req(transformed_data(), input$treatment_var, input$response_var, input$alpha)
    df <- transformed_data()
    # Pastikan data ada setelah potensi kegagalan transformasi
    req(df)
    
    df[[input$treatment_var]] <- as.factor(df[[input$treatment_var]])
    
    model <- tryCatch({
      if (input$design_type == "RAL") {
        formula_text <- paste0("`", input$response_var, "` ~ `", input$treatment_var, "`")
      } else {
        req(input$block_var)
        df[[input$block_var]] <- as.factor(df[[input$block_var]])
        formula_text <- paste0("`", input$response_var, "` ~ `", input$block_var, "` + `", input$treatment_var, "`")
      }
      aov(as.formula(formula_text), data = df)
    }, error = function(e) { return(NULL) })
    
    if (is.null(model)) return(list(error = TRUE, error_msg = "Gagal membuat model ANOVA. Periksa struktur data dan konfigurasi Anda."))
    
    summary_model <- summary(model)
    tukey_test <- TukeyHSD(model, which = input$treatment_var, conf.level = 1 - input$alpha)
    
    # FITUR BARU: Menghasilkan notasi huruf untuk boxplot
    cld_letters <- NULL
    p_value_treatment <- NULL
    
    summary_df <- summary_model[[1]]
    p_value_block <- NULL
    
    if (input$design_type == "RAK") {
      if (nrow(summary_df) >= 2) {
        p_value_block <- summary_df$`Pr(>F)`[1]
        p_value_treatment <- summary_df$`Pr(>F)`[2]
      }
    } else {
      if (nrow(summary_df) >= 1) {
        p_value_treatment <- summary_df$`Pr(>F)`[1]
      }
    }
    
    if (!is.null(p_value_treatment) && p_value_treatment < input$alpha) {
      try({
        # Menggunakan multcomp::cld untuk mendapatkan huruf
        tukey_cld <- multcomp::cld(multcomp::glht(model, linfct = mcp(treatment = "Tukey")), level = input$alpha)
        cld_letters_df <- as.data.frame(tukey_cld$mcletters$Letters)
        names(cld_letters_df) <- "cld"
        cld_letters_df <- cld_letters_df %>%
          rownames_to_column(var = "treatment")
        cld_letters <- cld_letters_df
      }, silent = TRUE)
    }
    
    ms_error <- anova(model)["Residuals", "Mean Sq"]
    # Gunakan mean dari data asli untuk CV jika data ditransformasi, untuk interpretasi yg lebih intuitif
    original_mean <- mean(data_input()[[input$response_var]], na.rm = TRUE)
    cv_value <- (sqrt(ms_error) / original_mean) * 100
    
    shapiro_res <- shapiro.test(residuals(model))
    levene_res <- leveneTest(model)
    
    # --- [MODIFIKASI] --- Menambahkan uji Durbin-Watson
    durbin_watson_res <- durbinWatsonTest(model)
    
    list(error = FALSE, model = model, model_summary = summary_model, tukey_summary = tukey_test, 
         p_value_treatment = p_value_treatment, p_value_block = p_value_block, 
         tukey_df = as.data.frame(tukey_test[[input$treatment_var]]), cv = cv_value,
         shapiro_test = shapiro_res, levene_test = levene_res,
         # --- [MODIFIKASI] --- Menyimpan hasil uji Durbin-Watson
         durbin_watson_test = durbin_watson_res,
         cld_results = cld_letters) # Menyimpan hasil notasi huruf
  })
  
  observeEvent(input$run_analysis, {
    # Validasi awal sebelum menjalankan analisis
    if (is.null(transformed_data())) {
      return() # Hentikan jika data transformasi gagal
    }
    shinyjs::disable("run_analysis")
    shinyjs::html("run_analysis", "Menganalisis...")
    updateTabsetPanel(session, inputId = "main_tabs", selected = "Diagnostik & Uji Asumsi")
    shinyjs::delay(1500, {
      shinyjs::enable("run_analysis")
      shinyjs::html("run_analysis", "Jalankan Analisis <i class='fa fa-play-circle' role='presentation' aria-label='play-circle icon'></i>")
    })
  })
  
  # (Sisa output lainnya tetap sama, dengan penyesuaian)
  output$data_table <- renderDT({ datatable(data_input(), options = list(pageLength = 10, scrollX = TRUE), class = 'cell-border stripe', filter = 'top') })
  output$hypotheses_output <- renderUI({req(analysis_results()); h_perlakuan <- div(tags$b("1. Pengaruh Perlakuan"), div(style="margin-left: 15px; margin-top: 5px;", HTML("<b>H&#8320;:</b> Rata-rata semua perlakuan sama.")), div(style="margin-left: 15px;", HTML("<b>H&#8321;:</b> Minimal ada satu rata-rata perlakuan yang berbeda."))); if (input$design_type == "RAK") { h_kelompok <- div(style="margin-top: 10px;", tags$b("2. Pengaruh Kelompok/Blok"), div(style="margin-left: 15px; margin-top: 5px;", HTML("<b>H&#8320;:</b> Tidak ada pengaruh dari kelompok.")), div(style="margin-left: 15px;", HTML("<b>H&#8321;:</b> Ada pengaruh dari kelompok."))); tagList(h_perlakuan, h_kelompok)} else { h_perlakuan }})
  output$anova_result <- renderPrint({ if(!is.null(analysis_results()$model_summary)) { print(analysis_results()$model_summary) } else { cat("Model ANOVA gagal dibuat. Periksa kembali data Anda.") }})
  output$cv_output <- renderUI({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(NULL); cv <- analysis_results()$cv; interp_cv <- if (cv < 10) {"Presisi percobaan sangat baik."} else if (cv >= 10 && cv < 20) {"Presisi percobaan baik."} else if (cv >= 20 && cv < 30) {"Presisi percobaan cukup."} else {"Presisi percobaan rendah."}; HTML(paste0("<div style='line-height: 1.2;'>", sprintf("<b>Koefisien Keragaman (KK): %.2f%%</b><br><small>%s</small>", cv, interp_cv), "</div>"))})
  output$anova_interpretation_output <- renderUI({ req(analysis_results()); res <- analysis_results(); if(isTRUE(res$error)) return(tags$p("Analisis gagal.", style="color:red;")); p_treat <- res$p_value_treatment; if(is.null(p_treat) || is.na(p_treat)) return(tags$p("Tidak dapat menghasilkan interpretasi.", style="color:red;")); alpha <- input$alpha; p_treat_formatted <- if(p_treat < 0.001) "<0.001" else format(round(p_treat, 4), nsmall = 4); if (p_treat < alpha) { line1 <- paste0("Karena p-value (", p_treat_formatted, ") < α (", alpha, "), maka keputusan untuk <b>menolak H₀</b> sudah tepat."); line2 <- "Menolak H₀ berarti terdapat bukti statistik untuk menyatakan adanya <b>perbedaan signifikan</b>."; interp_treat <- tagList(tags$p(HTML(line1)), tags$p(HTML(line2))) } else { line1 <- paste0("Karena p-value (", p_treat_formatted, ") ≥ α (", alpha, "), maka keputusan untuk <b>gagal menolak H₀</b> sudah tepat."); line2 <- "Gagal menolak H₀ berarti <b>tidak terdapat bukti statistik cukup</b> untuk menyatakan adanya perbedaan signifikan."; interp_treat <- tagList(tags$p(HTML(line1)), tags$p(HTML(line2)))}; interp_block <- NULL; if (input$design_type == "RAK" && !is.null(res$p_value_block)) {p_block <- res$p_value_block; p_block_formatted <- if(p_block < 0.001) "<0.001" else format(round(p_block, 4), nsmall = 4); if (p_block < alpha) { line1_b <- paste0("Karena p-value (", p_block_formatted, ") < α (", alpha, "), maka keputusan untuk <b>menolak H₀</b> sudah tepat."); line2_b <- "Ini menunjukkan bahwa pengelompokan (blocking) **efektif**."; interp_block <- tagList(tags$p(HTML(line1_b)), tags$p(HTML(line2_b))) } else { line1_b <- paste0("Karena p-value (", p_block_formatted, ") ≥ α (", alpha, "), maka keputusan untuk <b>gagal menolak H₀</b> sudah tepat."); line2_b <- "Ini menunjukkan bahwa pengelompokan (blocking) **tidak efektif**."; interp_block <- tagList(tags$p(HTML(line1_b)), tags$p(HTML(line2_b)))}}; tagList(tags$h6("Pengaruh Perlakuan:", style="font-weight:bold; margin-bottom: 0.5rem;"), interp_treat, if(!is.null(interp_block)) {tagList(hr(), tags$h6("Pengaruh Kelompok:", style="font-weight:bold; margin-bottom: 0.5rem;"), interp_block)})})
  output$uji_lanjut_ui_output <- renderUI({ req(analysis_results()); res <- analysis_results(); if(isTRUE(res$error)) return(NULL); p_treat <- res$p_value_treatment; if(is.null(p_treat) || is.na(p_treat)) return(NULL); if (p_treat >= input$alpha) { card(card_header(tags$h5(icon("info-circle"), "Informasi Uji Lanjut")), card_body(tags$p("Karena uji ANOVA menunjukkan perlakuan tidak berpengaruh nyata (p ≥ α), maka Uji Lanjut tidak relevan untuk dilakukan."))) } else { tagList(card(card_header(tags$h5(icon("table-cells-large"), "Hasil Uji Lanjut Tukey HSD")), card_body(verbatimTextOutput("tukey_result"))), card(card_header(tags$h5(icon("comment-check"), "Interpretasi Hasil Uji Lanjut")), card_body(htmlOutput("tukey_interpretation_output"))))}})
  output$tukey_result <- renderPrint({ analysis_results()$tukey_summary })
  
  # FITUR BARU: Boxplot dengan notasi huruf
  output$boxplot <- renderPlot({
    req(analysis_results(), transformed_data())
    res <- analysis_results()
    df <- transformed_data()
    if(isTRUE(res$error)) return(NULL)
    
    # Plot dasar
    p <- ggplot(df, aes_string(x = input$treatment_var, y = input$response_var, fill = input$treatment_var)) +
      geom_boxplot(alpha = 0.8, show.legend = FALSE, width = 0.7) +
      geom_jitter(width = 0.1, alpha = 0.6) +
      labs(title = "Distribusi Respons Berdasarkan Perlakuan",
           x = "Perlakuan", y = paste("Respons", if(input$transformation_type != "none") paste0("(", input$transformation_type, ")") else "")) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(size = 12), axis.text = element_text(size = 10))
    
    # Tambahkan notasi huruf jika ada
    if (!is.null(res$cld_results)) {
      # Hitung posisi y untuk teks
      stat_data <- df %>%
        group_by(!!sym(input$treatment_var)) %>%
        summarise(max_y = max(!!sym(input$response_var), na.rm = TRUE)) %>%
        rename(treatment = !!sym(input$treatment_var))
      
      # Gabungkan dengan hasil cld
      cld_data_with_pos <- res$cld_results %>%
        left_join(stat_data, by = "treatment")
      
      p <- p +
        geom_text(data = cld_data_with_pos, 
                  aes(x = treatment, y = max_y + (0.05 * max(stat_data$max_y, na.rm=TRUE)), label = str_trim(cld)),
                  vjust = -0.5, size = 5, fontface = "bold")
    }
    
    p
  })
  
  output$qq_plot <- renderPlot({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(NULL); model <- analysis_results()$model; plot(model, which = 2, pch = 19, col = "#007bff", main = "Normal Q-Q Plot dari Galat")})
  output$residual_plot <- renderPlot({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(NULL); model <- analysis_results()$model; plot(model, which = 1, pch = 19, col = "#007bff", main = "Plot Sisaan vs. Nilai Prediksi")})
  output$shapiro_test_raw_output <- renderPrint({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(cat("Uji tidak dapat dilakukan.")); analysis_results()$shapiro_test })
  output$shapiro_interpretation_output <- renderUI({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(NULL); test <- analysis_results()$shapiro_test; p_val <- test$p.value; p_val_formatted <- if(p_val < 0.001) "<0.001" else format(round(p_val, 4), nsmall = 4); alpha <- input$alpha; if(p_val > alpha) { line1 <- sprintf("<b>Interpretasi:</b> Karena p-value (%s) lebih besar dari taraf nyata (α = %s), kita <b>gagal menolak H₀</b>.", p_val_formatted, alpha); line2 <- "<b>Kesimpulan:</b> Asumsi normalitas galat <b>terpenuhi</b>."; style_color <- "color: #198754;" } else { line1 <- sprintf("<b>Interpretasi:</b> Karena p-value (%s) ≤ taraf nyata (α = %s), kita <b>menolak H₀</b>.", p_val_formatted, alpha); line2 <- "<b>Kesimpulan:</b> Asumsi normalitas galat <b>tidak terpenuhi</b>."; style_color <- "color: #dc3545;" }; tagList(tags$p(HTML(line1)), tags$p(HTML(line2), style = style_color), if(p_val <= alpha) tags$p(icon("lightbulb"), HTML("<i>Saran: Coba gunakan transformasi data (misal: Log atau Akar Kuadrat) pada panel kontrol dan jalankan ulang analisis.</i>")))})
  output$levene_test_raw_output <- renderPrint({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(cat("Uji tidak dapat dilakukan.")); analysis_results()$levene_test })
  output$levene_interpretation_output <- renderUI({ req(analysis_results()); if(isTRUE(analysis_results()$error)) return(NULL); test <- analysis_results()$levene_test; p_val <- test$`Pr(>F)`[1]; p_val_formatted <- if(p_val < 0.001) "<0.001" else format(round(p_val, 4), nsmall = 4); alpha <- input$alpha; if(p_val > alpha) { line1 <- sprintf("<b>Interpretasi:</b> Karena p-value (%s) lebih besar dari taraf nyata (α = %s), kita <b>gagal menolak H₀</b>.", p_val_formatted, alpha); line2 <- "<b>Kesimpulan:</b> Asumsi homogenitas ragam <b>terpenuhi</b>."; style_color <- "color: #198754;" } else { line1 <- sprintf("<b>Interpretasi:</b> Karena p-value (%s) ≤ taraf nyata (α = %s), kita <b>menolak H₀</b>.", p_val_formatted, alpha); line2 <- "<b>Kesimpulan:</b> Asumsi homogenitas ragam <b>tidak terpenuhi</b> (heteroskedastisitas)."; style_color <- "color: #dc3545;" }; tagList(tags$p(HTML(line1)), tags$p(HTML(line2), style = style_color), if(p_val <= alpha) tags$p(icon("lightbulb"), HTML("<i>Saran: Coba gunakan transformasi data (misal: Log atau Akar Kuadrat) pada panel kontrol dan jalankan ulang analisis.</i>")))})
  
  # --- [MODIFIKASI] --- Menambahkan output untuk Uji Durbin-Watson
  output$durbin_watson_test_raw_output <- renderPrint({
    req(analysis_results())
    if(isTRUE(analysis_results()$error)) return(cat("Uji tidak dapat dilakukan."))
    analysis_results()$durbin_watson_test
  })
  
  output$durbin_watson_interpretation_output <- renderUI({
    req(analysis_results())
    if(isTRUE(analysis_results()$error)) return(NULL)
    test <- analysis_results()$durbin_watson_test
    p_val <- test$p
    dw_stat <- test$dw
    p_val_formatted <- if(p_val < 0.001) "<0.001" else format(round(p_val, 4), nsmall = 4)
    alpha <- input$alpha
    
    if(p_val > alpha) {
      line1 <- sprintf("<b>Interpretasi:</b> Karena p-value (%s) lebih besar dari taraf nyata (α = %s), kita <b>gagal menolak H₀</b>. Nilai statistik D-W (%.2f) mendekati 2.", p_val_formatted, alpha, dw_stat)
      line2 <- "<b>Kesimpulan:</b> Asumsi independensi galat <b>terpenuhi</b> (tidak ada autokorelasi).";
      style_color <- "color: #198754;"
    } else {
      line1 <- sprintf("<b>Interpretasi:</b> Karena p-value (%s) ≤ taraf nyata (α = %s), kita <b>menolak H₀</b>.", p_val_formatted, alpha)
      line2 <- "<b>Kesimpulan:</b> Asumsi independensi galat <b>tidak terpenuhi</b> (terdapat bukti adanya autokorelasi antar galat).";
      style_color <- "color: #dc3545;"
    }
    tagList(
      tags$p(HTML(line1)),
      tags$p(HTML(line2), style = style_color),
      if(p_val <= alpha) tags$p(icon("lightbulb"), HTML("<i>Saran: Adanya autokorelasi seringkali disebabkan oleh data yang diukur secara berurutan (misal: time series) atau unit percobaan yang berdekatan secara spasial. Pastikan pengacakan dalam percobaan Anda sudah benar.</i>"))
    )
  })
  
  # Interpretasi Uji Lanjut yang lebih detail
  output$tukey_interpretation_output <- renderUI({
    req(analysis_results())
    res <- analysis_results()
    alpha <- input$alpha
    
    tukey_df <- res$tukey_df %>% as_tibble(rownames = "contrast")
    significant_pairs <- tukey_df %>% filter(`p adj` < alpha)
    
    if (nrow(significant_pairs) > 0) {
      detail_interpretations <- apply(significant_pairs, 1, function(row) {
        contrast <- row["contrast"]; groups <- stringr::str_split(contrast, "-")[[1]]
        group1 <- groups[1]; group2 <- groups[2]
        p_adj_val <- as.numeric(row["p adj"])
        p_adj_formatted <- if(p_adj_val < 0.001) "<0.001" else format(round(p_adj_val, 4), nsmall = 4)
        diff_val <- as.numeric(row["diff"])
        direction_text <- if(diff_val > 0) {paste0("lebih tinggi daripada Perlakuan <b>", group2, "</b>")} else {paste0("lebih rendah daripada Perlakuan <b>", group2, "</b>")}
        line1 <- sprintf("<b>Perbandingan %s:</b> Karena p-value yang disesuaikan (%s) lebih kecil dari α (%s), kita menyimpulkan ada perbedaan yang signifikan secara statistik antara Perlakuan <b>%s</b> dan <b>%s</b>.", contrast, p_adj_formatted, alpha, group1, group2)
        line2 <- sprintf("Secara spesifik, rata-rata respons untuk Perlakuan <b>%s</b> secara signifikan %s.", group1, direction_text)
        tagList(tags$p(HTML(line1)), tags$p(HTML(line2)), hr())
      })
      return(tagList(detail_interpretations))
    } else {
      return(HTML("<p>Meskipun uji ANOVA signifikan, Uji Lanjut Tukey tidak menemukan pasangan perlakuan spesifik yang berbeda nyata pada taraf α yang dipilih.</p>"))
    }
  })
  
  # --- [MODIFIKASI] --- Menghapus semua logika untuk download laporan PDF
  # output$download_report_ui <- renderUI(...) DIHAPUS
  # output$download_report <- downloadHandler(...) DIHAPUS
}

# 5. Menjalankan Aplikasi Shiny
shinyApp(ui = ui, server = server)