# 4. Server Logic
server <- function(input, output, session) {
  
  data_input <- reactive({ req(input$file_input); read.csv(input$file_input$datapath, header = TRUE, stringsAsFactors = TRUE) })
  output$treatment_var_ui <- renderUI({ req(data_input()); selectInput("treatment_var", "Pilih Variabel Perlakuan:", choices = names(data_input())) })
  output$block_var_ui <- renderUI({ req(data_input()); selectInput("block_var", "Pilih Variabel Kelompok/Blok:", choices = names(data_input())) })
  output$response_var_ui <- renderUI({ req(data_input()); numeric_cols <- names(data_input())[sapply(data_input(), is.numeric)]; selectInput("response_var", "Pilih Variabel Respons (Numerik):", choices = numeric_cols) })
  
  # --- [FITUR BARU] Otomatis memilih jenis rancangan berdasarkan nama kolom ---
  observeEvent(input$file_input, {
    req(input$file_input)
    df <- read.csv(input$file_input$datapath, header = TRUE, nrows = 1) # Cukup baca baris pertama untuk nama kolom
    col_names <- tolower(names(df))
    
    # Daftar kata kunci untuk variabel kelompok/blok
    block_keywords <- c("kelompok", "blok", "block", "ulangan", "replikasi", "replication")
    
    # Periksa apakah ada nama kolom yang cocok dengan kata kunci
    is_rak_data <- any(block_keywords %in% col_names)
    
    if (is_rak_data) {
      # Jika ditemukan kata kunci, pilih RAK
      updateRadioButtons(session, "design_type", selected = "RAK")
      showNotification("Terdeteksi kolom kelompok. Jenis rancangan diubah ke RAK.", type = "message")
    } else {
      # Jika tidak, biarkan default (RAL)
      updateRadioButtons(session, "design_type", selected = "RAL")
    }
  })
  # --- AKHIR FITUR BARU ---
  
  transformed_data <- reactive({
    req(data_input(), input$response_var, input$transformation_type)
    df <- data_input()
    resp_var <- input$response_var
    
    if (input$transformation_type == "log" && any(df[[resp_var]] <= 0, na.rm = TRUE)) {
      showNotification("Transformasi Log tidak dapat diterapkan pada nilai non-positif (<= 0).", type = "error", duration = 10)
      return(NULL)
    }
    if (input$transformation_type == "sqrt" && any(df[[resp_var]] < 0, na.rm = TRUE)) {
      showNotification("Transformasi Akar Kuadrat tidak dapat diterapkan pada nilai negatif.", type = "error", duration = 10)
      return(NULL)
    }
    
    if (input$transformation_type == "log") {
      df[[resp_var]] <- log(df[[resp_var]])
    } else if (input$transformation_type == "sqrt") {
      df[[resp_var]] <- sqrt(df[[resp_var]])
    }
    
    return(df)
  })