library(shiny)
library(bslib)
library(mirt)
library(DT)
library(readxl)

# --- UI (User Interface) ---
ui <- page_sidebar(
  title = "MultiArgue: Multidimensional GRM Assessment",
  
  # Kustomisasi Palet Warna: Biru Dominan + Sentuhan Kuning/Emas
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    primary = "#1E40AF",      # Royal Blue untuk tombol utama & elemen aktif
    secondary = "#EAB308",    # Amber Gold untuk aksen sekunder
    success = "#059669",      # Hijau Emerald untuk tombol download
    warning = "#D97706",      # Kuning Tua untuk peringatan
    bg = "#F8FAFC",           # Background terang yang lembut
    fg = "#0F172A"            # Teks gelap kontras
  ),
  
  sidebar = sidebar(
    title = "Data Management",
    fileInput("file_data", "Upload Data File (CSV / XLSX)",
              accept = c(".csv", ".xlsx", ".xls",
                         "text/csv", 
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         "application/vnd.ms-excel")),
    helpText("Data Format: 36 Items (Scored 1-5).\nCE: Items 1-6, 19-24\nEO: Items 7-12, 25-30\nRA: Items 13-18, 31-36"),
    hr(),
    actionButton("btn_analisis", "Run MGRM Estimation", class = "btn-primary w-100", style = "background-color: #1E40AF; border-color: #1E40AF;"),
    hr(),
    div(class = "p-2 rounded text-center", style = "background-color: #FEF3C7; border: 1px solid #FCD34D;",
        p("Developed by:", style = "margin-bottom: 2px; font-weight: bold; color: #92400E;"),
        p("Febrian Solikhin", style = "margin-bottom: 0; color: #78350F; font-size: 0.85rem;"),
        p("Antuni Wiyarsi", style = "margin-bottom: 0; color: #78350F; font-size: 0.85rem;"),
        p("Ali Muhson", style = "margin-bottom: 0; color: #78350F; font-size: 0.85rem;")
    )
  ),
  
  navset_card_tab(
    nav_panel(
      title = "Argumentation Rubric Guide",
      div(class = "p-3",
          h5("1. Argumentation Dimensions Overview", style = "color: #1E40AF; font-weight: bold;"),
          p("The 36 assessment items are categorized into three core argumentation dimensions:"),
          div(class = "row mb-4",
              div(class = "col-md-4",
                  div(class = "card h-100 mb-2", style = "border: 2px solid #1E40AF;",
                      div(class = "card-header text-white font-weight-bold", style = "background-color: #1E40AF;", "Constructing Explanation (CE)"),
                      div(class = "card-body",
                          p(class = "card-text", "Ability to construct scientifically sound explanations, formulate claims, and support them with empirical evidence and reasoning."),
                          p(class = "card-text small mb-0", style = "color: #D97706;", tags$b("Assessed Items: "), "Items 1-6 & 19-24 (12 Items)")
                      )
                  )
              ),
              div(class = "col-md-4",
                  div(class = "card h-100 mb-2", style = "border: 2px solid #D97706;",
                      div(class = "card-header text-white font-weight-bold", style = "background-color: #D97706;", "Evaluating Opinions (EO)"),
                      div(class = "card-body",
                          p(class = "card-text", "Ability to critically evaluate statements, assess the quality/validity of arguments, and analyze opposing scientific opinions."),
                          p(class = "card-text small mb-0", style = "color: #1E40AF;", tags$b("Assessed Items: "), "Items 7-12 & 25-30 (12 Items)")
                      )
                  )
              ),
              div(class = "col-md-4",
                  div(class = "card h-100 mb-2", style = "border: 2px solid #1E3A8A;",
                      div(class = "card-header text-white font-weight-bold", style = "background-color: #1E3A8A;", "Representing Arguments (RA)"),
                      div(class = "card-body",
                          p(class = "card-text", "Ability to represent arguments clearly using multi-representations, structured reasoning, and addressing counter-arguments/rebuttals."),
                          p(class = "card-text small mb-0", style = "color: #D97706;", tags$b("Assessed Items: "), "Items 13-18 & 31-36 (12 Items)")
                      )
                  )
              )
          ),
          hr(),
          h5("2. Polytomous Scoring Rubric (1-5 Scale)", style = "color: #1E40AF; font-weight: bold;"),
          p("Reference rubric for polytomous argumentation scoring based on CER+R framework across all dimensions:"),
          DTOutput("tabel_rubrik")
      )
    ),
    nav_panel(
      title = "Item Fit Analysis",
      div(class = "p-3",
          div(class = "d-flex justify-content-between align-items-center mb-3",
              p(class = "mb-0", "Orlando & Thissen (2000) S-X2 Item Fit statistic. Items with p < 0.05 are flagged as misfitting."),
              downloadButton("dl_item_fit", "Download Item Fit (CSV)", class = "btn-success btn-sm")
          ),
          DTOutput("tabel_fit")
      )
    ),
    nav_panel(
      title = "Global Model Fit (M2*)",
      div(class = "p-3",
          div(class = "d-flex justify-content-between align-items-center mb-3",
              p(class = "mb-0", "Global structural model fit evaluation via M2* statistic (CFI, TLI, and RMSEA):"),
              downloadButton("dl_global_fit", "Download Global Fit (CSV)", class = "btn-success btn-sm")
          ),
          DTOutput("tabel_global_fit")
      )
    ),
    nav_panel(
      title = "Multidimensional Parameters",
      div(class = "p-3",
          uiOutput("card_reliability"),
          br(),
          div(class = "d-flex justify-content-between align-items-center mb-3",
              p(class = "mb-0", "Multidimensional item parameters: Discriminations (a1=CE, a2=EO, a3=RA) and Thresholds (b1-b4)."),
              downloadButton("dl_item_params", "Download Parameters (CSV)", class = "btn-success btn-sm")
          ),
          DTOutput("tabel_parameter")
      )
    ),
    nav_panel(
      title = "Factor Correlations",
      div(class = "p-3",
          p("Correlation matrix between the three argumentation dimensions (CE, EO, RA):"),
          DTOutput("tabel_korelasi")
      )
    ),
    nav_panel(
      title = "Category Characteristic Curves (CCC)",
      div(class = "p-3",
          div(class = "d-flex justify-content-between align-items-center mb-3",
              uiOutput("select_item_ui"),
              downloadButton("dl_ccc_plot", "Download CCC Plot (PNG)", class = "btn-success btn-sm")
          ),
          plotOutput("plot_ccc", height = "450px")
      )
    ),
    nav_panel(
      title = "Individual Theta Scores",
      div(class = "p-3",
          div(class = "d-flex justify-content-between align-items-center mb-3",
              p(class = "mb-0", "Estimated latent argumentation ability (\u03b8) and SE for each dimension (CE, EO, RA)."),
              downloadButton("dl_theta_scores", "Download Theta Scores (CSV)", class = "btn-success btn-sm")
          ),
          DTOutput("tabel_theta")
      )
    ),
    nav_panel(
      title = "About & Citation",
      div(class = "p-4",
          h4("MultiArgue Application", style = "color: #1E40AF; font-weight: bold;"),
          p("An interactive R Shiny application for Multidimensional Graded Response Model (MGRM) assessment."),
          hr(),
          h5("Developers:"),
          tags$ul(
            tags$li(tags$b("Febrian Solikhin"), " - Lead Developer & Doctoral Researcher"),
            tags$li(tags$b("Antuni Wiyarsi"), " - Academic Advisor / Contributor"),
            tags$li(tags$b("Ali Muhson"), " - Academic Advisor / Contributor")
          ),
          br(),
          h5("How to Cite:"),
          div(class = "p-3 rounded", style = "background-color: #FEF3C7; border-left: 4px solid #EAB308;",
              p(style = "font-family: monospace; margin-bottom: 0; color: #78350F;",
                "Solikhin, F., Wiyarsi, A., & Muhson, A. (2026). MultiArgue: A Multidimensional Graded Response Model-Based Interactive Application for Assessing Argumentation Skills [Computer software]."
              )
          )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Tab 1: Rubric Guide Table
  output$tabel_rubrik <- renderDT({
    rubric_data <- data.frame(
      Score = 1:5,
      Component = c("Claim Only", "Claim + Evidence", "Claim + Evidence + Weak Reasoning", "Claim + Evidence + Strong Reasoning", "Claim + Evidence + Strong Reasoning + Rebuttal"),
      Operational_Description = c(
        "Presents a claim or statement without supporting evidence or reasoning.",
        "Presents a claim supported by empirical evidence/data, but lacks explanatory reasoning.",
        "Presents a claim, evidence, and reasoning, but the reasoning is overly general or weak.",
        "Presents a valid claim, solid empirical evidence, and clear, scientifically sound reasoning.",
        "Presents a valid claim, solid evidence, strong reasoning, and addresses a counter-argument/rebuttal."
      )
    )
    datatable(rubric_data, options = list(pageLength = 5, dom = 't'), rownames = FALSE)
  })
  
  # Reactive Data (36 Items)
  data_respon <- reactive({
    if (is.null(input$file_data)) {
      set.seed(123)
      df_dummy <- as.data.frame(matrix(sample(1:5, 3600, replace = TRUE, prob = c(0.1, 0.2, 0.35, 0.25, 0.1)), ncol = 36))
      colnames(df_dummy) <- paste0("Item_", 1:36)
      df_dummy
    } else {
      ext <- tools::file_ext(input$file_data$name)
      df <- if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file_data$datapath)
      } else if (ext == "csv") {
        read.csv(input$file_data$datapath, check.names = FALSE)
      } else {
        stop("Unsupported file format.")
      }
      df_clean <- na.omit(df)
      if (nrow(df_clean) < nrow(df)) {
        showNotification(paste("Removed", nrow(df) - nrow(df_clean), "rows containing missing values (NA)."), type = "warning")
      }
      df_clean
    }
  })
  
  # Helper to define multidimensional model structure
  get_mirt_model_syntax <- function(item_names) {
    item_indices <- as.numeric(gsub("[^0-9]", "", item_names))
    
    ce_idx <- which(item_indices %in% c(1:6, 19:24))
    eo_idx <- which(item_indices %in% c(7:12, 25:30))
    ra_idx <- which(item_indices %in% c(13:18, 31:36))
    
    syntax <- ""
    if (length(ce_idx) > 0) syntax <- paste0(syntax, "CE = ", paste(ce_idx, collapse = ","), "\n")
    if (length(eo_idx) > 0) syntax <- paste0(syntax, "EO = ", paste(eo_idx, collapse = ","), "\n")
    if (length(ra_idx) > 0) syntax <- paste0(syntax, "RA = ", paste(ra_idx, collapse = ","), "\n")
    syntax <- paste0(syntax, "COV = CE*EO, CE*RA, EO*RA\n")
    
    mirt.model(syntax)
  }
  
  # Step 1: Initial MGRM Estimation
  initial_model <- eventReactive(input$btn_analisis, {
    df <- as.data.frame(data_respon())
    model_spec <- get_mirt_model_syntax(colnames(df))
    mirt(df, model = model_spec, itemtype = 'graded', verbose = FALSE)
  }, ignoreNULL = FALSE)
  
  # Step 2: Calculate Item Fit (S-X2)
  item_fit_df <- reactive({
    fit_init <- initial_model()
    fit_stats <- itemfit(fit_init, method = 'S_X2', verbose = FALSE)
    fit_stats$Status <- ifelse(!is.na(fit_stats$p.S_X2) & fit_stats$p.S_X2 < 0.05, "Misfit (Excluded)", "Fit (Retained)")
    fit_stats$S_X2 <- round(fit_stats$S_X2, 3)
    fit_stats$p.S_X2 <- round(fit_stats$p.S_X2, 4)
    fit_stats
  })
  
  output$tabel_fit <- renderDT({
    datatable(item_fit_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$dl_item_fit <- downloadHandler(
    filename = function() { paste0("Item_Fit_Analysis_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(item_fit_df(), file, row.names = FALSE) }
  )
  
  # Step 3: Filter Retained Fit Items
  clean_data <- reactive({
    df <- as.data.frame(data_respon())
    fit_res <- item_fit_df()
    retained_items <- fit_res$item[fit_res$Status == "Fit (Retained)"]
    if (length(retained_items) == 0) {
      showNotification("Warning: All items were flagged as misfitting. Retaining all items.", type = "warning")
      return(df)
    }
    df[, retained_items, drop = FALSE]
  })
  
  # Step 4: Refit Final Multidimensional Model
  final_model <- reactive({
    df_clean <- clean_data()
    model_spec <- get_mirt_model_syntax(colnames(df_clean))
    mirt(df_clean, model = model_spec, itemtype = 'graded', verbose = FALSE)
  })
  
  # Global Model Fit Dataframe (M2* Statistic)
  global_fit_df <- reactive({
    fit <- final_model()
    m2_res <- tryCatch({
      M2(fit, type = 'M2*')
    }, error = function(e) {
      M2(fit)
    })
    
    data.frame(
      Fit_Index = c("M2 Statistic", "Degrees of Freedom (df)", "p-value", "RMSEA", "CFI", "TLI"),
      Estimated_Value = c(
        round(m2_res$M2, 3),
        m2_res$df,
        round(m2_res$p, 4),
        round(m2_res$RMSEA, 3),
        round(m2_res$CFI, 3),
        round(m2_res$TLI, 3)
      ),
      Acceptable_Threshold = c("-", "-", "> 0.05 (Non-significant)", "< 0.08 (Good fit)", "> 0.90 (Good fit)", "> 0.90 (Good fit)")
    )
  })
  
  output$tabel_global_fit <- renderDT({
    datatable(global_fit_df(), options = list(dom = 't'), rownames = FALSE)
  })
  
  output$dl_global_fit <- downloadHandler(
    filename = function() { paste0("Global_Model_Fit_M2_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(global_fit_df(), file, row.names = FALSE) }
  )
  
  # Output: Empirical Reliability Badge/Card (Desain Kuning-Biru Kontras)
  output$card_reliability <- renderUI({
    fit <- final_model()
    scores <- fscores(fit, full.scores.SE = TRUE)
    rel_vals <- empirical_rxx(scores)
    
    rel_str <- paste(names(rel_vals), "=", round(as.numeric(rel_vals), 3), collapse = " | ")
    
    div(class = "p-3 rounded d-flex align-items-center justify-content-between mb-0", 
        style = "background-color: #EFF6FF; border: 2px solid #1E40AF;",
        div(
          h5(class = "mb-1", style = "color: #1E40AF; font-weight: bold;", paste("Empirical Reliability (\u0153):", rel_str)),
          p(class = "mb-0 text-muted", "Empirical reliability coefficients across multidimensional factors (CE, EO, RA).")
        )
    )
  })
  
  # Final Parameters Matrix
  item_params_df <- reactive({
    fit <- final_model()
    coef_matrix <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
    as.data.frame(round(coef_matrix, 3))
  })
  
  output$tabel_parameter <- renderDT({
    datatable(item_params_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$dl_item_params <- downloadHandler(
    filename = function() { paste0("MGRM_Item_Parameters_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(item_params_df(), file, row.names = TRUE) }
  )
  
  # Factor Correlation Table
  output$tabel_korelasi <- renderDT({
    fit <- final_model()
    cov_matrix <- coef(fit, simplify = TRUE)$cov
    cor_matrix <- cov2cor(cov_matrix)
    datatable(round(cor_matrix, 3), options = list(dom = 't'))
  })
  
  # CCC Dropdown & Plot
  output$select_item_ui <- renderUI({
    df_clean <- clean_data()
    selectInput("selected_item", "Select Assessment Item (Fit Items Only):", choices = colnames(df_clean))
  })
  
  draw_ccc_plot <- function() {
    req(input$selected_item)
    fit <- final_model()
    item_idx <- match(input$selected_item, colnames(clean_data()))
    p <- itemplot(fit, item = item_idx, type = 'trace', main = paste("CCC -", input$selected_item))
    print(p)
  }
  
  output$plot_ccc <- renderPlot({ draw_ccc_plot() })
  
  output$dl_ccc_plot <- downloadHandler(
    filename = function() { 
      clean_item_name <- gsub("[^A-Za-z0-9_]", "_", input$selected_item)
      paste0("CCC_Plot_", clean_item_name, "_", Sys.Date(), ".png") 
    },
    content = function(file) {
      png(file, width = 2400, height = 1600, res = 300)
      draw_ccc_plot()
      dev.off()
    }
  )
  
  # Individual Theta Scores (CE, EO, RA)
  theta_scores_df <- reactive({
    fit <- final_model()
    df_clean <- clean_data()
    scores <- fscores(fit, full.scores.SE = TRUE)
    
    res <- data.frame(Respondent_ID = paste0("Respondent_", 1:nrow(df_clean)))
    
    if ("CE" %in% colnames(scores)) {
      res$Theta_CE <- round(scores[, "CE"], 3)
      res$SE_CE <- round(scores[, "SE_CE"], 3)
    }
    if ("EO" %in% colnames(scores)) {
      res$Theta_EO <- round(scores[, "EO"], 3)
      res$SE_EO <- round(scores[, "SE_EO"], 3)
    }
    if ("RA" %in% colnames(scores)) {
      res$Theta_RA <- round(scores[, "RA"], 3)
      res$SE_RA <- round(scores[, "SE_RA"], 3)
    }
    res
  })
  
  output$tabel_theta <- renderDT({
    datatable(theta_scores_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$dl_theta_scores <- downloadHandler(
    filename = function() { paste0("Multidimensional_Theta_Scores_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(theta_scores_df(), file, row.names = FALSE) }
  )
}

# --- RUN APPLICATION ---
shinyApp(ui = ui, server = server)