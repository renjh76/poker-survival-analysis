# =============================================================================
# app/app.R
# -----------------------------------------------------------------------------
# Shiny 仪表板:4 个标签页
#   1. 玩家画像   2. 群体洞察   3. 风险预测   4. 方法论展示
# 主题:深绿色牌桌
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(survival)
  library(survminer)
})

# ---- 1. 数据加载(优先从 data/processed) -----------------------------------

PROCESSED_DIR <- "../data/processed"
if (!dir.exists(PROCESSED_DIR)) PROCESSED_DIR <- "data/processed"

safe_read <- function(name) {
  p <- file.path(PROCESSED_DIR, name)
  if (file.exists(p)) readRDS(p) else NULL
}

clusters_obj <- safe_read("player_clusters.rds")
surv_df      <- safe_read("survival_data.rds")
cox_obj      <- safe_read("cox_model.rds")
xgb_obj      <- safe_read("xgb_model.rds")
comp_obj     <- safe_read("comparison_table.rds")

features <- if (!is.null(clusters_obj)) clusters_obj$features else NULL

# ---- 2. UI ------------------------------------------------------------------

felt_green <- "#0B6B3A"   # 深绿牌桌

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "🃏 Poker Survival Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("玩家画像",   tabName = "player",  icon = icon("user")),
      menuItem("群体洞察",   tabName = "cohort",  icon = icon("users")),
      menuItem("风险预测",   tabName = "risk",    icon = icon("triangle-exclamation")),
      menuItem("方法论展示", tabName = "methods", icon = icon("book"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(sprintf("
      .content-wrapper, .right-side { background-color: %s !important; color: #f5f5f5; }
      .box { background: #0F4D2E; border-top-color: #f5deb3; color: #f5f5f5; }
      .box-header { color: #f5deb3; }
      .small-box { background: #114a32 !important; color: #fff !important; }
    ", felt_green)))),

    tabItems(
      # ---- 1. 玩家画像 ----
      tabItem(tabName = "player",
        fluidRow(
          box(width = 4, title = "选择玩家",
              selectInput("pid", "Player ID",
                          choices = if (!is.null(features)) features$player_id else character(0)),
              verbatimTextOutput("player_meta")),
          box(width = 8, title = "行为指标雷达图",
              plotOutput("player_radar", height = "350px"))
        ),
        fluidRow(
          box(width = 12, title = "玩家所属簇 + 簇内 KM 曲线",
              plotOutput("player_km", height = "400px"))
        )
      ),

      # ---- 2. 群体洞察 ----
      tabItem(tabName = "cohort",
        fluidRow(
          valueBoxOutput("vb_n"),
          valueBoxOutput("vb_event"),
          valueBoxOutput("vb_auc")
        ),
        fluidRow(
          box(width = 6, title = "全部簇的 KM 曲线",
              plotOutput("km_all", height = "420px")),
          box(width = 6, title = "簇均值雷达 / 表",
              DTOutput("centroid_table"))
        )
      ),

      # ---- 3. 风险预测 ----
      tabItem(tabName = "risk",
        fluidRow(
          box(width = 4, title = "输入行为特征",
              numericInput("VPIP",       "VPIP",        0.30, 0, 1, 0.01),
              numericInput("PFR",        "PFR",         0.18, 0, 1, 0.01),
              numericInput("three_bet",  "3-bet",       0.06, 0, 1, 0.01),
              numericInput("cbet",       "C-bet",       0.55, 0, 1, 0.01),
              numericInput("AF",         "AF",          2.0, 0, 20, 0.1),
              numericInput("WTSD",       "WTSD",        0.27, 0, 1, 0.01),
              numericInput("bb_per_100", "BB/100",      0.0, -50, 50, 0.5),
              numericInput("volatility", "Volatility (BB)", 12, 0, 100, 0.5),
              actionButton("predict_btn", "预测", icon = icon("calculator"),
                           class = "btn-warning")),
          box(width = 8, title = "预测结果",
              h4("XGBoost — 30 天破产/流失概率"),
              verbatimTextOutput("pred_xgb"),
              h4("Cox — 相对风险 (相对于群体均值)"),
              verbatimTextOutput("pred_cox"))
        )
      ),

      # ---- 4. 方法论展示 ----
      tabItem(tabName = "methods",
        fluidRow(
          box(width = 12, title = "四种方法论对比",
              DTOutput("method_table"))
        ),
        fluidRow(
          box(width = 6, title = "Cox 模型主要协变量 HR",
              DTOutput("cox_table_ui")),
          box(width = 6, title = "XGBoost 特征重要性",
              DTOutput("xgb_imp_ui"))
        )
      )
    )
  )
)

# ---- 3. SERVER --------------------------------------------------------------

server <- function(input, output, session) {

  # ---- 玩家画像 ----
  player_row <- reactive({
    req(features); req(input$pid)
    features[features$player_id == input$pid, , drop = FALSE]
  })

  output$player_meta <- renderPrint({
    pr <- player_row(); if (nrow(pr) == 0) return("no data")
    cat("Style label:", pr$style_label, "\n")
    cat("Hands played:", pr$n_hands, "\n")
    cat("BB/100:", round(pr$bb_per_100, 2), "\n")
    cat("Cluster:", as.character(pr$cluster), "\n")
  })

  output$player_radar <- renderPlot({
    pr <- player_row(); if (nrow(pr) == 0) return(NULL)
    cols <- c("VPIP","PFR","three_bet","cbet","AF","WTSD")
    vals <- as.numeric(pr[, cols])
    df <- data.frame(metric = cols, value = vals)
    ggplot(df, aes(x = metric, y = value, group = 1)) +
      geom_polygon(fill = "#f5deb3", alpha = 0.4, color = "#f5deb3") +
      geom_point(color = "#fff", size = 3) +
      coord_polar() +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = felt_green, color = NA),
        plot.background  = element_rect(fill = felt_green, color = NA),
        text = element_text(color = "#f5f5f5"),
        panel.grid = element_line(color = "#1f7a4a")
      )
  })

  output$player_km <- renderPlot({
    if (is.null(surv_df)) return(NULL)
    pr <- player_row(); if (nrow(pr) == 0) return(NULL)
    sub <- surv_df[surv_df$style_label == pr$style_label, ]
    fit <- survival::survfit(Surv(time, event) ~ 1, data = sub)
    survminer::ggsurvplot(fit, data = sub, conf.int = TRUE,
                          palette = "#f5deb3",
                          title = paste("KM curve — cluster", pr$style_label))$plot
  })

  # ---- 群体洞察 ----
  output$vb_n <- renderValueBox({
    valueBox(if (!is.null(features)) nrow(features) else 0, "Players", icon = icon("users"))
  })
  output$vb_event <- renderValueBox({
    n_evt <- if (!is.null(surv_df)) sum(surv_df$event) else 0
    valueBox(n_evt, "Bust / churn events", icon = icon("triangle-exclamation"),
             color = "yellow")
  })
  output$vb_auc <- renderValueBox({
    auc <- if (!is.null(xgb_obj)) round(xgb_obj$auc, 3) else NA
    valueBox(auc, "XGBoost test AUC", icon = icon("chart-line"), color = "olive")
  })

  output$km_all <- renderPlot({
    if (is.null(surv_df)) return(NULL)
    fit <- survival::survfit(Surv(time, event) ~ style_label, data = surv_df)
    survminer::ggsurvplot(fit, data = surv_df, pval = TRUE, conf.int = FALSE,
                          palette = "Dark2",
                          legend.title = "Cluster")$plot
  })

  output$centroid_table <- renderDT({
    if (is.null(clusters_obj)) return(NULL)
    DT::datatable(clusters_obj$centroids, options = list(scrollX = TRUE))
  })

  # ---- 风险预测 ----
  observeEvent(input$predict_btn, {
    feat_cols <- c("VPIP","PFR","three_bet","cbet","AF","WTSD","bb_per_100","volatility")
    x <- sapply(feat_cols, function(n) input[[n]])

    # XGBoost 预测
    output$pred_xgb <- renderPrint({
      if (is.null(xgb_obj)) return("XGBoost model not loaded.")
      xmat <- matrix(x, nrow = 1, dimnames = list(NULL, feat_cols))
      p <- predict(xgb_obj$model, xmat)
      cat(sprintf("P(bust within 30d) = %.3f\n", p))
      cat("Risk band:", ifelse(p > 0.5, "HIGH", ifelse(p > 0.25, "MEDIUM", "LOW")), "\n")
    })

    # Cox 相对风险:exp(beta'x - beta'mean)
    output$pred_cox <- renderPrint({
      if (is.null(cox_obj) || is.null(surv_df)) return("Cox model not loaded.")
      coefs <- coef(cox_obj$model)
      means <- sapply(feat_cols, function(n) mean(surv_df[[n]], na.rm = TRUE))
      lp_x  <- sum(coefs * x[names(coefs)])
      lp_m  <- sum(coefs * means[names(coefs)])
      rr    <- exp(lp_x - lp_m)
      cat(sprintf("Hazard ratio vs population mean: %.3f\n", rr))
    })
  })

  # ---- 方法论展示 ----
  output$method_table <- renderDT({
    if (is.null(comp_obj)) return(NULL)
    DT::datatable(comp_obj$method_summary, options = list(scrollX = TRUE))
  })
  output$cox_table_ui <- renderDT({
    if (is.null(comp_obj)) return(NULL)
    DT::datatable(round(comp_obj$cox_table[, sapply(comp_obj$cox_table, is.numeric)], 3))
  })
  output$xgb_imp_ui <- renderDT({
    if (is.null(comp_obj)) return(NULL)
    DT::datatable(comp_obj$xgb_importance)
  })
}

shinyApp(ui, server)
