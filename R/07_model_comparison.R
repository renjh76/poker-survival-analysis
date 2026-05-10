# =============================================================================
# 07_model_comparison.R
# -----------------------------------------------------------------------------
# 把 4 种方法横向对比:
#   * 描述性 (K-Means)         —— 玩家画像
#   * 推断性 (Cox PH)          —— 各特征对生存的因果方向(在 PH 假设下)
#   * 分类性 (Ordinal Logit)   —— 玩家水平等级建模
#   * 预测性 (XGBoost)         —— 30 天破产二分类
#
# 输出:data/processed/comparison_table.rds, figures/model_comparison_table.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(gridExtra)
})

# ---- 1. 汇总 4 种方法的关键产物 ---------------------------------------------

build_comparison_table <- function(processed_dir = "data/processed") {
  cox      <- readRDS(file.path(processed_dir, "cox_model.rds"))
  polr     <- readRDS(file.path(processed_dir, "polr_model.rds"))
  xgb      <- readRDS(file.path(processed_dir, "xgb_model.rds"))
  clusters <- readRDS(file.path(processed_dir, "player_clusters.rds"))

  # Cox HR
  cox_smy <- summary(cox$model)
  cox_hr  <- as.data.frame(cox_smy$conf.int)
  cox_hr$feature <- rownames(cox_hr)
  cox_hr <- cox_hr %>%
    dplyr::transmute(
      feature,
      cox_HR     = `exp(coef)`,
      cox_lower  = `lower .95`,
      cox_upper  = `upper .95`
    )

  # polr OR
  polr_or <- as.data.frame(polr$summary$OR_CI)
  polr_or$feature <- rownames(polr_or)
  polr_or <- polr_or %>%
    dplyr::filter(!feature %in% levels(polr$data$skill_tier)) %>%
    dplyr::transmute(
      feature,
      polr_OR    = polr$summary$OR[feature],
      polr_lower = `2.5 %`,
      polr_upper = `97.5 %`
    )

  # XGBoost importance(Gain)
  xgb_imp <- xgb$importance %>%
    dplyr::transmute(feature = Feature, xgb_gain = Gain)

  # 簇标签下的玩家计数
  cluster_summary <- clusters$features %>%
    dplyr::count(style_label, name = "n_players")

  out <- list(
    cox_table        = cox_hr,
    polr_table       = polr_or,
    xgb_importance   = xgb_imp,
    cluster_summary  = cluster_summary,
    xgb_auc          = xgb$auc,
    method_summary   = methodology_summary()
  )
  out
}

# ---- 2. 方法论对比说明 ------------------------------------------------------

methodology_summary <- function() {
  tibble::tribble(
    ~method,            ~paradigm,         ~question_answered,                                      ~strength,                                  ~caveat,
    "K-Means (+EWM)",   "Descriptive",     "What player archetypes exist?",                         "无监督、可视化、解释性强",                "需主观选 k、对噪声敏感",
    "Cox PH",           "Inferential",     "Which behaviors causally affect survival?",             "提供 HR + CI,经典统计,可解释",         "依赖比例风险假设",
    "Ordinal Logit",    "Classificatory",  "What separates skill tiers?",                           "对有序响应建模、给出 OR",                 "需要比例优势假设",
    "XGBoost + SHAP",   "Predictive",      "Can we predict 30-day bust accurately?",                "高预测精度、自动捕捉非线性 / 交互",       "黑箱,需 SHAP 等解释工具"
  )
}

# ---- 3. 把对比表渲染成图 ----------------------------------------------------

render_comparison_table <- function(comp,
                                    fig_path = "figures/model_comparison_table.png") {
  tt <- gridExtra::ttheme_default(
    core    = list(fg_params = list(cex = 0.85)),
    colhead = list(fg_params = list(cex = 0.9, fontface = "bold"))
  )
  g <- gridExtra::tableGrob(comp$method_summary, rows = NULL, theme = tt)

  png(fig_path, width = 1400, height = 500, res = 150)
  grid::grid.newpage(); grid::grid.draw(g)
  dev.off()
  invisible(g)
}

# ---- 4. 主入口 --------------------------------------------------------------

run_model_comparison <- function(processed_dir = "data/processed",
                                 fig_dir       = "figures") {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  comp <- build_comparison_table(processed_dir)

  message("=========== METHOD COMPARISON ============")
  print(comp$method_summary)
  message("---- Cox HR ----");      print(comp$cox_table)
  message("---- polr OR ----");     print(comp$polr_table)
  message("---- XGB importance ---"); print(comp$xgb_importance)
  message(sprintf("---- XGB test AUC = %.3f", comp$xgb_auc %||% NA))
  message("---- Cluster sizes ----"); print(comp$cluster_summary)

  render_comparison_table(comp,
                          fig_path = file.path(fig_dir, "model_comparison_table.png"))

  saveRDS(comp, file.path(processed_dir, "comparison_table.rds"))
  message("[cmp] saved to ", processed_dir, "/comparison_table.rds")
  invisible(comp)
}

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

if (sys.nframe() == 0L) {
  run_model_comparison()
}
