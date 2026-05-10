# =============================================================================
# 06_xgboost_baseline.R
# -----------------------------------------------------------------------------
# XGBoost 二分类基线:预测玩家是否在 30 天内破产 / 流失。
# 与 Cox 模型形成 “预测 vs 推断” 的对比。
#
# 输出:data/processed/xgb_model.rds, figures/xgb_*.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(xgboost)
  library(SHAPforxgboost)
  library(ggplot2)
})

# ---- 1. 标签构造 ------------------------------------------------------------

#' 用生存表的 (time, event) 派生 “是否在 30 天内破产 / 流失” 二分类标签
#'
#' label = 1 if event == 1 AND time <= 30
make_xgb_label <- function(surv_df, horizon_days = 30) {
  dplyr::mutate(surv_df,
                bust_30d = as.integer(event == 1 & time <= horizon_days))
}

# ---- 2. 训练 / 评估 ---------------------------------------------------------

#' 训练 XGBoost 二分类模型
#'
#' @param df         含 bust_30d 列的数据
#' @param feat_cols  特征列名
#' @param test_frac  测试集比例
#' @return list(model, dtrain, dtest, X_train, X_test, y_test, pred)
train_xgb <- function(df, feat_cols, test_frac = 0.25, nrounds = 200,
                      seed = 2026) {
  set.seed(seed)
  n <- nrow(df)
  idx_test <- sample(n, size = floor(n * test_frac))
  train <- df[-idx_test, ]; test <- df[idx_test, ]

  X_train <- as.matrix(train[, feat_cols])
  X_test  <- as.matrix(test[,  feat_cols])
  y_train <- train$bust_30d
  y_test  <- test$bust_30d

  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest  <- xgb.DMatrix(X_test,  label = y_test)

  params <- list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,
    max_depth        = 4,
    subsample        = 0.85,
    colsample_bytree = 0.85,
    min_child_weight = 5
  )

  model <- xgb.train(
    params = params, data = dtrain, nrounds = nrounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 20, verbose = 0
  )

  pred  <- predict(model, dtest)

  list(model = model, dtrain = dtrain, dtest = dtest,
       X_train = X_train, X_test = X_test,
       y_train = y_train, y_test = y_test, pred = pred)
}

#' 计算 AUC(无依赖,梯形法)
auc_score <- function(prob, label) {
  ord  <- order(prob, decreasing = TRUE)
  prob <- prob[ord]; label <- label[ord]
  n_pos <- sum(label == 1); n_neg <- sum(label == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  rank_sum <- sum(which(label == 1))
  (rank_sum - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' 混淆矩阵 + 关键指标(threshold = 0.5)
confusion_table <- function(prob, label, threshold = 0.5) {
  pred <- as.integer(prob >= threshold)
  tab  <- table(pred = factor(pred, levels = c(0, 1)),
                actual = factor(label, levels = c(0, 1)))
  tn <- tab[1,1]; fp <- tab[2,1]; fn <- tab[1,2]; tp <- tab[2,2]
  list(
    table = tab,
    accuracy   = (tp + tn) / sum(tab),
    precision  = if ((tp + fp) > 0) tp / (tp + fp) else NA,
    recall     = if ((tp + fn) > 0) tp / (tp + fn) else NA,
    f1         = if ((tp + fp + fn) > 0) 2 * tp / (2 * tp + fp + fn) else NA
  )
}

# ---- 3. SHAP 解释 -----------------------------------------------------------

plot_shap_summary <- function(model, X_train,
                              fig_path = "figures/xgb_shap_summary.png") {
  shap_long <- SHAPforxgboost::shap.prep(xgb_model = model, X_train = X_train)
  p <- SHAPforxgboost::shap.plot.summary(shap_long) +
    ggtitle("XGBoost — SHAP feature importance")
  ggsave(fig_path, p, width = 7, height = 5, dpi = 150)
  invisible(shap_long)
}

# ---- 4. 主入口 --------------------------------------------------------------

run_xgboost_baseline <- function(processed_dir = "data/processed",
                                 fig_dir       = "figures",
                                 horizon_days  = 30) {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  surv_df <- readRDS(file.path(processed_dir, "survival_data.rds"))
  df <- make_xgb_label(surv_df, horizon_days = horizon_days)

  feat_cols <- c("VPIP", "PFR", "three_bet", "cbet",
                 "AF", "WTSD", "bb_per_100", "volatility")

  message("[xgb] training (label = bust within ", horizon_days, "d) ...")
  res <- train_xgb(df, feat_cols)

  auc <- auc_score(res$pred, res$y_test)
  cm  <- confusion_table(res$pred, res$y_test)
  message(sprintf("[xgb] test AUC = %.4f", auc))
  message(sprintf("[xgb] acc=%.3f  prec=%.3f  recall=%.3f  F1=%.3f",
                  cm$accuracy, cm$precision %||% NA, cm$recall %||% NA, cm$f1 %||% NA))
  print(cm$table)

  # 特征重要性条形图
  imp <- xgb.importance(model = res$model)
  p_imp <- ggplot(imp, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "#2E7D32") + coord_flip() +
    labs(title = "XGBoost feature importance (Gain)", x = NULL) +
    theme_minimal()
  ggsave(file.path(fig_dir, "xgb_importance.png"), p_imp,
         width = 6, height = 4, dpi = 150)

  # SHAP
  shap_long <- tryCatch(
    plot_shap_summary(res$model, res$X_train,
                      fig_path = file.path(fig_dir, "xgb_shap_summary.png")),
    error = function(e) { message("[xgb] SHAP plot failed: ", e$message); NULL }
  )

  saveRDS(list(model = res$model,
               auc = auc, confusion = cm,
               importance = imp,
               feat_cols = feat_cols),
          file.path(processed_dir, "xgb_model.rds"))

  message("[xgb] saved to ", processed_dir, "/xgb_model.rds")
  invisible(list(model = res$model, auc = auc))
}

# 简易 default-or 运算符
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

if (sys.nframe() == 0L) {
  run_xgboost_baseline()
}
