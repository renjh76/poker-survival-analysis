# =============================================================================
# 04_survival_analysis.R   ⭐ 项目核心
# -----------------------------------------------------------------------------
# Kaplan–Meier + Log-rank + Cox PH + AFT(Weibull),用于刻画
# “扑克玩家活跃寿命 / 破产风险” 的生存过程。
#
# 事件定义:筹码归零(stack <= 0)或 连续 30 天未出现新手牌 视为“流失/事件”。
# 删失:观察期结束时玩家仍活跃。
#
# 输出:
#   data/processed/survival_data.rds, cox_model.rds, aft_model.rds
#   figures/km_curves_by_cluster.png, cox_forest_plot.png 等
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(survival)
  library(survminer)
  library(ggplot2)
})

# ---- 1. 构造生存数据 --------------------------------------------------------

#' 从手牌明细构造每个玩家的 (time, event) 二元组
#'
#' time  : 从 first_hand 到 last_active 的天数(>=1)
#' event : 1 = 观察到事件(破产 OR 30 天未活跃),0 = 删失
#'
#' @param hands     手牌明细
#' @param features  每玩家行为特征(将与生存表合并)
#' @param obs_end_day 观察期结束 day_index(默认 hands 中最大 day + 1)
#' @param inactive_days 连续多少天未出现视为流失
#' @return tibble 每玩家一行
build_survival_data <- function(hands, features,
                                obs_end_day = NULL,
                                inactive_days = 30) {
  if (is.null(obs_end_day)) obs_end_day <- max(hands$day_index) + 1L

  surv <- hands %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      first_day  = min(day_index),
      last_day   = max(day_index),
      min_stack  = min(stack),
      end_stack  = stack[which.max(day_index)],
      n_hands    = dplyr::n(),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(
      bust       = as.integer(min_stack <= 0),
      churn      = as.integer((obs_end_day - last_day) > inactive_days),
      event      = bust,
      time       = pmax(last_day - first_day + 1L, 1L)
    )

  # 与行为特征合并
  out <- dplyr::left_join(surv, features, by = "player_id")
  out
}

# ---- 2. KM 曲线 + Log-rank --------------------------------------------------

#' 按聚类簇画 KM 生存曲线 + Log-rank
#'
#' @param surv_df  build_survival_data 输出
#' @param fig_dir  图保存目录
fit_km_by_cluster <- function(surv_df, fig_dir = "figures") {
  fit <- survival::survfit(Surv(time, event) ~ style_label, data = surv_df)
  lr  <- survival::survdiff(Surv(time, event) ~ style_label, data = surv_df)

  p <- survminer::ggsurvplot(
    fit, data = surv_df,
    pval = TRUE, conf.int = TRUE,
    risk.table = TRUE,
    palette = "Dark2",
    xlab = "Days since first hand",
    ylab = "Survival probability (still active)",
    title = "Kaplan–Meier survival curves by player cluster",
    legend.title = "Cluster"
  )
  # ggsurvplot 返回一个组合对象,用 png 设备保存最稳妥
  png(file.path(fig_dir, "km_curves_by_cluster.png"),
      width = 8, height = 6, units = "in", res = 150)
  print(p)
  dev.off()

  list(fit = fit, logrank = lr)
}

# ---- 3. Cox 比例风险模型 ---------------------------------------------------

#' 拟合 Cox PH 模型 + 比例风险检验 + 森林图
fit_cox_model <- function(surv_df, fig_dir = "figures") {
  # 用主要行为特征作为协变量
  fmla <- Surv(time, event) ~ VPIP + PFR + three_bet + cbet +
                              AF + WTSD + bb_per_100 + volatility
  fit <- survival::coxph(fmla, data = surv_df)
  print(summary(fit))

  # 比例风险假设检验
  zph <- survival::cox.zph(fit)
  message("[cox] PH assumption test:")
  print(zph)

  # 森林图
  p_forest <- tryCatch(
    survminer::ggforest(fit, data = surv_df,
                        main = "Cox PH — hazard ratios with 95% CI"),
    error = function(e) NULL
  )
  if (!is.null(p_forest)) {
    ggsave(file.path(fig_dir, "cox_forest_plot.png"),
           p_forest, width = 8, height = 5, dpi = 150)
  }

  # cox.zph 诊断图
  png(file.path(fig_dir, "cox_zph_diagnostics.png"),
      width = 1200, height = 800, res = 150)
  par(mfrow = c(2, 4)); plot(zph); par(mfrow = c(1, 1))
  dev.off()

  list(model = fit, zph = zph)
}

# ---- 4. AFT (Weibull) 模型 --------------------------------------------------

#' 加速失效时间模型(Weibull),作为 Cox 的补充
fit_aft_model <- function(surv_df) {
  fmla <- Surv(time, event) ~ VPIP + PFR + three_bet + cbet +
                              AF + WTSD + bb_per_100 + volatility
  fit <- survival::survreg(fmla, data = surv_df, dist = "weibull")
  print(summary(fit))
  fit
}

# ---- 5. 主流程 --------------------------------------------------------------

run_survival_analysis <- function(processed_dir = "data/processed",
                                  fig_dir       = "figures",
                                  inactive_days = 30) {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  hands     <- readRDS(file.path(processed_dir, "hands.rds"))
  clusters  <- readRDS(file.path(processed_dir, "player_clusters.rds"))
  features  <- clusters$features

  message("[surv] building (time, event) ...")
  surv_df <- build_survival_data(hands, features,
                                 inactive_days = inactive_days)
  saveRDS(surv_df, file.path(processed_dir, "survival_data.rds"))

  message("[surv] fitting KM + log-rank ...")
  km <- fit_km_by_cluster(surv_df, fig_dir = fig_dir)

  message("[surv] fitting Cox PH ...")
  cox <- fit_cox_model(surv_df, fig_dir = fig_dir)
  saveRDS(cox, file.path(processed_dir, "cox_model.rds"))

  message("[surv] fitting AFT (Weibull) ...")
  aft <- fit_aft_model(surv_df)
  saveRDS(aft, file.path(processed_dir, "aft_model.rds"))

  message("[surv] done.")
  invisible(list(surv_df = surv_df, km = km, cox = cox, aft = aft))
}

if (sys.nframe() == 0L) {
  run_survival_analysis()
}
