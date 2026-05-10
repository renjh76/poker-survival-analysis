# =============================================================================
# 05_ordinal_logit.R
# -----------------------------------------------------------------------------
# 把玩家按 BB/100 分位数划成 4 个有序等级,用比例优势 (proportional odds)
# Logistic 回归(MASS::polr)预测玩家水平等级。
#
# 输出:data/processed/polr_model.rds, figures/polr_effects.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(MASS)
  library(ggplot2)
})

# ---- 1. 等级划分 ------------------------------------------------------------

#' 按 BB/100 的四分位数划成 4 个有序等级:菜鸟 < 普通 < 熟练 < 高手
add_skill_tier <- function(df,
                           levels = c("菜鸟", "普通", "熟练", "高手")) {
  q <- quantile(df$bb_per_100, probs = seq(0, 1, 0.25), na.rm = TRUE)
  # 防止断点重复
  q <- unique(q)
  if (length(q) - 1 < length(levels)) {
    levels <- levels[seq_len(length(q) - 1)]
  }
  tier <- cut(df$bb_per_100, breaks = q,
              labels = levels, include.lowest = TRUE)
  df$skill_tier <- factor(tier, levels = levels, ordered = TRUE)
  df
}

# ---- 2. 拟合 polr -----------------------------------------------------------

fit_polr <- function(df) {
  fmla <- skill_tier ~ VPIP + PFR + three_bet + cbet +
                       AF + WTSD + volatility
  fit <- MASS::polr(fmla, data = df, Hess = TRUE, method = "logistic")
  print(summary(fit))
  fit
}

#' 提取系数 + p 值 + odds ratio + 95% CI
polr_summary_table <- function(fit) {
  ct  <- coef(summary(fit))
  pv  <- pnorm(abs(ct[, "t value"]), lower.tail = FALSE) * 2
  res <- cbind(ct, "p value" = pv)
  ci  <- suppressMessages(confint(fit))
  or  <- exp(coef(fit))
  or_ci <- exp(ci)
  list(coef_table = res, OR = or, OR_CI = or_ci)
}

# ---- 3. 比例优势假设检验(Brant 近似)--------------------------------------

#' 简化版比例优势检验:对每对相邻类别拟合二元 logit,比较系数稳定性
#' (完整 Brant 检验需 brant 包,这里给一个无依赖版本)
test_proportional_odds <- function(df, fit) {
  vars <- all.vars(formula(fit))[-1]
  tiers <- levels(df$skill_tier)
  out <- list()
  for (i in seq_len(length(tiers) - 1)) {
    df$y <- as.integer(as.integer(df$skill_tier) > i)
    f <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
    out[[paste0("split>", tiers[i])]] <- coef(glm(f, data = df, family = binomial()))
  }
  do.call(rbind, out)
}

# ---- 4. 边际效应可视化 -----------------------------------------------------

plot_polr_effects <- function(fit, df, fig_path = "figures/polr_effects.png") {
  # 简化的可视化:对 PFR 做边际效应,固定其他变量在均值
  newdat <- expand.grid(
    PFR = seq(min(df$PFR), max(df$PFR), length.out = 50)
  )
  others <- setdiff(all.vars(formula(fit))[-1], "PFR")
  for (v in others) newdat[[v]] <- mean(df[[v]], na.rm = TRUE)

  prob <- predict(fit, newdata = newdat, type = "probs")
  long <- as.data.frame(prob)
  long$PFR <- newdat$PFR
  long_l <- tidyr::pivot_longer(long, -PFR, names_to = "tier", values_to = "prob")

  p <- ggplot(long_l, aes(PFR, prob, color = tier)) +
    geom_line(size = 1.1) +
    labs(title = "Ordinal logit — predicted probability of each skill tier",
         subtitle = "Other features held at mean",
         y = "P(tier)", color = "Skill tier") +
    theme_minimal()

  ggsave(fig_path, p, width = 7, height = 4.5, dpi = 150)
  invisible(p)
}

# ---- 5. 主入口 --------------------------------------------------------------

run_ordinal_logit <- function(processed_dir = "data/processed",
                              fig_dir       = "figures") {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  clusters <- readRDS(file.path(processed_dir, "player_clusters.rds"))
  feats <- clusters$features
  feats <- add_skill_tier(feats)

  message("[polr] tier counts:")
  print(table(feats$skill_tier))

  fit <- fit_polr(feats)
  smy <- polr_summary_table(fit)
  message("[polr] OR table:"); print(round(smy$OR, 3))
  message("[polr] OR 95% CI:"); print(round(smy$OR_CI, 3))

  message("[polr] proportional odds check (binary cumulative logits):")
  print(round(test_proportional_odds(feats, fit), 3))

  plot_polr_effects(fit, feats,
                    fig_path = file.path(fig_dir, "polr_effects.png"))

  saveRDS(list(model = fit,
               summary = smy,
               data    = feats),
          file.path(processed_dir, "polr_model.rds"))

  message("[polr] saved to ", processed_dir, "/polr_model.rds")
  invisible(fit)
}

if (sys.nframe() == 0L) {
  run_ordinal_logit()
}
