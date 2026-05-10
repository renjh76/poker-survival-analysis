# =============================================================================
# main.R  —— 一键跑通整条流水线
# -----------------------------------------------------------------------------
# 顺序:
#   1. 数据模拟    -> data/processed/{players, hands}.rds
#   2. 行为特征    -> data/processed/player_features.rds
#   3. K-Means 聚类 -> data/processed/player_clusters.rds
#   4. 生存分析    -> data/processed/{survival_data, cox_model, aft_model}.rds
#   5. 有序 Logit  -> data/processed/polr_model.rds
#   6. XGBoost     -> data/processed/xgb_model.rds
#   7. 模型对比    -> data/processed/comparison_table.rds
#
# 用法:在项目根目录执行
#   source("main.R")
# =============================================================================

t0 <- Sys.time()

step <- function(msg) message("\n========== ", msg, " ==========\n")

step("1/7  data simulation")
source("R/01_data_simulation.R"); run_data_simulation()

step("2/7  behavioral features")
source("R/02_behavioral_features.R"); run_behavioral_features()

step("3/7  player clustering (K-Means + entropy weights)")
source("R/03_player_clustering.R"); run_player_clustering()

step("4/7  survival analysis (KM + Cox + AFT)")
source("R/04_survival_analysis.R"); run_survival_analysis()

step("5/7  ordinal logit (MASS::polr)")
source("R/05_ordinal_logit.R"); run_ordinal_logit()

step("6/7  XGBoost baseline + SHAP")
source("R/06_xgboost_baseline.R"); run_xgboost_baseline()

step("7/7  cross-paradigm model comparison")
source("R/07_model_comparison.R"); run_model_comparison()

elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2)
message(sprintf("\n[main] pipeline finished in %s min.", elapsed))
message("[main] outputs in data/processed/  &  figures/")
message("[main] next:")
message("  - shiny::runApp(\"app\")")
message("  - rmarkdown::render(\"reports/analysis_report.Rmd\")")
message("  - testthat::test_dir(\"tests\")")
