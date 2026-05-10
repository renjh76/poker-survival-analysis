# =============================================================================
# tests/test_features.R
# -----------------------------------------------------------------------------
# 单元测试:覆盖核心特征计算函数与熵权法
# 运行:  testthat::test_dir("tests")
# =============================================================================

library(testthat)

# 定位项目根目录(允许从根目录或 tests/ 目录运行)
root <- if (file.exists("R/02_behavioral_features.R")) "."
        else if (file.exists("../R/02_behavioral_features.R")) ".."
        else stop("Cannot locate project root.")

source(file.path(root, "R/01_data_simulation.R"))
source(file.path(root, "R/02_behavioral_features.R"))
source(file.path(root, "R/03_player_clustering.R"))

# ---- 测试 1: 模拟器输出结构 ------------------------------------------------

test_that("simulate_all 返回结构正确", {
  small <- list(n_players = 10L, n_hands = 500L, seed = 1L,
                days_span = 60L, big_blind = 2)
  out <- simulate_all(small)
  expect_named(out, c("players", "hands"))
  expect_equal(nrow(out$players), 10L)
  expect_gte(nrow(out$hands), 50L)         # 每玩家至少 5 手
  expect_true(all(c("player_id", "vpip", "pfr", "stack") %in% names(out$hands)))
})

# ---- 测试 2: 行为特征列存在 + 取值合理 -------------------------------------

test_that("compute_player_features 输出列齐全", {
  small <- list(n_players = 5L, n_hands = 1000L, seed = 2L,
                days_span = 60L, big_blind = 2)
  d <- simulate_all(small)
  feats <- compute_player_features(d$hands, big_blind = 2)
  expected <- c("player_id", "n_hands", "VPIP", "PFR", "three_bet", "cbet",
                "AF", "WTSD", "bb_per_100", "volatility")
  expect_true(all(expected %in% names(feats)))
  expect_true(all(feats$VPIP >= 0 & feats$VPIP <= 1))
  expect_true(all(feats$PFR >= 0  & feats$PFR  <= 1))
  expect_true(all(feats$WTSD >= 0 & feats$WTSD <= 1))
})

# ---- 测试 3: 熵权法 --------------------------------------------------------

test_that("entropy_weights 权重和为 1 且非负", {
  set.seed(0)
  M <- matrix(runif(100), ncol = 5)
  colnames(M) <- letters[1:5]
  w <- entropy_weights(M)
  expect_equal(sum(w), 1, tolerance = 1e-6)
  expect_true(all(w >= 0))
  expect_named(w, colnames(M))
})

test_that("熵权法对常数列稳健(不会出现 NaN)", {
  M <- cbind(a = rnorm(50), b = rnorm(50), c = rep(1, 50))
  w <- entropy_weights(M)
  expect_false(any(is.na(w)))
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

# ---- 测试 4: PFR <= VPIP(扑克常识恒等式)----------------------------------

test_that("玩家级 PFR <= VPIP", {
  small <- list(n_players = 8L, n_hands = 1500L, seed = 3L,
                days_span = 90L, big_blind = 2)
  d <- simulate_all(small)
  feats <- compute_player_features(d$hands, big_blind = 2)
  expect_true(all(feats$PFR <= feats$VPIP + 1e-9))
})
