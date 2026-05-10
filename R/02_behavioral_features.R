# =============================================================================
# 02_behavioral_features.R
# -----------------------------------------------------------------------------
# 给每个玩家计算扑克标准行为指标(per 100 hands),输出每个玩家一行的特征矩阵。
#
# 输入:data/processed/{players, hands}.rds
# 输出:data/processed/player_features.rds
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

# ---- 1. 单玩家特征 ----------------------------------------------------------

#' 计算单玩家的标准扑克行为指标
#'
#' 指标定义:
#' - VPIP: Voluntarily Put $ In Pot,主动入池率
#' - PFR : Pre-Flop Raise,翻前加注率
#' - AF  : Aggression Factor = (raise + bet) / call,这里用 cbet/showdown 近似
#' - 3-bet: 3-bet 频率 = 3-bet / 3-bet opportunity
#' - C-bet: 持续下注率 = c-bet / c-bet opportunity
#' - WTSD: Went To Showdown,看到摊牌的比例
#' - BB/100: 每 100 手赢得的大盲注数
#' - 平均筹码深度
#' - 输赢方差与波动率
#'
#' @param df_player  单玩家所有手牌
#' @param big_blind  大盲金额
#' @return 1 行 tibble
.compute_one_player <- function(df_player, big_blind = 2) {
  n <- nrow(df_player)
  if (n == 0) return(NULL)

  vpip <- mean(df_player$vpip)
  pfr  <- mean(df_player$pfr)

  # 3-bet 频率(在有 3-bet 机会的手中)
  three_bet <- if (sum(df_player$threebet_op) > 0)
    sum(df_player$threebet) / sum(df_player$threebet_op) else 0

  # C-bet 频率(在有 c-bet 机会的手中)
  cbet <- if (sum(df_player$cbet_op) > 0)
    sum(df_player$cbet) / sum(df_player$cbet_op) else 0

  wtsd <- if (sum(df_player$vpip) > 0)
    sum(df_player$showdown) / sum(df_player$vpip) else 0

  # AF 近似:bet/raise(用 pfr + cbet)与 call(showdown 但未加注)比值
  raises <- sum(df_player$pfr) + sum(df_player$cbet)
  calls  <- pmax(sum(df_player$vpip) - sum(df_player$pfr), 1)  # 避免 /0
  af     <- raises / calls

  # 收益指标
  total_bb     <- sum(df_player$win_amount) / big_blind
  bb_per_100   <- total_bb / n * 100
  win_var      <- var(df_player$win_amount)
  volatility   <- sqrt(win_var) / big_blind     # 单手标准差(BB)

  avg_stack    <- mean(df_player$stack)
  min_stack    <- min(df_player$stack)
  end_stack    <- df_player$stack[n]

  tibble::tibble(
    player_id   = df_player$player_id[1],
    n_hands     = n,
    VPIP        = vpip,
    PFR         = pfr,
    three_bet   = three_bet,
    cbet        = cbet,
    AF          = af,
    WTSD        = wtsd,
    bb_per_100  = bb_per_100,
    avg_stack   = avg_stack,
    min_stack   = min_stack,
    end_stack   = end_stack,
    win_var     = win_var,
    volatility  = volatility
  )
}

# ---- 2. 整批计算 ------------------------------------------------------------

#' 计算所有玩家的特征矩阵(一行一玩家)
#'
#' @param hands    手牌明细 tibble
#' @param big_blind 大盲金额
#' @return tibble
compute_player_features <- function(hands, big_blind = 2) {
  hands <- dplyr::arrange(hands, .data$player_id, .data$timestamp)
  split_by <- split(hands, hands$player_id)
  out <- lapply(split_by, .compute_one_player, big_blind = big_blind)
  dplyr::bind_rows(out)
}

# ---- 3. 与玩家潜变量合并 ----------------------------------------------------

#' 把行为特征与玩家先验属性合并
join_with_players <- function(features, players) {
  dplyr::left_join(features, players, by = "player_id")
}

# ---- 4. 主入口 --------------------------------------------------------------

run_behavioral_features <- function(processed_dir = "data/processed",
                                    big_blind = 2) {
  hands   <- readRDS(file.path(processed_dir, "hands.rds"))
  players <- readRDS(file.path(processed_dir, "players.rds"))

  message("[features] computing ... (", nrow(hands), " hands)")
  feats   <- compute_player_features(hands, big_blind = big_blind)
  full    <- join_with_players(feats, players)

  saveRDS(full, file.path(processed_dir, "player_features.rds"))
  message("[features] saved to ", processed_dir, "/player_features.rds")
  invisible(full)
}

if (sys.nframe() == 0L) {
  run_behavioral_features()
}
