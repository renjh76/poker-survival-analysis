# =============================================================================
# 01_data_simulation.R
# -----------------------------------------------------------------------------
# 目的:生成 1000 名玩家、共 50 万手牌的合成德州扑克数据。
# 优先尝试解析 IRC Poker Database;若不可用,则走合成路径。
#
# 输出:
#   data/processed/players.rds  —— 玩家潜在属性表(技能、风格、初始筹码)
#   data/processed/hands.rds    —— 每手牌一行的明细表
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(purrr)
})

# ---- 1. 全局参数 ------------------------------------------------------------

#' 模拟参数(可在 main.R 顶部覆盖)
#' @param n_players  玩家数量
#' @param n_hands    总手牌数
#' @param seed       随机种子
#' @param days_span  数据跨度(天)
SIM_PARAMS <- list(
  n_players = 1000L,
  n_hands   = 500000L,
  seed      = 20260510L,
  days_span = 365L,
  big_blind = 2          # 大盲注金额(美元)
)

# 玩家四类风格的总体先验占比,符合扑克生态常识
STYLE_PRIORS <- c(
  TAG  = 0.20,  # 紧凶  Tight-Aggressive
  LAG  = 0.15,  # 松凶  Loose-Aggressive
  TP   = 0.20,  # 紧弱  Tight-Passive
  LP   = 0.30,  # 松弱  Loose-Passive
  Fish = 0.15   # 鱼    极度松弱 / 完全菜
)

# ---- 2. 工具函数 ------------------------------------------------------------

#' 安全 logit 反函数
inv_logit <- function(x) 1 / (1 + exp(-x))

#' 截断到 [lo, hi]
clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# ---- 3. 尝试加载 IRC Poker Database -----------------------------------------

#' 尝试加载 IRC Poker Database。
#' 数据集主页:http://poker.cs.ualberta.ca/IRC/IRCdata.tgz(已离线多年)。
#' 这里仅给出最小占位入口;若本地存在 data/raw/irc/ 目录则走解析,否则返回 NULL。
#'
#' @return list(players, hands) 或 NULL
try_load_irc <- function(raw_dir = "data/raw/irc") {
  if (!dir.exists(raw_dir)) return(NULL)
  message("Found IRC directory but parser not implemented; falling back to simulation.")
  NULL
}

# ---- 4. 玩家潜变量 ----------------------------------------------------------

#' 抽样玩家的隐藏属性
#' @return tibble: player_id, style, true_skill, init_stack
sample_players <- function(n, priors = STYLE_PRIORS) {
  styles <- sample(names(priors), n, TRUE, prob = priors)
  # 不同风格的真实技能均值不同(技能 ~ N(mu_style, 1))
  mu_skill <- c(TAG = 1.2, LAG = 0.5, TP = -0.2, LP = -0.8, Fish = -1.5)
  skill <- rnorm(n, mean = mu_skill[styles], sd = 0.6)

  # 初始筹码服从对数正态(美元)
  init_stack <- round(rlnorm(n, meanlog = log(500), sdlog = 0.7))

  tibble(
    player_id  = sprintf("P%05d", seq_len(n)),
    style      = styles,
    true_skill = skill,
    init_stack = init_stack
  )
}

# ---- 5. 行为参数:风格 -> 行为概率 ------------------------------------------

#' 每种风格对应的行为参数(VPIP / PFR / 3-bet / agg)期望值
style_action_table <- function() {
  tibble::tribble(
    ~style, ~vpip_mu, ~pfr_mu, ~threebet_mu, ~agg_mu, ~bb100_mu,
    "TAG",   0.22,    0.18,    0.07,         3.0,     25,
    "LAG",   0.32,    0.26,    0.10,         3.5,     8,
    "TP",    0.20,    0.06,    0.02,         0.6,     -8,
    "LP",    0.42,    0.10,    0.03,         0.8,     -22,
    "Fish",  0.55,    0.08,    0.02,         0.5,     -38
  )
}

# ---- 6. 单玩家手牌模拟 ------------------------------------------------------

#' 给定玩家与目标手数,生成手牌行级数据
#'
#' @param player tibble 一行(玩家潜变量)
#' @param n_hands 目标手数
#' @param start_day 起始 day index
#' @param days_span 总跨度
#' @return tibble of hands
simulate_player_hands <- function(player, n_hands, start_day, days_span,
                                  action_tbl, big_blind = 2) {
  if (n_hands <= 0) return(NULL)

  par <- action_tbl[action_tbl$style == player$style, ]

  # 时间戳:在 days_span 内非均匀采样,有“活跃簇”
  # 在 [start_day, start_day + lifetime] 区间生成
  lifetime <- pmin(days_span,
                   round(rgamma(1, shape = 2, rate = 2 / days_span)))
  lifetime <- max(lifetime, 1)
  day_idx  <- sort(sample.int(lifetime, n_hands, replace = TRUE)) +
              (start_day - 1)
  ts <- as.POSIXct("2025-01-01", tz = "UTC") +
        day_idx * 86400 + runif(n_hands, 0, 86400)

  # 行为指标:在玩家先验均值附近抖动
  vpip_flag    <- rbinom(n_hands, 1, clamp(rnorm(1, par$vpip_mu, 0.04), 0.01, 0.95))
  pfr_flag     <- vpip_flag * rbinom(n_hands, 1,
                     clamp(par$pfr_mu / max(par$vpip_mu, 1e-3), 0.05, 0.99))
  threebet_op  <- rbinom(n_hands, 1, 0.10)            # 是否有 3-bet 机会
  threebet_flg <- threebet_op * rbinom(n_hands, 1,
                     clamp(par$threebet_mu * 5, 0.01, 0.7))
  cbet_op      <- pfr_flag * rbinom(n_hands, 1, 0.6)  # PFR 后是否有 c-bet 机会
  cbet_flg     <- cbet_op * rbinom(n_hands, 1, clamp(0.55 + 0.1 * (par$agg_mu - 1.5), 0.1, 0.95))
  showdown_flg <- vpip_flag * rbinom(n_hands, 1,
                     clamp(0.25 + 0.10 * (par$vpip_mu - 0.25), 0.05, 0.6))

  # 位置(0=BTN ... 5=UTG,扑克 6max 简化)
  position <- sample.int(6, n_hands, TRUE) - 1L

  # 底牌强度(0~1),技能高的玩家平均拿到的“游戏完牌”更强(更会择牌)
  hand_strength <- clamp(rbeta(n_hands, 2, 3) +
                         0.05 * player$true_skill, 0.01, 0.99)

  # 每手输赢:符合“技能 + 风格 + 噪音”
  # 期望 BB/100 转成单手期望
  ev_per_hand_bb <- (par$bb100_mu + 4 * player$true_skill) / 100
  ev_dollar     <- ev_per_hand_bb * big_blind
  win_amount    <- rnorm(n_hands, mean = ev_dollar,
                         sd = big_blind * 3)  # 高方差

  # 当前筹码:从初始筹码开始累计
  stack_path <- player$init_stack + cumsum(win_amount)
  stack_path <- pmax(stack_path, 0)   # 破产后筹码截断为 0

  tibble::tibble(
    player_id     = player$player_id,
    timestamp     = ts,
    day_index     = day_idx,
    position      = position,
    hand_strength = hand_strength,
    vpip          = as.integer(vpip_flag),
    pfr           = as.integer(pfr_flag),
    threebet_op   = as.integer(threebet_op),
    threebet      = as.integer(threebet_flg),
    cbet_op       = as.integer(cbet_op),
    cbet          = as.integer(cbet_flg),
    showdown      = as.integer(showdown_flg),
    win_amount    = win_amount,
    stack         = stack_path
  )
}

# ---- 7. 主入口 --------------------------------------------------------------

#' 生成完整数据集
#'
#' @param params  SIM_PARAMS 参数列表
#' @return list(players, hands)
simulate_all <- function(params = SIM_PARAMS) {
  set.seed(params$seed)

  players <- sample_players(params$n_players)

  # 每个玩家分配手数:按对数正态抽样后归一化到 n_hands
  raw <- rlnorm(params$n_players, meanlog = log(500), sdlog = 0.8)
  alloc <- pmax(round(raw / sum(raw) * params$n_hands), 5L)
  # 强制总和精确等于目标
  diff_n <- params$n_hands - sum(alloc)
  if (diff_n != 0) {
    idx <- sample.int(params$n_players, abs(diff_n), TRUE)
    alloc[idx] <- alloc[idx] + sign(diff_n)
  }
  players$n_hands_target <- alloc

  action_tbl <- style_action_table()

  message(sprintf("[sim] generating %s hands across %s players ...",
                  format(params$n_hands, big.mark = ","),
                  format(params$n_players, big.mark = ",")))

  hands_list <- vector("list", nrow(players))
  pb <- txtProgressBar(min = 0, max = nrow(players), style = 3)
  for (i in seq_len(nrow(players))) {
    setTxtProgressBar(pb, i)
    start_day <- sample.int(params$days_span, 1)
    hands_list[[i]] <- simulate_player_hands(
      player    = players[i, ],
      n_hands   = players$n_hands_target[i],
      start_day = start_day,
      days_span = params$days_span,
      action_tbl = action_tbl,
      big_blind = params$big_blind
    )
  }
  close(pb)

  hands <- dplyr::bind_rows(hands_list)
  message(sprintf("[sim] done. hands rows = %s",
                  format(nrow(hands), big.mark = ",")))

  list(players = players, hands = hands)
}

# ---- 8. 持久化 --------------------------------------------------------------

run_data_simulation <- function(params = SIM_PARAMS,
                                out_dir = "data/processed") {
  irc <- try_load_irc()
  data_pair <- if (!is.null(irc)) irc else simulate_all(params)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(data_pair$players, file.path(out_dir, "players.rds"))
  saveRDS(data_pair$hands,   file.path(out_dir, "hands.rds"))

  message("[sim] saved to ", out_dir, "/{players,hands}.rds")
  invisible(data_pair)
}

# 直接 source 该脚本即触发一次完整模拟
if (sys.nframe() == 0L) {
  run_data_simulation()
}
