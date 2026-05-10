# =============================================================================
# 03_player_clustering.R
# -----------------------------------------------------------------------------
# K-Means 玩家聚类 + 熵权法客观加权 + PCA 可视化 + 簇标签解释
#
# 输入:data/processed/player_features.rds
# 输出:data/processed/player_clusters.rds, figures/cluster_*.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(cluster)
  library(factoextra)
})

# ---- 1. 熵权法 --------------------------------------------------------------

#' 熵权法 (Entropy Weight Method) 计算各列客观权重
#'
#' 步骤:
#'  1. 归一化(min-max)到 [0,1]
#'  2. 计算每列的概率分布 p_ij = x_ij / sum_i x_ij
#'  3. 信息熵 e_j = -1/ln(n) * sum_i p_ij * ln(p_ij)
#'  4. 差异系数 d_j = 1 - e_j
#'  5. 权重 w_j = d_j / sum d_j
#'
#' @param X numeric matrix(行=样本,列=指标)
#' @return numeric vector,长度 = ncol(X)
entropy_weights <- function(X) {
  stopifnot(is.matrix(X) || is.data.frame(X))
  X <- as.matrix(X)
  n <- nrow(X); m <- ncol(X)

  # min-max 归一化
  rng <- apply(X, 2, function(v) max(v) - min(v))
  rng[rng == 0] <- 1
  Xn <- sweep(sweep(X, 2, apply(X, 2, min), "-"), 2, rng, "/")
  Xn <- Xn + 1e-9   # 避免 log(0)

  # 列概率
  P <- sweep(Xn, 2, colSums(Xn), "/")
  # 熵
  k <- 1 / log(n)
  e <- -k * colSums(P * log(P))
  d <- 1 - e
  w <- d / sum(d)
  names(w) <- colnames(X)
  w
}

# ---- 2. 选最优 k ------------------------------------------------------------

#' 用肘部法 + 轮廓系数选 k
#'
#' @param Xs scaled matrix
#' @param k_range 候选 k
#' @return list(elbow_plot, sil_plot, best_k)
choose_optimal_k <- function(Xs, k_range = 2:8) {
  set.seed(42)
  wss <- sapply(k_range, function(k) kmeans(Xs, centers = k,
                                            nstart = 25, iter.max = 50)$tot.withinss)
  sil <- sapply(k_range, function(k) {
    km <- kmeans(Xs, centers = k, nstart = 25, iter.max = 50)
    mean(cluster::silhouette(km$cluster, dist(Xs))[, 3])
  })

  df <- tibble::tibble(k = k_range, wss = wss, sil = sil)

  list(
    diag_df = df,
    best_k  = df$k[which.max(df$sil)]
  )
}

# ---- 3. 簇标签自动解释 -----------------------------------------------------

#' 根据每簇 VPIP / PFR / AF 的均值,自动打 TAG/LAG/TP/LP/Fish 标签
label_clusters <- function(centroids_df) {
  # centroids_df 必须包含列:cluster, VPIP, PFR, AF, bb_per_100
  centroids_df %>%
    dplyr::mutate(
      style_label = dplyr::case_when(
        VPIP <= 0.27 & PFR / pmax(VPIP, 1e-3) >= 0.55 & AF >= 1.5 & bb_per_100 >= 15  ~ "TAG-Pro (紧凶高手)",
        VPIP <= 0.27 & PFR / pmax(VPIP, 1e-3) >= 0.55 & AF >= 1.5 ~ "TAG (紧凶)",
        VPIP >  0.27 & PFR / pmax(VPIP, 1e-3) >= 0.55 & AF >= 1.5 ~ "LAG (松凶)",
        VPIP <= 0.27 & AF <  1.5                                  ~ "TP  (紧弱)",
        VPIP >  0.40 & AF <  1.0                                  ~ "Fish (鱼)(松弱)",
        TRUE                                                      ~ "LP  (松弱)"
      )
    )
}

# ---- 4. 主流程 --------------------------------------------------------------

run_player_clustering <- function(processed_dir = "data/processed",
                                  fig_dir       = "figures",
                                  k_range       = 2:8) {
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

  feats <- readRDS(file.path(processed_dir, "player_features.rds"))

  # 用于聚类的核心行为指标
  cols <- c("VPIP", "PFR", "three_bet", "cbet", "AF",
            "WTSD", "bb_per_100", "volatility")
  X <- as.matrix(dplyr::select(feats, dplyr::all_of(cols)))

  # ---- 4.1 熵权 ----
  w <- entropy_weights(X)
  message("[cluster] entropy weights:")
  print(round(w, 3))

  # 标准化后乘以权重 sqrt(w)(平方根权重在欧氏距离下等价于加权)
  Xs   <- scale(X)
  Xw   <- sweep(Xs, 2, sqrt(w), "*")

  # ---- 4.2 选 k ----
  diag <- choose_optimal_k(Xw, k_range)
  best_k <- diag$best_k
  message("[cluster] best k by silhouette = ", best_k)

  p_elbow <- ggplot(diag$diag_df, aes(k, wss)) +
    geom_line() + geom_point(size = 2) +
    labs(title = "Elbow Method", y = "Within SS") +
    theme_minimal()
  p_sil   <- ggplot(diag$diag_df, aes(k, sil)) +
    geom_line() + geom_point(size = 2, color = "darkgreen") +
    labs(title = "Average Silhouette", y = "silhouette") +
    theme_minimal()

  ggsave(file.path(fig_dir, "cluster_elbow.png"),       p_elbow, width = 6, height = 4, dpi = 150)
  ggsave(file.path(fig_dir, "cluster_silhouette.png"),  p_sil,   width = 6, height = 4, dpi = 150)

  # ---- 4.3 拟合 K-Means ----
  set.seed(42)
  km <- kmeans(Xw, centers = best_k, nstart = 50, iter.max = 100)

  feats$cluster <- factor(km$cluster)

  # ---- 4.4 PCA 可视化 ----
  p_pca <- factoextra::fviz_cluster(
    list(data = Xw, cluster = km$cluster),
    geom = "point", ellipse.type = "norm",
    main = sprintf("K-Means clusters (k = %d, entropy-weighted)", best_k),
    palette = "Dark2"
  ) + theme_minimal()
  ggsave(file.path(fig_dir, "cluster_pca.png"), p_pca,
         width = 7, height = 5.5, dpi = 150)

  # ---- 4.5 簇质心解释 + 自动标签 ----
  centroids <- feats %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(across(all_of(cols), mean), n = dplyr::n(),
                     .groups = "drop")
  centroids <- label_clusters(centroids)
  message("[cluster] centroids + labels:")
  print(centroids)

  feats <- feats %>%
    dplyr::left_join(dplyr::select(centroids, cluster, style_label),
                     by = "cluster")

  saveRDS(list(features = feats,
               centroids = centroids,
               weights   = w,
               kmeans    = km),
          file.path(processed_dir, "player_clusters.rds"))

  message("[cluster] saved to ", processed_dir, "/player_clusters.rds")
  invisible(feats)
}

if (sys.nframe() == 0L) {
  run_player_clustering()
}
