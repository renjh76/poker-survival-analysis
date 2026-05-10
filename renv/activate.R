# renv 占位激活脚本
# 真正使用时,请在项目根目录执行:
#   install.packages("renv")
#   renv::init()       # 首次初始化
#   renv::restore()    # 根据 renv.lock 恢复依赖
#
# 该文件仅作占位,确保 renv.lock 路径生效。
local({
  if (requireNamespace("renv", quietly = TRUE)) {
    options(renv.config.auto.snapshot = FALSE)
  } else {
    message("renv not installed; run install.packages('renv') then renv::restore().")
  }
})
