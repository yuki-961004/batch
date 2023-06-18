#' Title
#'
#' @param list 对象
#' @param Target Identity有几种情况
#' @param Paper_ID 文章的序号
#' @param Indice 这次分析的指标是什么indice ("rc", "acc", "dp", "eff", "ezddm", "rwddm")
#' @param mc 是否是蒙特卡洛的list
#' @param nc 需要用多少个核心。普通分半建议3个，蒙特卡洛分半则使用最大值
#'
#' @return 结果
#' @export 结果
#'
shr <- function(list, Target, Paper_ID, Indice, mc = FALSE, nc = 3) {
  if (mc) {
    result <- mcshr(list, Target, Paper_ID, Indice, nc)
  } else {
    result <- nmshr(list, Target, Paper_ID, Indice, nc)
  }
  return(result)
}
