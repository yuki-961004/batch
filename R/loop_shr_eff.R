#' Title
#'
#' @param list list of mc shr
#' @param Target Friend Stranger and ...
#' @param Paper_ID Paper_ID
#' @param Indice indice ("rc", "acc", "dp", "eff", "ezddm", "rwddm")
#' @param nc number of cores

#'
#' @return 结果
#' @export 结果
#'
loop_shr_eff <- function(list, Target, Paper_ID, Indice, nc) {

  # 设置并行计算
  registerDoParallel(cores = nc)

  # 初始化一个空的数据框
  df_output <- data.frame()

  # 获取列表的长度
  list_length <- length(list)

  # 定义每次迭代处理的元素个数
  subset_size <- nc

  # 循环遍历选取nc个元素的列表
  for (i in 1:ceiling(list_length / subset_size)) {
    # 计算当前迭代需要处理的元素范围
    start_index <- (subset_size * (i - 1)) + 1
    end_index <- min(subset_size * i, list_length)

    # 从列表中选取当前迭代需要处理的元素
    list_subset <- list[start_index:end_index]

    # 使用foreach循环并行执行迭代
    output <- foreach(j = 1:length(list_subset), .combine = rbind, .packages = c("dplyr", "tidyr", "yukiBP")) %dopar% {

      # 调用函数并得到数据框
      result <- yukiBP::shr_eff(list = list_subset[[j]], Target = Target) %>%
        dplyr::mutate(Iteration = (i - 1) * nc + j,
                      Paper_ID = Paper_ID,
                      Indice = Indice)

      return(result)

    }

    # 合并每次得到的结果
    df_output <- bind_rows(df_output, output)
  }

  # 停止并行计算
  stopImplicitCluster()

  return(df_output)
}
