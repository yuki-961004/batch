#' Title
#'
#' @param list list
#' @param Target target
#'
#' @return 结果
#' @export 结果
#'
shr_acc <- function(list, Target) {

  # 计算每个Identity下的参数值(参数可能是RT, ACC, D, EFF, DDMV, DDMZ)
  Half_1 <- list[[1]] %>%
    dplyr::filter(., Matching == "Matching", ACC == "1") %>%
    dplyr::group_by(Subject, Session, Identity) %>%
    dplyr::summarise(acc = mean(ACC)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = acc)

  Half_2 <- list[[2]] %>%
    dplyr::filter(., Matching == "Matching", ACC == "1") %>%
    dplyr::group_by(Subject, Session, Identity) %>%
    dplyr::summarise(acc = mean(ACC)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = acc)


  # 由于一定是与Self相减，所以i从2开始
  SPE_Half_1 <-list()
  SPE_Half_2 <-list()
  r <- list()
  for (i in 2 : length(Target)) {
    # 分别对两半计算每个Target下的SPE
    SPE_Half_1[[i-1]] <- Half_1 %>%
      dplyr::mutate(SPE_1 = Self - !!sym(Target[i])) %>%
      dplyr::select(Subject, Session, SPE_1)

    SPE_Half_2[[i-1]] <- Half_2 %>%
      dplyr::mutate(SPE_2 = Self - !!sym(Target[i])) %>%
      dplyr::select(Subject, Session, SPE_2)

    # 排除可能存在的缺失值
    df.cor <- SPE_Half_1[[i-1]] %>%
      dplyr::left_join(SPE_Half_2[[i-1]], by = c("Subject", "Session")) %>%
      dplyr::filter(!is.na(SPE_1) & !is.na(SPE_2)) %>%
      dplyr::filter(is.finite(SPE_1) & is.finite(SPE_2))

    # 计算分半信度,并存入r这个list中
    r[[i-1]] <-  cor(df.cor[,3], df.cor[,4], method = "pearson")
    names(r)[[i-1]] <- Target[i]
  }

  # 把r这个list转化为data frame,并删除行名
  result <- rbind(r) %>%
    as.data.frame() %>%
    `rownames<-`(NULL)

  return(result)
}
