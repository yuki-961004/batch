#' Title
#'
#' @param list list
#' @param Target target
#'
#' @return 结果
#' @export 结果
#'
shr_ezddm <- function(list, Target) {

  # 计算每个Identity下的参数值(参数可能是RT, ACC, D, EFF, DDMV, DDMZ)
  Half_1 <- list[[1]] %>%
    # 不关心Nonmatching组的情况，提前筛选掉，提高运行速度
    dplyr::filter(Matching == "Matching") %>%
    ###############################CORE CODES###################################
    hausekeep::fit_ezddm(data = ., rts = "RT_sec", responses = "ACC",
                         id = "Subject", group = c("Session", "Matching", "Identity")) %>%
    ###############################CORE CODES###################################
    dplyr::mutate(z = a / 2,
                  t = t0_Ter) %>%
    dplyr::select(Subject, Session, Matching, Identity, a, t, v, z)


  Half_2 <- list[[2]] %>%
    # 不关心Nonmatching组的情况，提前筛选掉，提高运行速度
    dplyr::filter(Matching == "Matching") %>%
    ###############################CORE CODES###################################
    hausekeep::fit_ezddm(data = ., rts = "RT_sec", responses = "ACC",
                         id = "Subject", group = c("Session", "Matching", "Identity")) %>%
    ###############################CORE CODES###################################
    dplyr::mutate(z = a / 2,
                  t = t0_Ter) %>%
    dplyr::select(Subject, Session, Matching, Identity, a, t, v, z)

  ################################################################################

  Half_1_v <- Half_1 %>%
    dplyr::select(Subject, Session, Matching, Identity, v) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = v)

  Half_2_v <- Half_2 %>%
    dplyr::select(Subject, Session, Matching, Identity, v) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = v)

  # 由于一定是与Self相减，所以i从2开始
  SPE_Half_1_v <-list()
  SPE_Half_2_v <-list()
  r_v <- list()
  for (i in 2 : length(Target)) {
    # 分别对两半计算每个Target下的SPE
    SPE_Half_1_v[[i-1]] <- Half_1_v %>%
      dplyr::mutate(SPE_1 = Self - !!sym(Target[i])) %>%
      dplyr::select(Subject, Session, SPE_1)

    SPE_Half_2_v[[i-1]] <- Half_2_v %>%
      dplyr::mutate(SPE_2 = Self - !!sym(Target[i])) %>%
      dplyr::select(Subject, Session, SPE_2)

    # 排除可能存在的缺失值
    df_cor_v <- SPE_Half_1_v[[i-1]] %>%
      dplyr::left_join(SPE_Half_2_v[[i-1]], by = c("Subject", "Session")) %>%
      dplyr::filter(!is.na(SPE_1) & !is.na(SPE_2)) %>%
      dplyr::filter(is.finite(SPE_1) & is.finite(SPE_2))

    # 计算分半信度,并存入r这个list中
    r_v[[i-1]] <-  cor(df_cor_v[,3], df_cor_v[,4], method = "pearson")
    names(r_v)[[i-1]] <- Target[i]
  }

  # 把r这个list转化为data frame,并删除行名
  result_v <- rbind(r_v) %>%
    as.data.frame() %>%
    `rownames<-`(NULL) %>%
    dplyr::mutate(Indice = "ez_v")

  ################################################################################

  Half_1_z <- Half_1 %>%
    dplyr::select(Subject, Session, Matching, Identity, z) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = z)

  Half_2_z <- Half_2 %>%
    dplyr::select(Subject, Session, Matching, Identity, z) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = z)

  # 由于一定是与Self相减，所以i从2开始
  SPE_Half_1_z <-list()
  SPE_Half_2_z <-list()
  r_z <- list()
  for (i in 2 : length(Target)) {
    # 分别对两半计算每个Target下的SPE
    SPE_Half_1_z[[i-1]] <- Half_1_z %>%
      dplyr::mutate(SPE_1 = Self - !!sym(Target[i])) %>%
      dplyr::select(Subject, Session, SPE_1)

    SPE_Half_2_z[[i-1]] <- Half_2_z %>%
      dplyr::mutate(SPE_2 = Self - !!sym(Target[i])) %>%
      dplyr::select(Subject, Session, SPE_2)

    # 排除可能存在的缺失值
    df_cor_z <- SPE_Half_1_z[[i-1]] %>%
      dplyr::left_join(SPE_Half_2_z[[i-1]], by = c("Subject", "Session")) %>%
      dplyr::filter(!is.na(SPE_1) & !is.na(SPE_2)) %>%
      dplyr::filter(is.finite(SPE_1) & is.finite(SPE_2))

    # 计算分半信度,并存入r这个list中
    r_z[[i-1]] <-  cor(df_cor_z[,3], df_cor_z[,4], method = "pearson")
    names(r_z)[[i-1]] <- Target[i]
  }

  # 把r这个list转化为data frame,并删除行名
  result_z <- rbind(r_z) %>%
    as.data.frame() %>%
    `rownames<-`(NULL) %>%
    dplyr::mutate(Indice = "ez_z")

  # 合并v和z的data frame
  output <- bind_rows(result_v, result_z)

  return(output)
}
