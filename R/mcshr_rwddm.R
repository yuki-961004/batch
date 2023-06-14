#' Title
#'
#' @param list list
#' @param Target target
#' @param Paper_ID Paper_ID
#'
#' @return 结果
#' @export 结果
#'
mcshr_rwddm <- function(list, Target, Paper_ID) {

  SPE_half_1 <- list[[1]] %>%
    # RWiener::wdm 只识别lower upper
    dplyr::mutate(ACC = case_when(ACC == 0 ~ "lower",
                                  ACC == 1 ~ "upper")) %>%
    # RWiener::wdm 只接受一个分组变量
    split(.$Subject) %>%
    # 执行wdm，然后将分割的结果重新组合
    base::lapply(., rwddm) %>%
    base::do.call(rbind, .)

  SPE_half_2 <- list[[2]] %>%
    # RWiener::wdm 只识别lower upper
    dplyr::mutate(ACC = case_when(ACC == 0 ~ "lower",
                                  ACC == 1 ~ "upper")) %>%
    # RWiener::wdm 只接受一个分组变量
    split(.$Subject) %>%
    # 执行wdm，然后将分割的结果重新组合
    base::lapply(., rwddm) %>%
    base::do.call(rbind, .)

  ################################################################################

  SPE_half_1_v <- SPE_half_1 %>%
    dplyr::select(Subject, Session, Matching, Identity, v) %>%
    dplyr::filter(Matching == "Matching") %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = v) %>%
    dplyr::mutate(v_SPE_1 = Self - !!sym(Target)) %>%
    dplyr::select(Subject, Session, v_SPE_1)

  SPE_half_2_v <- SPE_half_2 %>%
    dplyr::select(Subject, Session, Matching, Identity, v) %>%
    dplyr::filter(Matching == "Matching") %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = v) %>%
    dplyr::mutate(v_SPE_2 = Self - !!sym(Target)) %>%
    dplyr::select(Subject, Session, v_SPE_2)

  df_cor_v <- SPE_half_1_v %>%
    dplyr::left_join(SPE_half_2_v, by = c("Subject", "Session")) %>%
    dplyr::filter(!is.na(v_SPE_1) & !is.na(v_SPE_2)) %>%
    dplyr::filter(is.finite(v_SPE_1) & is.finite(v_SPE_2))

  r_values_v <- cor(df_cor_v[,3], df_cor_v[,4], method = "pearson")

  ################################################################################

  SPE_half_1_z <- SPE_half_1 %>%
    dplyr::select(Subject, Session, Matching, Identity, z) %>%
    dplyr::filter(Matching == "Matching") %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = z) %>%
    dplyr::mutate(z_SPE_1 = Self - !!sym(Target)) %>%
    dplyr::select(Subject, Session, z_SPE_1)

  SPE_half_2_z <- SPE_half_2 %>%
    dplyr::select(Subject, Session, Matching, Identity, z) %>%
    dplyr::filter(Matching == "Matching") %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = z) %>%
    dplyr::mutate(z_SPE_2 = Self - !!sym(Target)) %>%
    dplyr::select(Subject, Session, z_SPE_2)

  df_cor_z <- SPE_half_1_z %>%
    dplyr::left_join(SPE_half_2_z, by = c("Subject", "Session")) %>%
    dplyr::filter(!is.na(z_SPE_1) & !is.na(z_SPE_2)) %>%
    dplyr::filter(is.finite(z_SPE_1) & is.finite(z_SPE_2))

  r_values_z <- cor(df_cor_z[,3], df_cor_z[,4], method = "pearson")


  # Calculate the mean of the Pearson correlation coefficients
  df_result <- data.frame(v = r_values_v[,1], z = r_values_z[,1], Target = Target, Paper_ID = Paper_ID)
return(df_result)
}
