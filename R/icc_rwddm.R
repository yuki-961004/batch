#' Title
#'
#' @param df df
#' @param Target target
#'
#' @return output
#' @export 结果

icc_rwddm <- function(df, Target) {
  df <- df %>%
    # RWiener::wdm 只识别lower upper
    dplyr::mutate(ACC = case_when(ACC == 0 ~ "lower",
                                  ACC == 1 ~ "upper")) %>%
    # RWiener::wdm 只接受一个分组变量
    split(.$Subject) %>%
    # 执行wdm，然后将分割的结果重新组合
    base::lapply(., rwddm) %>%
    base::do.call(rbind, .)

  df_v <- df %>%
    dplyr::select(Subject, Session, Matching, Identity, v) %>%
    dplyr::filter(Matching == "Matching") %>%
    dplyr::group_by(Subject, Session) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = v) %>%
    dplyr::summarise(v_SPE = Self - !!sym(Target)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = Session, value = v_SPE) %>%
    dplyr::select(-Subject)

  df_z <- df %>%
    dplyr::select(Subject, Session, Matching, Identity, z) %>%
    dplyr::filter(Matching == "Matching") %>%
    dplyr::group_by(Subject, Session) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = z) %>%
    dplyr::summarise(z_SPE = Self - !!sym(Target)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = Session, value = z_SPE) %>%
    dplyr::select(-Subject)

  df_vz <- list(df_v, df_z)

  return(df_vz)
}
