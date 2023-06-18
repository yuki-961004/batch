#' Title
#'
#' @param df df
#' @param Target target
#'
#' @return output
#' @export 结果

icc_ezddm <- function(df, Target) {
  df <- df %>%
    # 不关心Nonmatching组的情况，提前筛选掉，提高运行速度
    dplyr::filter(Matching == "Matching") %>%
    ###############################CORE CODES###################################
    hausekeep::fit_ezddm(data = ., rts = "RT_sec", responses = "ACC",
                         id = "Subject", group = c("Session", "Matching", "Identity")) %>%
    ###############################CORE CODES###################################
    dplyr::mutate(z = a / 2,
                  t = t0_Ter) %>%
    dplyr::select(Subject, Session, Matching, Identity, a, t, v, z)


  df_v <- df %>%
    dplyr::select(Subject, Session, Matching, Identity, v) %>%
    dplyr::group_by(Subject, Session) %>%
    tidyr::pivot_wider(names_from = Identity,
                       values_from = v) %>%
    dplyr::summarise(v_SPE = Self - !!sym(Target)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = Session, value = v_SPE) %>%
    dplyr::select(-Subject)

  df_z <- df %>%
    dplyr::select(Subject, Session, Matching, Identity, z) %>%
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
