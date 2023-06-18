#' Title
#'
#' @param df df
#' @param Indice indice ("rc", "acc", "dp", "eff", "ddmv", "ddmz")
#' @param Target target
#'
#' @return 结果
#' @export 结果
#'
icc <- function(df, Indice, Target) {
  result <- switch(Indice,
                   "rt" = icc_rt(df, Target),
                   "acc" = icc_acc(df, Target),
                   "dp" = icc_dp(df, Target),
                   "eff" = icc_eff(df, Target),
                   "ezddm" = icc_ezddm(df, Target),
                   "rwddm" = icc_rwddm(df, Target),
                   stop("Invalid indice argument")
                   )
  return(result)
}
