#' Title
#'
#' @param df df
#' @param i Paper_ID 1-18
#' @param Indice indice ("rc", "acc", "dp", "eff", "ddmv", "ddmz")
#'
#' @return 结果
#' @export 结果
#'
df <- function(df, i, Indice) {
  result <- switch(Indice,
                   "rt" = df_rt(df, i),
                   "acc" = df_acc(df, i),
                   "dp" = df_dp(df, i),
                   "eta" = df_eta(df, i),
                   "ezddm" = df_ezddm(df, i),
                   "rwddm" = df_rwddm(df, i),
                   stop("Invalid indice argument")
  )
  return(result)
}
