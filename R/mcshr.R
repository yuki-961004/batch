#' Title
#'
#' @param list list
#' @param Indice indice ("rc", "acc", "dp", "eff", "ezddm", "rwddm")
#' @param Target target
#' @param Paper_ID Paper_ID
#' @param nc number of cores
#'
#' @return 结果
#' @export 结果
#'
mcshr <- function(list, Target, Paper_ID, Indice, nc) {
  result <- switch(Indice,
                   "rt" = loop_shr_rt(list, Target, Paper_ID, Indice, nc),
                   "acc" = loop_shr_acc(list, Target, Paper_ID, Indice, nc),
                   "dp" = loop_shr_dp(list, Target, Paper_ID, Indice, nc),
                   "eff" = loop_shr_eff(list, Target, Paper_ID, Indice, nc),
                   "ezddm" = loop_shr_ezddm(list, Target, Paper_ID, Indice, nc),
                   "rwddm" = loop_shr_rwddm(list, Target, Paper_ID, Indice, nc),
                   stop("Invalid indice argument")
                   )

  output <- result %>%
    tidyr::pivot_longer(cols = Target[2:length(Target)], names_to = "Target", values_to = "r") %>%
    dplyr::mutate(r = as.numeric(r)) %>%
    dplyr::group_by(Target, Indice) %>%
    dplyr::mutate(Mean = mean(r),
                  LLCI = quantile(r, 0.025),
                  ULCI = quantile(r, 0.975),
    ) %>%
    dplyr::select(Paper_ID, Indice, Target, r = Mean, LLCI, ULCI) %>%
    dplyr::distinct()

  return(output)
}

