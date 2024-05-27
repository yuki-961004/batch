#' Title
#'
#' @param df df
#' @param i i
#'
#' @return 结果
#' @export 结果

df_dp <- function(df,i) {
  result <- df %>%
    dplyr::group_by(Subject,Session,Identity) %>%
    dplyr::summarise(
      hit = length(ACC[Matching == "Matching" & ACC == 1]),
      fa = length(ACC[Matching == "Nonmatching" & ACC == 0]),
      miss = length(ACC[Matching == "Matching" & ACC == 0]),
      cr = length(ACC[Matching == "Nonmatching" & ACC == 1]),
      Dprime = qnorm(
        ifelse(hit / (hit + miss) < 1,
               hit / (hit + miss),
               1 - 1 / (2 * (hit + miss))
        )
      ) - qnorm(
        ifelse(fa / (fa + cr) > 0,
               fa / (fa + cr),
               1 / (2 * (fa + cr))
        )
      )) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"hit",-"fa",-"miss",-"cr")%>%
    dplyr::filter(!if_any(everything(), ~ is.na(.x) | is.infinite(.x))) %>%
    dplyr::group_by(Identity) %>%
    dplyr::summarise(d_mean = mean(Dprime),
                     d_sd = sd(Dprime)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Paper_ID = p[[i]])

  return(result)
}
