#' Title
#'
#' @param df df
#' @param i i
#'
#' @return 结果
#' @export 结果

df_acc <- function(df,i) {
  result <- df %>%
    dplyr::filter(., Matching == "Matching") %>%
    dplyr::group_by(Subject, Session, Identity) %>%
    dplyr::summarise(acc = mean(ACC)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!if_any(everything(), ~ is.na(.x) | is.infinite(.x))) %>%
    dplyr::group_by(Identity) %>%
    dplyr::summarise(ACC_mean = mean(acc),
                     ACC_sd = sd(acc)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Paper_ID = p[[i]])

  return(result)
}
