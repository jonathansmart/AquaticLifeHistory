


#' boot_data
#'
#' @param .data Length at Age data
#' @param n number of boostrap iterations
#'
#' @return a grouped_df produced by the group_by function in dplyr

boot_data <- function(.data, n = 100){
  new.dat <- list()
  for(i in 1:n){
    new_ids <- sample(1:nrow(.data),replace = TRUE)

    tmp <- .data[new_ids,]
    tmp$id <- i
    new.dat[[i]] <- tmp

  }

  new.dat <- plyr::ldply(new.dat) %>%
    dplyr::group_by(id)

  return(new.dat)
}
