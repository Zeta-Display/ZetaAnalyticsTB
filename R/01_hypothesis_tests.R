#' Performs various hypothesis tests on data.
#'
#' Hypothesis tests performed consider the different proportions of sales
#' of advertised products between specified control period (\code{cntr_period})
#' and testing period (\code{test_period}), the latter being characterized by
#' the placement of screens to enhance consumer demand and increase sales.
#'
#' @param data_set the data set for a specific butik/shop
#' @param ref_unit a character specifying the reference unit of either
#'   \itemize{
#'     \item "zoner"
#'     \item "kampanj"
#'     \item "skylttyp"
#'   }
#'   for which the hypothesis test should be performed. Additionally, total
#'   sales i.e. the overall comparison for a shop/butik between testing and
#'   control phase is performed. The number of hypothesis tests equals therefor
#'   the number of different values for the reference unit + 1 (the overall
#'   test).
#' @param cntr_period an integer sequence (from to e.g. \code{28:33}) specifying
#'   the weeks of the control period
#' @param test_period an integer sequence (from to e.g. \code{34:35}) specifying
#'   the weeks of the testing period
#'
#' @return a list of hypothesis outputs of dimension \code{length(ref_unit) + 1}
#'   with each element containing the corresponding tests performed (Fisher, ..)
#' @export
perform_tests <- function(data_set,
                          ref_unit,
                          cntr_period,
                          test_period) {
  ref_unit2 <- paste0("sales_", ref_unit, "_count")
  cntr_period_full <- as.numeric(paste0("2022", cntr_period))
  test_period_full <- as.numeric(paste0("2022", test_period))
  all_periods_char <- c(cntr_period_full, test_period_full)
  tmp_data <- data_set %>%
    dplyr::filter(.data$vecka %in% all_periods_char) %>%
    dplyr::select(.data$vecka, .data$antal_kvitton, dplyr::contains(ref_unit2))

  names_subtests <- setdiff(names(tmp_data), c("vecka",
                                               "butik",
                                               "antal_kvitton"))
  num_subtests <- length(names_subtests)
  names_tests <- c("fisher", "Boschlo")
  out_test_results <- vector("list", num_subtests)
  for (i in 1:num_subtests) {
    out_test_results[[i]] <- test_fisher(tmp_data,
                                         var_to_test = names_subtests[i],
                                         cntr_period = cntr_period_full,
                                         test_period = test_period_full)
    # out_test_results[[i]][[1]] <- test_fisher(tmp_data,
    #                                      var_to_test = names_subtests[i],
    #                                      cntr_period = cntr_period_full,
    #                                      test_period = test_period_full)
    # out_test_results[[i]][[2]] <- OTHER TEST(tmp_data,
    #                                      var_to_test = names_subtests[i],
    #                                      cntr_period = cntr_period_full,
    #                                      test_period = test_period_full)
    # names(out_test_results[[i]]) <- names_tests
  }
  names(out_test_results) <- names_subtests
  return(out_test_results)
}
#' Performs the exact Fisher test for the specific subset of data.
#'
#' @param data_sub_set a data set as passed via \code{data_set} from
#'    [perform_tests()] but additionally filtered for testing and control period
#' @inheritParams perform_tests
#'
#' @return test matrix for, and output matrix from [fisher.test()] for specific
#'    data subset
test_fisher <- function(data_sub_set, var_to_test, cntr_period, test_period,
                         options = NULL) {
  if (!is.null(options())) {
    conf_lvl <- 0.95
    alternative <- "two.sided"
  } else {
    conf_lvl <- options$confidence
    alternative <- options$alternative
  }

  test_fisher_mat <- get_matrix_fisher(data_sub_set,
                                       var_to_test,
                                       cntr_period,
                                       test_period)
  test_fisher <- fisher.test(test_fisher_mat,
                             alternative = alternative,
                             conf.level = conf_lvl)
  list(testmatrix = test_fisher_mat,
       testoutput = test_fisher)
}

#' Generates numeric matrix from data subset suitable for [fisher.test()].
#'
#' @inheritParams test_fisher
#'
#' @return a matrix suitable for [fisher.test()] with appropriate
#'   \code{dimnames}
get_matrix_fisher <- function(data_sub_set,
                              var_to_test,
                              cntr_period,
                              test_period) {
  output_dimnames <- list(c("testing weeks",
                            "control weeks"),
                          c("sales target product",
                            "sales other products"))
  total_sales_cntr <- data_sub_set %>%
    dplyr::filter(.data$vecka %in% cntr_period) %>%
    dplyr::pull(.data$`antal_kvitton`)%>%
    round(digits = 0)

  total_sales_test <- data_sub_set %>%
    dplyr::filter(.data$vecka %in% test_period) %>%
    dplyr::pull(.data$`antal_kvitton`)%>%
    round(digits = 0)

  target_sales_cntr <- data_sub_set %>%
    dplyr::filter(.data$vecka %in% cntr_period) %>%
    dplyr::pull(var_to_test) %>%
    round(digits = 0)

  target_sales_test <- data_sub_set %>%
    dplyr::filter(.data$vecka %in% test_period) %>%
    dplyr::pull(var_to_test)%>%
    round(digits = 0)

  out <- matrix(sapply(list(target_sales_test, total_sales_test,
                           target_sales_cntr, total_sales_cntr), sum),
                nrow = 2, ncol = 2,
                byrow = TRUE)
  out[, 2] <- out[, 2, drop = FALSE] - out[, 1, drop = FALSE]
  dimnames(out) <- output_dimnames
  return(out)
}
