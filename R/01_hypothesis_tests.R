#' Performs various hypothesis tests on data.
#'
#' Hypothesis tests performed consider the different proportions of sales
#' of advertised products between specified control group (\code{cntrl})
#' and testing group (\code{test}), the latter being characterized by
#' the placement of screens to enhance consumer demand and increase sales.
#'
#' @param data_set the data set for a specific butik/shop
#'
#' @param type type of discriminating variable to fix test vs. control groups:
#'   either vecka (comparing groups) or butik (comparing stores) against each
#'   other
#' @param cntrl an integer sequence (from to e.g. \code{28:33}) specifying
#'   the weeks of the control group or a character sequence specifying the butik
#'   names of the control group
#' @param test an integer sequence (from to e.g. \code{34:35}) specifying
#'   the weeks of the testing group or a character sequence specifying the butik
#'   names of the testing group
#'
#' @return a list of hypothesis-test outputs of dimension
#'   \code{length(ref_unit) + 1} with each element containing the corresponding
#'   tests performed (Fisher, ..)
#' @export
perform_tests <- function(data_set,
                          type = NULL,
                          cntrl,
                          test) {
  names_subtests <- setdiff(names(data_set),
                            c("butik", "vecka", "datum","antal_kvitton"))
  num_subtests <- length(names_subtests)
  names_tests <- c("fisher", "Boschlo")
  out_test_results <- vector("list", num_subtests)
  for (i in 1:num_subtests) {
    out_test_results[[i]] <- test_fisher(data_set,
                                         var_to_test = names_subtests[i],
                                         type = type,
                                         cntrl = cntrl,
                                         test = test)
    # out_test_results[[i]][[1]] <- test_fisher(tmp_data,
    #                                      var_to_test = names_subtests[i],
    #                                      cntrl = cntrl_full,
    #                                      test = test_full)
    # out_test_results[[i]][[2]] <- OTHER TEST(tmp_data,
    #                                      var_to_test = names_subtests[i],
    #                                      cntrl = cntrl_full,
    #                                      test = test_full)
    # names(out_test_results[[i]]) <- names_tests
  }
  names(out_test_results) <- names_subtests
  return(out_test_results)
}
#' Performs the exact Fisher test for the specific subset of data.
#'
#' @param data_sub_set a data set as passed via \code{data_set} from
#'    [perform_tests()] but additionally filtered for testing and controlgroup
#' @inheritParams perform_tests
#'
#' @return test matrix for, and output matrix from [fisher.test()] for specific
#'    data subset
test_fisher <- function(data_sub_set,
                        var_to_test,
                        type,
                        cntrl, test,
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
                                       type,
                                       cntrl,
                                       test)
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
                              type,
                              cntrl,
                              test) {
  stopifnot(`unknown arg-value for 'type'` = any(type %in% c("vecka", "butik")))
  if (type == "vecka") {
    output_dimnames <- list(c("testing weeks",
                              "control weeks"),
                            c("sales target product",
                              "sales other products"))

  } else {
    output_dimnames <- list(c("testing stores",
                              "control stores"),
                            c("sales target product",
                              "sales other products"))
  }

  total_sales_cntr <- data_sub_set %>%
    dplyr::filter(.data[[type]] %in% cntrl) %>%
    dplyr::pull(.data$`antal_kvitton`)%>%
    round(digits = 0)

  total_sales_test <- data_sub_set %>%
    dplyr::filter(.data[[type]] %in% test) %>%
    dplyr::pull(.data$`antal_kvitton`)%>%
    round(digits = 0)

  target_sales_cntr <- data_sub_set %>%
    dplyr::filter(.data[[type]] %in% cntrl) %>%
    dplyr::pull(var_to_test) %>%
    round(digits = 0)

  target_sales_test <- data_sub_set %>%
    dplyr::filter(.data[[type]] %in% test) %>%
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
