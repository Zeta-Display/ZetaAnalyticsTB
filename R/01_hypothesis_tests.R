#' Performs various hypothesis tests on data.
#'
#' Hypothesis tests performed consider the different proportions of sales
#' of advertised products between specified control group (\code{cntrl_group})
#' and testing group (\code{test_group}), the latter being characterized by
#' the placement of screens to enhance consumer demand and increase sales.
#'
#' @param data_set the data set for a specific butik/shop
#' @param type_test character value identifying the type of test to perform
#' @param type_group_var type_group_var of discriminating variable to fix test vs.
#'   control groups: either vecka (comparing groups) or butik (comparing stores)
#'   against each other
#' @param cntrl_group an integer sequence (from to e.g. \code{28:33}) specifying
#'   the weeks of the control group or a character sequence specifying the butik
#'   names of the control group
#' @param test_group an integer sequence (from to e.g. \code{34:35}) specifying
#'   the weeks of the testing group (if \code{type_group} == "vecka") or
#'   (if \code{type_group} == "butik"), a character sequence specifying the
#'   butik names of the testing group
#' @param options options passed to test function as a list of two elements:
#'    \itemize{
#'    \item \code{conf_level}: defaults to 0.95 and within range (0,1)
#'    \item \code{alternative}: defaults to "two.sided" with other options
#'       being "greater" or "less"
#'    }
#'
#' @return a list of hypothesis-test outputs with each element being a test for
#'   a sub-sample of the data (see internals for details) that contains the
#'   test-output (which varies depending on \code{type_test})
#' @export
hs_perform_tests <- function(data_set,
                             type_test,
                             type_group_var = NULL,
                             cntrl_group,
                             test_group,
                             options) {
  names_data_subsets <- setdiff(names(data_set),
                                c("butik", "vecka", "datum","antal_kvitton"))
  num_subtests <- length(names_data_subsets)

  out_test_results <- vector("list", num_subtests)
  for (i in 1:num_subtests) {
    out_test_results[[i]] <- perform_test(data_set,
                                          var_to_test = names_data_subsets[i],
                                          type_test = type_test,
                                          type_group_var = type_group_var,
                                          cntrl_group = cntrl_group,
                                          test_group = test_group,
                                          options = options)
  }
  names(out_test_results) <-names_data_subsets
  return(out_test_results)
}
#' Performs the exact Fisher test for the specific subset of data.
#'
#' @param data_sub_set a data set as passed via \code{data_set} from
#'    [hs_perform_tests()] but additionally filtered for testing and controlgroup
#' @inheritParams hs_perform_tests
#'
#' @return data matrix for, and output matrix from different tests e.g.
#'    [fisher.test()] for a single specific data (subset)
hs_perform_test <- function(data_sub_set,
                            var_to_test,
                            type_test,
                            type_group_var,
                            cntrl_group,
                            test_group,
                            options = list(conf_level = 0.95,
                                           alternative = "two.sided")) {
  if (is.null(options)) {
    options <- list()
    options$conf_level  <- 0.95
    options$alternative <- "tow.sided"
  }
  test_matrix <- get_test_matrix(data_sub_set,
                                 var_to_test,
                                 type_group_var,
                                 cntrl_group,
                                 test_group)
  if (type_test == "fisher") {
    test_output <- fisher.test(x = test_matrix,
                               alternative = options$alternative,
                               conf.level = options$conf_level)
  } else if (type_test == "proportional") {
    test_output <- prop.test(x = t(test_matrix),
                             n = NULL, # argument ignored as 'x' is a matrix
                             alternative = options$alternative,
                             conf.level = options$conf_level)
  } else {
    stop("Unknown val. to arg. 'type_test'; see help for supported values.")
  }

  list(testmatrix = test_matrix,
       testoutput = test_output)
}
#' Generates numeric matrix from data subset suitable for different tests
#'
#' Supported tests are [prop.test()], [fisher.test()], ...
#'
#' @inheritParams hs_perform_test
#'
#' @return a matrix suitable as first argument to these tests (see Descripiton)
hs_get_test_matrix <- function(data_sub_set,
                               var_to_test,
                               type_group_var,
                               cntrl_group,
                               test_group) {
  check_group_type <- any(type_group_var %in% c("vecka", "butik"))
  stopifnot(`unknown arg-value for 'type_group_var'` = check_group_type)
  if (type_group_var == "vecka") {
    output_dimnames <- list(c("sales target product",
                              "sales other products"),
                            c("testing weeks",
                              "control weeks"))

  } else {
    output_dimnames <- list(c("sales target product",
                              "sales other products"),
                            c("testing stores",
                              "control stores"))
  }

  total_sales_cntr <- data_sub_set %>%
    dplyr::filter(.data[[type_group_var]] %in% cntrl_group) %>%
    dplyr::pull(.data$`antal_kvitton`)%>%
    round(digits = 0)

  total_sales_test <- data_sub_set %>%
    dplyr::filter(.data[[type_group_var]] %in% test_group) %>%
    dplyr::pull(.data$`antal_kvitton`)%>%
    round(digits = 0)

  target_sales_cntr <- data_sub_set %>%
    dplyr::filter(.data[[type_group_var]] %in% cntrl_group) %>%
    dplyr::pull(var_to_test) %>%
    round(digits = 0)

  target_sales_test <- data_sub_set %>%
    dplyr::filter(.data[[type_group_var]] %in% test_group) %>%
    dplyr::pull(var_to_test)%>%
    round(digits = 0)

  out <- matrix(sapply(list(target_sales_test, total_sales_test,
                            target_sales_cntr, total_sales_cntr), sum),
                nrow = 2, ncol = 2)
  out[2, ] <- out[2, , drop = FALSE] - out[1, , drop = FALSE]
  dimnames(out) <- output_dimnames
  return(out)
}
