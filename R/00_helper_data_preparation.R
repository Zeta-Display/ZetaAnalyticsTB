#' Generates list of zoner, kampanj and skylttyp for a specific butik and week.
#'
#' The butiks can probably be treated equally but the weeks will have different
#' products for zoner, kampanj or skylttyp so the product sets referring to
#' those need adjustments
#'
#' @param pth path to test week as a string
#' @param sheet_name name of the sheet (referencing a "week" testing period) as
#'   a string
#' @param names_validate the names of product IDs for which campaigns and zones
#'   are defined, but which are not available in the raw data set
#'
#' @return a list of appropriate structure giving the product set for zoner,
#'   kampanj and skylttyp
#' @export
get_list_test_week <- function(pth,
                               sheet_name,
                               names_validate) {
  data_tmp <- readxl::read_excel(pth, sheet = sheet_name)
  check_tmplist <- data_tmp$produktID
  check_avail   <- check_tmplist %in% names_validate
  if (!(all(check_avail))) {
    id_non_avail <- which(!check_avail)
    cat(crayon::green("These product-IDs (for zoner/kampanj/skylttyp) are ",
                      "defined in kampanjplanering but not in data for \n"),
        crayon::yellow("week ", sheet_name, " ...\n"))
    cat(crayon::red(paste0(check_tmplist[id_non_avail], "\n")))
    cat(crayon::bgMagenta("Removing these entries... \n"))
  }
  data_tmp <-data_tmp %>%
    dplyr::filter(!(.data$produktID %in% check_tmplist[id_non_avail]))

  names_zoner <- unique(data_tmp$zoner)
  names_kampanj <- unique(data_tmp$kampanj)
  names_skylttyp <- unique(data_tmp$skylttyp)

  num_zoner    <- length(names_zoner)
  num_kampanj  <- length(names_kampanj)
  num_skylttyp <- length(names_skylttyp)

  out_tmplist      <- vector("list", 3)
  out_zonerlist    <- vector("list", num_zoner)
  out_kampanjlist  <- vector("list", num_kampanj)
  out_skylttyplist <- vector("list", num_skylttyp)

  out_tmplist[[1]] <- out_zonerlist
  out_tmplist[[2]] <- out_kampanjlist
  out_tmplist[[3]] <- out_skylttyplist

  names(out_tmplist) <- c("zoner", "kampanj", "skylttyp")
  names(out_tmplist[[1]]) <- names_zoner
  names(out_tmplist[[2]]) <- names_kampanj
  names(out_tmplist[[3]]) <- names_skylttyp

  for (i in 1:num_zoner) {
    products_tmp <- data_tmp %>%
      dplyr::filter(.data$zoner == names_zoner[i]) %>%
      dplyr::select(.data$produktID, .data$produktnamn)
    tmp_vec <- products_tmp$produktID
    names(tmp_vec) <- products_tmp$produktnamn
    out_tmplist[[1]][[i]] <- tmp_vec
  }
  for (i in 1:num_kampanj) {
    products_tmp <- data_tmp %>%
      dplyr::filter(.data$kampanj == names_kampanj[i]) %>%
      dplyr::select(.data$produktID, .data$produktnamn)
    tmp_vec <- products_tmp$produktID
    names(tmp_vec) <- products_tmp$produktnamn
    out_tmplist[[2]][[i]] <- tmp_vec
  }
  for (i in 1:num_skylttyp) {
    products_tmp <- data_tmp %>%
      dplyr::filter(.data$skylttyp == names_skylttyp[i]) %>%
      dplyr::select(.data$produktID, .data$produktnamn)
    tmp_vec <- products_tmp$produktID
    names(tmp_vec) <- products_tmp$produktnamn
    out_tmplist[[3]][[i]] <- tmp_vec
  }
  return(out_tmplist)
}
#' Generates list of zoner, kampanj and skylttyp for a all butiks and weeks.
#'
#' The butiks can probably be treated equally but the weeks will have different
#' products for zoner, kampanj or skylttyp so the product sets referring to
#' those need adjustments. The list summarizes the output for each week as
#' returned from [get_list_test_week()].
#'
#' @param pth path to data set that stores the plan of campaigns
#' @param list_data_validate a list of data sets to validate product IDs against
#'    plan of campaigns
#'
#' @return a list of lists, with element of the latter being output from
#'   [get_list_test_week()].
#' @export
get_meta_data <- function(pth, list_data_validate) {
  num_vecka   <- length(list_data_validate)
  names_vecka <- names(xlsx::getSheets(xlsx::loadWorkbook(pth)))
  names_vecka <-names_vecka[names_vecka %in% names(list_data_validate)]

  product_ids_avail_list <- vector("list", num_vecka)
  for (i in 1:num_vecka) {
    product_ids_avail_list[[i]] <- names(list_data_validate[[i]])
  }
  out_list_test_weeks <- vector("list", num_vecka)
  for (i in seq_len(num_vecka)) {
    out_list_test_weeks[[i]] <- get_list_test_week(pth, names_vecka[i],
                                                   product_ids_avail_list[[i]])
  }
  names(out_list_test_weeks) <-names_vecka
  return(out_list_test_weeks)
}
#' Generates merged data set on sales.
#'
#' Uses first argument to get information on sales (count) of relevant products
#' and second data set on counts of all products sold, and both to compute
#' fractions (count/total) of relevant products. The third arguments identifies
#' the different product sets of "relevant products" depending on zoner, kampanj
#' or skylttyp.
#'
#' @param data_sales1 data on sales of relevant products (per butik, vecka,
#'    datum, and timme)
#' @param data_sales2 data on sales of all products (per butik, vecka, datum,
#'    and timme)
#' @param list_test_weeks output from [get_()]
#' @param time_frq character setting the time frequency of the data set
#'   to be merged: either 'weekly' which generates weekly frequencies and counts
#'   of relevant products per zoner, kampanj and skylttyp or 'daily' for daily
#'   counts and frequencies
#'
#' @return the data set computed as outlined in the description
#' @export
get_merged_data_set <- function(data_sales1,
                                data_sales2,
                                list_test_weeks,
                                time_frq = "weekly") {

  data_sets_list <- get_data_sets_list(data_sales1 = data_sales1,
                                       data_sales2 = data_sales2,
                                       time_frq = time_frq)
  data_final <- data_sets_list$data_final
  col_names_removed <- data_sets_list$col_names_removed

  data_final <- data_final %>%
    add_zoner_vars(tmp_list = list_test_weeks) %>%
    add_skylttyp_vars(tmp_list = list_test_weeks) %>%
    add_kampanj_vars(tmp_list = list_test_weeks)

  data_out <- data_final[, which(!(names(data_final) %in% col_names_removed))]
  if (time_frq == "daily") {
    data_out <- data_out %>% dplyr::arrange(dplyr::across(c("butik",
                                                            "vecka",
                                                            "datum"),
                                                          dplyr::desc)) %>%
      dplyr::ungroup()
    data_out <- data_out %>% dplyr::select(c("butik",
                                             "vecka",
                                             "datum",
                                             "antal_kvitton",
                                             "sales_zoner_count_TOTAL",
                                             "sales_zoner_frac_TOTAL"),
                                           dplyr::contains("sales_zoner"),
                                           c("sales_kampanj_count_TOTAL",
                                             "sales_kampanj_frac_TOTAL"),
                                           dplyr::contains("sales_kampanj"),
                                           c("sales_skylttyp_count_TOTAL",
                                             "sales_skylttyp_frac_TOTAL"),
                                           dplyr::contains("sales_skylttyp"))
  } else if (time_frq == "weekly") {
    data_out <- data_out %>% dplyr::arrange(dplyr::across(c("butik",
                                                            "vecka"),
                                                          dplyr::desc)) %>%
      dplyr::ungroup()
    data_out <- data_out %>% dplyr::select(c("butik",
                                             "vecka",
                                             "antal_kvitton",
                                             "sales_zoner_count_TOTAL",
                                             "sales_zoner_frac_TOTAL"),
                                           dplyr::contains("sales_zoner"),
                                           c("sales_kampanj_count_TOTAL",
                                             "sales_kampanj_frac_TOTAL"),
                                           dplyr::contains("sales_kampanj"),
                                           c("sales_skylttyp_count_TOTAL",
                                             "sales_skylttyp_frac_TOTAL"),
                                           dplyr::contains("sales_skylttyp"))
  }
  return(data_out)
}
get_data_sets_list <- function(data_sales1, data_sales2, time_frq) {
    data_sets     <- vector("list", 2)
  if (time_frq == "daily") {
    data_sets[[1]] <- data_sales1 %>%
      dplyr::group_by(.data$butik, .data$vecka, .data$datum) %>%
      dplyr::summarize_if(.predicate = is.numeric,.funs = sum, na.rm = TRUE)

    data_sets[[2]] <- data_sales2 %>%
      dplyr::select(-.data$timme) %>%
      dplyr::group_by(.data$butik, .data$vecka, .data$datum) %>%
      dplyr::summarize_if(.predicate = is.numeric,.funs = sum, na.rm = TRUE)

    product_ids <- setdiff(names(data_sets[[1]]),
                           c("butik", "vecka", "datum"))
    col_names_removed <- product_ids

  } else if (time_frq == "weekly") {
    data_sets[[1]] <- data_sales1 %>%
      dplyr::select(-.data$datum) %>%
      dplyr::group_by(.data$butik, .data$vecka) %>%
      dplyr::summarize_if(.predicate = is.numeric,.funs = sum, na.rm = TRUE)

    data_sets[[2]] <- data_sales2 %>%
      dplyr::select(-.data$datum) %>%
      dplyr::select(-.data$timme) %>%
      dplyr::group_by(.data$butik, .data$vecka) %>%
      dplyr::summarize_if(.predicate = is.numeric,.funs = sum, na.rm = TRUE)

    product_ids <- setdiff(names(data_sets[[1]]),
                           c("butik", "vecka"))
    col_names_removed <- product_ids
  } else {
    msg <- paste0("Unrecognized argument value to 'time_frq': ",
                  "either daily or weekly. ")
    stop(msg)
  }

  data_final <- dplyr::inner_join(data_sets[[1]], data_sets[[2]])
  return(list(data_final = data_final, col_names_removed = col_names_removed))
}
add_zoner_vars <- function(data_set, tmp_list) {
  names_zoner <- names(tmp_list[["zoner"]])
  num_zoner   <- length(names_zoner)
  for(j in 1:num_zoner) {
    tmp_prod_ids <- as.character(tmp_list[["zoner"]][[j]])
    tmp_name1    <- paste0("sales_zoner_count_", names_zoner[j])
    tmp_name2    <- paste0("sales_zoner_frac_", names_zoner[j])
    data_set <- data_set %>%
      dplyr::mutate({{ tmp_name1 }} := rowSums(dplyr::across(tidyselect::all_of(tmp_prod_ids)),
                                               na.rm = TRUE)) %>%
      dplyr::mutate({{ tmp_name2 }} := .data[[tmp_name1]] / .data$antal_kvitton)
  }
  data_set <- data_set %>%
    dplyr::mutate(sales_zoner_count_TOTAL = rowSums(dplyr::across(dplyr::contains("sales_zoner_count_"),
                                                                  na.rm = TRUE))) %>%
    dplyr::mutate(sales_zoner_frac_TOTAL = .data$sales_zoner_count_TOTAL / .data$antal_kvitton)
  data_set
}
add_skylttyp_vars <- function(data_set, tmp_list) {
  names_skylttyp <- names(tmp_list[["skylttyp"]])
  num_skylttyp   <- length(names_skylttyp)
  for(j in 1:num_skylttyp) {
    tmp_prod_ids <- as.character(tmp_list[["skylttyp"]][[j]])
    tmp_name1    <- paste0("sales_skylttyp_count_", names_skylttyp[j])
    tmp_name2    <- paste0("sales_skylttyp_frac_", names_skylttyp[j])
    data_set <- data_set %>%
      dplyr::mutate({{ tmp_name1 }} := rowSums(dplyr::across(tidyselect::all_of(tmp_prod_ids)),
                                               na.rm = TRUE)) %>%
      dplyr::mutate({{ tmp_name2 }} := .data[[tmp_name1]] / .data$antal_kvitton)
  }
  data_set <- data_set %>%
    dplyr::mutate(sales_skylttyp_count_TOTAL = rowSums(dplyr::across(dplyr::contains("sales_skylttyp_count_"),
                                                                     na.rm = TRUE))) %>%
    dplyr::mutate(sales_skylttyp_frac_TOTAL = .data$sales_skylttyp_count_TOTAL / .data$antal_kvitton)
  data_set
}
add_kampanj_vars <- function(data_set,
                             tmp_list) {
  names_kampanj <- names(tmp_list[["kampanj"]])
  num_kampanj   <- length(names_kampanj)
  for(j in 1:num_kampanj) {
    tmp_prod_ids <- as.character(tmp_list[["kampanj"]][[j]])
    tmp_name1    <- paste0("sales_kampanj_count_", names_kampanj[j])
    tmp_name2    <- paste0("sales_kampanj_frac_", names_kampanj[j])
    data_set <- data_set %>%
      dplyr::mutate({{ tmp_name1 }} := rowSums(dplyr::across(tidyselect::all_of(tmp_prod_ids)),
                                               na.rm = TRUE)) %>%
      dplyr::mutate({{ tmp_name2 }} := .data[[tmp_name1]] / .data$antal_kvitton)
  }
  data_set <- data_set %>%
    dplyr::mutate(sales_kampanj_count_TOTAL = rowSums(dplyr::across(dplyr::contains("sales_kampanj_count_"),
                                                                    na.rm = TRUE))) %>%
    dplyr::mutate(sales_kampanj_frac_TOTAL = .data$sales_kampanj_count_TOTAL / .data$antal_kvitton)
  data_set
}
#' Generate data sets per butik/shop, week, and reference unit.
#'
#' Applies filtering commands to daily or weekly data sets according to uniquely
#' available butiks in the given data, the reference unit (zoner, kampanj,
#' skylttyp) and subject to the time frequency.
#'
#' @param data_all the overall data set (containing all possible refernce units)
#' @param data_meta the meta data as generated via [get_meta_data()]
#' @param time_frq a character specifying the time frequency (either 'daily' or
#'    'weekly')
#' @param ref_unit a character specifying the reference unit of either
#'   \itemize{
#'     \item "zoner"
#'     \item "kampanj"
#'     \item "skylttyp"
#'   }
#'
#' @return a list of length number of weeks, with elements being lists of length
#'   number of unique butiks which gives the overall data set as parts/slices
#'   per week and butik
#' @export
get_data_butik_reference_unit <- function(data_all,
                                          data_meta,
                                          time_frq,
                                          ref_unit) {
  num_weeks   <- length(data_meta)
  names_weeks <- names(data_meta)

  names_butik <- unique(data_all$butik)
  num_butik   <- length(names_butik)

  output <- vector("list", num_weeks)
  for (i in 1:num_weeks) {
    data_meta_tmp <- data_meta[[i]]
    name_zoner <- names(data_meta_tmp[[ref_unit]])

    if (time_frq == "daily") {
      data_all_tmp <- data_all %>% dplyr::select(.data$butik,
                                                 .data$vecka,
                                                 .data$datum,
                                                 .data$antal_kvitton,
                                                 tidyselect::contains(ref_unit))
    } else if (time_frq == "weekly") {
      data_all_tmp <- data_all %>% dplyr::select(.data$butik,
                                                 .data$vecka,
                                                 .data$antal_kvitton,
                                                 tidyselect::contains(ref_unit))
    } else {
      stop("Unknown time frequency passed via 'time_frq': check for typos")
    }

    output[[i]] <- vector("list", num_butik)
    for (j in 1:num_butik) {
      output[[i]][[j]] <- data_all_tmp %>%
        dplyr::filter(.data$butik == names_butik[j])
    }
    names(output[[i]]) <- names_butik
  }
  names(output) <- names_weeks
  return(output)
}
