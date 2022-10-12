#' Read sales data on advertised products and split into test and control weeks
#'
#' The data set need to be split because this is required input for all
#' subsequent operations which are based on data (for all butiks) in the test
#' week under consideration and control weeks 2022-01 to 2022-33, which are
#' passed via corresponding arguments. Also the 'HEMKÖP' part in the butik name
#'  is removed.
#'
#' @param pth_to_dataset pth to the overall data set
#' @param vecka_seq a vector of double digit integers specifying the test weeks
#' @param cntrl_min 6 digit integer indicating the first control week (e.g.
#'   202201)
#' @param cntrl_max 6 digit integer indicating the last control week (e.g.
#'   202233)
#'
#' @return side effect function writing the data sets to the "data/..." location
#' @export
get_data_to_weekly <- function(pth_to_dataset,
                               vecka_seq,
                               cntrl_min, cntrl_max) {
  num_vecka <- length(vecka_seq)
  data_all <- readxl::read_excel(pth_to_dataset)
  data_all$butik <- stringr::str_replace_all(string = data_all$butik,
                                             pattern = "HEMKÖP ",
                                             replacement = "")
  for (i in 1:num_vecka) {
    tmp_data <- data_all %>%
      dplyr::filter(.data$vecka %in% c(202200 + vecka_seq[i],
                                       cntrl_min:cntrl_max))
    pth_to_write <- paste0("data/data_products_advertised",
                           "_v",
                           vecka_seq[i], ".xlsx")
    cat(crayon::green("Writing data to: \n"),
        crayon::yellow(pth_to_write), "\n")
    writexl::write_xlsx(tmp_data, pth_to_write)
  }
}
#' Generates a list of data sets with advertised (target) products.
#'
#' Output List elements are taken per week (vecka)
#'
#' @param vecka_seq integer vector giving the week numbers to use
#' @param pth_data_prod_adv a character vector of paths to the data sets (per
#'   week) that have all IDs
#'
#' @return a list of data sets witch each element representing data (for a week)
#'   that is cleaned for relevant product IDs
#' @export
get_list_data_sales_prod_adv <- function(vecka_seq,
                                         pth_data_prod_adv) {

  num_vecka <- length(vecka_seq)
  num_prod_list <- vector("numeric", length = num_vecka)
  data_sales_prod_adv <- vector("list", length = num_vecka)
  names(data_sales_prod_adv) <- paste0("v", vecka_seq)

  for (i in 1:num_vecka) {
    num_prod_list[i] <- ncol(readxl::read_excel(pth_data_prod_adv[i],
                                                sheet = 1)) - 4 # e.g. 119-4 or 65-4
    vec_col_types <- c("text", "numeric", "guess", "text",
                       rep("numeric", times = num_prod_list[i]))
    data_sales_prod_adv[[i]] <- readxl::read_excel(pth_data_prod_adv[i],
                                                   sheet = 1,
                                                   col_types = vec_col_types)
  }
  return(data_sales_prod_adv)
}
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
#' @inheritParams get_meta_data
#'
#' @return a list of appropriate structure giving the product set for zoner,
#'   kampanj and skylttyp; optionally, the list's second element returns product
#'   IDs that are in the data but never appear in kampanjplanering
#' @export
get_list_test_week <- function(pth,
                               sheet_name,
                               names_validate,
                               validate_data,
                               validate_kampanj) {
  data_tmp <- readxl::read_excel(pth, sheet = sheet_name)
  check_tmplist <- data_tmp$produktID

  if (validate_data) {
    check_avail   <- check_tmplist %in% names_validate
    if (!(all(check_avail))) {
      id_non_avail <- which(!check_avail)
      cat(crayon::green("These product-IDs (for zoner/kampanj/skylttyp) are ",
                        "defined in kampanjplanering but not in data for \n"),
          crayon::yellow("week ", sheet_name, " ...\n"))
      cat(crayon::red(paste0(check_tmplist[id_non_avail], "\n")))
      cat(crayon::bgMagenta("Removing these entries... \n"))
    }
  }
  if (validate_kampanj) {
    id_non_avail2 <- names_validate[!(names_validate %in% c("butik",
                                                            "vecka",
                                                            "datum",
                                                            "timme",
                                                            check_tmplist))]
  }
  id_non_avail <- which(!(check_tmplist %in% names_validate))
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
  return(list(out_tmplist, id_non_avail2))
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
#' @param validate_data logical; if \code{TRUE} checks if there are product IDs
#'   in \code{kampanjplanering} (data on planned advertising/campaigns) but not
#'   in the sales data (sales of advertised products)
#' @param validate_kampanj logical; if \code{TRUE} checks if there are product
#'   IDs in the sales data (sales of advertised products) but not in
#'   \code{kampanjplanering} (data on planned advertising/campaigns)
#'
#' @return a list of lists, with element of the latter being output from
#'   [get_list_test_week()].
#' @export
get_meta_data <- function(pth, list_data_validate,
                          validate_data,
                          validate_kampanj) {
  num_vecka   <- length(list_data_validate)
  names_vecka <- names(xlsx::getSheets(xlsx::loadWorkbook(pth)))
  names_vecka <-names_vecka[names_vecka %in% names(list_data_validate)]

  product_ids_avail_list <- vector("list", num_vecka)
  for (i in 1:num_vecka) {
    product_ids_avail_list[[i]] <- names(list_data_validate[[i]])
  }
  out_list_test_weeks  <- vector("list", num_vecka)
  out_validate_kampanj <- vector("list", num_vecka)
  for (i in seq_len(num_vecka)) {
    tmp_out <- get_list_test_week(pth,
                                  names_vecka[i],
                                  product_ids_avail_list[[i]],
                                  validate_data,
                                  validate_kampanj)
    out_list_test_weeks[[i]] <- tmp_out[[1]]
    if (validate_kampanj) out_validate_kampanj[[i]] <- tmp_out[[2]]
  }
  if (validate_kampanj) {
    cat(crayon::green("These product-IDs (for zoner/kampanj/skylttyp) are ",
                      "defined in data ... \n"))
    cat(crayon::green("but not in kampanjplanering ..\n"))
    cat(crayon::yellow(Reduce(intersect, out_validate_kampanj)))
  }
  names(out_list_test_weeks) <-names_vecka
  return(out_list_test_weeks)
}
#' Generates merged data set on sales.
#'
#' Uses first argument to get information on sales (count) of relevant products
#' and second data set on counts of all products sold, and both to compute
#' conversion rates (count/total) of relevant products. The third arguments
#' identifies the different product sets of "relevant products" depending on
#' zoner, kampanj or skylttyp.
#'
#' @param data_sales1 data on sales of relevant products (per butik, vecka,
#'    datum, and timme)
#' @param data_sales2 data on sales of all products (per butik, vecka, datum,
#'    and timme)
#' @param list_test_weeks output from [get_meta_data()]
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
                                             "zoner_count_TOTAL",
                                             "zoner_convrate_TOTAL"),
                                           dplyr::contains("zoner"),
                                           c("kampanj_count_TOTAL",
                                             "kampanj_convrate_TOTAL"),
                                           dplyr::contains("kampanj"),
                                           c("skylttyp_count_TOTAL",
                                             "skylttyp_convrate_TOTAL"),
                                           dplyr::contains("skylttyp"))
  } else if (time_frq == "weekly") {
    data_out <- data_out %>% dplyr::arrange(dplyr::across(c("butik",
                                                            "vecka"),
                                                          dplyr::desc)) %>%
      dplyr::ungroup()
    data_out <- data_out %>% dplyr::select(c("butik",
                                             "vecka",
                                             "antal_kvitton",
                                             "zoner_count_TOTAL",
                                             "zoner_convrate_TOTAL"),
                                           dplyr::contains("zoner"),
                                           c("kampanj_count_TOTAL",
                                             "kampanj_convrate_TOTAL"),
                                           dplyr::contains("kampanj"),
                                           c("skylttyp_count_TOTAL",
                                             "skylttyp_convrate_TOTAL"),
                                           dplyr::contains("skylttyp"))
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
    tmp_name1    <- paste0("zoner_count_", names_zoner[j])
    tmp_name2    <- paste0("zoner_convrate_", names_zoner[j])
    data_set <- data_set %>%
      dplyr::mutate({{ tmp_name1 }} := rowSums(dplyr::across(tidyselect::all_of(tmp_prod_ids)),
                                               na.rm = TRUE)) %>%
      dplyr::mutate({{ tmp_name2 }} := .data[[tmp_name1]] / .data$antal_kvitton)
  }
  data_set <- data_set %>%
    dplyr::mutate(zoner_count_TOTAL = rowSums(dplyr::across(dplyr::contains("zoner_count_"),
                                                            na.rm = TRUE))) %>%
    dplyr::mutate(zoner_convrate_TOTAL = .data$zoner_count_TOTAL / .data$antal_kvitton)
  data_set
}
add_skylttyp_vars <- function(data_set, tmp_list) {
  names_skylttyp <- names(tmp_list[["skylttyp"]])
  num_skylttyp   <- length(names_skylttyp)
  for(j in 1:num_skylttyp) {
    tmp_prod_ids <- as.character(tmp_list[["skylttyp"]][[j]])
    tmp_name1    <- paste0("skylttyp_count_", names_skylttyp[j])
    tmp_name2    <- paste0("skylttyp_convrate_", names_skylttyp[j])
    data_set <- data_set %>%
      dplyr::mutate({{ tmp_name1 }} := rowSums(dplyr::across(tidyselect::all_of(tmp_prod_ids)),
                                               na.rm = TRUE)) %>%
      dplyr::mutate({{ tmp_name2 }} := .data[[tmp_name1]] / .data$antal_kvitton)
  }
  data_set <- data_set %>%
    dplyr::mutate(skylttyp_count_TOTAL = rowSums(dplyr::across(dplyr::contains("skylttyp_count_"),
                                                               na.rm = TRUE))) %>%
    dplyr::mutate(skylttyp_convrate_TOTAL = .data$skylttyp_count_TOTAL / .data$antal_kvitton)
  data_set
}
add_kampanj_vars <- function(data_set,
                             tmp_list) {
  names_kampanj <- names(tmp_list[["kampanj"]])
  num_kampanj   <- length(names_kampanj)
  for(j in 1:num_kampanj) {
    tmp_prod_ids <- as.character(tmp_list[["kampanj"]][[j]])
    tmp_name1    <- paste0("kampanj_count_", names_kampanj[j])
    tmp_name2    <- paste0("kampanj_convrate_", names_kampanj[j])
    data_set <- data_set %>%
      dplyr::mutate({{ tmp_name1 }} := rowSums(dplyr::across(tidyselect::all_of(tmp_prod_ids)),
                                               na.rm = TRUE)) %>%
      dplyr::mutate({{ tmp_name2 }} := .data[[tmp_name1]] / .data$antal_kvitton)
  }
  data_set <- data_set %>%
    dplyr::mutate(kampanj_count_TOTAL = rowSums(dplyr::across(dplyr::contains("kampanj_count_"),
                                                              na.rm = TRUE))) %>%
    dplyr::mutate(kampanj_convrate_TOTAL = .data$kampanj_count_TOTAL / .data$antal_kvitton)
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
