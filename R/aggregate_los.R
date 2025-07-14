#' @title Aggregate duration of record level events
#'
#' @description Aggregate duration of record level events
#'
#' @param start_date Date. Event start date
#' @param end_date Date. Event end date
#' @param los_range NULL|Date. A date range of the period to cover in the aggregation.
#' If \code{NULL} (default), the rane of \code{start_date} and \code{start_date} is used.
#' @param level Character. Desired level. Passed to 'by' in \code{\link{seq}}.
#' Options include all accepted in \code{\link[=seq.POSIXt]{seq.POSIXt(..., by = XX)}}
#' @param strata Atomic. Population strata
#' @param silent Logical. Print (or not) current iteration of loop.
#' @param count_id Atomic. If provided the unique count of \code{count_id}
#' per iteration (batch) is included
#' @param event_id Atomic. If provided the unique count of \code{event_id}
#' per iteration (batch) is included
#' @param fill Logical. If TRUE, place holders are included if
#' there is no occurrence of the event within the batch of the loop.
#' @param units Character. Desired unit for LOS. Passed to \code{\link{difftime}}.
#' Options include all accepted in \code{\link{difftime}}s
#'
#' @return A data frame with the following columns
#' \itemize{
#' \item \code{strata} - See \code{strata} argument.
#' \item \code{days_origin_0} - Total length of stay per \code{strata} when the first day in each period was taken day 0.
#' \item \code{days_origin_1} - Total length of stay per \code{strata} when the first day in each period was taken day 1.
#' \item \code{unique.count_id} - Unique count of \code{count.id} per per \code{strata}.
#' \item \code{unique.event_id} - Unique count of \code{event_id} per per \code{strata}.
#' \item \code{batch} - Batch of length of stay per \code{strata} and \code{level}
#' \item \code{start_date} - Start date of \code{batch}
#' \item \code{end_date} - End date of \code{batch}
#' }
#' @examples
#' # Example of dated events
#' dfr <- data.frame(
#'   start_date = as.Date(c('2024-05-28', '2024-05-05', '2024-06-01')),
#'   end_date = as.Date(c('2024-06-05', '2024-05-11', '2024-06-20')),
#'   strata = c('A', 'A', 'B'))
#'
#' # Length of stay across the entire period
#'  dfr$duration <- difftime(dfr$end_date, dfr$start_date, 'days')
#'  dfr
#'
#'  # Aggregate to a annual level
#'  aggregate_los(
#'    start_date = dfr$start_date,
#'    end_date = dfr$end_date,
#'    level = 'year',
#'    strata = dfr$strata
#'  )
#'
#'  # Aggregate to a monthly level
#'  aggregate_los(
#'    start_date = dfr$start_date,
#'    end_date = dfr$end_date,
#'    level = 'month',
#'    strata = dfr$strata
#'  )
#'
#'  # Cover a range of wider of dates. Use `fill` for empty rows
#'  aggregate_los(
#'    start_date = dfr$start_date,
#'    end_date = dfr$end_date,
#'    level = 'month',
#'    strata = dfr$strata,
#'    los_range =  c('2024-01-01', '2024-12-01')
#'  )
#'
#'  aggregate_los(
#'    start_date = dfr$start_date,
#'    end_date = dfr$end_date,
#'    level = 'month',
#'    strata = dfr$strata,
#'    los_range =  c('2024-01-01', '2024-12-01'),
#'    fill = TRUE
#'  )
#'
#'  # Return LOS is different units of time
#'  aggregate_los(
#'    start_date = dfr$start_date,
#'    end_date = dfr$end_date,
#'    level = 'month',
#'    strata = dfr$strata,
#'    los_range =  c('2024-01-01', '2024-12-01'),
#'    units = 'weeks'
#'  )
#'
#'
#'  # Return LOS is different units of time
#'  aggregate_los(
#'    start_date = dfr$start_date,
#'    end_date = dfr$end_date,
#'    level = '2 years',
#'    strata = dfr$strata,
#'    los_range =  c('2020-01-01', '2025-12-01'),
#'    units = 'weeks',
#'    fill = TRUE
#'  )
#'
#' @export
aggregate_los <- function(
    start_date, end_date, level = diff(range(start_date, end_date)),
    strata = NULL, count_id = NULL, event_id = NULL,
    fill = FALSE, silent = TRUE, units = 'days', los_range = NULL){

  dfr <- list(
    start_date = start_date,
    end_date = end_date,
    count_id = count_id,
    event_id = event_id,
    strata = strata
  )

  dfr$los_period <- difftime(dfr$start_date, dfr$end_date, units = units)

  dt_range <- c(dfr$start_date, dfr$end_date)
  if(!is.null(los_range)){
    dt_range <- c(dt_range, los_range)
  }
  ll_dt_a <- range(dt_range)

  if(!units %in% c("days", "weeks")){
    ll_dt_a <- as.POSIXct(ll_dt_a)
  }

  bd_a <- seq(from = ll_dt_a[1], to = ll_dt_a[2], by = level)
  bd_a <- unique(c(bd_a, ll_dt_a[2]))
  bd_z <- bys_lead(bd_a)

  if(
    (grepl('^day|^1 day|^1$', as.character(level)) & any(grepl('^day', units))) |
    (grepl('^sec|^1 sec|^1$', as.character(level)) & any(grepl('^sec', units)))
    ){
    indx <- 1:length(bd_a)-1
    bd_z[indx] <- bd_z[indx]-1

    indx <- length(bd_a)
    bd_z[indx] <- bd_a[indx]

  }else{
    indx <- length(bd_a)-1
    bd_a <- bd_a[1:indx]
    bd_z <- bd_z[1:indx]

    indx <- 1:length(bd_a)-1
    bd_z[indx] <- bd_z[indx]-1
  }

  st <- unique(dfr$strata)
  st_len <- length(st)
  bd_tot <- length(bd_a)
  los_lst <- list()
  for(i in seq_len(bd_tot)){
    if(!silent){
      print(paste0('Batch ', i, " of ", bd_tot))
    }

    dfr4 <- dfr
    lgk <- which(
      !(bd_z[i] < dfr4$start_date | bd_a[i] > dfr4$end_date)
    )
    dfr4 <- lapply(dfr4, function(x) x[lgk])

    if(length(dfr4[[1]]) == 0){
      if(fill){
        los_lst[[i]] <- list(
          strata = st, los_origin_0 = rep(0, st_len),
          los_origin_1 = rep(0, st_len),
          unique.event_id = rep(0, st_len), unique.count_id = rep(0, st_len),
          batch = rep(i, st_len), start_date = rep(bd_a[i], st_len),
          end_date = rep(bd_z[i], st_len))
      }
      next
    }

    # flag for spells that cut across months
    lgk_l <- dfr4$start_date < bd_a[i]
    dfr4$start_date[lgk_l] <- bd_a[i]
    lgk_r <- dfr4$end_date > bd_z[i]
    dfr4$end_date[lgk_r] <- bd_z[i]
    dfr4$tmp.los_a <-
      difftime(dfr4$end_date, dfr4$start_date, units = units)
    dfr4$tmp.los_b <-
      difftime(dfr4$end_date + lubridate::ddays(1), dfr4$start_date, units = units)

    # Adding 1-day diff (overnight stay) that's lost due to split from a previous month
    dfr4$tmp.los_a[lgk_l] <- dfr4$tmp.los_a[lgk_l] + 1
    dfr4$unique.count_id <- !duplicated(dfr4$count_id)
    dfr4$unique.event_id <- !duplicated(dfr4$event_id)

    if(length(dfr4$strata) == 0){
      dfr4$strata <- dfr4$cmb <- rep(NA, length(dfr4$start_date))
    }else{
      dfr4$cmb <- combi(dfr4$strata)
    }

    dfr4$los_origin_0 <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$tmp.los_a))
    dfr4$los_origin_1 <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$tmp.los_b), na.rm = FALSE)
    dfr4$unique.event_id <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$unique.event_id), na.rm = TRUE)
    dfr4$unique.count_id <- bys_sum(by = dfr4$cmb, as.numeric(dfr4$unique.count_id), na.rm = TRUE)

    lgk <- which(!duplicated(dfr4$cmb))
    dfr4 <- lapply(dfr4[c("strata", "los_origin_0", "los_origin_1",
                          "unique.count_id", 'unique.event_id')],
                   function (x) x[lgk])

    dfr4$batch <- rep(i, length(dfr4$strata))

    dfr4$start_date <- bd_a[i]
    dfr4$end_date <- bd_z[i]

    los_lst[[i]] <- as.data.frame(dfr4)
    rm(dfr4)
  }

  los_lst <- do.call("rbind", los_lst)
  los_lst <- los_lst[order(los_lst$strata, los_lst$batch),]

  los_lst$unique.count_id[is.na(los_lst$unique.count_id)] <- 0
  los_lst$unique.event_id[is.na(los_lst$unique.event_id)] <- 0

  return(los_lst)
}

