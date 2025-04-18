#' A sumsurv_df Function
#'
#' data frame of summary.survfit object
#' @param sumsurv summary.survfit object
#' @keywords summary survfit survival
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @export sumsurv_df
#'



sumsurv_df <- function(sumsurv){
  colsel <- c("strata", "time","n.risk","n.event", "surv", "std.err", "lower","upper" )
  cols <- lapply(colsel , function(x) sumsurv[x])
  tbl <- do.call(data.frame, cols)
  return(tbl)
}



