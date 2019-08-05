#' A kable_ueb Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param tab table to kable
#' @param font_size A numeric input for table font size
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have 100% width. Since HTML and pdf have different flavors on the preferable format for full_width. If not specified, a HTML table will have full width by default but this option will be set to FALSE for a LaTeX table
#' @keywords kable html ueb
#' @export kable_ueb
#' @import kableExtra
#' @examples
#' aa <- factor(rep(c("A","B","C"), 10))
#' kable_ueb(table(aa))
#' kable_ueb(table(aa,aa))



kable_ueb <- function(tab,
                      font_size = 13,
                      full_width = FALSE, ...){
  kable(tab) %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = font_size, full_width = full_width,...) %>%
    row_spec(0,background = "#993489", color = "white")
}
