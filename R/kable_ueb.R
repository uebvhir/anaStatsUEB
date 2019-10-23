#' A kable_ueb Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param tab table to kable
#' @param caption Character vector containing the table's caption or title. Set to NULL to suppress the caption. Default value is NULL.
#' @param font_size A numeric input for table font size
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table should have 100% width. Since HTML and pdf have different flavors on the preferable format for full_width. If not specified, a HTML table will have full width by default but this option will be set to FALSE for a LaTeX table
#' @export kable_ueb
#' @keywords kable html ueb
#' @import kableExtra
#' @examples
#' aa <- factor(rep(c("A","B","C"), 10))
#' kable_ueb(table(aa))
#' kable_ueb(table(aa,aa))



kable_ueb <- function(tab,
                      caption = NULL,
                      font_size = 13,
                      full_width = FALSE,
                      escape = FALSE, ...){
  kable(tab, caption = caption, escape = escape, ...) %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = font_size, full_width = full_width,...) %>%
    row_spec(0,background = "#993489", color = "white")
}
