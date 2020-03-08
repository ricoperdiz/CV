#' Function to produce very basic table, no lines or headings
#'
#' Copied from [Rob J Hyndman Github account](https://github.com/robjhyndman/CV/blob/master/baretable.R).
#' 
#' @author Rob J Hyndman
#' @param tbl 
#' @param digits 
#' @param include.colnames 
#' @param include.rownames 
#' @param hline.after 
#' @param size 
#' @param add.to.row 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
baretable <- function(tbl, digits = 0,
                      include.colnames=FALSE, include.rownames=FALSE,
                      hline.after=NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row =  getOption("xtable.add.to.row", NULL),
                      ...) {
  tbl %>%
    xtable::xtable(digits = digits, ...) %>%
    print(
      include.colnames = include.colnames,
      include.rownames = include.rownames,
      hline.after = hline.after,
      comment = FALSE,
      latex.environments = NULL,
      floating = FALSE,
      size=size,
      add.to.row=add.to.row,
      sanitize.text.function = function(x) {
        x
      }
    )
}
