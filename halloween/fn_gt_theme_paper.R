#' Apply dot matrix theme to a gt table
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param ... Additional arguments passed to `gt::tab_options()`
#' @param color A string indicating the color of the row striping, defaults to a light green. Accepts either named colors or hex colors.
#' @return An object of class `gt_tbl`.
#' @export
#' @section Examples:
#' ```r
#' library(gt)
#' themed_tab <- head(mtcars) %>%
#'   gt() %>%
#'   gt_theme_dot_matrix() %>%
#'   tab_header(title = "Styled like dot matrix printer paper")
#' ```
#' @section Figures:
#' \if{html}{\figure{gt_dot_matrix.png}{options: width=100\%}}
#'
#' @family Themes

gt_theme_paper <- function(gt_object, ...){
  
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  
  gt_object %>%
    opt_table_font(font = "Courier") %>%
    tab_options(
      ...,
      table.background.color = "#F5F5F5",
      heading.align = "left",
      heading.border.bottom.style = "none",
      heading.background.color = "#FFFFFF",
      # heading.border.bottom.color = "none",
      column_labels.text_transform = "lowercase",
      column_labels.font.weight = "bold",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.color = "#96ADE9CC",
      column_labels.border.bottom.width = px(2),
      table.border.bottom.style = "none",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "white",
      table.border.top.style = "none",
      table_body.hlines.style = "solid",
      table_body.hlines.color = "#96ADE9CC",
      table_body.vlines.style = "none",
      data_row.padding = px(1),
      row_group.font.weight = "normal",
      row_group.font.size = pct(90),
      row_group.text_transform = "lowercase",
      row_group.border.bottom.color = "#96ADE9CC",
      row_group.border.top.color = "#FFFFFF",
      footnotes.font.size = pct(80),
      footnotes.background.color = "#FFFFFF",
      summary_row.text_transform = "uppercase"
    ) %>%
    opt_css(
      "
      td.gt_row:first-child{
        border-right: 2px solid #DE1F1F99 !important;
      }
      
      th.gt_col_heading:first-child {
        border-right: 2px solid #DE1F1F99 !important;
      }
      
      tbody tr:last-child {
        border-bottom: 2px solid #ffffff00;
      }
      "
      , add = TRUE
      # , allow_duplicates = TRUE
    )
}