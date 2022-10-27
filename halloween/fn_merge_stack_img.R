web_image_plus <- function(
    url,
    height = 30, 
    more_css = ""
) {
  
  if (is.numeric(height)) {
    height <- paste0(height, "px")
  }
  
  paste0("<img src=\"", url, "\" style=\"height:", height, ";", 
         more_css,
         "\">")
}

gt_merge_stack_image <- function(gt_object, col1, col2, 
                           img_source = "web", 
                           img_height = 30, img_css = "",
                           palette = "black", ..., small_cap = TRUE,
                           font_size = "14px", font_weight = "normal") {
  
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  stopifnot("'font_size' must be a string with 'px'" = all(grepl(x = font_size, pattern = "px")))
  stopifnot("'font_weight' must be a 'bold', 'normal' or 'lighter'" = font_weight %in% c("bold", "normal", "lighter"))
  
  # translate colors to hcl. Allows R color names like "grey30".
  colors <- scales::col2hcl(palette, ...)
  
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == "stub")]
  
  # segment data with bare string column name
  # col2_bare <- rlang::enexpr(col2) %>% rlang::as_string()
  # data_in <- gt_object[["_data"]][[col2_bare]]
  data_in <- gt_index(gt_object, column = {{ col2 }})
  
  gt_object %>%
    text_transform(
      locations = if (isTRUE(row_name_var == col1_bare)) {
        cells_stub(rows = gt::everything())
      } else {
        cells_body(columns = {{ col1 }})
      },
      fn = function(x) {
        if (small_cap) {
          font_variant <- "small-caps"
        } else {
          font_variant <- "normal"
        }
        
        if(img_source == "web"){
          img_div <- web_image_plus(url = x, height = img_height,
                                    more_css = img_css)
        } else if(img_source == "local") {
          img_div <- local_image(filename = x, height = img_height)
        }

        glue::glue(
          "<div>{img_div}</div>
        <div style='line-height:{font_size[1]}'><span style ='font-weight:{font_weight[1]};color:{colors[1]};font-size:{font_size[1]}'>{data_in}</span></div>"
        )
      }
    ) %>%
    cols_hide(columns = {{ col2 }})
}
