gt_text_as_fa <- function(gt_object, column, 
                         icon_table,
                         category_table,
                         ...,
                         align = "center") {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))
  
  text_transform(
    gt_object,
    locations = cells_body(columns = {{ column }}),
    
    fn = function(x) {
      
      lapply(X = x, FUN = function(xy) {
        # handle missing values
        if(gtExtras:::is_blank(xy) || is.na(xy)){
          return(gt::html("&nbsp;"))
        }

        this_category = category_table %>% filter(costume_detail == xy) %>% pull(costume_category)
      
        this_icon = icon_table %>% filter(costume_category == this_category) %>% pull(fa_icon)
        this_fill = icon_table %>% filter(costume_category == this_category) %>% pull(color)
        
        this_fa <- fontawesome::fa(this_icon, ..., 
                                      fill = this_fill, 
                                      height = "20px", a11y = "sem",
                                   title = xy) %>%
          as.character() %>%
          gt::html()
        
        label <- paste(xy, this_icon)
        
        htmltools::div(
          title = xy, "aria-label" = xy, role = "img",
          list(this_fa)
        )
      })
    }
  ) %>%
    cols_align(align = align, columns = {{ column }})
}