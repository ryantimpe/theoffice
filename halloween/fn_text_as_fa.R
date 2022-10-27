gt_text_as_fa <- function(gt_object, column, 
                         reference_table,
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
        
        this_icon = reference_table %>% filter(costume_category == xy) %>% pull(fa_icon)
        this_fill = reference_table %>% filter(costume_category == xy) %>% pull(color)
        
        this_fa <- fontawesome::fa(this_icon, ..., 
                                      fill = this_fill, 
                                      height = "20px", a11y = "sem") %>%
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