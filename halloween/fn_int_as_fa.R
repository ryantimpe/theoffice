gt_int_as_fa <- function(gt_object, column, 
                        name = c("check", "check-double"), 
                        palette = c("black", "red"),
                        threshold = 1,
                        ...,
                        align = "center") {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))
  
  text_transform(
    gt_object,
    locations = cells_body(columns = {{ column }}),
    
    fn = function(x) {
      int_conv <- suppressWarnings(as.integer(x))
      
      lapply(X = int_conv, FUN = function(xy) {
        # handle missing values
        if(gtExtras:::is_blank(xy) || is.na(xy)){
          return(gt::html("&nbsp;"))
        }
        
        this_index = if(xy <= threshold){1}else{2}
        
          fa_repeats <- fontawesome::fa(name[this_index], ..., 
                                        fill = palette[this_index], height = "20px", a11y = "sem") %>%
            as.character() %>%
            gt::html()

        label <- paste(xy, name[this_index])
        
        htmltools::div(
          title = label, "aria-label" = label, role = "img",
          list(fa_repeats)
        )
      })
    }
  ) %>%
    cols_align(align = align, columns = {{ column }})
}