gt_fa_gauge <- function(gt_object, column, 
                        name = c("check", "check-double"), 
                        threshold = 1,
                        palette = c("black", "red"),
                        ...,
                        align = "left",
                          direction = 1) {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))
  
  text_transform(
    gt_object,
    locations = cells_body(columns = {{ column }}),
    
    fn = function(x) {
      int_conv <- suppressWarnings(as.integer(x))
      int_x <- int_conv[!is.na(int_conv)]
      
      
      lapply(X = int_conv, FUN = function(xy) {
        # handle missing values
        if(gtExtras:::is_blank(xy) || is.na(xy)){
          return(gt::html("&nbsp;"))
        }
        
        
        fct_lvl <- suppressWarnings(unique(x[!is.na(as.integer(x))]))
        
        if(xy <= threshold){
          fa_repeats <- fontawesome::fa(name[1], ..., fill = palette[1], height = "20px", a11y = "sem") %>%
            as.character() %>%
            # rep(., xy) %>%
            gt::html()
          
          nn <- name[1]
        } else {
          fa_repeats <- fontawesome::fa(name[2], ..., fill = palette[2], height = "20px", a11y = "sem") %>%
            as.character() %>%
            # rep(., xy) %>%
            gt::html()
          
          nn <- name[2]
        }
        
        
        label <- paste(xy, nn)
        
        htmltools::div(
          title = label, "aria-label" = label, role = "img",
          list(fa_repeats)
        )
      })
    }
  ) %>%
    cols_align(align = align, columns = {{ column }})
}