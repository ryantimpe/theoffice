gt_fa_rating2 <- function(gt_object, column, max_rating = 5, ...,
                         color = "orange",
                         icon = "star") {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))
  
  text_transform(
    gt_object,
    locations = cells_body(columns = {{ column }}),
    fn = function(x) {
      
      # convert the raw text to numeric
      num_x <- suppressWarnings(as.numeric(x))
      
      lapply(X = num_x, FUN = function(rating) {
        
        # handle missing values & return a blank space if missing
        if(gtExtras:::is_blank(rating) || rating %in% c(NA, "NA", "")){
          return(gt::html("&nbsp;"))
        }
        # adapted from: glin.github.io/reactable/articles/cookbook/cookbook.html#rating-stars
        rounded_rating <- floor(rating + 0.5) # always round up
        stars <- lapply(seq_len(max_rating), function(i) {
          if (i <= rounded_rating) {
            fontawesome::fa(icon, fill = color, height = "20px", a11y = "sem")
          } else {
            fontawesome::fa(icon, fill = "grey", height = "20px", a11y = "sem",
                            fill_opacity = 0)
          }
        })
        label <- sprintf("%s out of %s", rating, max_rating)
        div_out <- htmltools::div(title = label, "aria-label" = label, role = "img", stars, style = "padding:0px")
        
        # need to convert from text to html
        as.character(div_out) %>%
          gt::html()
      })
    }
  ) %>%
    cols_align(align = "left", columns = {{ column }})
}