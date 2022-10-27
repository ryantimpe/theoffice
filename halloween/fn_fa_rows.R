gt_fa_rows <- function(gt_object, columns, 
                        palette = list("check" = "black", 
                                        "check-double" = "red"),                        ...,
                        align = "center") {
  stopifnot("Table must be of class 'gt_tbl'" = "gt_tbl" %in% class(gt_object))
  
  text_transform(
    gt_object,
    locations = cells_body(columns = {{ columns }}),
    
    fn = function(x) {

      lapply(X = x, FUN = function(xy) {
        # handle missing values
        if(gtExtras:::is_blank(xy) || is.na(xy) || xy == "NA"){
          return(gt::html("&nbsp;"))
        }
        
          this_fa <- fontawesome::fa(xy, ..., 
                                        fill = "black", #palette[[x]], 
                                     height = "20px", a11y = "sem") %>%
            as.character() %>%
            gt::html()
        
        
        label <- xy
        
        htmltools::div(
          title = label, "aria-label" = label, role = "img",
          list(this_fa)
        )
      })
    }
  ) %>%
    cols_align(align = align, columns = {{ columns }})
}