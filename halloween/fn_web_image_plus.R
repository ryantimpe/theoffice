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