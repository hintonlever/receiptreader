# https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html

library(tesseract)

get_clean_bbox <- function(ocr_data_df) {
  # bbox = left, bottom, right, top
  tmp <- strsplit(ocr_data_df$bbox,",")
  tmp2 <- lapply(tmp,as.numeric)

  ocr_data_df$bbox_left    <- unlist(lapply(tmp2,function(x) {x[1]}))
  ocr_data_df$bbox_bottom  <- unlist(lapply(tmp2,function(x) {x[2]}))
  ocr_data_df$bbox_right   <- unlist(lapply(tmp2,function(x) {x[3]}))
  ocr_data_df$bbox_top     <- unlist(lapply(tmp2,function(x) {x[4]}))
  return(ocr_data_df)
}



find_numbers <- function(img) {
  numbers <- tesseract(options = list(tessedit_char_whitelist = ".0123456789"))

  return(ocr_data(img,engine = numbers))
}


find_all_text <- function(img) {
  # text <- tesseract(options = list(tessedit_char_whitelist = ".0123456789"))

  return(ocr_data(img))
}



# get_clean_bbox(text)
