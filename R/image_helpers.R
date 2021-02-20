# https://cran.r-project.org/web/packages/magick/vignettes/intro.html
# https://cran.r-project.org/web/packages/magick/magick.pdf

library(magick)

# library(optim)

load_image <- function(filepath) {
  # img is class of: magick-image
  img <- image_read(filepath)

  print(image_info(img))
  return(img)
}

get_color_for_confidence <- function(confidence) {
  # assumes confidence is on a 0 to 100 scale

  rgb(red = (1 - confidence / 100.0) * 255,green = (confidence / 100.0) * 255,blue = 0, maxColorValue = 255)
}


add_rect_for_one_bbox <- function(bbox_string,border = "red", lty = "solid", lwd = 1) {
  # bbox = left, bottom, right, top
  rect(bbox_string[1],bbox_string[2], bbox_string[3], bbox_string[4],border = border, lty = lty, lwd = lwd )

}



add_all_rect <- function(ocr_data_df , color = 'green') {
  num_rows <- nrow(ocr_data_df)
  # colors <- lapply(ocr_data_df$confidence,get_color_for_confidence)



  for ( i in 1:num_rows) {
    # print(colors[[i]])
    bbox <- c(
      ocr_data_df$bbox_left[i],
      ocr_data_df$bbox_bottom[i],
      ocr_data_df$bbox_right[i],
      ocr_data_df$bbox_top[i]
    )
    add_rect_for_one_bbox(bbox, border = color)
  }
}




#
#
# cost_function <- function(clean_bbox_list,num_of_lines = 1) {
#
#
# }
#
#
#
# fr <- function(x) {   ## Rosenbrock Banana function
#   x1 <- x[1]
#   x2 <- x[2]
#   100 * (x2 - x1 * x1)^2 + (1 - x1)^2
# }
# grr <- function(x) { ## Gradient of 'fr'
#   x1 <- x[1]
#   x2 <- x[2]
#   c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
#     200 *      (x2 - x1 * x1))
# }
#
#
# my_fun <- function(x) {
#   x^2 + 10
# }
#
# opt_point <- optimise(my_fun,lower = -20, 200)
#
# plot(seq(-1.2,1,0.001),my_fun(seq(-1.2,1,0.001)),type = 'l')
# points(opt_point$minimum,opt_point$objective,col = 'red')
#
#
#
# clean_bbox_list[[1]]
#
# bbox_1
