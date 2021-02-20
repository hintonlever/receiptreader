install.packages("tesseract")
install.packages("here")
install.packages("magick")

# OCR recognition
library(tesseract)

# Use here to make it folders easy
library(here)

library(magick)

here::here()

eng <- tesseract("eng")


img1 <- load_image(here("examples","woolworths_5.jpg"))
image_draw(img1)
# text <- ocr(img1, engine = eng)
# text_detail <- ocr_data(img1, engine = eng)
# cat(text)


img_preprocessed <- img1 %>%
  # image_resize("50x") %>%
  image_convert(type = 'Grayscale') %>%
  image_deskew()
  # image_trim(fuzz = 40)


text <- img_preprocessed %>% find_all_text()
text <- text %>% get_clean_bbox
text <- desribe_all_lines_through_box(text)
text <- apply_cluster_analysis(text)


clustered_bbox <- text %>%
  group_by(cluster) %>%
  summarise(
    bbox_left = min(bbox_left),
    bbox_bottom = min(bbox_bottom),
    bbox_right = max(bbox_right),
    bbox_top = max(bbox_top)
    )
# numbers <- img_preprocessed %>% find_numbers()



output_img <- image_draw(img_preprocessed,res = 200)
add_all_rect(text)
add_all_rect(clustered_bbox,color = 'purple')

dev.off()




