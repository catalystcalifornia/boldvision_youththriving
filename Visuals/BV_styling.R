#The purpose of this script is to centralized styling for ggplot functions in one place.

#The following aim to follow the Bold Vision Style Guide stored here: "W:/Project/OSI/Bold Vision/BV 2023/BV Style"
library(ggplot2)

##Colors
gray <- "#D6D7D6"
pink <- "#F75EC1"
dark_pink <- "#EF4A66"
orange <- "#F57E20"
yellow <- "#FFBF00"
light_green <- "#00A75A"
dark_green <- "#00864A"
blue  <- "#2A12B2"
light_blue <- "#465adc"


## FONTS 
font_add(family = "Manifold Regular", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Manifold/Fonts/manifoldcf-regular.otf")
font_add(family = "Manifold CF", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Manifold/Fonts/manifoldcf-heavy.otf")
font_add(family = "HelveticaNeueLTStdMdCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCn.otf")
font_add(family = "HelveticaNeueLTStdHvCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-HvCn.otf")
font_add(family = "HelveticaNeueLTStdMdCnO", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCnO.otf")
font_add(family = "HelveticaNeueLTStdMd", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-Md.otf")

# font_import()
loadfonts(device = "win")
windowsFonts()
showtext_auto()
# define fonts in chart
font_title <- "HelveticaNeueLTStdHvCn"
font_subtitle <- "HelveticaNeueLTStdMdCn"
font_caption <- "Manifold Regular"
font_bar_label <- "Manifold Regular"
font_axis_label <- "Manifold Regular"


## FONT SIZES 
bar_label_fs <- 2.6
lengend_text_fs <- 14
legend_title_fs <- 12
caption_fs <- 11
title_fs <- 18
subtitle_fs <- 15


