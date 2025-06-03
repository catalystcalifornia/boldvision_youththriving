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
legend_text_fs <- 14
legend_title_fs <- 12
caption_fs <- 11
title_fs <- 18
subtitle_fs <- 15


#COLOR SPECTRUMS BASED ON SECTIONS OF REPORT
pink_gradient <- c("#FDE1F3", "#FCAEDC", "#F979CA", "#F757BB", "#F75EC1") #use for Positive Mental Health 
dark_pink_gradient <- c("#FDD8DD", "#F9A3AF", "#F66F85", "#F14968", "#EF4A66") #use for Positive Identity and Hope  
green_gradient <- c("#C4F0DC", "#7CDDBA", "#33C898", "#00A769", "#00864A") #use for Supportive Social Connections 
blue_gradient <- c("#D9D5FA", "#A7A0F4", "#756AEA", "#4435DE", "#2A12B2") #use for Equity, Opportunity, and Inclusion


