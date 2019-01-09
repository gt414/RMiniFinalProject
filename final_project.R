# Insttalling Pkg
##packageNames <- c("RCurl", "ggplot2", "gcookbook", "dplyr")
##install.packages(packageNames)

library(RCurl)
library(ggplot2) 
library(gcookbook) 
library(dplyr)

# 設定編碼為 UTF-8
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
# 用 RCurl 下載資料
URL <- "http://www.rjsd.moj.gov.tw/rjsdweb/OpenData.ashx?code=G01123"
downloaded <- getURL(URL, .encoding = "UTF-8")
# 轉成 dataframe
data <- read.csv (text = downloaded)
names(data) <- c("drug_type", "edu_level", "gender", "date", "total")
data <- data[data$date>100 &data$date<107,]
# 整理資料
edu = data[,c(2,4,5)]
edu = edu %>%
  group_by(date, edu_level) %>%
  summarise(total = sum(total)) %>%
  filter(edu_level != "教育程度合計" & edu_level != "自修" & edu_level != "不詳(含法人)" & edu_level != "不識字") %>% 
  mutate(edu_level = factor(edu_level, levels = c("大專以上", "高中(職)", "國中", "國小")))
# 畫圖
ggplot(edu, aes(x = date, y = total, colour = edu_level, label=total)) + 
  theme_bw(base_family="STHeiti") +
  geom_line() + 
  ylim(50, 12000) +
  geom_point() + 
  geom_text(aes(label=total),hjust=0.5, vjust=-0.5, show.legend = FALSE) +
  labs(colour = "教育程度") + 
  labs(x = "年份") + 
  labs(y = "人數") +
  labs(title = "101 - 106 監獄新入監毒品罪人數 - 教育程度")
# 存圖
ggsave("by_education_level.jpg")

# 整理資料
gender = data[,c(3,4,5)]
gender = gender %>%
  group_by(date, gender) %>%
  summarise(total = sum(total)) %>%
  mutate(gender = factor(gender, levels = c("性別合計", "男性", "女性")))
# 畫圖
ggplot(gender, aes(x = date, y = total, colour = gender)) + 
  theme_bw(base_family="STHeiti") +
  geom_line() + 
  ylim(2000, 24000) +
  geom_point() + 
  geom_text(aes(label=total),hjust=0.5, vjust=-0.5, show.legend = FALSE) +
  labs(colour = "教育程度") + 
  labs(x = "年份") + 
  labs(y = "人數") +
  labs(title = "101 - 106 監獄新入監毒品罪人數 - 性別")
# 存圖
ggsave("by_gender.jpg")