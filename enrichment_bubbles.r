
library(ggplot2)
library(RColorBrewer)
library(dplyr) # 用于数据处理

data_up <- read_excel("HepPDCs_P4-VS-iHPs_P4（Up）.xlsx")
data_down<-read_excel("HepPDCs_P4-VS-iHPs_P4（Down）.xlsx")
# --- 2. 数据处理：将 "66/1036" 转换为小数 (66/1036) ---
# 这里的逻辑是：提取分子 / 分母
data_down <- data_down %>%
  mutate(
    # 提取分子
    Numerator = as.numeric(sapply(strsplit(as.character(GeneRatio), "/"), `[`, 1)),
    # 提取分母
    Denominator = as.numeric(sapply(strsplit(as.character(GeneRatio), "/"), `[`, 2)),
    # 计算小数比例
    Ratio_Score = Numerator / Denominator
  )

data_plot <- data_down %>%
  arrange(Ratio_Score) %>% # 排序：P值小的在最上面
  mutate(Description = fct_inorder(Description)) # 锁定顺序

xlab <- "GeneRatio"
title <- paste0( "\n", "HepPDCs_P4-VS-iHPs_P4(Down)")
size.lab <- "GeneNumber"

p <- qplot(Ratio_Score, Description,
           data = data_plot, size = Count,
           colour = pvalue, xlab = xlab, ylab = ""
) +
  theme_bw() +
  #theme(panel.grid = element_blank()) +
  theme(panel.border = element_rect(
    color = "black", size = 1, fill = NA
  )) +
  theme(axis.line = element_blank(), strip.background = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text = element_text(color = "black") )+
  ggtitle(title) +
  # 大小标度：设置气泡的大小范围
  scale_size_continuous(range = c(5, 10), breaks = c(10, 20, 30, 40, 50, 60)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_gradientn(colours = brewer.pal(11, "RdYlBu")) +
  labs(size = size.lab) +
  labs(colour = "pvalue") +
  guides(size = guide_legend(order = 1))
print(p)

ggsave(
  filename = "HepPDCs_P4-VS-iHPs_P4(Down).png",
  #paste0("GO.top.Total.png", opt$mark, ".pdf"),
  height = 12, width = 11, plot = p
)

ggsave(
  filename = "HepPDCs_P4-VS-iHPs_P4(Down).pdf",
  #paste0("GO.top.Total.png", opt$mark, ".pdf"),
  height = 10, width = 10, plot = p
)






































title <- "HepPDCs_P4-VS-iHPs_P4(Up)"

p <- ggplot(data_up, aes(x = Ratio_Score, y = Description, size = Count, color = pvalue)) +
  # 绘制气泡，设置透明度
  guides(
    size = guide_legend(title = "GeneNumber", order = 1), # order=1 放在上面
    color = guide_colorbar(title = "pvalue", order = 2)    # order=2 放在下面
  ) +
  # 坐标轴标签
  labs(
    x = "GeneRatio",
    y = "",
    size = "Gene Number",
    color = "P-value"
  ) +
  
  # 颜色标度：使用红黄蓝配色，并转换为对数刻度以更好显示显著性
  scale_color_gradientn(colours = brewer.pal(11, "RdYlBu"), trans = "log10") +
  
  # 大小标度：设置气泡的大小范围
  scale_size_continuous(range = c(2, 10), breaks = c(10, 20, 30, 40, 50, 60)) +
  
  # 主题设置：模仿您之前的风格
  theme_bw() +
  theme(
    #panel.grid = element_blank(), # 去掉网格线
    panel.border = element_rect(color = "black", size = 1, fill = NA), # 添加边框
    axis.line = element_blank(), # 去掉坐标轴线
    plot.title = element_text(hjust = 0.5), # 标题居中
    axis.text.y = element_text(size = 10) # 调整Y轴文字大小
  ) + geom_point(alpha = 0.8) +
  ggtitle(title)
print(p)

