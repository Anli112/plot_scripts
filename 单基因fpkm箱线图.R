library(tidyverse)
library(ggplot2)

# 1. 设置工作目录 (请确保路径正确)
setwd('C:/Users/li.pan/Desktop/售后汇总/26年3月/DZOE2026021750_程国平老师_有参转录组售后分析_260312/')

# 2. 读取数据
# 注意：read.delim 默认 header=TRUE, sep="\t"
data <- read.delim("原报告\\result\\OECloud_tools\\fpkm.xls", header = TRUE, sep = '\t') %>% as_tibble()

# 3. 定义需要绘制的基因列表
genes_to_plot <- c("Ccl2", "Ccl3", "Ccl5", "Ccl7", "Ccl8", "Ccl17", 
                   "Cxcl1", "Cxcl2", "Cxcl9", "Cxcl10", "Cxcl12")

# 4. 数据预处理：转换为长格式 (只需做一次)
df_long <- data %>%
  pivot_longer(cols = -id, names_to = "sample", values_to = "expression") %>%
  mutate(Group = ifelse(sample %in% c("P1", "P2", "P3"), "P", "C")) %>%
  # 确保 Group 是因子，这样顺序固定 (P 在左，C 在右)
  mutate(Group = factor(Group, levels = c("P", "C")))
#df_long$expression <- log10(df_long$expression+1)
# 5. 创建输出文件夹 (避免文件散落在根目录)
if (!dir.exists("Boxplot_Output")) {
  dir.create("Boxplot_Output")
}

# 6. 开始循环绘图
for (gene in genes_to_plot) {
  
  # --- A. 筛选当前基因的数据 ---
  gene_data <- filter(df_long, id == gene)
  
  # 检查数据是否存在，避免报错
  if (nrow(gene_data) == 0) {
    message(paste("警告: 未找到基因", gene, ", 跳过。"))
    next
  }
  
  # --- B. 构建绘图对象 ---
  p <- ggplot(gene_data, aes(x = Group, y = expression, fill = Group)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, size = 0.8) +
    labs(title = paste("Expression of", gene), x = "", y = "FPKM") +
    theme_minimal() +
    scale_fill_manual(values = c("P" = "#1f78b4", "C" = "#e31a1c")) +
    theme(
      panel.grid = element_blank(), 
      panel.border = element_rect(color = "black", size = 1.2, linetype = "solid"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # 标题也加粗一点
      #axis.ticks.x = element_blank(),
      axis.ticks.x.bottom = element_line(),# 移除 X 轴刻度线
      axis.ticks.y = element_line(color = "black", size = 0.5), # Y 轴保留细刻度线或设为 blank()
      axis.text.x = element_text(
        size = 12,           
        face = "plain",       # 修正：这里必须是 "bold", "plain", "italic" 之一
        angle = 0,           # 如果只有 P 和 C 两组，建议设为 0 (水平)，更清晰；若重叠再改 45
        hjust = 0.5,         # 居中
        color = "black"
      ),
      axis.text.y = element_text(size = 10, color = "black"),
      legend.position = "right" # 既然 X 轴标签已经是 P/C，图例可以隐藏，让图更简洁
    )
  
  # --- C. 定义文件名 ---
  file_name_base <- paste0("Boxplot_", gene)
  
  # --- D. 导出 PNG (高分辨率，适合插入 PPT/Word) ---
  ggsave(filename = paste0("Boxplot_Output/", file_name_base, ".png"), 
         plot = p, 
         width = 5, height = 6 )
  
  # --- E. 导出 PDF (矢量图，适合论文投稿) ---
  ggsave(filename = paste0("Boxplot_Output/", file_name_base, ".pdf"), 
         plot = p, 
         width = 5, height = 6)
  
  message(paste("✅ 已生成:", gene))
}

