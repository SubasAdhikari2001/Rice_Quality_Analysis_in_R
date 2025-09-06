###Correlation matrix####
round(subas <- cor(Analysis_working[,-1]),2)
symnum(subas)
heatmap(subas, Rowv = FALSE, symm = T, margins = c(6,6))
heatmap(subas, symm = T, margins = c(6,6))
########with custom colors
library(fields)
subas <- cor(Analysis_working[,-1])
subas <- round(subas, 2)

# Custom colors
colors <- colorRampPalette(c("#01257D","#185886", "#FFFFFF", "#FF9200", "#7D0125"))(100)
colors <- colorRampPalette(c("#0d001e","#185886", "#FFFFFF", "#FF9200", "#7D0125"))(100)

# Set layout
layout(matrix(c(1, 2), ncol = 2), widths = c(4, 2))  # More space for heatmap

# Heatmap
par(mar = c(5, 4, 4, 1))  
heatmap(subas, Rowv = FALSE, symm = TRUE, margins = c(6,6), col = colors)
#600dpi 
heatmap_correlation <- heatmap(subas, Rowv = FALSE, symm = TRUE, margins = c(6,6), col = colors)
#tiff (full page)
tiff("heatmap_fullpage.tiff", width = 6.5, height = 6.5, units = "in", res = 600, compression = "lzw")
heatmap(subas, Rowv = FALSE, symm = TRUE, margins = c(6,6), col = colors)
dev.off()
#png(full page)
png("heatmap_fullpage.png", width = 6.5, height = 6.5, units = "in", res = 600)
heatmap(subas, Rowv = FALSE, symm = TRUE, margins = c(6,6), col = colors)
dev.off()

# Adding Color legend 
par(mar = c(5, 0.5, 5, 2))  
image.plot(legend.only = TRUE,
           col = colors,
           zlim = range(subas, na.rm = TRUE),
           horizontal = FALSE,
           legend.width = 1,     
           legend.mar = 4.5)        

############
