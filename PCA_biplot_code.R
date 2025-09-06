#PCA codes for corrplot and biplot. 
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(ggrepel)

head(Analysis_working)
subas <- Analysis_working
View(subas)
subas_2 <- subas[1:24, 1:31]
subas_2
View(subas_2)
subas_1 <- subas_2[, -1]
head(subas_1[,1:6], 4)

PCA(subas_1, scale.unit = T, ncp = 5, graph = T)
library(FactoMineR)

res.pca<- PCA(subas_1, graph = F)
print(res.pca)
library(factoextra)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = T, ylim = c(0,30))
#tiff(600dpi)
# Save PCA scree plot as TIFF at 600 dpi
tiff("PCA_eigenvalues.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30))
dev.off()

var <- get_pca_var(res.pca)
var
head((var$coord))
head((var$cos2))
head(var$contrib)
head(var$coord,4)
fviz_pca_var(res.pca, col.var = "black")
head(var$cos2, 4)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$cos2, is.corr = FALSE, col = colorRampPalette(c("white", "#1f77b4"))(200))
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$cos2, is.corr = FALSE, col = colorRampPalette(c("white", "#1f77b4"))(200))
# Using squares instead of circles in corrplot
corrplot(var$cos2, is.corr = FALSE, method = "square", 
         col = colorRampPalette(c("white", "#1f77b4"))(200))

corrplot(var$cos2, is.corr = FALSE, method = "square", 
         col = colorRampPalette(c("white", "#084594"))(200))
corrplot(var$cos2, is.corr = FALSE, method = "square", 
         col = colorRampPalette(c("white", "#084594"))(200), 
         cl.cex = 1.0, 
         cl.align.text = "c", 
         tl.cex = 0.8) 

#final corrplot for PCA
corrplot(var$cos2, is.corr = FALSE, method = "square", 
         col = colorRampPalette(c("white", "#084564"))(200), 
         cl.cex = 0.66,       
         cl.ratio = 0.36,     
         cl.offset = 2,      
         tl.cex = 0.7)       

#black labels 
corrplot(var$cos2, 
         is.corr = FALSE, 
         method = "square",
         cl.cex = 0.66,       
         cl.ratio = 0.40,     
         cl.offset = 18,      
         tl.cex = 0.7,        
         tl.col = "black") 

##tiff 600dpi
tiff("corrplot_cos2.tiff", width = 6.5, height = 6.5, units = "in", res = 600, compression = "lzw")
corrplot(var$cos2,
         is.corr = FALSE,
         method = "square",
         cl.cex = 0.66,       
         cl.ratio = 0.40,     
         cl.offset = 18,      
         tl.cex = 0.7,        
         tl.col = "black")    
dev.off()

#### PCA biplot
fviz_pca_biplot(res.pca, 
                geom.ind = "point", 
                col.ind = "#E7B800", 
                pointshape = 21, 
                pointsize = "cos2", 
                col.var = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                repel = TRUE)
##### PCA biplot with labels 
fviz_pca_biplot(res.pca,                  
                geom.ind = "point",                  
                col.ind = "#E7B800",                  
                pointshape = 21,                  
                pointsize = "cos2",                  
                col.var = "cos2",                  
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),                  
                repel = TRUE) + 
  geom_text(aes(label = rownames(res.pca$ind$coord)), 
            size = 3, 
            color = "black")

####without overlap
fviz_pca_biplot(res.pca,                  
                geom.ind = "point",                  
                col.ind = "#ffa12c",                  
                pointshape = 16,                  
                pointsize = "cos2",                  
                col.var = "cos2",                  
                gradient.cols = c("#0f0e0f", "#7b0708", "#e60001"),                  
                repel = TRUE) + 
  geom_text_repel(aes(label = rownames(res.pca$ind$coord)), 
                  size = 3, 
                  box.padding = 0.2,    
                  point.padding = 0.2,  
                  max.overlaps = 10)   
#####PCA 3 and PCA 4
fviz_pca_biplot(res.pca, 
                axes = c(3,4), 
                geom.ind = "point",                  
                col.ind = "#ffa12c",                  
                pointshape = 16,                  
                pointsize = "cos2",                  
                col.var = "cos2",                  
                gradient.cols = c("#0f0e0f", "#7b0708", "#e60001"),                  
                repel = TRUE) + 
  geom_text_repel(aes(label = rownames(res.pca$ind$coord)), 
                  size = 3, 
                  box.padding = 0.5,    
                  point.padding = 0.5,  
                  max.overlaps = 10)   
### PCA1 and PCA 2 
fviz_pca_biplot(res.pca, 
                axes = c(1, 2),       
                geom.ind = "point", 
                col.ind = "#ffa12c", 
                pointshape = 16, 
                pointsize = "cos2", 
                col.var = "cos2", 
                gradient.cols = c("#0f0e0f", "#7b0708", "#e60001"), 
                repel = TRUE) +    
  geom_text_repel(aes(label = rownames(res.pca$ind$coord)), 
                  size = 3, 
                  box.padding = 0.5,    
                  point.padding = 0.5,  
                  max.overlaps = 10)  

######
ind.coord <- as.data.frame(res.pca$ind$coord)

ind.coord$Treatment <- subas_2$Treatment
ind.coord$Treatment
ind.coord
########

###PCA biplot with actual label names
fviz_pca_biplot(res.pca, 
                axes = c(3, 4),       # Specify dimensions 
                geom.ind = "point", 
                col.ind = "#ffa12c", 
                pointshape = 16, 
                pointsize = "cos2", 
                col.var = "cos2", 
                gradient.cols = c("#0f0e0f", "#7b0708", "#e60001"), 
                repel = TRUE) +
  geom_text_repel(data = ind.coord,  
                  aes(x = Dim.3, y = Dim.4, label = Treatment), 
                  size = 3.5, 
                  box.padding = 0,    
                  point.padding = 0.05,  
                  segment.size = 0,   
                  max.overlaps = 6) + 
  labs(x = "PC3 (x%)", y = "PC4 (y%%)")  

######################