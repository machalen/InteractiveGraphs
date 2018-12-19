SameScale.scatter <-
function(num_mat1, num_mat2, col1.cutoff, col2.cutoff, FC.Col1, FC.Col2, cutoff, mainName, filePath=NULL, PNG=TRUE) {
  
  library(ggplot2)
  library(cowplot)
  library(tidyverse)
  library(VennDiagram)
  library(gridExtra)
  library(plotly)
  
  IntersGenes <- intersect(rownames(num_mat1), rownames(num_mat2))
  
  num_mat1.s <- num_mat1[IntersGenes,]
  num_mat2.s <- num_mat2[IntersGenes,]
  all.equal(rownames(num_mat1.s), rownames(num_mat2.s))
  de_all <- cbind(num_mat1.s,num_mat2.s)
  vect1 <- de_all[,grep(col1.cutoff, colnames(de_all))]
  vect2 <- de_all[,grep(col2.cutoff, colnames(de_all))]
  
  #Add columns for plot information
  de_all$GeneID <- IntersGenes
  de_all$significance <- "None"
  
  # Columns of adjusted p-value
  de_all$significance[vect1 <= cutoff] <- paste(col1.cutoff ,"<", cutoff, "only")
  de_all$significance[vect2 <= cutoff] <- paste(col2.cutoff ,"<", cutoff, "only")
  de_all$significance[vect2 <= cutoff & vect1 <= cutoff] <- paste("Both <", cutoff)
  de_all$significance <- factor(de_all$significance, ordered=TRUE, levels=c("None", paste(col1.cutoff ,"<", cutoff, "only"), 
                                                                            paste(col2.cutoff ,"<", cutoff, "only"), paste("Both <", cutoff)))
  print(table(de_all$significance))
  # None         RNA-seq only        Ribo-seq only Ribo-seq and RNA-seq
  # 15896                 6536                   16                   83
  
  p <- ggplot(de_all %>% arrange(significance), aes_string(x=grep(FC.Col1, colnames(de_all), value=TRUE),
                                                           y=grep(FC.Col2, colnames(de_all), value=TRUE),
                                                           color="significance", group="GeneID")) + geom_point() +
    ggtitle(mainName) +
    scale_color_manual(values=c("gray87", "deepskyblue4", "goldenrod3", "firebrick3"))
  
  if(PNG == TRUE) {
    png(file= paste(file.path(filePath, mainName), "png", sep="."),width=1000,height=1000,res=100)
    print(p)
    dev.off()
    
  }
  
  
  p.ly <- ggplotly(p)
  print(p.ly)
}
