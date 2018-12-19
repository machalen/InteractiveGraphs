Volcano.scatter <-
function(num_mat, pval_col, FC_col, pval_th, FC_th, mainName, filePath=NULL, PNG=TRUE) {
  
  library(ggplot2)
  library(cowplot)
  library(tidyverse)
  library(VennDiagram)
  library(gridExtra)
  library(plotly)

  num_mat[,grep(pval_col, colnames(num_mat))] <- -log10(num_mat[,grep(pval_col, colnames(num_mat))])
  pval_th <- -log10(pval_th)
  pVal_str <- num_mat[,grep(pval_col, colnames(num_mat))]
  FC_str <- num_mat[,grep(FC_col, colnames(num_mat))]
  num_mat$significance <- "None"
  num_mat$significance[pVal_str >= pval_th & FC_str <= -FC_th] <- "Down Expressed"
  num_mat$significance[pVal_str >= pval_th & FC_str >= FC_th] <- "Over Expressed"
  num_mat$significance <- factor(num_mat$significance, ordered=TRUE, levels=c("None", "Down Expressed", "Over Expressed"))
  print(table(num_mat$significance))
  num_mat$GeneID <- rownames(num_mat)
  
  p <- ggplot(num_mat %>% arrange(significance), aes_string(x=grep(FC_col, colnames(num_mat), value=TRUE), 
                                                            y=grep(pval_col, colnames(num_mat), value=TRUE), 
                                                            color="significance", group="GeneID")) + geom_point() +
    ggtitle(paste("Volcano plot in", mainName)) +
    scale_color_manual(values=c("gray87", "deepskyblue4", "goldenrod3"))
  
  if(PNG == TRUE) {
    png(file= paste(file.path(filePath, mainName), "png", sep="."),width=1000,height=1000,res=100)
    print(p)
    dev.off()
    
  }
  
  
  p.ly <- ggplotly(p)
  print(p.ly)
}
