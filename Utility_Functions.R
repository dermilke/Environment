plotCorrelationMatrix <- function(datatable, ytextOffset = 1, plotHalf = F, main = "", orderParams = F) {
  
  corMat <- corMatrixWrapper(datatable, orderParams) 
  
  my.color.ramp<-colorRamp(c("blue","white","red"))
  
  x <- seq(-1,0.9,length.out = ncol(corMat))
  x_diff <- x[2]-x[1]
  
  x_space = 0.005
  y_space = 0.005
  
  par(mar = c(0,0,5,0),xpd = T)
  
  plot(0,0,type = "n", axes = FALSE, xlab = '',ylab = '',xlim = c(-1.5,1.5),ylim = c(-1.2,1.4))
  
  for (k in 1:(nrow(corMat)-1)) {
    for (i in ifelse(plotHalf,k+1,1):(nrow(corMat))) {
      
      rect(-1+((x_diff)*(i-1))+x_space,1-(x_diff*(k-1)),-1+((x_diff)*i),1-(x_diff*k)+y_space, border = "darkgrey")
      rect(-1+((x_diff)*(i-1))+x_space,1-(x_diff*(k-1)),-1+((x_diff)*i),1-(x_diff*k)+y_space, border = "NA",
           col=rgb(my.color.ramp(corMat[k,i]),maxColorValue=255))
      
    } 
  }
  
  col_bar(60, c(-0.85,-1.25), 0.03,2, c(-1,1),0.8, "Linear correlation (Spearman)",my.color.ramp,FALSE)
  
  text(-0.1,-1,main,adj = 0, cex = 1.2)
  
  text(x[ifelse(plotHalf,2,1):nrow(corMat)]+x_diff/2,rep(1.05,ifelse(plotHalf,nrow(corMat)-1,nrow(corMat))),
       labels = colnames(corMat)[ifelse(plotHalf, 2,1):nrow(corMat)],srt = 90, cex = .8,adj = 0)
  
  text(rep(0.98 ,ifelse(plotHalf,nrow(corMat)-1,nrow(corMat))),
       x[ifelse(plotHalf,nrow(corMat)-1, nrow(corMat)):1]+x_diff*ifelse(plotHalf, ytextOffset+1, ytextOffset),
           labels = colnames(corMat)[1:ifelse(plotHalf, nrow(corMat)-1, nrow(corMat))],cex = .8,adj = 0)
  
}

removeNA <- function(datatable) {
  datatable[is.na(datatable)] <- 0
  return(datatable)
}

ordiModel <- function(ordination, parameter) {
  ordiModel <- vegan::ordisurf(ordination, parameter)$grid %>% 
    with(., {expand.grid(x = x, y = y) %>%
        list(., z = as.vector(z)) %>%
        data.frame() %>%
        na.omit()})
}

corMatrixWrapper <- function(datatable, orderParams = F) {
  cor_mat <- cor(datatable, use = "pairwise.complete.obs", method = "spearman") %>%
    removeNA() %>%
    vegan::decostand(method = "range") %>%
    as.matrix()
  
  if (orderParams) {
    clusterOrder <- cor_mat  %>%
      vegan::vegdist() %>%
      hclust() %>%
      .$order
    
    cor_mat <- cor_mat[clusterOrder, clusterOrder]
  }
  
  return(cor_mat)
}

col_bar <- function(n.colors.bar, pos, size.bar.cell,width.bar.cell, range.val, cex.lab, bar.label,color_ramp,pos_range,min_max_label = c("-1","1"),add_label = TRUE) {
  min.z<- range.val[1]
  max.z<- range.val[2]
  
  my.color.ramp<-color_ramp
  i<-0
  
  for(i in 0:(n.colors.bar-1)) {
    
    polygon(c(pos[1]+size.bar.cell*i,pos[1]+size.bar.cell*(i+1),pos[1]+size.bar.cell*(i+1),pos[1]+size.bar.cell*i),
            c(pos[2],pos[2],pos[2]+width.bar.cell*size.bar.cell,pos[2]+width.bar.cell*size.bar.cell),
            border=NA,col=rgb(my.color.ramp(1/(n.colors.bar-1)*i),maxColorValue=255))
    
    if(i==n.colors.bar-1) {
      
      text(pos[1],pos[2]+size.bar.cell,pos=2,min_max_label[1],cex=cex.lab); 
      text(pos[1]+size.bar.cell*(i+1),pos[2]+size.bar.cell,pos=4,min_max_label[2],cex=cex.lab)
      text(pos[1]+size.bar.cell*(i+1)/2,pos[2]+(width.bar.cell+1)*size.bar.cell,labels=bar.label,pos=3,cex=cex.lab)
    }
  }
  
  i = seq(0.1,0.9,0.1)
  
  if (add_label) {
    if (pos_range) {
      text(pos[1]+size.bar.cell*(i*n.colors.bar),pos[2] + width.bar.cell*size.bar.cell*1.5,
           as.character(round(max.z*i,digits = 2)),cex = cex.lab)
    } else {
      text(pos[1]+size.bar.cell*(i*n.colors.bar),pos[2] + width.bar.cell*size.bar.cell*1.5,
           as.character(round(max.z*(i*2)-1,digits = 2)),cex = cex.lab)
    }
  }
}

pca_wrapper <- function(datatable, modelParam) {
  
  pca <- datatable %>%
    select_if(is.numeric) %>%
    vegan::decostand(method = "standardize") %>%
    vegan::vegdist(method = "euclidian",na.rm = T) %>%
    cmdscale(eig = T) 
  
  explVar <- round((pca$eig/sum(pca$eig))[1:2]*100, digits = 2)
  
  ordiModel <- vegan::ordisurf(pca, modelParam, plot = F)$grid %>% 
    with(., {expand.grid(x = x, y = y) %>%
        list(., z = as.vector(z)) %>%
        data.frame() %>%
        na.omit()})
  
  datatable_return <- list(data = datatable %>% mutate(pca_1 = pca$points[,1], pca_2 = pca$points[,2]),
                           ordiModel = ordiModel,
                           explVar = explVar,
                           pca = pca)
  
  return(datatable_return)
  
}