pacman::p_load(corrplot)

bh <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(bh) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

corrplot(cor(bh))   #default
corrplot(cor(bh), method="number", diag=F)
corrplot(cor(bh), method="number", order="hclust", addrect=2, diag=F)

#lets use this mtest from http://stackoverflow.com/questions/30656481/r-combine-cor-mtest-and-p-adjust
# pearson Correlation analysis
cor.test.mat <- function(mat){
  n <- ncol(mat)
  pmat <- matrix(0, nrow = n, ncol = n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      pmat[i,j] <- cor.test(mat[,i], mat[,j], method="pearson")$p.value
    }
  }
  pmat[lower.tri(pmat)] <- t(pmat)[lower.tri(pmat)] #fill lower triangle with upper triangle
  return(pmat)
}  
  
pvals <- cor.test.mat(bh)

corrplot(cor(bh), method="number", order="hclust", addrect=2, diag=F)
corrplot(cor(bh), p.mat = pvals, sig.level=0, insig = "p-value", method="ellipse", order="hclust", 
         type="upper", addrect=2, tl.pos = "n", cl.pos="n", diag=F, add=T)