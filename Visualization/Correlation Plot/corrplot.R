#correlation plot


pacman::p_load(lme4, lmerTest, corrplot, multcomp, tidyr, dplyr, ggplot2, knitr)
movies = read.csv("C:/Users/rjsai/Dropbox/UMN Courses/STAT 8801/movies.csv")


#replace missing with median
for(i in 6:16) movies[,i] = ifelse(is.na(movies[,i]), median(movies[,i], na.rm=TRUE), movies[,i])

# pearson Correlation analysis
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){2
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level, method="pearson")
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res <- cor.mtest(cor(movies[,6:16]),0.95)

corrplot(cor(movies[,6:16]), method="number", order="hclust", addrect=2, diag=F)
corrplot(cor(movies[,6:16]), p.mat = res[[1]], sig.level=0, insig = "p-value", method="ellipse", order="hclust", 
         type="upper", addrect=2, tl.pos = "n", cl.pos="n", diag=F, add=T)