pacman::p_load(stringdist, dplyr, tm, gplots)

#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Clean strings (lower case, remove punctation, remove all white space)
str_clean <- function(strings) {
  require(dplyr); require(tm)
  strings %>%
    tolower() %>%
    removePunctuation() %>%
    stripWhitespace() %>%
    trim()  
}

prods = c(
  "Bose SoundLink Mini Bluetooth Speaker II (Pearl)", 
  "Bose SoundLink Mini Bluetooth Speaker II (Carbon)", 
  "Bose SoundLink Color Bluetooth speaker II - Soft black", 
  "Bose SoundLink Color Bluetooth Speaker II - Polar White", 
  "UE BOOM 2 Phantom Wireless Mobile Bluetooth Speaker (Waterproof and Shockproof)", 
  "UE BOOM 2 Yeti Wireless Mobile Bluetooth Speaker (Waterproof and Shockproof)", 
  "UE ROLL 2 Volcano Wireless Portable Bluetooth Speaker (Waterproof)", 
  "UE ROLL 2 Atmosphere Wireless Portable Bluetooth Speaker (Waterproof)"
)


clean_prods = str_clean(prods)
n = length(clean_prods)

methods <- c('lcs','osa','cosine') #distance methods
q <- c(0,0,3)  #size of q-gram

dist.methods<-list()
for(m in 1:length(methods)){
  dist = matrix(NA, ncol = n, nrow = n)  #initialize empty matrix
  #row.names(dist) = prods
  for(i in 1:n) {
    for(j in 1:n) { 
      dist[i,j] <- stringdist(clean_prods[i],clean_prods[j], method = methods[m], q=q[m])   
    }  
  }
  dist.methods[[m]] <- dist
}

par(mfrow = c(2,2))

a = heatmap.2(x = dist.methods[[1]], Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = dist.methods[[1]], notecol = "black", notecex = 1, 
          trace = "none", key = FALSE, margins = c(7, 11))


b = heatmap.2(x = dist.methods[[2]], Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = dist.methods[[2]], notecol = "black", notecex = 1, 
          trace = "none", key = FALSE, margins = c(7, 11))

c = heatmap.2(x = dist.methods[[3]], Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(dist.methods[[3]],2), notecol = "black", notecex = 1, 
          trace = "none", key = FALSE, margins = c(7, 11))

