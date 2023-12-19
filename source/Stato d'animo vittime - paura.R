
library(png)
library(ggplot2)
library(extrafont)


genderselection <- read.table(text="
  Gender Freq
                              F   36.19
                                  # Siccome il codice colora il grafico come se si trattasse di un rettangolo, è stato necessario fare degli 
                                  #accorgimenti sulle percentuali per rendere meglio la percentuale di aria coperta nell'immagine:
                                  # 
                                  # L'immagine è suddivisa in pixel bianchi(b) e neri(n) nel seguente modo:
                                  #
                                  # Gambe: 24041 b - 47221 n
                                  #
                                  # busto-braccia : 23260 b - 7877 n
                                  #
                                  # braccia - spalle :60745 b - 20147 n
                                  #                   
                                  # spalle - testa : 10440 b - 30648 n
                                  #
                                  # tot : 118486 b + 105893 n
                                  # 
                                  # i dati a nostra disposizione sono:  
                                  # 
                                  # Tot domande : 9889 (escluse Non disponibili)
                                  #
                                  # 1) persone con paura per la propria incolumità : 2623 -> 26.52%
                                  # 2) persone con ansia : 2039 -> 20.62%
                                  # 3) persone con grave soggezione : 2965 -> 29.98%
                                  # 
                                  # calcolo proporzione:
                                  # 
                                  # 26.52%(118486) = 31422
                                  # 
                                  # 20.62%(118486) = 24432
                                  #
                                  # 29.98%(118486) = 35522
                                  #
                                  # 1) DEVO COLORARE 31422 Pixel partendo dal basso:
                                  #
                                  # 31422-(24041) = 7381 -> 7381/23260 = 0.3172 = 31.72%
                                  #   
                                  #   h = h_gambe + 31.72%(h_busto-braccia) = 222 + 31.72%(97) = 253 px 
                                  # 
                                  #   253 : x = h_tot : 100
                                  #
                                  #   253 : x = 699 : 100
                                  #     
                                  #         x = 36.19%
                                  # 
                                  # 
                                  # 2) DEVO COLORARE 24432 Pixel partendo dal basso:
                                  #
                                  # 24432-(24041) = 391 -> 391/23260 = 0.0168 = 16.68%
                                  #   
                                  #   h = h_gambe + 16.68%(h_busto-braccia) = 222 + 16.68%(97) = 238 px 
                                  # 
                                  #   238 : x = h_tot : 100
                                  #
                                  #   238 : x = 699 : 100
                                  #     
                                  #         x = 34.04%
                                  # 
                                  # 
                                  # 3) DEVO COLORARE 35522 Pixel partendo dal basso:
                                  #
                                  # 35522-(24041) = 11481 -> 11481/23260 = 0.4935 = 49.35%
                                  #   
                                  #   h = h_gambe + 49.35%(h_busto-braccia) = 222 + 49.35%(97) = 270 px 
                                  # 
                                  #   270 : x = h_tot : 100
                                  #
                                  #   270 : x = 699 : 100
                                  #     
                                  #         x = 38.62%
                                  # 
                                  
                              ", header=T)
pcts <- round(genderselection$Freq)

# Load png file as binary
con <- url("file:///C:/Users/Luca/Desktop/woman_BN.png",
           open='rb')
rawpng <- readBin(con, what='raw', n=6005)
close(con)

img <- readPNG(rawpng)
h <- dim(img)[1]
w <- dim(img)[2]


# Find the rows where feet starts and head ends
pos1 <- which(apply(img[,,1], 1, function(y) any(y==1)))
mn1 <- min(pos1)
mx1 <- dim(img)[1]



pospctF <- round((mx1-mn1)*pcts[1]/100+mn1)

# Fill bodies with a different color according to percentages
# Note that this relies on the fact that the png is a bitmap.
# The png is expressed as a matrix with a cell for each pixel
# and 3 layers for r,g,b.

# Create a 2d matrix by just taking the red values
# Image is black and white so black corresponds to 0
# white corresponds to 1. Then change the values of
#  the cells to correspond to one of three categories.

imgmtx <- img[h:1,,1]

whitemtx <- (imgmtx==1)
colmtx <- matrix(rep(FALSE,h*w),nrow=h)
#midpt <- round(w/2)-10
#colmtx[mx1:pospctM,1:midpt] <- TRUE
colmtx[mx1:pospctF,(0):w] <- TRUE
imgmtx[whitemtx & colmtx] <- 0.5

# Need to melt the matrix into a data.frame that ggplot can understand
df <- reshape2::melt(imgmtx)


cols <- c(rgb(255,255,255,maxColorValue = 255),
          rgb(245, 197, 220,maxColorValue = 255), 
          rgb(188, 70, 153,maxColorValue = 255))

# Then use a heatmap with 3 colours for background, and percentage fills
# Converting the fill value to a factor causes  a discrete scale.
# geom_tile takes three columns: x, y, fill corresponding to 
# x-coord, y-coord, and colour of the cell.
plot <- ggplot(df, aes(x = Var2, y = Var1, fill = factor(value)))+
  geom_tile() +
  scale_fill_manual(values = cols) +
  theme_void()+
  theme(legend.position = "none")



plot + annotate("text", x = 160, y = 275, label = "26.5%", family = "Comfortaa Light") 