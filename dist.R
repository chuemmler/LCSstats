#### 2020 Spring LCS Champs selected ####

install.packages("tidyverse")
install.packages("ggimage")

library(tidyverse)
library(readxl)
library(ggimage)

#import data
df <- read_excel("esports/LCS/Spring2020.xlsx", sheet = "Sheet3")
img <- "https://gamepedia.cursecdn.com/lolesports_gamepedia_en/1/18/AhriSquare.png?version=b063ffe901511592494d06e363dc4dcd"

#get team picks into dataframe
pdf <- t(data.frame(strsplit(df$Picks[1], ", ")))

for(i in 2:nrow(df)){
  r2 <- t(data.frame(strsplit(df$Picks[i], ", ")))
  pdf <- rbind(pdf, r2)
}


pdf2 <- t(data.frame(strsplit(df$Picks2[i], ", ")))
for(i in 2:nrow(df)){
  r4 <- t(data.frame(strsplit(df$Picks2[i], ", ")))
  pdf2 <- rbind(pdf2, r4)
}

pdf <- data.frame(pdf)
pdf2 <- data.frame(pdf2)


colnames(pdf) <- c("TOP","JNG","MID","ADC","SUP")
rownames(pdf) <- c()

colnames(pdf2) <- c("TOP","JNG","MID","ADC","SUP")
rownames(pdf2) <- c()

pdf$patch <- df$P
pdf2$patch <- df$P

pdf <- rbind(pdf, pdf2)

pdf$PB <- "Pick"

#same thing for bans
bdf <- t(data.frame(strsplit(df$Bans[1], ", ")))

for(i in 2:nrow(df)){
  r3 <- t(data.frame(strsplit(df$Bans[i], ", ")))
  bdf <- rbind(bdf, r3)
}

for(i in 1:nrow(df)){
  r3 <- t(data.frame(strsplit(df$Bans2[i], ", ")))
  bdf <- rbind(bdf, r3)
}

bdf <- data.frame(bdf)

colnames(bdf) <- c("TOP","JNG","MID","ADC","SUP")
rownames(bdf) <- c()

banlist <- cbind(bdf$TOP, bdf$JNG)



#graphing time

pdf$img <- img
pdf$ADC <- fct_reorder(pdf$ADC, pdf$ADC, length, .desc= T)

ggplot(pdf, aes(x = ADC, fill = as.factor(patch)))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  geom_image(aes(x = 2, y=1 , image =img))+
  theme_minimal()
  
  
#reformatting data
sdf <- data.frame(summary(pdf$ADC))
sdf$champ <- rownames(sdf)
rownames(sdf) <- c()
colnames(sdf) <- c("pickcount","champ")

b1 <- as.vector(bdf$TOP)
b2 <- as.vector(bdf$JNG)
b3 <- as.vector(bdf$MID)
b4 <- as.vector(bdf$ADC)
b5 <- as.vector(bdf$SUP)

sbdf <- c(b1, b2, b3, b4, b5)

sbdf <- data.frame(sbdf)
colnames(sbdf) <- c("champ")

bansum <- data.frame(summary(sbdf$champ, maxsum = 150))
bansum$champ <- rownames(bansum)
rownames(bansum) <- c()
colnames(bansum) <- c("bancount","champ")

combdf <- left_join(sdf, bansum)

combdf[is.na(combdf)] <- 0

ggplot(combdf, aes(x = champ))+
  geom_bar(aes(y = pickcount), stat = "identity", fill = "red")+
  geom_bar(aes(y = bancount), stat = "identity", fill = "blue")
