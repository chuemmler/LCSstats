### 2020 Summer ADC Picks

#install.packages("tidyverse")
#install.packages("ggimage")

library(tidyverse)
library(readxl)
library(ggimage)
library(forcats)

df <- read_excel("esports/LCS/sumweek4.xlsx", sheet = "Sheet3")
champdata <- read_excel("esports/LCS/champdata", 
                        col_types = c("numeric", "text", "text", 
                                      "date", "text", "text", "text", "text"))


## get rid of double pick columns
RP1 <- c()
RP2 <- c()
BP2 <- c()
BP3 <- c()
BP4 <- c()
BP5 <- c()

for(i in 1:nrow(df)){
  split1 <- strsplit(df$`RP1-2`[i], ", ")
  RP1[i] <- split1[[1]][1]
  RP2[i] <- split1[[1]][2]
  split2 <- strsplit(df$`BP2-3`[i], ", ")
  BP2[i] <- split2[[1]][1]
  BP3[i] <- split2[[1]][2]
  split3 <- strsplit(df$`BP4-5`[i], ", ")
  BP4[i] <- split3[[1]][1]
  BP5[i] <- split3[[1]][2]
}

df <- add_column(df, RP1 = RP1, .after = "BP1")
df <- add_column(df, RP2 = RP2, .after = "RP1")
df <- add_column(df, BP2 = BP2, .after = "RP2")
df <- add_column(df, BP3 = BP3, .after = "BP2")
df <- add_column(df, BP4 = BP4, .after = "RP4")
df <- add_column(df, BP5 = BP5, .after = "BP4")
df = subset(df, select = -c(`RP1-2`,`BP2-3`,`BP4-5` ))

### create pick DF

pickdf <- data.frame(B1 = df$BP1, R1 = df$RP1, B2 = df$BP2, R2 = df$RP2, B3 = df$BP3, 
                     R3 = df$RP3, B4 = df$BP4, R4 = df$RP4, B5 = df$BP5, R5 = df$RP5)

bandf <- data.frame(B1 = df$BB1, R1 = df$RB1, B2 = df$BB2, R2 = df$RB2, B3 = df$BB3, 
                  R3 = df$RB3, B4 = df$BB4, R4 = df$RB4, B5 = df$BB5, R5 = df$RB5)

pickorder <- df[26:35]

talldf <- data.frame(c(as.vector(pickdf$B1), as.vector(pickdf$R1),
                       as.vector(pickdf$B2), as.vector(pickdf$R2),
                       as.vector(pickdf$B3), as.vector(pickdf$R3),
                       as.vector(pickdf$B4), as.vector(pickdf$R4),
                       as.vector(pickdf$B5), as.vector(pickdf$R5)))

talldf$PB <- "Pick"
colnames(talldf) <- c("Name","PB")

tall1df <- data.frame(c(as.vector(bandf$B1), as.vector(bandf$R1),
                       as.vector(bandf$B2), as.vector(bandf$R2),
                       as.vector(bandf$B3), as.vector(bandf$R3),
                       as.vector(bandf$B4), as.vector(bandf$R4),
                       as.vector(bandf$B5), as.vector(bandf$R5)))

tall1df$PB <- "Ban"
colnames(tall1df) <- c("Name","PB")

talldf <- rbind(talldf, tall1df)
talldf <- left_join(talldf, champdata)

#garp


ggplot(talldf[which(talldf$Role == "ADC"),], aes(x = fct_infreq(Name), fill = PB))+
  geom_bar()+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  geom_image(aes(y = -4, image = img), size = .1)+
  labs(title = "LCS ADC Pick/Bans", subtitle = "As of Week 4", y = "Games", x = "Champion", 
       caption = "Data taken from 'https://lol.gamepedia.com/LCS/2020_Season/Summer_Season/'", fill = "")


  

  
