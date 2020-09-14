library(dplyr)
library(knitr)

filename <- "StormData.csv.bz2"

if(!file.exists("./data")) {
  dir.create("./data")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  print("Downloading File.....")
  download.file(fileUrl, destfile = paste("./data/", filename)) 
  print("File Downloaded Successfully!")
}

storm_df <- read.csv(paste("./data/", filename), na.strings = c("", " ", "NA"))

dim(storm_df)
names(storm_df)
# colnames(storm_df)
class(storm_df$EVTYPE)
table(factor(storm_df$EVTYPE))
length(unique(storm_df$EVTYPE))

sum(is.na(storm_df))
mean(is.na(storm_df)) * 100
table(is.na(storm_df))
sum(table(is.na(storm_df)))

sapply(storm_df, FUN = function(x) sum(is.na(x)))
apply(storm_df, 2, function(x) length(which(is.na(x))))

summary(storm_df)
str(storm_df)
colSums(is.na(storm_df))

class(colnames(storm_df))
sapply(storm_df, class)
storm_sub <- c("EVTYPE", 
               "FATALITIES", 
               "INJURIES", 
               "PROPDMG", 
               "PROPDMGEXP", 
               "CROPDMG", 
               "CROPDMGEXP") %>% 
  subset(storm_df, select = .) %>% 
  subset(., subset = EVTYPE != "?" & (FATALITIES > 0 | 
           INJURIES > 0 | 
           PROPDMG > 0 | 
           CROPDMG > 0))

sum(!is.na(storm_sub$CROPDMGEXP))

26739224+6645765
31639042+1745947 

618413 + 283884

mean(is.na(storm_sub)) * 100
storm_sub2 <- subset(storm_sub, subset = FATALITIES > 0 | 
                       INJURIES > 0 | 
                       PROPDMG > 0 | 
                       CROPDMG > 0)

sData <- storm_sub[storm_sub$FATALITIES > 0 | storm_sub$INJURIES > 0 | storm_sub$PROPDMG > 
                     0 | storm_sub$CROPDMG > 0, ]

mean(is.na(storm_sub)) * 100
mean(is.na(sData)) * 100

propDmgEXP <- unique (storm_sub$PROPDMGEXP)
cropDmgEXP <- unique (storm_sub$CROPDMGEXP)
table(storm_sub$CROPDMGEXP)
table(storm_sub$PROPDMGEXP)


#Approach:
# Make the scale Uniform: need to scale the data value accordingly
# B/b --> billion : 10e(9)
# M/m --> million : 10e(6)
# K/k --> thousand: 10e(3)
# H/h --> hundred : 10e(2)
# "-" --> 10e0
# "?" --> 10e0
# "number" -> 10e(number)


# function to convert DMGEXP character to multiplication number
#       but suppress NA warnings caused by unnecessary 10^character attempts
exp_factor <- function(x){
  ifelse(x %in% c("+","-","?"), 10^0,
    ifelse(x %in% as.character(0:8), 10^as.numeric(x),
           ifelse(x %in% c("b","B"), 10^9,    # billion
                  ifelse(x %in% c("m","M"), 10^6,    # million/mega
                         ifelse(x %in% c("k","K"), 10^3,    # kilo   
                                ifelse(x %in% c("h","H"), 10^2,    # hecto
                                       1))))))
}
df_copy <- storm_sub
df_copy$PROPDMG <- df_copy$PROPDMG * exp_factor(df_copy$PROPDMGEXP)
df_copy$CROPDMG <- df_copy$CROPDMG * exp_factor(df_copy$CROPDMGEXP)

fatalities <- aggregate(cbind(FATALITIES, INJURIES, FATALITIES + INJURIES) ~ EVTYPE , data=df_copy, FUN=sum) 
fatalities <- fatalities[order(-fatalities[, 4]), ]
MaxFatalities <- fatalities[1:10, ]
print(MaxFatalities)


ggplot(MaxFatalities, aes(x=reorder(EVTYPE, MaxFatalities[, 4]), y = MaxFatalities[, 4])) + 
  geom_bar(stat="identity", aes(fill=MaxFatalities[, 4])) + 
  # theme(axis.text.x = element_text(angle=45, hjust=1)) +
  #geom_line(aes(group = 1), col="red") +
  #geom_point(col="red", size = 4) +
  geom_text(aes(label = round(MaxFatalities[, 4])), size = 3.5, hjust = -.2) +
  coord_flip() + ylim(0, max(MaxFatalities[, 4] + 10^4))
pal3 <- colorRampPalette(colors = c("#0a2258", "#ffffc1"))(10)
barplot(MaxFatalities[order(MaxFatalities[, 2], decreasing = TRUE), ][, 2],
        las = 3, names= MaxFatalities[order(MaxFatalities[, 2], decreasing = TRUE), ][, 1], 
        main = "Weather Events With\n The Top 10 Highest Fatalities", 
        ylab = "Number of Fatalities", col = pal3)
pal2 <- colorRampPalette(colors = c("#063615", "#ffffc1"))(10)
barplot(MaxFatalities[order(MaxFatalities[, 3], decreasing = TRUE), ][, 3],
        las = 3, names= MaxFatalities[order(MaxFatalities[, 3], decreasing = TRUE), ][, 1], 
        main = "Weather Events With\n The Top 10 Highest Fatalities", 
        ylab = "Number of Fatalities", col = pal2)
pal <- colorRampPalette(colors = c("#6b001d", "#ffffc1"))(10)
b3 <- barplot(MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 4],
        las = 3, names= MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 1], 
        main = "Weather Events With\n The Top 10 Highest Fatalities", 
        ylab = "Number of Fatalities", col=pal, ylim = c(0, 1.1 * max(MaxFatalities$V3)))
text(x = b3, 
     y = MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 4],
     labels = MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 4],
     pos = 3)

MaxFatalities[order(MaxFatalities[, 2], decreasing = TRUE), ]

# default par(mar = c(5.1 4.1 4.1 2.1))
par(mar = c(5.1, 8.1, 4.1, 4.1))
options(scipen=1)
b4 <- barplot(as.matrix(MaxFatalities[, 2:4]), col=pal, 
        las = 1, beside = T, 
        main = "Weather Events With\n The Top 10 Highest Fatalities", 
        ylab = "", legend = MaxFatalities$EVTYPE, 
        args.legend = list(x = "topright", bty = 'n', inset=c(-0.25,0)),
        ylim = c(0, 1.1 * max(MaxFatalities$V3)))
text(x = b4, 
     y = as.matrix(MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 2:4]),
     labels = paste(as.character(
       as.matrix(round(
         MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 2:4]/1000, 2))), 'K'), 
     pos = 3, cex = 0.6, srt =45)
mtext("Number of Fatalities", line = 4.5, side = 2)





axis(2, line = -0.5, MaxFatalities$V, las = 1)
dim(b4[, 1])
legend("topright", legend = MaxFatalities$EVTYPE, bty = "n")

dim(MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 4])
as.matrix(paste(as.character(MaxFatalities[order(MaxFatalities[, 4], decreasing = TRUE), ][, 2:4]/100), 'K'))


# Create data
set.seed(112)
data <- matrix(sample(1:30,15) , nrow=3)
colnames(data) <- c("A","B","C","D","E")
rownames(data) <- c("var1","var2","var3")

# Grouped barplot
barplot(data, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(data), 
        xlab="group", 
        font.lab=2)
barplot(as.matrix(MaxFatalities))

ecodmg <- aggregate(cbind(PROPDMG + CROPDMG, PROPDMG, CROPDMG) ~ EVTYPE , data=df_copy, FUN=sum) 
ecodmg <- ecodmg[order(-ecodmg[, 2]), ]
ecodmgMax <- ecodmg[1:10, ]
print(ecodmgMax)

ecodmg2 <- aggregate(cbind(PROPDMG + CROPDMG, PROPDMG, CROPDMG) ~ EVTYPE , data=df_copy, FUN=sum) %>% 
  .[order(-.[, 2]), ] %>% .[1:10, ]
row.names(ecodmg2) <- NULL
names(ecodmg2)[2] <- "Total"
print(kable(x=ecodmg2))
dim(ecodmg2)
par(mar = c(5.1, 8.1, 4.1, 4.1))
b5 <- barplot(as.matrix(ecodmgMax[, 2:4]), col=pal2, 
              las = 1, beside = T, 
              main = "Weather Events With\n The Top 10 Highest Fatalities", 
              ylab = "", legend = ecodmgMax$EVTYPE, 
              args.legend = list(x = "topright", bty = 'n', inset=c(-0.08,0)),
              ylim = c(0, 1.1 * max(ecodmgMax[, 2])))
text(x = b5, 
     y = as.matrix(ecodmgMax[order(-ecodmgMax[, 2]), ][, 2:4]),
     labels = 
       as.matrix(signif(
         ecodmgMax[order(-ecodmgMax[, 2]), ][, 2:4], digits=1)), 
     pos = 3, cex = 0.6, srt =45)
mtext("Number of Damage", line = 4.5, side = 2)


