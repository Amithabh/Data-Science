library(tidyr)
library(dplyr)
storm <- read.csv("StormData.csv")
# H,h,K,k,M,m,B,b,+,-,?,0,1,2,3,4,5,6,7,8, and blank-character
# H,h = hundreds = 100
# K,k = kilos = thousands = 1,000
# M,m = millions = 1,000,000
# B,b = billions = 1,000,000,000
# (+) = 1
# (-) = 0
# (?) = 0
# black/empty character = 0
# numeric 0..8 = 10
names(storm) <- tolower(names(storm))
storm$evtype <- tolower(storm$evtype)
storm_s <- storm[,c("state", "evtype", "fatalities", "injuries", "propdmg", "propdmgexp", "cropdmg", "cropdmgexp")]
storm_s1 <- storm_s[1:200000,]
storm_s2 <- storm_s[200001:400000,]
storm_s3 <- storm_s[400001:600000,]
storm_s4 <- storm_s[600001:800000,]
storm_s5 <- storm_s[800001:902297,]
sym <- c("H","h","K","k","M","m","B","b","+","-","?","0","1","2","3","4","5","6","7","8")
for (i in seq_along(storm_s5$propdmgexp)) {
  if(storm_s5$propdmgexp[i] == 'H'|storm_s5$propdmgexp[i] == 'h'){
    storm_s5$propdmg[i] = storm_s5$propdmg[i]*10^2
  } else if(storm_s5$propdmgexp[i] == 'K' | storm_s5$propdmgexp[i] == 'k') {
    storm_s5$propdmg[i] = storm_s5$propdmg[i]*10^3
  } else if(storm_s5$propdmgexp[i] == 'M' | storm_s5$propdmgexp[i] == 'm') {
    storm_s5$propdmg[i] = storm_s5$propdmg[i]*10^6
  } else if(storm_s5$propdmgexp[i] == 'B' | storm_s5$propdmgexp[i] == 'b')  {
    storm_s5$propdmg[i] = storm_s5$propdmg[i]*10^9
  } else if(storm_s5$propdmgexp[i] == '+') {
    storm_s5$propdmg[i] = storm_s5$propdmg[i]
  } else if(storm_s5$propdmgexp[i] == '-' | storm_s5$propdmgexp[i] == '?' | storm_s5$propdmgexp[i] == ' '){
    storm_s5$propdmg[i] <- 0
  } else if(storm_s5$propdmgexp[i] %in% (0:8)){
    storm_s5$propdmg[i] = storm_s5$propdmg[i]*10
  }
  if(storm_s5$cropdmgexp[i] == 'H'|storm_s5$cropdmgexp[i] == 'h'){
    storm_s5$cropdmg[i] = storm_s5$cropdmg[i]*10^2
  } else if(storm_s5$cropdmgexp[i] == 'K' | storm_s5$cropdmgexp[i] == 'k') {
    storm_s5$cropdmg[i] = storm_s5$cropdmg[i]*10^3
  } else if(storm_s5$cropdmgexp[i] == 'M' | storm_s5$cropdmgexp[i] == 'm') {
    storm_s5$cropdmg[i] = storm_s5$cropdmg[i]*10^6
  } else if(storm_s5$cropdmgexp[i] == 'B' | storm_s5$cropdmgexp[i] == 'b')  {
    storm_s5$cropdmg[i] = storm_s5$cropdmg[i]*10^9
  } else if(storm_s5$cropdmgexp[i] == '+') {
    storm_s5$cropdmg[i] = storm_s5$cropdmg[i]
  } else if(storm_s5$cropdmgexp[i] == '-' | storm_s5$cropdmgexp[i] == '?' | storm_s5$cropdmgexp[i] == ' '){
    storm_s5$cropdmg[i] <- 0
  } else if(storm_s5$cropdmgexp[i] %in% (0:8)){
    storm_s5$cropdmg[i] = storm_s5$cropdmg[i]*10
  }
}
storm_exp <- rbind(storm_s1, storm_s2, storm_s3, storm_s4, storm_s5)
storm_fatalities <- aggregate(formula = fatalities ~ evtype, data = storm_exp, FUN = sum)
storm_fatalities_ord <- storm_fatalities[order(storm_fatalities$fatalities, decreasing = TRUE),]
storm_10_fatalities <- storm_fatalities_ord[1:10,]
par(mar=c(5,8,4,2), las = 2)
barplot(storm_10_fatalities$fatalities, horiz = TRUE, col = topo.colors(10), names.arg = storm_10_fatalities$evtype, xlab = "Fatalities", main = "Top 10 events which are fatal to people")

storm_injuries <- aggregate(formula = injuries ~ evtype, data = storm_exp, FUN = sum)
storm_injuries_ord <- storm_injuries[order(storm_injuries$injuries, decreasing = TRUE),]
storm_10_injuries <- storm_injuries_ord[1:10,]
barplot(storm_10_injuries$injuries, horiz = TRUE, col = topo.colors(10), names.arg = storm_10_injuries$evtype, xlab = "Injuries", main = "Top 10 events which cause most injuries to people")

storm_propdmg <- aggregate(formula = propdmg ~ evtype, data = storm_exp, FUN = sum)
storm_propdmg_ord <- storm_propdmg[order(storm_propdmg$propdmg, decreasing = TRUE),]
storm_10_propdmg <- storm_propdmg_ord[1:10,]
barplot(log10(storm_10_propdmg$propdmg), horiz = TRUE, col = topo.colors(10), names.arg = storm_10_propdmg$evtype, xlab = expression('log'[10]*'(propdmg)'), main = "Top 10 events which cause most damage to property")

storm_cropdmg <- aggregate(formula = cropdmg ~ evtype, data = storm_exp, FUN = sum)
storm_cropdmg_ord <- storm_cropdmg[order(storm_cropdmg$cropdmg, decreasing = TRUE),]
storm_10_cropdmg <- storm_cropdmg_ord[1:10,]
barplot(log10(storm_10_cropdmg$cropdmg), horiz = TRUE, col = topo.colors(10), names.arg = storm_10_cropdmg$evtype, xlab = expression('log'[10]*'(cropdmg)'), main = "Top 10 events which cause most damage to crops")