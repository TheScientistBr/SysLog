library("lubridate")
library("stringr")
library("stringi")

files <- list.files(path = "//cronos04/C$/SAS/Config/Lev1/Web/WebAppServer/SASServer12_1/logs",
                    pattern = "localhost*")
dates <- as.Date(str_sub(string = files,start = 22,end = 31))

files <- data.frame(date=dates,file = files,stringsAsFactors = F)
files <- subset(x = files,subset = date >= "2019-01-01")

df <- data.frame()

for(i in 1:length(files$date)){
        dff <- read.delim(paste0("//cronos04/C$/SAS/Config/Lev1/Web/WebAppServer/SASServer12_1/logs/",
                                 files$file[i]),fileEncoding = "utf-8")
        names(dff) <- c("linha")
        df <- rbind(dff,df)
}

df$linha <- as.character(df$linha)

library("RCurl")
library("XML")
library("urltools")

df_temp <- data.frame()

dff <- data.frame(linha = stri_subset_regex(str = df[[1]],pattern = "Unimed_Vix"),stringsAsFactors = F)

for(i in 1:length(dff$linha)) {
        dia <- substr(x = dff[[1]][i],start = 22,stop = 32)
        dia <- as.vector(strsplit(dia,split = "/"))
        dia[[1]][2] <- match(dia[[1]][2],month.abb)
        dia[[1]][2] <- as.character(ifelse(as.integer(dia[[1]][2])< 10, paste0("0",dia[[1]][2]),dia[[1]][2]))
        dia <- dmy(paste0(dia[[1]][1],"-",dia[[1]][2],"-",dia[[1]][3]))

        desc <- url_decode(dff[[1]][i])
        desc <- sub('.*Unimed_Vix/', '', desc)
        desc <- sub('&.*', '', desc)
        Encoding(desc) <- "UTF-8"
        desc <- strsplit(x = desc,split = "/")
        
        ndf <- data.frame(dia = dia, l1 = desc[[1]][1], l2 = desc[[1]][2], l3 = desc[[1]][3],stringsAsFactors = F)
        df_temp <- rbind(df_temp,ndf)
}

df_temp$n <- 1

df1 <- aggregate(df_temp$n ~ df_temp$l1,data = df_temp,FUN = sum)
names(df1) <- c("l1","f")
df1 <- df1[order(df1$f,decreasing = T),]
df1 <- subset(x = df1,subset = f > max(df1$f)*.01)
par(mar=c(11,4,4,4))
mybar <- barplot(df1$f,names.arg = df1$l1,cex.names=0.8,las=2,col = "darkgreen")
text(mybar, df1$f,df1$f, xpd=TRUE, col = "green")

df2 <- aggregate(df_temp$n ~ df_temp$l2,data = df_temp,FUN = sum)
names(df2) <- c("l2","f")
df2 <- df2[order(df2$f,decreasing = T),]
df2 <- subset(x = df2,subset = f > max(df2$f)*.01)
par(mar=c(11,4,4,4))
mybar <- barplot(df2$f,names.arg = df2$l2,cex.names=0.8,las=2)
text(mybar, df2$f,df2$f, xpd=TRUE, col = "blue")

df3 <- aggregate(df_temp$n ~ df_temp$l3,data = df_temp,FUN = sum)
names(df3) <- c("l3","f")
df3 <- df3[order(df3$f,decreasing = T),]
df3 <- subset(x = df3,subset = f > max(df3$f)*.1)
par(mar=c(11,4,4,4))
mybar <- barplot(df3$f,names.arg = df3$l3,cex.names=0.8,las=2,horiz = F,beside=TRUE)
text(mybar, df3$f,df3$f, xpd=TRUE, col = "blue")

