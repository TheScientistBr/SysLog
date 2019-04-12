library("lubridate")
library("stringr")

files <- list.files(path = "//cronos04/C$/SAS/Config/Lev1/Web/WebAppServer/SASServer12_1/logs",
                    pattern = "localhost*")
dates <- as.Date(str_sub(string = files,start = 22,end = 31))

files <- data.frame(date=dates,file = files,stringsAsFactors = F)

df <- subset.data.frame(x = files,subset = date > "2019-01-01")


df_temp <- data.frame()

i <- 16694

for(i in grep(df$X3,pattern = "/Unimed_Vix/Analytics")) {
        desc <- strsplit(x = df$X3[i],split = "/")
        l1 <- desc[[1]][10]
        l2 <- desc[[1]][11]
        l3 <- desc[[1]][12]
        l3 <- str_remove(l3,"HTTP")
        l3 <- str_trim(l3,c("both"))
        dia <- substr(x = df$X1[i],start = 2,stop = 12)
        ifelse(mdy(dia) > "2019/01/01",1,next)
        ndf <- data.frame(dia = mdy(dia), l1 = l1, l2 = l2, l3 = l3)
        df_temp <- rbind(df_temp,ndf)
}

df_temp$n <- 1

df1 <- aggregate(df_temp$n ~ df_temp$l1,data = df_temp,FUN = sum)
names(df1) <- c("l1","f")
df1 <- df1[order(df1$f,decreasing = T),]
df1 <- subset(x = df1,subset = f > max(df1$f)*.01)
par(mar=c(11,4,4,4))
barplot(df1$f,names.arg = df1$l1,cex.names=0.8,las=2)
text(df1$f -5, labels=df1$f, xpd=TRUE, col = "blue")

df2 <- aggregate(df_temp$n ~ df_temp$l2,data = df_temp,FUN = sum)
names(df2) <- c("l2","f")
df2 <- df2[order(df2$f,decreasing = T),]
df2 <- subset(x = df2,subset = f > max(df2$f)*.01)
par(mar=c(11,4,4,4))
barplot(df2$f,names.arg = df2$l2,cex.names=0.8,las=2)
text(df2$f -5, labels=df2$f, xpd=TRUE, col = "blue")

df3 <- aggregate(df_temp$n ~ df_temp$l3,data = df_temp,FUN = sum)
names(df3) <- c("l3","f")
df3 <- df3[order(df3$f,decreasing = T),]
df3 <- subset(x = df3,subset = f > max(df3$f)*.1)
par(mar=c(11,4,4,4))
mybar <- barplot(df3$f,names.arg = df3$l3,cex.names=0.8,las=2,horiz = F,beside=TRUE)
text(df3$f -5, labels=df3$f, xpd=TRUE, col = "blue")

text(mybar, df3$f+2 , df3$f ,cex=1) 
