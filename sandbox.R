source('settings.R')
source('getData.R')

# Gewone gemiddeldes
s <- getLocalNumbers()
smelt <- melt.data.table(s,id.vars=c("municipality","year"),value.name="metrics")

# Gewogen gemiddeldes
t <- s[,.(ingeschreven=sum(ingeschreven),neergelegd=sum(neergelegd),blanco_ongeldig=sum(blanco_ongeldig)),by=year]
t <- t[order(year)]
t <- t[,c("opkomstratio","blancoratio"):=list((1-neergelegd/ingeschreven)*100,blanco_ongeldig/ingeschreven*100)]
t <- t[,.(year,
          ingeschreven,
          neergelegd,
          blanco_ongeldig,
          opkomstratio,
          blancoratio,
          opkomstdiff=c(NA,diff(opkomstratio)),
          blancodiff=c(NA,diff(blancoratio)))]
tmelt <- melt.data.table(t,id.vars=c("year"),value.name="metrics")

tplot <- ggplot(tmelt[variable %in% c("opkomstratio","blancoratio")],aes(as.factor(year),metrics,fill=variable)) + 
    geom_bar(stat="identity") +
    ylim(0,15) + xlab("Verkiezing") + ylab("%") +
    scale_fill_discrete(name="",
                        labels=c("Thuisblijvers (%)", "Blancostemmers (%)"))

# Gemeentes
u <- s[,.(ingeschreven=mean(ingeschreven),neergelegd=mean(neergelegd),blanco_ongeldig=mean(blanco_ongeldig)),by=municipality]
u <- u[order(municipality)]
u <- u[,c("opkomstratio","blancoratio"):=list((1-neergelegd/ingeschreven)*100,blanco_ongeldig/ingeschreven*100)]
u <- u[,.(municipality,
          ingeschreven,
          neergelegd,
          blanco_ongeldig,
          opkomstratio,
          blancoratio,
          opkomstdiff=c(NA,diff(opkomstratio)),
          blancodiff=c(NA,diff(blancoratio)))]

uplot <- ggplot(u,aes(u$opkomstratio,u$blancoratio)) + geom_point()

# 2012
tempV <- c()
for (i in 1:nrow(t)) {
  v <- s[year==t[i]$year]
  v <- v[order(-ingeschreven)]
  v <- v[,c("big","foertratio"):=list(ifelse(ingeschreven>quantile(ingeschreven,0.95),"Grote gemeente","Kleine gemeente"),blancoratio+opkomstratio)]
  
  v1plot <- ggplot(v,aes(opkomstratio,blancoratio,col=big)) + geom_point() + geom_smooth(method="lm")
  v2plot <- ggplot(v,aes(ingeschreven,foertratio)) + 
    geom_point() + 
    geom_smooth(method = 'nls', formula = 'y~a*x^b',se=FALSE) +
    geom_smooth(method = 'lm',se=FALSE)
  
  v1LM <- summary(lm(log(v$foertratio)~log(v$ingeschreven)))
  v2LM <- summary(lm(v$foertratio~v$ingeschreven))
  tempV[i] <- v1LM$r.squared
}
t$model <- tempV

# All years
w <- s[ingeschreven > 12000 & year == '2012-10-04']
w <- w[order(-ingeschreven)]
w <- w[,c("foertratio"):=list(blancoratio+opkomstratio)]
wplot <- ggplot(w,aes(ingeschreven,foertratio,col=as.factor(year))) + 
    geom_point(alpha=4/10) + 
    geom_smooth(method = 'nls', formula = 'y~a*x^b',se=FALSE) +
    scale_color_brewer(palette="Dark2",name="Verkiezingsdatum")

w1LM <- summary(lm(log(w$foertratio)~log(w$ingeschreven)))


# Gent
gmelt <- melt.data.table(s[municipality=="Gent"],id.vars=c("municipality","year"),value.name="metrics")
gplot <- ggplot(gmelt[variable %in% c("opkomstratio","blancoratio")],aes(as.factor(year),metrics,fill=variable)) + 
    geom_bar(stat="identity") +
    ylim(0,15)

