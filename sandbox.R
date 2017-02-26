library(httr)
library(XML)
library(rvest)
library(data.table)
library(jsonlite)
library(xlsx)
library(ggplot2)
library(reshape2)
library(plotly)

###########################
# Local Elections: Part 1 #
###########################
getOldLocalNumbers <- function() {
    getLinks <- function() {
        localElectionsLink <- 'http://www.ibzdgip.fgov.be/result/nl/result_date.php?type=year&vt=CG'
        localElectionsPage <- read_html(localElectionsLink)
        
        # Get links for each election year
        localElectionsYearLinks <- localElectionsPage %>% html_nodes("#AutoNumber8 tr td a") %>% html_attr("href")
        
        # Get links for each municipality
        for (i in 1:length(localElectionsYearLinks)) {
            page <- read_html(paste0("http://www.ibzdgip.fgov.be/result/nl/",localElectionsYearLinks[i]))
            year <- page %>% html_nodes("#AutoNumber7 tr td span .result") %>% html_text()
            year <- year[1]
            municipalities <- page %>% html_nodes("#AutoNumber9 tr td a") %>% html_text()
            links <- page %>% html_nodes("#AutoNumber9 tr td a") %>% html_attr("href")
            thisYear <- data.table(cbind(paste0("http://www.ibzdgip.fgov.be/result/nl/",localElectionsYearLinks[i]),year,municipalities,paste0("http://www.ibzdgip.fgov.be/result/nl/",links)))
            if (i == 1) {
                localElectionsCityLinks <- thisYear
            } else {
                localElectionsCityLinks <- rbind(localElectionsCityLinks,thisYear)
            }
        }
        colnames(localElectionsCityLinks) <- c("yearlink","year","municipality","link")
        localElectionsCityLinks
    }
    # Crawl every municipality for each election year
    if (!file.exists("local.csv")) {
        file <- getLinks()
        for (i in 1:nrow(file)) {
            try(page <- read_html(file$link[i]))
            ingeschreven <- page %>% html_nodes("#AutoNumber8 tr td span font") %>% html_text()
            file$ingeschreven[i] <- as.numeric(ingeschreven[2])
            neergelegd <- page %>% html_nodes("#AutoNumber8 tr td span font") %>% html_text()
            file$neergelegd[i] <- as.numeric(neergelegd[3])
            blanco_ongeldig <- page %>% html_nodes("#AutoNumber8 tr td span font") %>% html_text()
            file$blanco_ongeldig[i] <- as.numeric(blanco_ongeldig[5])
            message(paste0("Crawling page ",i," of ",nrow(file)))
        }
        write.csv(file,"local.csv")
    } else {
        file <- data.table(read.csv("local.csv",sep=";",stringsAsFactors=F))
    }
    # OCMW-raad en districtraad uitfilteren
    file <- file[!grepl("KO_OC|KO_GD",link)]
    
    file <- file[,yearlink:=NULL]
    file <- file[,link:=NULL]
    file <- file[,year:=as.Date(year,"%d/%m/%Y")]
    file
}

##################################
# Local Elections: Part 2 - 2006 #
##################################
get2006LocalNumbers <- function() {
    parsedXML <- xmlParse("http://www.vlaanderenkiest.be/verkiezingen2006/data/LVK320/GE/06/LVK320_GE_06_GEW_02000.xml")
    countyList <- xmlToList(parsedXML)
    file <- data.table(year=character(),municipality=character(),ingeschreven=numeric(),neergelegd=numeric(),blanco_ongeldig=numeric())
    for (i in 4:(length(countyList$Election$GeoUnit)-1)) {
        for (j in 4:(length(countyList$Election$GeoUnit[[i]])-1)) {
            newRow <- data.table("2006-10-08",
                        countyList$Election$GeoUnit[[i]][[j]]$Name,
                        as.numeric(countyList$Election$GeoUnit[[i]][[j]]$Voters[1]),
                        as.numeric(countyList$Election$GeoUnit[[i]][[j]]$Voters[2]),
                        as.numeric(countyList$Election$GeoUnit[[i]][[j]]$Votes[2])+as.numeric(countyList$Election$GeoUnit[[i]][[j]]$Votes[3]))
            if (nrow(file)==0) {
                file <- newRow
            } else {
                file <- rbind(file,newRow,fill=T)
            }
        }
    }
    colnames(file) <- c("year","municipality","ingeschreven","neergelegd","blanco_ongeldig")
    file <- file[,year:=as.Date(year,"%Y-%m-%d")]
    file
}

##################################
# Local Elections: Part 3 - 2012 #
##################################

get2012LocalNumbers <- function() {
    nis <- data.table(read.csv("http://ckan-001.corve.openminds.be/storage/f/2013-06-06T12%3A17%3A06.134Z/gemeentecodes.csv",sep=";",stringsAsFactors=F))
    nis$NIS.code <- as.character(nis$NIS.code)
    nis <- nis[,.(NIS.code=head(NIS.code,1)),by=.(Gemeente)]
    countyList <- fromJSON("http://www.vlaanderenkiest.be/verkiezingen2012/2012/gemeente/statistiek.json")
    countyNames <- names(countyList$G)
    file <- data.table(year=character(),municipality=character(),ingeschreven=numeric(),neergelegd=numeric(),blanco_ongeldig=numeric())
    for (i in 1:length(countyNames)) {
        newRow <- data.table("2012-10-14",countyNames[i],countyList$G[[i]]$sg,countyList$G[[i]]$ui,countyList$G[[i]]$os)
        if (nrow(file)==0) {
            file <- newRow
        } else {
            file <- rbind(file,newRow,fill=T)
        }
    }
    colnames(file) <- c("year","municipality","ingeschreven","neergelegd","blanco_ongeldig")
    file <- merge(file,nis,by.x="municipality",by.y="NIS.code",all.x=T,all.y=F)
    file$municipality <- file$Gemeente
    file <- file[,Gemeente:=NULL]
    setcolorder(file,c("year","municipality","ingeschreven","neergelegd","blanco_ongeldig"))
    file <- file[,year:=as.Date(year,"%Y-%m-%d")]
    file
}

getLocalNumbers <- function() {
    vlaamseGemeenten <- data.table(municipality=unique(get2012LocalNumbers()$municipality))
    fd <- data.table(rbind(getOldLocalNumbers(),get2006LocalNumbers(),get2012LocalNumbers()))
    # Data kuisen
    ## Moerbeke -> Moerbeke-Waas
    fd[grepl("Moerbeke",municipality)]$municipality <- "Moerbeke-Waas"
    
    ## Sint-Niklaas -> Sint-Niklaas (Prov. Oost-Vlaanderen)
    fd[grepl("Sint-Niklaas",municipality)]$municipality <- "Sint-Niklaas (Prov. Oost-Vlaanderen)"
    
    ## Kapelle-Op-Den-Bos -> Kapelle-op-den-Bos
    fd[grepl("Kapelle-op-den-Bos",municipality)]$municipality <- "Kapelle-Op-Den-Bos"
    
    ## Diverse Herk De Stad schrijfwijzen -> Herk-De-Stad
    fd[grepl("Herk",municipality)]$municipality <- "Herk-De-Stad"
    
    ## Heist-Op-Den-Berg -> Heist-op-den-Berg
    fd[grepl("Heist-op-den-Berg",municipality)]$municipality <- "Heist-Op-Den-Berg"
    
    ## Wortegem Petegem -> Wortegem-Petegem
    fd[grepl("Wortegem Petegem",municipality)]$municipality <- "Wortegem-Petegem"
    
    ## Baarle Hertog -> Baarle-Hertog
    fd[grepl("Baarle Hertog",municipality)]$municipality <- "Baarle-Hertog"
    
    ## Dilsen-Stokkem -> Dilsen
    fd[grepl("Dilsen-Stokkem",municipality)]$municipality <- "Dilsen"
    
    fd <- merge(fd,vlaamseGemeenten,by="municipality",all.X=F,all.y=T)
    
    # Alles van voor de verkiezingen van 1988 uitfilteren omdat er geen cijfers bestaan over het aantal ingeschrevenen
    fd <- fd[year > "1987-12-31"]
    
    # Herstappe eruit
    fd <- fd[municipality != "Herstappe"]
    
    fd <- fd[order(year)]
    fd <- fd[,c("opkomstratio","blancoratio"):=list((1-neergelegd/ingeschreven)*100,blanco_ongeldig/ingeschreven*100)]
    
    fd <- fd[order(year)]
    fd <- fd[,.(year,
                municipality,
                ingeschreven,
                neergelegd,
                blanco_ongeldig,
                opkomstratio,
                blancoratio,
                opkomstdiff=c(NA,diff(opkomstratio)),
                blancodiff=c(NA,diff(blancoratio)))]
    
    fd 
}

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
v <- s[year=="2012-10-14"]
v <- v[order(-ingeschreven)]
v <- v[,c("big","foertratio"):=list(ifelse(ingeschreven>quantile(ingeschreven,0.95),"Grote gemeente","Kleine gemeente"),foert=blancoratio+opkomstratio)]

v1plot <- ggplot(v,aes(opkomstratio,blancoratio,col=big)) + geom_point() + geom_smooth(method="lm")
v2plot <- ggplot(v,aes(ingeschreven,foertratio)) + 
    geom_point() + 
    geom_smooth(method = 'nls', formula = 'y~a*x^b',se=FALSE) +
    geom_smooth(method = 'lm',se=FALSE)

vLM <- summary(lm(log(v$foertratio)~log(v$ingeschreven)))
vLM <- summary(lm(log(v$foertratio)~log(v$ingeschreven)))

# Gent
gmelt <- melt.data.table(s[municipality=="Gent"],id.vars=c("municipality","year"),value.name="metrics")
gplot <- ggplot(gmelt[variable %in% c("opkomstratio","blancoratio")],aes(as.factor(year),metrics,fill=variable)) + 
    geom_bar(stat="identity") +
    ylim(0,15)

