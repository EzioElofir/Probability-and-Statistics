setwd("C:/Users/matti.LAPTOP-G9EU4DA3/Desktop/UniMib/statistica/STATSeJSIIS/database")
rm(list=ls())
ls()
library(readr)
library(countrycode)
#install.packages("ggplot2")
library("ggplot2")
library(ggflags)
#install.packages("gghighlight")
library("gghighlight")
#install.packages("vioplot")
library("vioplot")
#install.packages("reldist")
library(reldist)
#install.packages("ggpubr")
library("ggpubr")
library(openxlsx)
#######tabelle da leggere######################################
perc_lau = read.csv("educ_uoe_enrt08_1_Data.csv",stringsAsFactors=FALSE, header=TRUE) #laureati in europa
SILC = read.csv("IT_2013p_EUSILC.csv",header=TRUE,stringsAsFactors=FALSE) #redditi e livello studio congiunti italia
redditi_paese= read.csv("ilc_di04_1_Data.csv",header=TRUE,stringsAsFactors=FALSE) #reddito mediano paesi EU
gdp=read_tsv("estat_sdg_08_10_filtered.tsv") #gdp per capita paesi EU
prod=read_tsv("estat_tesem160_filtered.tsv") #produttività paesi EU


#############PUNTO 1, LEGO I PAESI AL NUMERO DI LAUREATI########
perc_lau$Value[perc_lau$Value==':']<- NA
Nperc_lau <- perc_lau[complete.cases(perc_lau$Value), ]

###################studio solo il 2017#########################
p_lau <- Nperc_lau[Nperc_lau$TIME=='2017',] #prendo solo il 2017

#########laureati fra 20 e 24 anni 2017#######################
p_l <- p_lau[p_lau$AGE=='From 20 to 24 years',]

##########laureati totale 2017 #################################
p_totale <- p_lau[p_lau$AGE=='Total',]

#################pulisco il nome della germania nelle due tabelle#########
p_l[5,2]<- "Germany"
p_totale[5,2]<- "Germany"

#######trasformo in numeric il valore della percentuale nelle due tabelle#########
p_l$Value <- as.numeric(p_l$Value)
p_totale$Value <- as.numeric(p_totale$Value)

#############ora possiamo trovare il sommario per le due tabelle############

########elimino valori palesemente scorretti##################
p_l$Value[p_l$Value>100]<- NA
p_totale$Value[p_totale$Value>100]<- NA

p_l<- na.omit(p_l)
p_totale<- na.omit(p_totale)

#####It comprises ISCED levels 5 (short-cycle tertiary education), 6 (bachelor's or equivalent level), 7 (master's or equivalent level) and 8 (doctoral or equivalent level)###
summary(p_l$Value)
summary(p_totale$Value)

######grafico per vedere quali paesi hanno più laureatim confronti####


p_l_df <- p_l[,c(2,6)]

ggplot(p_l_df, aes(x=reorder(GEO,-Value), Value)) +
  geom_col()+
  labs(title="istruzione terziaria",x="paesi",y="percentuale")+
  theme(axis.text.x = element_text(angle = 90, hjust =1))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=median(p_l_df$Value),linetype="dashed", color = "green", size=0,5)+
  gghighlight(Value==33.1)


p_totale_df <- p_totale[,c(2,6)]

ggplot(p_l_df, aes(x=reorder(GEO,-Value), Value)) +
  geom_col()+
  labs(title="istruzione terziaria",x="paesi",y="percentuale")+
  theme(axis.text.x = element_text(angle = 90, hjust =1))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=median(p_l_df$Value),linetype="dashed", color = "green", size=0,5)+
  gghighlight(Value==33.1)




  

###########redditi livello di studio in italia####################
#DATI: REDDITO INDIVIDUALE E LIVELLO DI STUDIO RILEVATI CONGIUNTAMENTE 
#0 pre-primary education 1 primary education 2 lower secondary education 3 (upper) secondary education 4post-secondary non tertiary education 5 first stage of tertiary education (not leading directly to an advanced research qualification) 6second stage of tertiary education (leading to an advanced research qualification)
#PY010G/PY010N: EMPLOYEE CASH OR NEAR CASH INCOME  
#PY020G/PY020N: NON-CASH EMPLOYEE INCOME  
#The employee income is broken down into: Gross employee cash or near cash income (PY010G); Gross non-cash employee income (PY020G); Employers’ social insurance contributions (PY030G).
#Self-employment income is broken down into:
#Gross cash profits or losses from self-employment (including royalties) (PY050G);Value of goods produced for own consumption (HY170G).
#PB010 YEAR OF THE SURVEY 
#PB140 ANNO DI NASCITA

######dati disponibili del 2013############
myvars2 <- c("PB010","PB140","PY010G","PY020G","PY030G", "PY050G", "PE040") #HY170G=0
mydata2 <- SILC[myvars2]
mynewdata2 <- na.omit(mydata2)#omette le righe con valori nulli
colnames(mynewdata2)<- c('anno', 'nascita', 'cash', 'non_cash', 'insur','gross_profits','livello_studio')


#########aggiungo una colonna che tiene conto di tutte le entrate per ogni persona#####
mynewdata2$reddito <- rowSums(mynewdata2[,c(3,4,5,6)])


######trovare percentuale popolazione per ogni livello di studio#########
perc_livello_studio <- as.data.frame(100*table(mynewdata2$livello_studio)/length(mynewdata2$livello_studio))
colnames(perc_livello_studio)<-c("livello", "perc")
perc_livello_studio

slices <- c(perc_livello_studio[1,2], perc_livello_studio[2,2],perc_livello_studio[3,2], perc_livello_studio[4,2],perc_livello_studio[5,2],perc_livello_studio[6,2],perc_livello_studio[7,2])
lbls <-c("livello 0: ", "livello 1: ", "livello 2: ", "livello 3: ","livello 4: ","livello 5: ","livello 6: " )
pct <- round(slices)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Composizione livelli studio campione")



######ANALISI SITUAZIONE ITALIANA##########################
summary(mynewdata2$reddito)
boxplot(x=mynewdata2$livello_studio, main="livello di studio in Italia") 
################DISTRIBUZIONE REDDITI PER OGNI LIVELLO DI STUDIO############
boxplot(data=mynewdata2,mynewdata2$reddito~mynewdata2$livello_studio, main="distribuzione redditi per livello di studio",xlab="livello studio",ylab="reddito")
vioplot(data=mynewdata2,mynewdata2$reddito~mynewdata2$livello_studio, main="distribuzione redditi per livello di studio",xlab="livello studio",ylab="reddito")



###################gini index###########################à
gini_livello_studio <- aggregate(reddito ~ livello_studio,data = mynewdata2,FUN = "gini")
gini_globale<- gini(mynewdata2$reddito) #vale 0 se i valori sono tutti uguali
#write.xlsx(gini_livello_studio,"Gini-livello-studio.xlsx")

mediana_livello_studio <- aggregate(mynewdata2$reddito,by=list(mynewdata2$livello_studio), median)
mediana_globale <- as.data.frame(median(mynewdata2$reddito))
#write.xlsx(mediana_livello_studio,"mediana_livello_studio.xlsx")

ggscatter(mediana_livello_studio, x = "Group.1", y = "x",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio", ylab = "Reddito mediano")






########################PUNTO 3, LIVELLO STUDIO E REDDITO MEDIO NEI VARI PAESI EU##############
######################
#Reddito medio di ogni paese
#DATI: REDDITO MEDIO DI OGNI PAESE UE E LIVELLO DI ISTRUZIONE MEDIO PAESE

reddito_globale_paese = redditi_paese[redditi_paese$HHTYP=='Total',]
reddito_globale_paese = reddito_globale_paese[reddito_globale_paese$UNIT=='Euro',]
reddito_globale_paese <- reddito_globale_paese[reddito_globale_paese$TIME=="2017",]


#######voglio in un unica tabella il reddito mediano e il livello di studio####
reddito_globale_paese$Value[reddito_globale_paese$Value==':']<- NA
reddito_globale_paese <- na.omit(reddito_globale_paese)

####pulisco il nome della germania#######################
reddito_globale_paese[reddito_globale_paese=="Germany (until 1990 former territory of the FRG)"] <- "Germany"

################creo un'unica tabella con reddito e livello di studio##########
reddito_studio_paese <- merge(reddito_globale_paese,p_totale, by = "GEO", all.y = TRUE)
reddito_studio_paese <- na.omit(reddito_studio_paese)

write.xlsx(reddito_studio_paese,"reddito-mediano-paese-livello-studio.xlsx")


reddito_studioyoung_paese <- merge(reddito_globale_paese,p_l, by = "GEO", all.y = TRUE)
reddito_studioyoung_paese <- na.omit(reddito_studioyoung_paese)



reddito_studio_paese$Value.x <- as.numeric(gsub(",", "", reddito_studio_paese$Value.x))
x <- reddito_studio_paese$Value.y #livello studio
y <- reddito_studio_paese$Value.x #reddito mediano paese
#####cerco correlazione fra reddito mediano paese e livello di studio#####
res <- cor.test(x, y, method=c("pearson"))
str(res)
# Add regression line
plot(x, y, main = "Livello di studio VS Reddito Mediano",
     xlab = "Percentuale Educazione Terziaria", ylab = "Y Reddito Mediano",
     pch = 19, frame = FALSE, add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
abline(lm(y ~ x, data = reddito_studio_paese), col = "green")


########creo un altro grafico, più completo############
ggscatter(reddito_studio_paese, x = "Value.y", y = "Value.x",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio", ylab = "Reddito mediano")



############legame fra reddito mediano e numero di giovani laureati###########
reddito_studioyoung_paese$Value.x <- as.numeric(gsub(",", "", reddito_studioyoung_paese$Value.x))
x <- reddito_studioyoung_paese$Value.y #livello studio
y <- reddito_studioyoung_paese$Value.x #reddito mediano paese
#####cerco correlazione fra reddito mediano paese e livello di studio#####
res3 <- cor.test(x, y, method=c("pearson"))
str(res3)
# Add regression line
plot(x, y, main = "Livello di studio giovani VS Reddito Mediano",
     xlab = "Percentuale Educazione Terziaria giovani", ylab = "Y Reddito Mediano",
     pch = 19, frame = FALSE, add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
abline(lm(y ~ x, data = reddito_studioyoung_paese), col = "green")


########creo un altro grafico, più completo############
ggscatter(reddito_studioyoung_paese, x = "Value.y", y = "Value.x",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio giovani", ylab = "Reddito mediano")





####################PUNTO 4,LEGAME GDP PER CAPITA E LIVELLO DI STUDIO########
gdp <- gdp[,c(1,19)]#prendo solo il 2017


#################PULISCO I NOMI DEI PAESI#######################
for (i in (1:38)){
  gdp[i,] <- sub("A,CLV10_EUR_HAB,B1GQ,", "",gdp[i,])
end
}
colnames(gdp)<-c("paese","GDP_pc")

###################pulisco i dati mancanti###############à
gdp$GDP_pc[gdp$GDP_pc==":"] <- NA
gdp <- na.omit(gdp)
gdp$paese[gdp$paese==""]<-NA
gdp<-gdp[complete.cases(gdp),]

gdp$paese <- countrycode(gdp$paese,"eurostat","country.name")


#####################unica tabella con GDP_pc e livello studio#############
gdp_studiotot_paese <- merge(p_totale, gdp, by.x="GEO", by.y="paese",all.y=TRUE)
gdp_studiotot_paese <- na.omit(gdp_studiotot_paese)

gdp_studioyoung_paese <- merge(p_l, gdp, by.x="GEO", by.y="paese",all.y=TRUE)
gdp_studioyoung_paese <- na.omit(gdp_studioyoung_paese)


#################studio legami fra livello di studio medio e gdp_pc###########
gdp_studiotot_paese$GDP_pc <- as.numeric(gsub("p", "", gdp_studiotot_paese$GDP_pc))

x <- gdp_studiotot_paese$Value
y <- gdp_studiotot_paese$GDP_pc

res1 <- cor.test(x, y, method=c("pearson"))
str(res1)

ggscatter(gdp_studiotot_paese, x = "Value", y = "GDP_pc",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio globale", ylab = "GDP per capita")




#################studio legami fra livello studio medio giovani e gdp_pc############
gdp_studioyoung_paese$GDP_pc <- as.numeric(gsub("p", "", gdp_studioyoung_paese$GDP_pc))


x <- gdp_studioyoung_paese$Value
y <- gdp_studioyoung_paese$GDP_pc

res2 <- cor.test(x, y, method=c("pearson"))
str(res2)

ggscatter(gdp_studioyoung_paese, x = "Value", y = "GDP_pc",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio giovani", ylab = "")






###############PUNTO 5, PRODUTTIVITà E LIVELLO DI STUDIO############
prod <- prod[,c(1,14)]

for (i in (1:37)) {
  prod[i,] <- sub("A,PC_EU28_MPPS_CP,NLPR_PER,", "",prod[i,])
end
}
colnames(prod)<-c("PAESE","PROD")

########PULISCO LA PRODUTTIVITà##################
prod$PROD<- gsub("[p]","",prod$PROD)
prod$PROD<- gsub("[d]","",prod$PROD)
prod$PROD<- gsub("[c]","",prod$PROD)

prod$PAESE<-gsub("[A-Z0-9]{4}",NA ,prod$PAESE)
prod$PROD[prod$PROD==":"]<- NA
prod$PROD[prod$PROD==": "]<- NA #con spazio
prod$PROD<- as.numeric(as.character(prod$PROD))
prod$PAESE <- countrycode(prod$PAESE,"eurostat","country.name")
prod <- na.omit(prod)

###########METTO IN UNICA TABELLA PRODUTTIVITà E LIVELLO STUDIO#######

##############studio legame fra produttività e livello studio globale#########
prod_studiotot_paese <- merge(p_totale,prod,by.x="GEO",by.y="PAESE")

x <- prod_studiotot_paese$Value
y <- prod_studiotot_paese$PROD

res4 <- cor.test(x, y, method=c("pearson"))
str(res4)

ggscatter(prod_studiotot_paese, x = "Value", y = "PROD",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio", ylab = "Produttività")







##########studio legame fra produttività e livello di studio giovan######i
prod_studioyoung_paese <- merge(p_l,prod,by.x="GEO",by.y="PAESE")

x <- prod_studioyoung_paese$Value
y <- prod_studioyoung_paese$PROD

res5 <- cor.test(x, y, method=c("pearson"))
str(res5)

ggscatter(prod_studioyoung_paese, x = "Value", y = "PROD",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Livello di studio giovani", ylab = "Produttività")











