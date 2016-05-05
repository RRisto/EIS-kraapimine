library(rvest)

#url="http://eelnoud.valitsus.ee/main/mount/rss/home/submission.rss"
url="http://eelnoud.valitsus.ee/main/mount/rss/home/review.rss"

algleht <- read_html(url)

#siit saab kõigi eelnõudel lingid kätte
lingid=algleht%>%
  html_nodes("body channel item guid")%>%
  html_text()
#linkitest duplikaadid välja
lingid_algne=lingid
lingid2=gsub("\\?activity=\\d", "", lingid)
lingid2=unique(lingid2)
#
url2=lingid[2]
url3=lingid[9]
url8=lingid[4578]
# url5=lingid[55]
#url8=lingid[4062]
leht1 <- read_html(url2)
leht2 <- read_html(url3)
# leht3=read_html(url4)
# leht4=read_html(url5)
leht8=read_html(url8)

#töötab
kooskõlastused=leht2%>%
  html_nodes("#content > div > div > table > tbody > tr")%>%
  html_text()

pealkiri=leht2%>%
  html_nodes("#content > div.heading.clear > div > h1")%>%
  html_text()

data=list()
pealkiri=list()
##loopi proov
for (i in 1:100) {
  leht <- read_html(lingid[i])
  temp=leht%>%
    html_nodes("#content > div > div > table > tbody > tr >td")%>%
    html_text()
  data[[i]]=temp
  
  pealktemp=leht%>%
    html_nodes("#content > div.heading.clear > div > h1")%>%
    html_text()
  pealkiri[[i]]=pealktemp
}

#loobin kraabitud andmed struktuuri
kooskolastaja=c()
tahtaeg=c()
vastamisaeg=c()
markused=c()
aktinimi=c()
link=c()

for (i in 1:length(data)) {
  for (j in 1:(length(data[[i]])/6)) {
    kooskolastaja=append(kooskolastaja, data[[i]][(j-1)*6+2])
    tahtaeg=append(tahtaeg, data[[i]][(j-1)*6+3])
    vastamisaeg=append(vastamisaeg, data[[i]][(j-1)*6+4])
    markused=append(markused, data[[i]][(j-1)*6+5])
    aktinimi=append(aktinimi, pealkiri[[i]][1])
    link=append(link, lingid[i])
  }
}


koikandmed=data.frame(kooskolastaja, tahtaeg, vastamisaeg, markused, aktinimi,
                      link)


#variant 2, parsin listi kogu sodi, selt grepin välja
data=list()
pealkiri=list()
##loopi proov
#puhasta lingid ära!!!!
# for (i in 1:length(lingid)) {
  for (i in 2459:nrow(lingid)) {
  leht <- read_html(lingid[i])
  temp=leht%>%
    html_nodes("#content > div > div > table > tbody > tr ")%>%
    html_text()
  data[[i]]=temp
  
  pealktemp=leht%>%
    html_nodes("#content > div.heading.clear > div > h1")%>%
    html_text()
  pealkiri[[i]]=pealktemp
  print(i)
}

#eemdladab elemendid, mis on tühjad
p=lapply(data, function(x) x[x != ""])
p=lapply(p, function(x) x[grepl("Vaata andmeid|seaduse",x)==F])

#proovime nüüd listida
esitaja=lapply(p, function(x) unlist(strsplit(x, "TĆ¤htaeg:"))[1])

esitaja2=c()
for (i in 1:length(esitaja)) {
  esitaja2[i]=esitaja[[i]][1]
}
#muu sodi, et edasi puhastada
muusodi=lapply(p, function(x) unlist(strsplit(x, "TĆ¤htaeg:"))[2])
tahtaeg=lapply(muusodi, function(x) unlist(strsplit(x, "Vastatud:"))[1])
tahtaeg2=c()
for (i in 1:length(tahtaeg)) {
  tahtaeg2[i]=tahtaeg[[i]][1]
}

##asutus
tulem=unlist(p)
#asutus=strsplit(tulem, "TĆ¤htaeg:|Vastatud:")
asutus=strsplit(tulem, "TĆ¤htaeg:")

asutus2=c()
for (i in 1:length(asutus)) {
  asutus2[i]=asutus[[i]][1]
}

#ülejäänud sodi
muu=c()
for (i in 1:length(asutus)) {
  muu[i]=asutus[[i]][2]
}

#tähtaeg
sodi=strsplit(muu, "[A-Z]")
tahtaeg=c()
for (i in 1:length(sodi)) {
  tahtaeg[i]=sodi[[i]][1]
}
library(stringr)
tahtaeg=str_trim(tahtaeg, side = c("both"))
tahtaeg_aeg=strptime(tahtaeg, "%d.%m.%Y %H:%M")
#eemaldame muust nüüd aja
vastus=c()
for (i in 1:length(muu)) {
  vastus[i]=gsub(tahtaeg[i], "", muu[i])
}
vastus=str_trim(vastus, side = c("both"))


#dfiks
data_df=data.frame(asutus2, tahtaeg_aeg, vastus)
#ainult JUMi asjad
jum=data_df[grepl("Justiits",data_df$asutus2),]
jum=jum[!grepl("Vastatud",jum$asutus2),]
jum=jum[!grepl("Uus",jum$asutus2),]

#vastus_puhas
jum$vastusPuhas=NA
for(i in 1:nrow(jum)){
  if (grepl("KooskĆµlastatud mĆ¤rkustega", jum$vastus[i])) {
    jum$vastusPuhas[i]="KooskĆµlastatud mĆ¤rkustega"
  }  else if (grepl("Mitte kooskĆµlastatud", jum$vastus[i])) {
    jum$vastusPuhas[i]="Mitte kooskĆµlastatud"
  } else if (grepl("KooskĆµlastatud", jum$vastus[i])) {
    jum$vastusPuhas[i]="KooskĆµlastatud"
  } else {
      next
    }
}

#puhastame vastamise aja välja
jum$vastamise_aeg=NA
jum$vastamise_aeg=gsub("[A-Z]", "", jum$vastus)
jum$vastamise_aeg=gsub("[a-z]", "", jum$vastamise_aeg)
jum$vastamise_aeg=gsub("Ć¤", "", jum$vastamise_aeg)
jum$vastamise_aeg=gsub("Ć¼", "", jum$vastamise_aeg)
jum$vastamise_aeg=gsub("Ćµ", "", jum$vastamise_aeg)
jum$vastamise_aeg=gsub(": ", "", jum$vastamise_aeg)
jum$vastamise_aeg=str_trim(jum$vastamise_aeg, side=c("both"))

jum$vastamise_aeg2=strptime(jum$vastamise_aeg, "%d.%m.%Y %H:%M")
#leiame ridade arvu, kus jum on hiljaks jäänud
jum$hilinemine=NA
for (i in 1:nrow(jum)) {
  if (!is.na(jum$vastamise_aeg2[i])) {
    jum$hilinemine[i]=jum$tahtaeg_aeg[i]<jum$vastamise_aeg2[i]
  }
}

#sordime tähtaja järgi
jum=jum[order(jum$tahtaeg_aeg, decreasing = T),]
jum_algne=jum
jum=jum[1:1276,]
#mitu korda hilineti
sum(jum$hilinemine, na.rm = T)
sum(jum$hilinemine, na.rm = T)/(nrow(jum)-3) #%

#kui võtame tuhende
sum(jum$hilinemine[1:1000], na.rm = T)/1000 #%
sum(jum$hilinemine[1:500], na.rm = T)/500 #%
sum(jum$hilinemine[1:100], na.rm = T)/100 #%
sum(jum$hilinemine[1:50], na.rm = T)/50 #%
#hilinetud päevade arv
jum$hilinemise_kestvus=NA
for (i in 1:nrow(jum)) {
  if (!is.na(jum$hilinemine[i])) {
  if (jum$hilinemine[i]==T) {
    jum$hilinemise_kestvus[i]=jum$vastamise_aeg2[i]-jum$tahtaeg_aeg[i]
  } 
    }else {
    next
  }
}

#keskmine hilinemine
median(jum$hilinemise_kestvus, na.rm = T)
mean(jum$hilinemise_kestvus, na.rm = T)
max(jum$hilinemise_kestvus, na.rm = T)
#1000
median(jum$hilinemise_kestvus[1:1000], na.rm = T)
#lühema perioodi kohta, 500
median(jum$hilinemise_kestvus[1:500], na.rm = T)
mean(jum$hilinemise_kestvus[1:500], na.rm = T)
max(jum$hilinemise_kestvus[1:500], na.rm = T)

#100 viimast
median(jum$hilinemise_kestvus[1:100], na.rm = T)
mean(jum$hilinemise_kestvus[1:100], na.rm = T)
max(jum$hilinemise_kestvus[1:100], na.rm = T)

##############nüüd sama asi teiste minnide, va jum
#dfiks
data_df=data.frame(asutus2, tahtaeg_aeg, vastus)
#ainult JUMi asjad
mittejum=data_df[!grepl("Justiits",data_df$asutus2),]
mittejum=mittejum[!grepl("Vastatud",mittejum$asutus2),]

#vastus_puhas
mittejum$vastusPuhas=NA
for(i in 1:nrow(mittejum)){
  if (grepl("KooskĆµlastatud mĆ¤rkustega", mittejum$vastus[i])) {
    mittejum$vastusPuhas[i]="KooskĆµlastatud mĆ¤rkustega"
  }  else if (grepl("Mitte kooskĆµlastatud", mittejum$vastus[i])) {
    mittejum$vastusPuhas[i]="Mitte kooskĆµlastatud"
  } else if (grepl("KooskĆµlastatud", mittejum$vastus[i])) {
    mittejum$vastusPuhas[i]="KooskĆµlastatud"
  } else {
    next
  }
}

#puhastame vastamise aja välja
mittejum$vastamise_aeg=NA
mittejum$vastamise_aeg=gsub("[A-Z]", "", mittejum$vastus)
mittejum$vastamise_aeg=gsub("[a-z]", "", mittejum$vastamise_aeg)
mittejum$vastamise_aeg=gsub("Ć¤", "", mittejum$vastamise_aeg)
mittejum$vastamise_aeg=gsub("Ć¼", "", mittejum$vastamise_aeg)
mittejum$vastamise_aeg=gsub("Ćµ", "", mittejum$vastamise_aeg)
mittejum$vastamise_aeg=gsub(": ", "", mittejum$vastamise_aeg)
mittejum$vastamise_aeg=str_trim(mittejum$vastamise_aeg, side=c("both"))

mittejum$vastamise_aeg2=strptime(mittejum$vastamise_aeg, "%d.%m.%Y %H:%M")
#leiame ridade arvu, kus mittejum on hiljaks jäänud
mittejum$hilinemine=NA
for (i in 1:nrow(mittejum)) {
  if (!is.na(mittejum$vastamise_aeg2[i])) {
    mittejum$hilinemine[i]=mittejum$tahtaeg_aeg[i]<mittejum$vastamise_aeg2[i]
  }
}

#tähtaja järgi järjekorda
mittejum=mittejum[order(mittejum$tahtaeg_aeg, decreasing = T),]
mittejum_algne=mittejum
mittejum=mittejum[1:12297,] #mingi sodi on ikka sees, võtan selle välja
#mitu korda hilineti
sum(mittejum$hilinemine, na.rm = T)
sum(mittejum$hilinemine, na.rm = T)/nrow(mittejum) #%

#kui võtame tuhende kuupäeva jumi järgi
jum1000=min(which(mittejum$tahtaeg_aeg==jum[1000, "tahtaeg_aeg"]))
sum(mittejum$hilinemine[1:jum1000], na.rm = T)/jum1000 #%
jum500=min(which(mittejum$tahtaeg_aeg==jum[500, "tahtaeg_aeg"]))
sum(mittejum$hilinemine[1:jum500], na.rm = T)/jum500 #%
jum100=min(which(mittejum$tahtaeg_aeg==jum[100, "tahtaeg_aeg"]))
sum(mittejum$hilinemine[1:jum100], na.rm = T)/jum100 #%
jum50=min(which(mittejum$tahtaeg_aeg==jum[50, "tahtaeg_aeg"]))
sum(mittejum$hilinemine[1:jum50], na.rm = T)/jum50 #%
#hilinetud päevade arv
mittejum$hilinemise_kestvus=NA
for (i in 1:nrow(mittejum)) {
  if (!is.na(mittejum$hilinemine[i])) {
    if (mittejum$hilinemine[i]==T) {
      mittejum$hilinemise_kestvus[i]=mittejum$vastamise_aeg2[i]-mittejum$tahtaeg_aeg[i]
    } 
  }else {
    next
  }
}


#keskmine hilinemine
median(mittejum$hilinemise_kestvus, na.rm = T)
mean(mittejum$hilinemise_kestvus, na.rm = T)
max(mittejum$hilinemise_kestvus, na.rm = T)

#1000 päeva   
median(mittejum$hilinemise_kestvus[1:jum1000], na.rm = T)

#lühema perioodi kohta, 500
median(mittejum$hilinemise_kestvus[1:jum500], na.rm = T)
mean(mittejum$hilinemise_kestvus[1:jum500], na.rm = T)
max(mittejum$hilinemise_kestvus[1:jum500], na.rm = T)

#100 viimast
median(mittejum$hilinemise_kestvus[1:jum100], na.rm = T)
mean(mittejum$hilinemise_kestvus[1:jum100], na.rm = T)
max(mittejum$hilinemise_kestvus[1:jum100], na.rm = T)

