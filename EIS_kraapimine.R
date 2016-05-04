library(rvest)

#url="http://eelnoud.valitsus.ee/main/mount/rss/home/submission.rss"
url="http://eelnoud.valitsus.ee/main/mount/rss/home/review.rss"

algleht <- read_html(url)

#siit saab kõigi eelnõudel lingid kätte
lingid=algleht%>%
  html_nodes("body channel item guid")%>%
  html_text()

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
  for (i in 644:length(lingid)) {
  leht <- read_html(lingid[i])
  temp=leht%>%
    html_nodes("#content > div > div > table > tbody > tr ")%>%
    html_text()
  data[[i]]=temp
  
  pealktemp=leht%>%
    html_nodes("#content > div.heading.clear > div > h1")%>%
    html_text()
  pealkiri[[i]]=pealktemp
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


