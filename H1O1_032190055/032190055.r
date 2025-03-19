library(corrr)
library(tidyverse)
library(nycflights13)

# View(planes)

# Soru1. Havada kal???? s??resi (air_time) en fazla olan u??ak i??in ??retim y??l?? (year), ??retici (manufacturer) ve model (model) bilgisi nedir? (Kodunuzda hata al??yorsan??z year de??i??kenini verinizde kontrol ediniz!)
data1<- left_join(flights,planes,by="tailnum") 

data1 %>% filter(air_time==max(air_time,na.rm = T)) %>%
  select(year.y, manufacturer,model)

# Soru2. Havada kal???? s??resi (air_time), s??cakl??k (temp), r??zgar h??z?? (wind_speed) ve nem (humid) aras??nda ne t??r bir ili??ki vard??r? ??nceleyip yorumlay??n??z.
data2<- left_join(weather,flights,by=c("origin","year","month","day","hour"))
data2 %>% select(air_time,temp,wind_speed,humid) %>% 
  correlate() 

# Soru3. Her bir hava yolu ??irketi (name) i??in ortalama ve ortanca gecikme s??resilerini (dep_delay) inceleyip yorumlay??n??z.
data3<- right_join(airlines,flights,by="carrier")
data3 %>% filter(dep_delay>0) %>% na.omit() %>% group_by(name) %>% summarise(mean=mean(dep_delay),median=median(dep_delay))

# Soru4. flights veri setinde yer alan gecikme s??resi (dep_delay) de??i??keni i??in 2013 y??l?? ortalama gecikme s??resi ka??t??r? (erken var????lar?? dikkate almay??n??z.)
flights %>% filter(dep_delay>0) %>% summarise(mean=mean(dep_delay))

------------------------
library(Lahman)
library(tidyverse)

# Soru1. AwardsPlayers veri setini inceledi??inizde en ??ok ??d??l alan oyuncu kimdir? (People veri seti nameFirst ve nameLast de??i??kenleri ile yan??tlay??n??z.)
t<-table(AwardsPlayers$playerID) %>% as.data.frame()
names(t)<-c("playerID","num.of.aw")
data1<-People %>% left_join(t,by="playerID")    # hepsine na.omit() uygulamak?
data1 %>% filter(num.of.aw==max(num.of.aw,na.rm=T)) %>%   
  select(nameFirst,nameLast)

# Soru2. People veri setinde yer alan weight ve height de??i??kenlerini kullanarak body mass index (BMI) hesaplay??n??z. BMI form??l?? ve de??erlendirme tablosu a??a????da yer almaktad??r.
People <- data.frame(
  weight = c(150, 180, 200, 120, 160),  # pound cinsinden a????rl??k
  height = c(65, 70, 72, 60, 68)       # in?? cinsinden boy
)

# BMI hesaplama
People$BMI <- (People$weight / (People$height^2)) * 703

# S??n??fland??rma
People$BMI_class <- cut(People$BMI,
                        breaks = c(-Inf, 18.5, 24.9, 29.9, 34.9, 39.9, Inf),
                        labels = c("Under weight", "Normal weight", "Over weight", 
                                   "Obesity class 1", "Obesity class 2", "Obesity class 3"))

# Sonu??lar?? yazd??rma
print(People)

# Oyunculardan ka?? tanesi normal weight kategorisine girmektedir?
People %>% mutate(BMI=weight/(height^2)*703) %>% filter(BMI>=25 & BMI<29.9) %>% nrow()

# Soru3. Salaries ve AwardsPlayers veri setlerini inceleyiniz, en fazla ??d??l alan oyuncunun en y??ksek maa??a sahip oldu??u s??ylenebilir mi?
#soru 1 de odul sayilari hesaplandi (t)

data2<-Salaries %>% left_join(t,by="playerID") 
which.max(data2$salary)==which.max(data2$num.of.aw)
slice(data2,which.max(data2$salary))     #max salary
slice(data2,which.max(data2$num.of.aw))  #max num of awards

# Soru4. En ??ok al??nan ??d??l hangisidir?
table(AwardsPlayers$awardID) %>% as.data.frame() %>% arrange(desc(Freq))

library(ggplot2)
library(tidyverse)

# Soru1. diamonds veri setinde yer alan x, y ve z de??i??kenleri kullan??larak yeni bir t de??i??keni olu??turuluyor. t de??i??keni  t=  x^2 ??? ???y + z^(???2) ??eklinde tan??mlanacak olursa, en d??????k t de??erine kar????l??k gelen depth de??eri nedir?
diamonds %>% mutate(t=x^2-sqrt(y)+(1/z)) %>% filter(t==min(t)) %>% select(depth)

# Soru2. Pandemi nedeniyle azalan sat????lar??n?? hareketlendirmek isteyen bir ma??aza sahibi ??r??nlerine cut de??i??kenine ba??l?? olarak indirim uyguluyor. ??ndirim oranlar?? a??a????daki gibidir. ??ndirimli fiyatlar ??zerinden al????veri?? yapan bir m????teri ???E color??? ve ???SI1 clarity??? ??r??n i??in en az ka?? $ ??deyecektir?
diamonds %>% mutate(discount = case_when(
  cut=="Fair" ~ price*0.01,
  cut=="Good" ~ price*0.02,
  cut=="Very Good" ~ price*0.025,
  cut=="Premium" ~ price*0.03,
  cut=="Ideal" ~ price*0.03,
),new.price=price-discount) %>% filter(color=="E" & clarity=="SI2" & new.price==min(new.price)) 

# Alternatif
diamonds %>% mutate(discount= case_when(
  cut=="Fair" ~ price*0.1,
  cut=="Good" ~ price*0.12,
  cut=="Very Good" ~ price*0.15,
  cut=="Premium" ~ price*0.18,
  cut=="Ideal" ~ price*0.18,
),new.price=price-discount) %>% filter(color=="E"& clarity=="SI1") %>% arrange(new.price)

# Soru3. diamonds verisi clarity de??i??keni ka?? farkl?? level i??ermektedir? (Ka?? farkl?? clarity tipi vard??r?)
cl<-diamonds$clarity %>% as.factor() 
cl %>% levels()
cl %>% nlevels()

# Soru4. Ma??azan??n ???Ideal??? cut tipine sahip ??r??nlerden elde etti??i ortalama kazan?? nedir?
diamonds %>% group_by(cut) %>% summarise(mean.pr=mean(price))

# Alternatif
diamonds %>% filter(cut=="Ideal") %>% summarise(mean(price))

# Soru5. Ma??azan??n cut tipine g??re elde etti??i price ortalamalar?? incelendi??inde en az kazan?? sa??layan cut tipi hangisidir?
# dplyr paketini y??kleyin
library(dplyr)

# ??rnek veri seti (Ma??aza verisi)
# 'cut' ve 'price' s??tunlar?? oldu??unu varsayal??m
store_data <- data.frame(
  cut = c("Fair", "Good", "Very Good", "Fair", "Good", "Excellent", "Fair", "Excellent", "Very Good", "Good"),
  price = c(300, 500, 700, 250, 600, 1000, 320, 1100, 750, 550)
)

# Her bir 'cut' tipi i??in fiyat ortalamalar??n?? hesaplay??n
average_price_by_cut <- store_data %>%
  group_by(cut) %>%
  summarise(average_price = mean(price))

# En d??????k ortalama fiyata sahip 'cut' tipini bulma
lowest_avg_price_cut <- average_price_by_cut %>%
  filter(average_price == min(average_price))

# Sonu??lar?? yazd??rma
print(lowest_avg_price_cut)
