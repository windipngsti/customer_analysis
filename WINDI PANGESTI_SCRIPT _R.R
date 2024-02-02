# ANALISIS PELANGGAN BRAND A

##install package/library 
#install.packages("foreign")
#install.packages("dplyr")
#install.packages("psych")

library(foreign)
library(ggplot2)
library(dplyr)
library(psych)

# Import data
data <- read.spss("dataset_tes_data_analyst_frontier.sav", to.data.frame = TRUE)
View(data)

# Data 
str(data) ## data terdiri dari 300 baris, 12 variabel dan tipe data sudah sesuai
summary(data) 

# check missing value pada data
colSums(is.na(data)) 


## 1
## Distribusi pelanggan berdasarkan sex
# Data transformation
sex_grup <- data %>% 
  group_by(sex) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(persentase = scales::percent(round((n / sum(n)),2)))

# Visualisasi
ggplot(sex_grup, aes(x = "", y = persentase, fill = sex)) + 
  geom_col() +
  geom_text(aes(label = persentase),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+theme_void() +
  labs(title="Total pelanggan berdasarkan gender") +
  scale_fill_manual(values=c("#F9AB40","#6495ED"))+
  theme(plot.title=element_text(hjust=0.5))

## Distribusi pelanggan berdasarkan Kota
# Data transformation
kota_grup<-data %>% 
  group_by(kota, sex) %>% 
  count() 

# Visualisasi
ggplot(kota_grup, aes(kota,n,fill = sex))+ 
  geom_col() + theme_minimal()+
  geom_text(aes(label= n),size=3,hjust=0.5,vjust=3,position="stack")+
  scale_fill_manual(values=c("#6495ED","#F9AB40"))+
  ggtitle("Total pelanggan berdasarkan kota")+
  theme(plot.title=element_text(hjust=0.5))

## Distribusi pelanggan berdasarkan Status
# Data transformasi
status_grup <- data %>% 
  group_by(status) %>%
  count() %>% 
  ungroup() %>% 
  mutate(persentase = scales::percent(round((n / sum(n)),2)))

# Visualisasi
ggplot(status_grup, aes(x= reorder(status,-(n / sum(n))), y = persentase, fill = status))+ 
  geom_col(width=0.75) + theme_minimal() +
  geom_text(aes(label= persentase),size=3,vjust=-0.5)+
  theme(legend.position =  "none")+
  scale_fill_brewer(palette = "Set2")+
  labs(y= "Total pelanggan (%)", x = "Status")+
  labs(title = "Persentase pelanggan berdasarkan status")


## Distribusi pelanggan berdasarkan usia
# Data transformasi
usiar_grup <- data %>% 
  group_by(usiar) %>% 
  count()

# Visualisasi
ggplot(usia_group, aes(x=reorder(usiar,-n),y=n,fill = usiar))+ 
  geom_col(width=0.75)+ 
  geom_text(aes(label= n),size=3,vjust=-0.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Set2")+
  labs(y= "Total pelanggan", x = "Rentang usia")+
  labs(title = "Total pelanggan berdasarkan usia")

# Total pelanggan berdasarkan Tingkat pendidikan
didik_grup <- data %>% 
  group_by(didik) %>% 
  count()
  
# Visualisasi
ggplot(didik_grup, aes(x=reorder(didik,n),y=n,fill = didik))+ 
  geom_col(width=0.75)+
  geom_text(aes(label= n),size=3,hjust=-0.5,position="stack")+
  theme_minimal()+
  theme(legend.position =  "none")+
  scale_fill_brewer(palette = "Set2")+
  labs(y= "Total pelanggan", x = "Tingkat pendidikan")+
  labs(title = "Total pelanggan berdasarkan tingkat pendidikan")+
  coord_flip()

# No.2
# Uji Validitas
# Transformasi data kategorik 
data <- data %>% 
  mutate(skor_lay = case_when(puas_lay== "Very Much Dissatisfied"~ 1,
                              puas_lay== "Dissatisfied"~ 2,
                              puas_lay== "Neutral"~ 3,
                              puas_lay== "Satisfied"~ 4,
                              puas_lay== "Very Much Satisfied"~ 5),
         skor_kua = case_when(puas_kua== "Very Much Dissatisfied"~ 1,
                              puas_kua== "Dissatisfied"~ 2,
                              puas_kua== "Neutral"~ 3,
                              puas_kua== "Satisfied"~ 4,
                              puas_kua== "Very Much Satisfied"~ 5),
         skor_harga = case_when(puas_pri== "Very Much Dissatisfied"~ 1,
                                puas_pri== "Dissatisfied"~ 2,
                                puas_pri== "Neutral"~ 3,
                                puas_pri== "Satisfied"~ 4,
                                puas_pri== "Very Much Satisfied"~ 5),
         skor_all = case_when(puas_all== "Very Much Dissatisfied"~ 1,
                                puas_all== "Dissatisfied"~ 2,
                                puas_all== "Neutral"~ 3,
                                puas_all== "Satisfied"~ 4,
                                puas_all== "Very Much Satisfied"~ 5))

skor_kepuasan <- data.frame(skor_lay = data$skor_lay,skor_kua = data$skor_kua, skor_harga = data$skor_harga)

# Menghitung total skor

skor_kepuasan <- skor_kepuasan %>%
  mutate(total_skor = skor_lay + skor_kua + skor_harga)

View(skor_kepuasan)

# Uji validitas
correlation = round(cor(skor_kepuasan),2)
correlation

validitas_lay <- cor.test(skor_kepuasan$skor_lay, skor_kepuasan$total_skor)
validitas_kua <- cor.test(skor_kepuasan$skor_kua, skor_kepuasan$total_skor)
validitas_harga <- cor.test(skor_kepuasan$skor_harga, skor_kepuasan$total_skor)

validitas_lay
validitas_kua
validitas_harga
# nilai rhitung>rtabel, Variabel berpengaruh signifikan pada tingkat kepuasan pelanggan dengan baik


# Uji reliabilitas
# batas alpha 0.6
skor_kepuasan%>% select(-total_skor) %>% alpha()
# nilai alpha cronbach ) > 0,6, variabel reliabel

## NO.3
## 3a
## rata-rata tingkat kepuasan
cat("rata-rata tingkat kepuasan konsumen terhadap pelayanan :", round(mean(data$skor_lay),2), 
    "\nrata-rata tingkat kepuasan konsumen terhadap kualitas produk :", round(mean(data$skor_kua),2),
    "\nrata-rata tingkat kepuasan konsumen terhadap harga produk :", round(mean(data$skor_harga),2), 
    "\nrata-rata tingkat kepuasan konsumen secara keseluruhan :", round(mean(data$skor_all),2))

# Tingkat kepuasan Gabungan 
# Data transformation
layanan<-data %>% 
  group_by(level =puas_lay) %>% 
  count(name = "jumlah")%>%
  ungroup() %>% 
  mutate(persentase = scales::percent(round((jumlah / sum(jumlah)),3)))%>%
  mutate(kepuasan= rep("Pelayanan", n()))

kualitas<-data %>% 
  group_by(level =puas_kua) %>% 
  count(name = "jumlah")%>%
  ungroup() %>% 
  mutate(persentase = scales::percent((jumlah / sum(jumlah))))%>%
  mutate(kepuasan= rep("Kualitas produk", n()))

harga<-data %>% 
  group_by(level =puas_pri) %>% 
  count(name = "jumlah")%>%
  ungroup() %>% 
  mutate(persentase = scales::percent((jumlah / sum(jumlah))))%>%
  mutate(kepuasan= rep("Harga produk", n()))

all<-data %>% 
  group_by(level =puas_all) %>% 
  count(name = "jumlah")%>%
  ungroup() %>% 
  mutate(persentase = scales::percent((jumlah / sum(jumlah))))%>%
  mutate(kepuasan= rep("Keseluruhan", n()))

combined_df <- rbind(layanan, kualitas, harga, all)
View(combined_df)

# Visualisasi
ggplot(combined_df, aes(x=jumlah,y=kepuasan,fill =level))+ 
  geom_col(width=0.7) + theme_minimal()+
  geom_text(aes(label= paste(persentase, "\n(", jumlah, ")")),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_brewer(palette = "Pastel2")+
  ggtitle("Tingkat kepuasan pelanggan")+
  theme(plot.title=element_text(hjust=0.5))

## Tingkat kepuasan perkota
tingkat_kepuasan_perkota <- data %>% 
  group_by(kota, puas_all) %>%
  count(name = "jumlah")  %>%
  group_by(kota) %>%
  mutate(perc=jumlah / sum(jumlah))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi

ggplot(tingkat_kepuasan_perkota, aes(x=perc,y=kota,fill = puas_all))+ 
  geom_col(width=0.7) + theme_minimal()+ 
  geom_text(aes(label= paste(persentase, "\n(", jumlah, ")")),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_brewer(palette = "Pastel2")+
  labs(title="Tingkat kepuasan pelanggan tiap kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

## Tingkat kepuasan palnggan masing-masing kota
## Tingkat kepuasan thd pelayanan perkota
puas_lay_perkota <- data %>% 
  group_by(kota, puas_lay) %>%
  count(name = "jumlah")  %>%
  group_by(kota) %>%
  mutate(perc=jumlah / sum(jumlah))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi
ggplot(puas_lay_perkota, aes(x=perc,y=kota,fill = puas_lay))+ 
  geom_col(width = 0.75) + theme_minimal()+ 
  geom_text(aes(label= paste(persentase, "\n(", jumlah, ")")),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#d6eadf","#a9d6e5","#89c2d9","#61a5c2","#2c7da0")) +  # Set custom colors
  labs(title="Tingkat kepuasan pelanggan terhadap pelayanan di setiap kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

## Tingkat kepuasan thd kualitas produk perkota
puas_kua_perkota <- data %>% 
  group_by(kota, puas_kua) %>%
  count(name = "jumlah")  %>%
  group_by(kota) %>%
  mutate(perc=jumlah / sum(jumlah))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi
ggplot(puas_kua_perkota, aes(x=perc,y=kota,fill = puas_kua))+ 
  geom_col(width=0.75) + theme_minimal()+ 
  geom_text(aes(label= paste(persentase, "\n(", jumlah, ")")),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#a9d6e5","#89c2d9","#61a5c2","#2c7da0")) +  # Set custom colors
  labs(title="Tingkat kepuasan pelanggan terhadap kualitas produk tiap kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

## Tingkat kepuasan thd harga produk perkota
puas_harga_perkota <- data %>% 
  group_by(kota, puas_pri) %>%
  count(name = "jumlah")  %>%
  group_by(kota) %>%
  mutate(perc=jumlah / sum(jumlah))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi

ggplot(puas_harga_perkota, aes(x=perc,y=kota,fill = puas_pri))+ 
  geom_col(width=0.7) + theme_minimal()+ 
  geom_text(aes(label= paste(persentase, "\n(", jumlah, ")")),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_manual(values = c("#89c2d9","#61a5c2","#2c7da0")) +  # Set custom colors
  labs(title="Tingkat kepuasan pelanggan terhadap kualitas produk tiap kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

## 3b (uji anova)'
# Transformasi data
lay <- data %>%
  select(kota, skor_lay)%>%
  rename(skor = skor_lay)
kua <- data %>%
  select(kota, skor_kua)%>%
  rename(skor = skor_kua)
harga <- data %>%
  select(kota, skor_harga)%>%
  rename(skor = skor_harga)
all <- data %>%
  select(kota, skor_all)%>%
  rename(skor = skor_all)

data_anova <- rbind(lay, kua, harga, all)

aggregate(data_anova$skor, list(data_anova$kota), FUN=mean)
mean(data_anova$skor)

# uji anova
model = aov(skor ~ kota, data = data_anova)
summary(model)

# kesimpulan
#sig (Pr) < 0.05, maka terdapat perbedaan tingkat kepuasan pada masing-masing kota

TukeyHSD(model)

#kota dengan signifikansi (p) < 0.05 terdapat perbedaan.
#maka kota dengan tingkat kepuasan yang berbeda yaitu:
#makasar dengan bandung, jakarta, medan, semarang, dan surabaya
#medan dengan bandung dan surabaya
#surabaya dengan jakarta

#VISUALISASI
## Pelanggan berdasarkan media hiburan yang digunakan
media_skor <- data %>% 
  group_by(Media,puas_all) %>% 
  count()%>%
  group_by(Media) %>%
  mutate(tot=sum(n))

# Visualisasi
ggplot(media_skor, aes(y=reorder(Media,tot),x=n,fill = puas_all))+ 
  geom_col(width=0.75) + theme_minimal()+ 
  geom_text(aes(label= n),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_brewer(palette = "Set2") + 
  labs(title="Tingkat kepuasan pelanggan terhadap kualitas produk tiap kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")


## Pelanggan setiap kota berdasarkan media hiburan yang digunakan
puas_all_by_gender <- data %>% 
  group_by(sex, status) %>% 
  count()%>%
  ungroup()%>%
  mutate(perc=n/ sum(n))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi
ggplot(puas_all_by_gender, aes(x=sex,y=persentase,fill = status))+ 
  geom_col(position = "dodge",width = 0.75) + theme_minimal()+ 
  geom_text(aes(label= persentase),size=3,vjust=-0.5,position = position_dodge(0.7))+
  scale_fill_brewer(palette = "Set2") + 
  labs(title="Distribusi status perkawinan pelanggan berdasarkan gender")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

## Tingkat pendidikan pelanggan berdasarkan kota
kota_didik <- data %>% 
  group_by(kota, didik) %>%
  count(name = "jumlah")  %>%
  group_by(kota) %>%
  mutate(perc=jumlah / sum(jumlah))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi

ggplot(kota_didik, aes(x=perc,y=kota,fill = didik))+ 
  geom_col(width=0.7) + theme_minimal()+ 
  geom_text(aes(label= persentase),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_brewer(palette = "Blues")+
  labs(title="Tingkat pendidikan berdasarkan kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

## Distribusi status pernikahan pada masing masing kota
kota_status <- data %>% 
  group_by(kota, status) %>%
  count(name = "jumlah")  %>%
  group_by(kota) %>%
  mutate(perc=jumlah / sum(jumlah))%>%
  mutate(persentase = scales::percent(perc))

# Visualisasi
ggplot(kota_status, aes(x=perc,y=kota,fill = status))+ 
  geom_col(width=0.7) + theme_minimal()+ 
  geom_text(aes(label= persentase),size=3,  position=position_stack(vjust=0.5))+
  scale_fill_brewer(palette = "Blues")+
  labs(title="Status Pernikahan berdasarkan kota")+
  theme(plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

# Boxplot usia pelanggan
boxplot(data$usia,
        main="Boxplot usia pelanggan",
        ylab="usia",
        border = "steelblue",
        col= c("#56B4E9"))

# Correlation matrix plot
corPlot(skor_kepuasan, cex = 0.6)

# Plot Tukey's HSD intervals
tukey<-TukeyHSD(model)
set=as.numeric(apply(tuk$`kota`[,2:3],1,prod)>=0)+1
op=par(mar=c(4.2,9,3.8,2))
plot(tukey,col=set,yaxt="n")
for (j in 1:length(set)){
  axis(2,at=j,labels=rownames(tuk$`kota`)[length(set)-j+1],
       las=1,cex.axis=.8,col.axis=set[length(set)-j+1])
}
par(op)
