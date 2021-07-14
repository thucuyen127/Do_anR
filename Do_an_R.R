library(tidyverse)
library(dplyr)
Confi = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)
Dea = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)
Reco = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", header =  TRUE
                )
#Group
Confirmed_nn = Confi %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths_nn = Dea %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered_nn = Reco %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
Cdr_join = full_join(Confirmed_nn, deaths_nn) %>% full_join(recovered_nn)
Cdr_join$date = Cdr_join$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
Cdr_join = Cdr_join %>% group_by(Country.Region) %>% mutate( day = date - first(date) + 1)
allworld = Cdr_join %>% group_by(date) %>% summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>% mutate(day = date - first(date) + 1)
country = Cdr_join %>% group_by(Country.Region) %>% summarize(cumconfirmed = max(confirmed), cumdeaths = max(deaths), cumrecovered = max(recovered))


world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)


library(ggplot2)


#plot4

dt_Confirmed = dt %>% summarise(Confirmed = sum(Confirmed))
dt_Confirmed = dt_Confirmed[order(-dt_Confirmed$Confirmed),]
dt_Confirmed_n = dt_Confirmed[1:10,]
others_Confirmed=sum(dt_Confirmed$Confirmed)-sum(dt_Confirmed_n$Confirmed)
others_Confirmed=data.frame("Others", others_Confirmed)
names(others_Confirmed)<-c("Country_Region","Confirmed")
dt_Confirmed <- rbind(dt_Confirmed_n, others_Confirmed)
dt_Confirmed$prop = sprintf((dt_Confirmed$Confirmed / sum(dt_Confirmed$Confirmed) *100), fmt = '%#.2f')
ggplot(dt_Confirmed,aes(x="",y = Confirmed,fill=Country_Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("Biểu đồ tròn thể hiện tỉ lệ nhiễm covid của các nước") +
  theme_void()



#plot5

dt_Deaths = dt %>% summarise(Deaths = sum(Deaths))
dt_Deaths = dt_Deaths[order(-dt_Deaths$Deaths),]
dt_Deaths_n = dt_Deaths[1:10,]
others_Deaths=sum(dt_Deaths$Deaths)-sum(dt_Deaths_n$Deaths)
others_Deaths=data.frame("Others", others_Deaths)
names(others_Deaths)<-c("Country_Region","Deaths")
dt_Deaths <- rbind(dt_Deaths_n, others_Deaths)
dt_Deaths$prop = sprintf((dt_Deaths$Deaths / sum(dt_Deaths$Deaths) *100), fmt = '%#.2f')
ggplot(dt_Deaths,aes(x="",y = Deaths,fill=Country_Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("Biểu đồ tròn thể hiện tỉ lệ tử vong do  covid ở các nước") +
  theme_void()



#plot6
dt_Recovered = dt %>% summarise(Recovered = sum(Recovered))
dt_Recovered = dt_Recovered[order(-dt_Recovered$Recovered),]
dt_Recovered_n = dt_Recovered[1:10,]
others_Recovered=sum(dt_Recovered$Recovered)-sum(dt_Recovered_n$Recovered)
others_Recovered=data.frame("Others", others_Recovered)
names(others_Recovered)<-c("Country_Region","Recovered")
dt_Recovered <- rbind(dt_Recovered_n, others_Recovered)
dt_Recovered$prop = sprintf((dt_Recovered$Recovered / sum(dt_Recovered$Recovered) *100), fmt = '%#.2f')
ggplot(dt_Recovered,aes(x="",y = Recovered,fill=Country_Region)) +
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(label = prop),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer(palette="Set3")+
  ggtitle("Biểu đồ tròn thể hiện tỉ lệ chữa khỏi covid ở các nước") +
  theme_void()


#plot7
vietnam =  Cdr_join %>% filter(Country.Region=="Vietnam")
ggplot()+
  geom_line(data = vietnam, aes(x= date, y= confirmed ),size = 2,color = 'black') + 
  geom_line(data = vietnam, aes(x= date, y= deaths),size = 2,color = "red")+ 
  geom_line(data = vietnam, aes(x= date, y= recovered),size = 2,color = 'green') +
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid tại việt nam")+
  theme_light()


#plot8
us = Cdr_join %>% filter(Country.Region =="US")
ggplot()+
  geom_line(data = us, aes(x= date, y= confirmed ),size = 2,color = 'black') + 
  geom_line(data = us, aes(x= date, y= deaths),size = 2,color = "red")+ 
  geom_line(data = us, aes(x= date, y= recovered),size = 2,color = 'green') +
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid tại Mỹ")+
  theme_light()





#plot9

ggplot(world, aes(x=days, y=confirmed)) + geom_smooth() + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Tình hình nhiễm Covid trên thế giới", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))


#Plot10

Cdrjoin = Cdr_join[order(-Cdr_join$day),]
Cdrjoin1 = Cdrjoin %>% group_by(date) %>% summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>% mutate(day = date - first(date) + 1)
ggplot()+
  geom_line(data = Cdrjoin1, aes(x= date, y= confirmed ),size = 2,color = 'black') + 
  geom_line(data = Cdrjoin1, aes(x= date, y= deaths),size = 2,color = "red")+ 
  geom_line(data = Cdrjoin1, aes(x= date, y= recovered),size = 2,color = 'green') +
  xlab("Thời gian")+
  ylab("Số ca")+
  ggtitle("Tình hình covid trên thế giới")+
  theme_light()

#plot11
world %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=days, y=Cases, colour=Type)) + geom_line() +
  theme_classic() +
  labs(title = "Biểu đồ line thể hiện tình trạng Covid trên thế giới", x= "Days", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5))


#plot12
df = read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-31-2020.csv',header = TRUE)
View(df)

#Group
dt = df %>% group_by(Country_Region)
dtt = dt %>% summarise(Confirmed = sum(Confirmed),Deaths = sum(Deaths),Recovered = sum(Recovered))
dtt = dtt[order(-dtt$Confirmed),]
for (i in 1:7){
  dtt$Recovered[i] = dtt$Recovered[i] - dtt$Deaths[i]
  dtt$Confirmed[i] = dtt$Confirmed[i] - dtt$Deaths[i] - dtt$Recovered[i]
}
cot1 = c(rep(dtt$Country_Region[1],3),
         rep(dtt$Country_Region[2],3),
         rep(dtt$Country_Region[3],3),
         rep(dtt$Country_Region[4],3),
         rep(dtt$Country_Region[5],3),
         rep(dtt$Country_Region[6],3),
         rep(dtt$Country_Region[7],3)
)
cot2 = rep(c("Số ca còn nhiễm","Số ca chữa khỏi","Số ca tử vong"),7)
cot3 = c(dtt$Confirmed[1],dtt$Recovered[1],dtt$Deaths[1],
         dtt$Confirmed[2],dtt$Recovered[2],dtt$Deaths[2],
         dtt$Confirmed[3],dtt$Recovered[3],dtt$Deaths[3],
         dtt$Confirmed[4],dtt$Recovered[4],dtt$Deaths[4],
         dtt$Confirmed[5],dtt$Recovered[5],dtt$Deaths[5],
         dtt$Confirmed[6],dtt$Recovered[6],dtt$Deaths[6],
         dtt$Confirmed[7],dtt$Recovered[7],dtt$Deaths[7])
dtt_n = data.frame(cot1,cot2,cot3)
ggplot(dtt_n, aes(fill=cot2, y=cot3, x=cot1)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Quốc gia")+
  ylab("số ca")+
  ggtitle("Tổng số ca tử vong, chữa khỏi, còn nhiễm covid 19 của 7 quốc gia có số ca nhiễm nhiều nhất")+
  theme(legend.position = "bottom")


#Plot13

Cdr_join <- Cdr_join %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed),
                                                             days = date - first(date) + 1)
world <- Cdr_join %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
str(world)
world %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=days, y=Cases, colour=Type)) + geom_bar(stat="identity", width=0.1, fill="white") +
  theme_classic() +
  labs(title = "Biểu đồ thể hiện tình trạng tử vong và phục hồi của Covid_19", x= "Days", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.1))



#plot14

countryselection <- country %>% filter(Country.Region==c("US", "Italy", "China", "France", "United Kingdom", "Germany"))
ggplot(countryselection, aes(x=date, y=confirmed, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")



#plot15
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(countryselection, aes(x=day, y=confirmed, )) + 
  labs(
    title="Tình hình nhiễm Covid ở một số nước")

g + geom_jitter(aes(col = Country.Region )) + 
  geom_smooth(aes(col = Country.Region), method="lm", se=F)


#plot16
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(tidyverse)

theme_set(theme_minimal())
#ConfirmDataset
confirm_url<-'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirm <- read_csv(confirm_url)
confirm <- confirm %>%
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long),
               names_to = "date",
               values_to = "confirmed_n"
  ) %>%
  select(-c(Lat, Long)) %>%
  rename(
    province_state = `Province/State`,
    country_region = `Country/Region`
  ) %>%
  mutate(date = mdy(date)) %>%
  group_by(country_region, date) %>%
  summarise(confirmed_n = sum(confirmed_n)) %>%
  ungroup()
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>%
  ungroup()
confirm <- confirm %>%
  arrange(date) %>%
  group_by(country_region) %>%
  ungroup()
#Brazil
bra<-confirm %>% filter(country_region == "Brazil")
bra <-bra[c('country_region','new_cases_n')]
vio<-bra

#US
us<-confirm %>% filter(country_region == "US")
us <-us[c('country_region','new_cases_n')]
vio<-rbind(vio, us) 

#Fig
vio %>%
  ggplot( aes(x=country_region, y=new_cases_n, fill=country_region)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("So sánh tình hình nhiễm Covid ở Mỹ và Brazil") +
  xlab("")


#plot 17
dt_Confirmed_Deaths = dt %>% summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths))
dt_Confirmed_Deaths = dt_Confirmed_Deaths[order(-dt_Confirmed_Deaths$Confirmed),]
dt_Confirmed_Deaths_n = dt_Confirmed_Deaths[1:10,]
View(dt_Confirmed_Deaths_n)
ggplot(dt_Confirmed_Deaths_n,aes(x = Confirmed, y = Deaths,fill = Country_Region))+
  geom_point(size = 3.5 , aes(col = Country_Region))+
  xlab("Số ca nhiễm")+ylab("số ca tử vong")+
  labs(title = "Biểu đồ thể hiện tương quan giữa số ca nhiễm và ca tử vong của 10 nước có số ca nhiễm nhiều nhất vào 31/3/2020 ")+
  theme(legend.position = "bottom")


#plot18

dt_Confirmed_Deaths = dt %>% summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths))
dt_Confirmed_Deaths = dt_Confirmed_Deaths[order(-dt_Confirmed_Deaths$Confirmed),]
dt_Confirmed_Deaths_n = dt_Confirmed_Deaths[1:10,]
View(dt_Confirmed_Deaths_n)
ggplot(dt_Confirmed_Deaths_n,aes(x = Confirmed, y = Deaths,fill = Country_Region))+
  geom_point(size = 3.5 , aes(col = Country_Region))+
  xlab("Số ca nhiễm")+ylab("số ca tử vong")+
  labs(title = "Biểu đồ thể hiện tương quan giữa số ca nhiễm và ca tử vong của 10 nước có số ca nhiễm nhiều nhất vào 31/3/2020 ")+
  theme(legend.position = "bottom") +
  theme(legend.background = element_rect(fill="green",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"))


#plot19

g <- ggplot(countryselection, aes(x= day))
g + geom_density(aes(fill = Country.Region), alpha=0.5) + 
  labs(title="Density plot", 
       subtitle="Tình hình Covid ở các nước",
       caption="Source: Covid",
       fill="# Country")

#Plot 20
g = ggplot(allworld, aes(x = date)) + 
  geom_line(aes(y = confirmed)) + 
  labs(title="Time Series Chart", 
       subtitle="Tình hình nhiễm Covid trên thế giới từ 22/01/2020 đến 12/07/2021",
       y="Số ca")

g + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

g + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes") 

#plot21
g = ggplot(allworld, aes(x = deaths)) + 
  geom_line(aes(y = confirmed), col = 'red') + 
  labs(title="Time Series Chart", 
       subtitle="Tình hình tử vong do Covid trên thế giới từ 22/01/2020 đến 12/07/2021", 
       caption="Source: Economics", 
       y="Số ca")

#Plot22

dt_Deaths = dt %>% summarise(Deaths = sum(Deaths))
dt_Deaths = dt_Deaths[order(-dt_Deaths$Deaths),]
dt_Deaths_n = dt_Deaths[1:10,]


ggplot(dt_Deaths_n,aes(x =reorder(Country_Region,-Deaths),y = Deaths))+
  geom_col(color = 'red',fill = "red")+
  xlab("quốc gia")+
  ylab("số ca mắc")+
  labs(title = "Top 10 nước có số ca tử vong nhiều nhất theo thứ tự giảmm dần vào 31/3/2020")



#plot13
dt_Confirmed = dt %>% summarise(Confirmed = sum(Confirmed))
dt_Confirmed = dt_Confirmed[order(-dt_Confirmed$Confirmed),]
dt_Confirmed_n = dt_Confirmed[1:10,]
View(dt_Confirmed_n)


ggplot(dt_Confirmed_n,aes(x =reorder(Country_Region,-Confirmed),y = Confirmed))+
  geom_col(color = 'red',fill = "black")+
  xlab("quốc gia")+
  ylab("số ca mắc")+
  labs(title = "Top 10 nước có số ca nhiễm nhiều nhất theo thứ tự giảm dần vào 31/3/2020")

