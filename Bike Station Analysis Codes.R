 library(readr) 
library(dplyr) 
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggmap)
library(ape)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#setwd("~/Documents/679-2/project/data/data_final")
station = read_csv(file = "Divvy_Stations_2015.csv")
Y2015Q1raw = read_csv(file = "Divvy_Trips_2015-Q1.csv")
# 202349 obs
Y2015Q2raw = read_csv(file = "Divvy_Trips_2015-Q2.csv")
# 893890 obs
Y2015M7raw = read_csv(file = "Divvy_Trips_2015_07.csv")
Y2015M8raw = read_csv(file = "Divvy_Trips_2015_08.csv")
Y2015M9raw = read_csv(file = "Divvy_Trips_2015_09.csv")
Y2015Q4raw = read_csv(file = "Divvy_Trips_2015_Q4.csv")
# 631365 obs
Y2015Q3raw = rbind(Y2015M7raw, Y2015M8raw, Y2015M9raw)
# 1455835 obs
Y2015raw = rbind(Y2015Q1raw, Y2015Q2raw, Y2015Q3raw, Y2015Q4raw)
# 3183439 obs


Y2015Q1 = Y2015Q1raw %>% filter(tripduration > 60 & from_station_id != to_station_id)
# 197842 obs
Y2015Q2 = Y2015Q2raw %>% filter(tripduration > 60 & from_station_id != to_station_id)
# 854564 obs
Y2015Q3 = Y2015Q3raw %>% filter(tripduration > 60 & from_station_id != to_station_id)
# 1393304 obs
Y2015Q4 = Y2015Q4raw %>% filter(tripduration > 60 & from_station_id != to_station_id)
# 617921 obs
Y2015 = Y2015raw %>% filter(tripduration > 60 & from_station_id != to_station_id)
# 3063631 obs


# separate date day month year hour minute
Y2015Q1_NEW = Y2015Q1 %>% separate(starttime, sep = " ", into = c("start_date", "start_time")) %>% separate(stoptime, sep = " ", into = c("stop_date", "stop_time")) %>% separate(start_time, sep = ":", into = c("start_hour", "start_min"), remove = F) %>% separate(stop_time, sep = ":", into = c("stop_hour", "stop_min"), remove = F) %>% separate(start_date, sep = "/", into = c("start_month", "start_day", "start_year"), remove = F) %>% separate(stop_date, sep = "/", into = c("stop_month", "stop_day", "stop_year"), remove = F)
Y2015Q2_NEW = Y2015Q2 %>% separate(starttime, sep = " ", into = c("start_date", "start_time")) %>% separate(stoptime, sep = " ", into = c("stop_date", "stop_time")) %>% separate(start_time, sep = ":", into = c("start_hour", "start_min"), remove = F) %>% separate(stop_time, sep = ":", into = c("stop_hour", "stop_min"), remove = F) %>% separate(start_date, sep = "/", into = c("start_month", "start_day", "start_year"), remove = F) %>% separate(stop_date, sep = "/", into = c("stop_month", "stop_day", "stop_year"), remove = F)
Y2015Q3_NEW = Y2015Q3 %>% separate(starttime, sep = " ", into = c("start_date", "start_time")) %>% separate(stoptime, sep = " ", into = c("stop_date", "stop_time")) %>% separate(start_time, sep = ":", into = c("start_hour", "start_min"), remove = F) %>% separate(stop_time, sep = ":", into = c("stop_hour", "stop_min"), remove = F) %>% separate(start_date, sep = "/", into = c("start_month", "start_day", "start_year"), remove = F) %>% separate(stop_date, sep = "/", into = c("stop_month", "stop_day", "stop_year"), remove = F)
Y2015Q4_NEW = Y2015Q4 %>% separate(starttime, sep = " ", into = c("start_date", "start_time")) %>% separate(stoptime, sep = " ", into = c("stop_date", "stop_time")) %>% separate(start_time, sep = ":", into = c("start_hour", "start_min"), remove = F) %>% separate(stop_time, sep = ":", into = c("stop_hour", "stop_min"), remove = F) %>% separate(start_date, sep = "/", into = c("start_month", "start_day", "start_year"), remove = F) %>% separate(stop_date, sep = "/", into = c("stop_month", "stop_day", "stop_year"), remove = F)
Y2015_NEW = Y2015 %>% separate(starttime, sep = " ", into = c("start_date", "start_time")) %>% separate(stoptime, sep = " ", into = c("stop_date", "stop_time")) %>% separate(start_time, sep = ":", into = c("start_hour", "start_min"), remove = F) %>% separate(stop_time, sep = ":", into = c("stop_hour", "stop_min"), remove = F) %>% separate(start_date, sep = "/", into = c("start_month", "start_day", "start_year"), remove = F) %>% separate(stop_date, sep = "/", into = c("stop_month", "stop_day", "stop_year"), remove = F)


# changeinto date format
Y2015Q1_NEW$start_date = as.Date(Y2015Q1_NEW$start_date, "%m/%d/%Y")
Y2015Q2_NEW$start_date = as.Date(Y2015Q2_NEW$start_date, "%m/%d/%Y")
Y2015Q3_NEW$start_date = as.Date(Y2015Q3_NEW$start_date, "%m/%d/%Y")
Y2015Q4_NEW$start_date = as.Date(Y2015Q4_NEW$start_date, "%m/%d/%Y")
Y2015_NEW$start_date = as.Date(Y2015_NEW$start_date, "%m/%d/%Y")

Y2015Q1_NEW$stop_date = as.Date(Y2015Q1_NEW$stop_date, "%m/%d/%Y")
Y2015Q2_NEW$stop_date = as.Date(Y2015Q2_NEW$stop_date, "%m/%d/%Y")
Y2015Q3_NEW$stop_date = as.Date(Y2015Q3_NEW$stop_date, "%m/%d/%Y")
Y2015Q4_NEW$stop_date = as.Date(Y2015Q4_NEW$stop_date, "%m/%d/%Y")
Y2015_NEW$stop_date = as.Date(Y2015_NEW$stop_date, "%m/%d/%Y")


Y2015_NEW = Y2015_NEW %>% mutate(start_weekday = weekdays(start_date), stop_weekday = weekdays(stop_date)) 
bike_weekday=Y2015_NEW %>% filter(start_weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")) 
bike_weekend=Y2015_NEW %>% filter(start_weekday %in% c("Saturday","Sunday"))

# time window separate
Y2015_NEW0103start = Y2015_NEW %>% filter(start_hour %in% c("1","2"))
Y2015_NEW0305start = Y2015_NEW %>% filter(start_hour %in% c("3","4"))
Y2015_NEW0507start = Y2015_NEW %>% filter(start_hour %in% c("5","6"))
Y2015_NEW0709start = Y2015_NEW %>% filter(start_hour %in% c("7","8"))
Y2015_NEW0911start = Y2015_NEW %>% filter(start_hour %in% c("9","10"))
Y2015_NEW1113start = Y2015_NEW %>% filter(start_hour %in% c("11","12"))
Y2015_NEW1315start = Y2015_NEW %>% filter(start_hour %in% c("13","14"))
Y2015_NEW1517start = Y2015_NEW %>% filter(start_hour %in% c("15","16"))
Y2015_NEW1719start = Y2015_NEW %>% filter(start_hour %in% c("17","18"))
Y2015_NEW1921start = Y2015_NEW %>% filter(start_hour %in% c("19","20"))
Y2015_NEW2123start = Y2015_NEW %>% filter(start_hour %in% c("21","22"))
Y2015_NEW2301start = Y2015_NEW %>% filter(start_hour %in% c("23","0"))

Y2015_NEW0103stop = Y2015_NEW %>% filter(stop_hour %in% c("1","2"))
Y2015_NEW0305stop = Y2015_NEW %>% filter(stop_hour %in% c("3","4"))
Y2015_NEW0507stop = Y2015_NEW %>% filter(stop_hour %in% c("5","6"))
Y2015_NEW0709stop = Y2015_NEW %>% filter(stop_hour %in% c("7","8"))
Y2015_NEW0911stop = Y2015_NEW %>% filter(stop_hour %in% c("9","10"))
Y2015_NEW1113stop = Y2015_NEW %>% filter(stop_hour %in% c("11","12"))
Y2015_NEW1315stop = Y2015_NEW %>% filter(stop_hour %in% c("13","14"))
Y2015_NEW1517stop = Y2015_NEW %>% filter(stop_hour %in% c("15","16"))
Y2015_NEW1719stop = Y2015_NEW %>% filter(stop_hour %in% c("17","18"))
Y2015_NEW1921stop = Y2015_NEW %>% filter(stop_hour %in% c("19","20"))
Y2015_NEW2123stop = Y2015_NEW %>% filter(stop_hour %in% c("21","22"))
Y2015_NEW2301stop = Y2015_NEW %>% filter(stop_hour %in% c("23","0"))

# count check_out and check_in number
#0103
Y0103out = Y2015_NEW0103start %>% count(from_station_id)
Y0103in = Y2015_NEW0103start %>% count(to_station_id)

Y0305out = Y2015_NEW0305start %>% count(from_station_id)
Y0305in = Y2015_NEW0305start %>% count(to_station_id)

Y0507out = Y2015_NEW0507start %>% count(from_station_id)
Y0507in = Y2015_NEW0507start %>% count(to_station_id)

Y0709out = Y2015_NEW0709start %>% count(from_station_id)
Y0709in = Y2015_NEW0709start %>% count(to_station_id)

Y0911out = Y2015_NEW0911start %>% count(from_station_id)
Y0911in = Y2015_NEW0911start %>% count(to_station_id)

Y1113out = Y2015_NEW1113start %>% count(from_station_id)
Y1113in = Y2015_NEW1113start %>% count(to_station_id)

Y1315out = Y2015_NEW1315start %>% count(from_station_id)
Y1315in = Y2015_NEW1315start %>% count(to_station_id)

Y1517out = Y2015_NEW1517start %>% count(from_station_id)
Y1517in = Y2015_NEW1517start %>% count(to_station_id)

Y1719out = Y2015_NEW1719start %>% count(from_station_id)
Y1719in = Y2015_NEW1719start %>% count(to_station_id)

Y1921out = Y2015_NEW1921start %>% count(from_station_id)
Y1921in = Y2015_NEW1719start %>% count(to_station_id)

Y2123out = Y2015_NEW2123start %>% count(from_station_id)
Y2123in = Y2015_NEW2123start %>% count(to_station_id)

Y2301out = Y2015_NEW2301start %>% count(from_station_id)
Y2301in = Y2015_NEW2301start %>% count(to_station_id)

# join the check in and check out
# first not calculate net check in and check out
# do clustering accourding to 24 variables, i.e. all check in and out info.
# attempt fail when use 24 variables!!!!
Ytotal = station %>% left_join(Y0103in, c("id" = "to_station_id")) %>% left_join(Y0103out, c("id" = "from_station_id")) %>% left_join(Y0305in, c("id" = "to_station_id")) %>% left_join(Y0305out, c("id" = "from_station_id")) %>% left_join(Y0507in, c("id" = "to_station_id")) %>% left_join(Y0507out, c("id" = "from_station_id")) %>% left_join(Y0709in, c("id" = "to_station_id")) %>% left_join(Y0709out, c("id" = "from_station_id")) %>% left_join(Y0911in, c("id" = "to_station_id")) %>% left_join(Y0911out, c("id" = "from_station_id")) %>% left_join(Y1113in, c("id" = "to_station_id")) %>% left_join(Y1113out, c("id" = "from_station_id")) %>% left_join(Y1315in, c("id" = "to_station_id")) %>% left_join(Y1315out, c("id" = "from_station_id")) %>% left_join(Y1517in, c("id" = "to_station_id")) %>% left_join(Y1517out, c("id" = "from_station_id")) %>% left_join(Y1719in, c("id" = "to_station_id")) %>% left_join(Y1719out, c("id" = "from_station_id")) %>% left_join(Y1921in, c("id" = "to_station_id")) %>% left_join(Y1921out, c("id" = "from_station_id")) %>% left_join(Y2123in, c("id" = "to_station_id")) %>% left_join(Y2123out, c("id" = "from_station_id")) %>% left_join(Y2301in, c("id" = "to_station_id")) %>% left_join(Y2301out, c("id" = "from_station_id"))


Ytotal = Ytotal %>% select(-name, -latitude, -longitude, -dpcapacity, -landmark)
colnames(Ytotal) = c("id", "X0103in", "X0103out", "X0305in", "X0305out", "X0507in", "X0507out", "X0709in", "X0709out", "X0911in", "X0911out", "X1113in", "X1113out", "X1315in", "X1315out", "X1517in", "X1517out", "X1719in", "X1719out", "X1921in", "X1921out", "X2123in", "X2123out", "X2301in", "X2301out")
for(i in 2:dim(Ytotal)[2]){
  Ytotal[which(is.na(Ytotal[, i])), i] = 0
}
map_of_Chicago <- get_map(location = c(lon = mean(station$longitude),lat = mean(station$latitude)),zoom = 12,maptype = "roadmap", scale = 2)

Ynet = Ytotal
Ynet = Ynet %>% mutate(net0103 = X0103in - X0103out, net0305 = X0305in - X0305out, net0507 = X0507in - X0507out, net0709 = X0709in - X0709out, net0911 = X0911in - X0911out, net1113 = X1113in - X1113out, net1315 = X1315in - X1315out, net1517 = X1517in - X1517out, net1719 = X1719in - X1719out, net1921 = X1921in - X1921out, net1921 = X1921in - X1921out, net2123 = X2123in - X2123out, net2301 = X2301in - X2301out)
mnet = data.frame(Ynet[, 26:37], row.names = as.character(Ynet$id))
distnet = dist(mnet, method = "manhattan")
clustnet = hclust(distnet, method = "complete")
par(cex = 0.7)
plot(as.phylo(clustnet),label.offset = 1)
par(cex = 1)

outnet = cutree(clustnet, k = 4)
netclass1 = as.numeric(names(outnet[outnet == 1]))
netclass2 = as.numeric(names(outnet[outnet == 2]))
netclass3 = as.numeric(names(outnet[outnet == 3]))
netclass4 = as.numeric(names(outnet[outnet == 4]))
ggmap(map_of_Chicago)+
  geom_point(aes(x = longitude, y = latitude, colour = "cluster1 as general part"), size = 1.5, 
             data = station[station$id %in% netclass1, ]) + 
  geom_point(aes(x = longitude, y = latitude, colour = "cluster2 as tourist part"), size = 1.5, data =station[station$id %in% netclass2, ]) + 
  geom_point(aes(x = longitude, y = latitude, colour = "cluster3 as business part"), size = 1.5, data =station[station$id %in% netclass3, ]) + 
  geom_point(aes(x = longitude, y = latitude, colour = "cluster4 as transition part"), size = 1.5, data =station[station$id %in% netclass4, ])


plotY = Y2015_NEW
plotY$start_hour = as.numeric(plotY$start_hour)
plotY$stop_hour = as.numeric(plotY$stop_hour)
plotYfrom1 = plotY %>% filter(from_station_id %in% netclass1)
plotYto1 = plotY %>% filter(to_station_id %in% netclass1)
plotYfrom2 = plotY %>% filter(from_station_id %in% netclass2)
plotYto2 = plotY %>% filter(to_station_id %in% netclass2)
plotYfrom3 = plotY %>% filter(from_station_id %in% netclass3)
plotYto3 = plotY %>% filter(to_station_id %in% netclass3)
plotYfrom4 = plotY %>% filter(from_station_id %in% netclass4)
plotYto4 = plotY %>% filter(to_station_id %in% netclass4)

nfrom1 = plotYfrom1 %>% count(start_hour)
nto1 = plotYto1 %>% count(stop_hour)
nfrom2 = plotYfrom2 %>% count(start_hour)
nto2 = plotYto2 %>% count(stop_hour)
nfrom3 = plotYfrom3 %>% count(start_hour)
nto3 = plotYto3 %>% count(stop_hour)
nfrom4 = plotYfrom4 %>% count(start_hour)
nto4 = plotYto4 %>% count(stop_hour)

p1<-ggplot()+
  geom_line(aes(x = nfrom1$start_hour, y = nfrom1$n, colour = "check out"))+
  geom_line(aes(x = nto1$stop_hour, y = nto1$n, colour = "check in"))+xlab("Time in a day") +
  ylab("Amount of checkin and checkout")+ ggtitle("General")

p2<-ggplot()+
  geom_line(aes(x = nfrom2$start_hour, y = nfrom2$n, colour = "check out"))+
  geom_line(aes(x = nto2$stop_hour, y = nto2$n, colour = "check in"))+xlab("Time in a day") +
  ylab("Amount of checkin and checkout")+ ggtitle("Tourist")

p3<-ggplot()+
  geom_line(aes(x = nfrom3$start_hour, y = nfrom3$n, colour = "check out"))+
  geom_line(aes(x = nto3$stop_hour, y = nto3$n, colour = "check in"))+xlab("Time in a day") +
  ylab("Amount of checkin and checkout")+ ggtitle("Business")

p4<-ggplot()+
  geom_line(aes(x = nfrom4$start_hour, y = nfrom4$n, colour = "check out"))+
  geom_line(aes(x = nto4$stop_hour, y = nto4$n, colour = "check in"))+xlab("Time in a day") +ylab("Amount of checkin and checkout")+ ggtitle("Transition")
multiplot(p1, p2, p3, p4, cols=2)

residence = netclass4
business = netclass3
general = netclass1
tourist = netclass2
#######5:00 am to 10:00 am  ###########
RtoB = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% business) %>% filter(between(as.numeric(start_hour), 5, 10))
RtoBcount = RtoB %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoG = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% general) %>% filter(between(as.numeric(start_hour), 5, 10))
RtoGcount = RtoG %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoT = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% tourist) %>% filter(between(as.numeric(start_hour), 5, 10))
RtoTcount = RtoT %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))

par(mfrow=c(1,3))
pie(x = c(dim(RtoB)[1]/dim(RtoBcount)[1], dim(RtoG)[1]/dim(RtoGcount)[1], dim(RtoT)[1]/dim(RtoTcount)[1]), labels = c("business", "general", "tourist"))

#### 10:00 am to 15:00 am #########
RtoB = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% business) %>% filter(between(as.numeric(start_hour), 10, 15))
RtoBcount = RtoB %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoG = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% general) %>% filter(between(as.numeric(start_hour), 10, 15))
RtoGcount = RtoG %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoT = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% tourist) %>% filter(between(as.numeric(start_hour), 10, 15))
RtoTcount = RtoT %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))

pie(x = c(dim(RtoB)[1]/dim(RtoBcount)[1], dim(RtoG)[1]/dim(RtoGcount)[1], dim(RtoT)[1]/dim(RtoTcount)[1]),
    labels = c("business", "general", "tourist"))


#### 15:00 am to 20:00 am #########
RtoB = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% business) %>% filter(between(as.numeric(start_hour), 15, 20))
RtoBcount = RtoB %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoG = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% general) %>% filter(between(as.numeric(start_hour), 15, 20))
RtoGcount = RtoG %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoT = Y2015_NEW %>% filter(from_station_id %in% residence, to_station_id %in% tourist) %>% filter(between(as.numeric(start_hour), 15, 20))
RtoTcount = RtoT %>% group_by(to_station_id) %>% summarise(count = n()) %>% arrange(desc(count))

pie(x = c(dim(RtoB)[1]/dim(RtoBcount)[1], dim(RtoG)[1]/dim(RtoGcount)[1], dim(RtoT)[1]/dim(RtoTcount)[1]), labels = c("business", "general", "tourist"))

#######5:00 am to 10:00 am  ###########
RtoB = Y2015_NEW %>% filter(from_station_id %in% business, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 5, 10))
RtoBcount = RtoB %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoG = Y2015_NEW %>% filter(from_station_id %in% general, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 5, 10))
RtoGcount = RtoG %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoT = Y2015_NEW %>% filter(from_station_id %in% tourist, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 5, 10))
RtoTcount = RtoT %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))

par(mfrow=c(1,3))

pie(x = c(dim(RtoB)[1]/dim(RtoBcount)[1], dim(RtoG)[1]/dim(RtoGcount)[1], dim(RtoT)[1]/dim(RtoTcount)[1]),labels = c("business", "general", "tourist"))

#### 10:00 am to 15:00 am #########
RtoB = Y2015_NEW %>% filter(from_station_id %in% business, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 10, 15))
RtoBcount = RtoB %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoG = Y2015_NEW %>% filter(from_station_id %in% general, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 10, 15))
RtoGcount = RtoG %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoT = Y2015_NEW %>% filter(from_station_id %in% tourist, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 10, 15))
RtoTcount = RtoT %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))

pie(x = c(dim(RtoB)[1]/dim(RtoBcount)[1], dim(RtoG)[1]/dim(RtoGcount)[1], dim(RtoT)[1]/dim(RtoTcount)[1]),labels = c("business", "general", "tourist"))

#### 15:00 am to 20:00 am #########
RtoB = Y2015_NEW %>% filter(from_station_id %in% business, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 15, 20))
RtoBcount = RtoB %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoG = Y2015_NEW %>% filter(from_station_id %in% general, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 15, 20))
RtoGcount = RtoG %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))
RtoT = Y2015_NEW %>% filter(from_station_id %in% tourist, to_station_id %in% residence) %>% filter(between(as.numeric(stop_hour), 15, 20))
RtoTcount = RtoT %>% group_by(from_station_id) %>% summarise(count = n()) %>% arrange(desc(count))

pie(x = c(dim(RtoB)[1]/dim(RtoBcount)[1], dim(RtoG)[1]/dim(RtoGcount)[1], dim(RtoT)[1]/dim(RtoTcount)[1]),labels = c("business", "general", "tourist"))

plotY = bike_weekend
plotY$start_hour = as.numeric(plotY$start_hour)
plotY$stop_hour = as.numeric(plotY$stop_hour)
plotYfrom1 = plotY %>% filter(from_station_id %in% netclass1)
plotYto1 = plotY %>% filter(to_station_id %in% netclass1)
plotYfrom2 = plotY %>% filter(from_station_id %in% netclass2)
plotYto2 = plotY %>% filter(to_station_id %in% netclass2)
plotYfrom3 = plotY %>% filter(from_station_id %in% netclass3)
plotYto3 = plotY %>% filter(to_station_id %in% netclass3)
plotYfrom4 = plotY %>% filter(from_station_id %in% netclass4)
plotYto4 = plotY %>% filter(to_station_id %in% netclass4)

nfrom1 = plotYfrom1 %>% count(start_hour)
nto1 = plotYto1 %>% count(stop_hour)
nfrom2 = plotYfrom2 %>% count(start_hour)
nto2 = plotYto2 %>% count(stop_hour)
nfrom3 = plotYfrom3 %>% count(start_hour)
nto3 = plotYto3 %>% count(stop_hour)
nfrom4 = plotYfrom4 %>% count(start_hour)
nto4 = plotYto4 %>% count(stop_hour)

plotYfrom2to3=plotY %>% filter(from_station_id %in% residence,to_station_id %in% business)
nfrom2to3 = plotYfrom2to3 %>% count(start_hour)


par(mfrow=c(2,2))

plot(x = nfrom1$start_hour, y = nfrom1$n, col = "black", type = "l", main = "General part flow pattern in weekend",xlab = "Time in a day",ylab = "Checkin and checkout amount")
lines(x = nto1$stop_hour, y = nto1$n, col = "red")
legend("topleft", bty = "n", legend = c("check out", "check in"), lty = c(1, 1), col = c("black", "red"), cex = 0.6)
plot(x = nfrom2$start_hour, y = nfrom2$n, col = "black", type = "l",main = "Tourist part flow pattern in weekend",xlab = "Time in a day",ylab = "Checkin and checkout amount",ylim = c(0,9000))
lines(x = nto2$stop_hour, y = nto2$n, col = "red")
legend("topleft", bty = "n", legend = c("check out", "check in"), lty = c(1, 1), col = c("black", "red"), cex = 0.6)
plot(x = nfrom3$start_hour, y = nfrom3$n, col = "black", type = "l",main = "Business part flow pattern in weekend",xlab = "Time in a day",ylab = "Checkin and checkout amount")
lines(x = nto3$stop_hour, y = nto3$n, col = "red")
legend("topleft",  bty = "n",legend = c("check out", "check in"), lty = c(1, 1), col = c("black", "red"), cex = 0.6)
plot(x = nfrom4$start_hour, y = nfrom4$n, col = "black", type = "l",main="Transition part flow pattern in weekend",xlab = "Time in a day",ylab = "Checkin and checkout amount")
lines(x = nto4$stop_hour, y = nto4$n, col = "red")
legend("topleft", bty = "n",legend = c("check out", "check in"), lty = c(1, 1), col = c("black", "red"), cex = 0.6)

usertest<-function(data,class1,class2,time){
  ##the trip from class1 to class2
  target1=data%>% filter(from_station_id %in% class1,to_station_id %in% class2) %>% left_join(station,c("to_station_id" = "id"))
  ##the trip time from 5:00 to 10:00
  target2=target1 %>% filter(start_hour %in% time)
  ## analyse the usetype
  user_target=target2 %>% group_by(usertype) %>% summarise(count=n())
  user_total=data %>% group_by(usertype) %>% summarise(count=n())
  total=c(user_target[1,2]+user_target[2,2],user_total[1,2]+user_total[3,2])
  subcriber=c(user_target[2,2],user_total[3,2])
  subcriber=as.numeric(subcriber)
  total=as.numeric(total)
  out1 = prop.test( subcriber, total,correct=FALSE)
  out1
}

gendertest<-function(data,class1,class2,time){
  ##the trip from class1 to class2
  target1=data%>% filter(from_station_id %in% class1,to_station_id %in% class2) %>% left_join(station,c("to_station_id" = "id"))
  ##the trip time from 5:00 to 10:00
  target2=target1 %>% filter(start_hour %in% time)
  gender_target=target2 %>% group_by(gender) %>% summarise(count=n())
  gender_target=gender_target[c(1,2),]
  gender_total=Y2015_NEW%>% left_join(station,c("from_station_id"="id"))%>% group_by(gender) %>% summarise(count=n())
  gender_total=gender_total[c(1,2),]
  
  total=c(gender_target[1,2]+gender_target[2,2],gender_total[1,2]+gender_total[2,2])
  male=c(gender_target[2,2],gender_total[2,2])
  total=as.numeric(total)
  male=as.numeric(male)
  
  out2 = prop.test(male,total, correct=FALSE)
  out2
  
}

myfunc <- function(v1) {
  deparse(substitute(v1))
}
##analysize age
agetest<-function(data,class1,class2,time){
  ##the trip from class1 to class2
  target1=data%>% filter(from_station_id %in% class1,to_station_id %in% class2) %>% left_join(station,c("to_station_id" = "id"))
  ##the trip time from 5:00 to 10:00
  target2=target1 %>% filter(start_hour %in% time)
  
  classcount=target2 %>% filter(usertype=="Subscriber") %>% mutate(age =2016-birthyear)%>%group_by(age) %>% summarise(count=n()) 
  
  totalcount=Y2015_NEW%>% filter(usertype=="Subscriber") %>% mutate(age =2016-birthyear)%>%group_by(age) %>% summarise(count=n()) 
  return(list(classcount,totalcount))
  
}


p1<-agetest(Y2015_NEW,residence,business,5:10)[[1]] %>% ggplot()+
  geom_point(mapping=aes(x=age,y=count/sum(count)))+
  xlab("the range of age")+
  ylab("the proportion of the count")+
  ggtitle("from Transition to Business")
p2<-agetest(Y2015_NEW,residence,business,5:10)[[2]] %>% ggplot()+
  geom_point(mapping=aes(x=age,y=count/sum(count)))+
  xlab("the range of age")+
  ylab("the proportion of the count")+
  ggtitle("from Total to Total")
p3<-agetest(Y2015_NEW,business,residence,5:10)[[1]] %>% ggplot()+
  geom_point(mapping=aes(x=age,y=count/sum(count)))+
  xlab("the range of age")+
  ylab("the proportion of the count")+
  ggtitle("from Business to Transition")
p4<-agetest(Y2015_NEW,business,residence,5:10)[[2]] %>% ggplot()+
  geom_point(mapping=aes(x=age,y=count/sum(count)))+
  xlab("the range of age")+
  ylab("the proportion of the count")+
  ggtitle("from Total to Total")
multiplot(p1, p2, p3, p4, cols=2)