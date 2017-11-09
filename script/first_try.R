# first try loading tracker data

#### 9.10.2017 Walk around Kamerun ####
gps_9okt <- read_gps("personal_tracker_data\\9_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_10_09_41_16_Location.csv")
imu_9okt <- read_imu("personal_tracker_data\\9_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_10_09_41_16_IMU.csv")

gps_9okt$dist <- get_distance(gps_9okt)
gps_9okt$bearing <- get_bearing(gps_9okt)

#calculating mean of speed/min
gps_9okt_1minmean <- aggregate(gps_9okt$Speed, list(rep(1:(nrow(gps_9okt)%/%60+1),each=60,len=nrow(gps_9okt))),mean)[-1];

dat.xts <- xts(gps_9okt$Speed,
               as.POSIXct(gps_9okt$ts))
mean.xts <- period.apply(dat.xts, INDEX=endpoints(dat.xts, "minutes"), FUN=mean)

merge_9okt <- merge_sources(gps_9okt,imu_9okt)

gps_9okt.sp <- gps_9okt
coordinates(gps_9okt.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_9okt.sp,radius=0.1)
m

plot(imu_9okt$ts,imu_9okt$acc_tot,type="l")

#### 13.10.2017 Drive to Dömitz + Cycling to Hitzacker ####
gps_13okt <- read_gps("D:\\Daten\\Eigene Dokumente\\Studium\\Masterthesis\\personal_trackers\\personal_tracker_data\\13_10,14_10\\CDD Personal Tracker v1.6 - 4_2017_10_19_11_24_52_Location.csv")
imu_13okt <- read_imu("D:\\Daten\\Eigene Dokumente\\Studium\\Masterthesis\\personal_trackers\\personal_tracker_data\\13_10,14_10\\CDD Personal Tracker v1.6 - 4_2017_10_19_11_24_52_IMU.csv")

gps_13okt$dist <- get_distance(gps_13okt)
merge_13okt <- merge_sources(gps_13okt,imu_13okt)

gps_13okt.sp <- gps_13okt
coordinates(gps_13okt.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_13okt.sp,radius=0.1)
m

plot(imu_13okt$ts,imu_13okt$acc_tot,type="l")

#### 20.10.2017 Cycling to Katzensee ####
gps_20okt <- read_gps("personal_tracker_data\\19_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_25_10_15_02_Location.csv")
imu_20okt <- read_imu("personal_tracker_data\\19_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_25_10_15_02_IMU.csv")

gps_20okt$dist <- get_distance(gps_20okt)
merge_20okt <- merge_sources(gps_20okt,imu_20okt)

gps_20okt.sp <- gps_20okt
coordinates(gps_20okt.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_20okt.sp,radius=0.1)
m

plot(imu_20okt$ts,imu_20okt$acc_tot,type="l")

#### 3.11.2017 Cycling to Irchel ####
gps_3nov <- read_gps("personal_tracker_data\\03_11_17\\CDD Personal Tracker v1.6 - 4_2017_11_03_10_24_47_Location.csv")
imu_3nov <- read_imu("personal_tracker_data\\03_11_17\\CDD Personal Tracker v1.6 - 4_2017_11_03_10_24_47_IMU.csv")

gps_3nov$dist <- get_distance(gps_3nov)
merge_3nov <- merge_sources(gps_3nov,imu_3nov)

gps_3nov.sp <- gps_3nov
coordinates(gps_3nov.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  addTiles() %>%
  addCircles(data=gps_3nov.sp,radius=0.1)
m

plot(imu_3nov$ts,imu_3nov$acc_tot,type="l")

matplot(imu_3nov$ts, cbind(imu_3nov$Acc_X.mg.,imu_3nov$Acc_Y.mg.,imu_3nov$Acc_Z.mg.), type= "l")




#### 3.11.2017 Cycling to Hönggerberg + walk ####
gps_3nov_2 <- read_gps("personal_tracker_data\\03_11_17\\CDD Personal Tracker v1.6 - 4_2017_11_03_16_51_14_Location.csv")
imu_3nov_2 <- read_imu("personal_tracker_data\\03_11_17\\CDD Personal Tracker v1.6 - 4_2017_11_03_16_51_14_IMU.csv")

gps_3nov_2$dist <- get_distance(gps_3nov_2)
merge_3nov_2 <- merge_sources(gps_3nov_2,imu_3nov_2)

gps_3nov_2.sp <- gps_3nov_2
coordinates(gps_3nov_2.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_3nov_2.sp,radius=0.1)

m%>%addPolylines(data= gps_3nov_2.sp,lng=~Longitude,lat=~Latitude)

plot(imu_3nov_2$ts,imu_3nov_2$acc_tot,type="l")

#### 6.11.2017 Tram ride to Irchel ####
gps_6nov <- read_gps("personal_tracker_data\\6_11_17\\CDD Personal Tracker v1.6 - 4_2017_11_06_11_27_51_Location.csv")
imu_6nov <- read_imu("personal_tracker_data\\6_11_17\\CDD Personal Tracker v1.6 - 4_2017_11_06_11_27_51_IMU.csv")

gps_6nov$dist <- get_distance(gps_6nov)
merge_6nov <- merge_sources(gps_6nov,imu_6nov)

gps_6nov.sp <- gps_6nov
coordinates(gps_6nov.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_6nov.sp,radius=0.1)
m

plot(imu_6nov$ts,imu_6nov$acc_tot,type="l")
