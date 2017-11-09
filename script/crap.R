# crappy data

#### 8.10.2017 Car drive from Hitzacker to Kamerun (crap -> GPS and IMU not from the same day...) ####
gps_8okt <- read_gps("D:\\Daten\\Eigene Dokumente\\Studium\\Masterthesis\\personal_trackers\\personal_tracker_data\\8_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_09_14_10_10_Location.csv")
imu_8okt <- read_imu("D:\\Daten\\Eigene Dokumente\\Studium\\Masterthesis\\personal_trackers\\personal_tracker_data\\8_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_09_14_10_10_IMU.csv")

merged_8okt <- merge_sources(gps_8okt,imu_8okt)

gps_8okt.sp <- gps_8okt
coordinates(gps_8okt.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_8okt.sp,radius=0.1)
m

#### 26.10.2017 Cycling to Irchel (crap, no GPS)####
gps_26okt <- read_gps("personal_tracker_data\\26_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_29_14_09_13_Location.csv")
imu_26okt <- read_imu("personal_tracker_data\\26_10_17\\CDD Personal Tracker v1.6 - 4_2017_10_29_14_09_13_IMU.csv")


gps_26okt.sp <- gps_26okt
coordinates(gps_26okt.sp)<-~ Longitude+Latitude

m <- leaflet() %>%
  
  addTiles() %>%
  addCircles(data=gps_26okt.sp,radius=0.1)
m

plot(imu_26okt$ts,imu_26okt$acc_tot,type="l")