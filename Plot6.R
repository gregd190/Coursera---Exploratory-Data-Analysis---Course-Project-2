#Read in data files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#There are considerably different numbers of measurements between these years.
#It is therefore not reasonable to simply sum all the measurement locations. 
#We will sum only the SCC values that are common to all 4 years, and use that to
#plot the relative accumulated emissions. 

#Create subsets by year - While we could add more datapoints, the more years we use
#smaller the number of measurement points that had valid data in those years
NEI99 = NEI[NEI$year == 1999,]
NEI02 = NEI[NEI$year == 2002,]
NEI05 = NEI[NEI$year == 2005,]
NEI08 = NEI[NEI$year == 2008,]

#Determine which measurement locations (SCC values) were common to all years
commonSCC <- Reduce(intersect, list(NEI99$SCC, NEI02$SCC, NEI05$SCC, NEI08$SCC)) 

#Select the values of SCC$SCC.Level.Two that correspond to vehicle use

vehicles = SCC[grepl("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE),]


#Subset revelant values from NEI for each of the cities
VehicleNEIB = NEI[NEI$SCC %in% vehicles$SCC & NEI$year %in% c("1999","2002","2005","2008") 
& NEI$fips == "24510" & NEI$SCC %in% commonSCC,]
VehicleNEIL = NEI[NEI$SCC %in% vehicles$SCC & NEI$year %in% c("1999","2002","2005","2008") 
& NEI$fips == "06037" & NEI$SCC %in% commonSCC,]


#Aggregate Car Emissions by year
VehicleByYearB<- tapply(VehicleNEIB$Emissions, VehicleNEIB$year, sum)
VehicleByYearB <- data.frame(VehicleByYear)
VehicleByYearL<- tapply(VehicleNEIL$Emissions, VehicleNEIL$year, sum)
VehicleByYearL <- data.frame(VehicleByYearL)

#Create png device
png("plot6.png", width=480, height=480)

#Plot total emissions by year
plot(as.numeric(row.names(VehicleByYearB)), VehicleByYearB[,1],col = "red", ylim = c(0,3000),ylab = "Relative Total Vehicle Emissions (pm2.5)", 
xlab = "Year", main = "Vehicle Emissions - Baltimore vs LA")
points(as.numeric(row.names(VehicleByYearL)), VehicleByYearL[,1], pch = 4, col = "blue")
legend("topright", legend=c("Baltimore", "Los Angeles"),
       col=c("red", "blue"), pch = c(1,4), cex=0.8)


#Close plot device
dev.off()