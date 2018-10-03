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

#Subset SEI to determine which SCC values are applicable with Coal Combustion
comb = SCC[grep("Coal", SCC$SCC.Level.Three),]
comb = comb[grep("Combustion", a$SCC.Level.One),]

#See which of the coal combustion SCC values were in use for all 4 years of interest. 
CoalCombSCCs = intersect(comb$SCC, commonSCC)

#Subset revelant values from NEI
CoalCombNEI = NEI[NEI$SCC %in% CoalCombSCCs & NEI$year %in% c("1999","2002","2005","2008"),]


#Aggregate Coal Emissions by year and type
CoalByYear<- tapply(CoalCombNEI$Emissions, CoalCombNEI$year, sum)
CoalByYear <- data.frame(CoalByYear)

#Create png device
png("plot4.png", width=480, height=480)

#Plot total emissions by year
plot(as.numeric(row.names(CoalByYear)), CoalByYear[,1], ylim = c(0,600000), ylab = "Relative Total Coal Combustion Emissions", 
xlab = "Year", main = "Coal Combustion Emissions - All US")

#Close plot device
dev.off()