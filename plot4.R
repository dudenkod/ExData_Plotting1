#Estimate memory needs. assuming 8 byte per value

memory_needs_MB <- 2075259*9*8/(1024*1024)

message <- paste("Memory needs estimate:",round(memory_needs_MB,digits = 1),"MB")
print(message)
print("if more than 1GB, load with caution!")


#Reading data from file
fileName="./household_power_consumption.txt"
data_file=file(fileName,open="r")
all_lines=readLines(data_file)
close(data_file)

#Replacing all NA values labeled ? to NA
NA_labels="\\?"
missed_values <- grepl(NA_labels,all_lines)
all_lines[missed_values] <- gsub(NA_labels,"NA",all_lines[missed_values])

#Taking only between 1 and 2 of February 2007
specific_dates_pattern="^((0)?1|(0)?2)\\/(0)?2\\/2007"
specific_dates_index <- grepl(specific_dates_pattern,all_lines)
col_names <- strsplit(all_lines[1],";")
chopped_list <- strsplit(all_lines[specific_dates_index], ";")

#Shaping extracted data
chopped_data <- do.call(rbind, lapply(chopped_list, c))
chopped_data_frame <- data.frame(chopped_data)
colnames(chopped_data_frame) <- unlist(col_names)

for( i in 3:9 ){
  chopped_data_frame[[i]] <- sapply(chopped_data_frame[[i]], as.character)
  chopped_data_frame[[i]] <- sapply(chopped_data_frame[[i]], as.numeric)
              }

#Creating DateTime as suggested
chopped_data_frame$DateTime <- strptime(paste(chopped_data_frame$Date, chopped_data_frame$Time), format="%d/%m/%Y %H:%M:%S")

#FINAL MERGER
par(mfrow = c(2,2))
with(chopped_data_frame, {
     plot(chopped_data_frame$DateTime, chopped_data_frame$Global_active_power, type = "l", 
     ylab = "Global Active Power", xlab = "")
     plot(chopped_data_frame$DateTime, chopped_data_frame$Voltage, type = "l", ylab = "Voltage", xlab = "datetime")
     plot(chopped_data_frame$DateTime, chopped_data_frame$Sub_metering_1, type = "l", ylab = "Energy sub metering",
          xlab = "")
     lines(chopped_data_frame$DateTime, chopped_data_frame$Sub_metering_2, col = "Red")
     lines(chopped_data_frame$DateTime, chopped_data_frame$Sub_metering_3, col = "Blue")
     legend("topright", col = c("Black", "Red", "Blue"), lty=c(1,1), lwd=c(1,1), 
             bty = "n",
             legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
     plot(chopped_data_frame$DateTime, chopped_data_frame$Global_reactive_power, type = "l", 
          ylab = "Global_rective_power", xlab = "datetime")
})

dev.copy(png, file="./plot4.png", width=480, height=480)
dev.off()

