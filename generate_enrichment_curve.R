#Import package
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)

#Import excel sheet, edit accordingly
ride_output_occ10 <- read_excel("C:/Schapira_Lab/git/fragment-based-virtual-screening/mdm2/ride_realall_out10_1_1000000_combined.xlsx") %>%
  arrange(desc(Norm_Score)) %>%
  filter(Norm_Score >= 0.8)
percent_screened_occ10 <- which(!is.na(str_match(ride_output_occ10$NAME_, "^[a-zA-Z0-9]{4}$")[,1]))/1000000*100
percent_retrieved_occ10 <- seq(1, length(percent_screened_occ10))/10*100

ride_output_occ07 <- read_excel("C:/Schapira_Lab/git/fragment-based-virtual-screening/mdm2/ride_realall_out07_1_1000000_combined.xlsx") %>%
  arrange(desc(Norm_Score)) %>%
  filter(Norm_Score >= 0.75)
percent_screened_occ07 <- which(!is.na(str_match(ride_output_occ07$NAME_, "^[a-zA-Z0-9]{4}$")[,1]))/1000000*100
percent_retrieved_occ07 <- seq(1, length(percent_screened_occ07))/10*100

ride_output_occ05 <- read_excel("C:/Schapira_Lab/git/fragment-based-virtual-screening/mdm2/ride_realall_out05_1_1000000_combined.xlsx") %>%
  arrange(desc(Norm_Score)) %>%
  filter(Norm_Score >= 0.7)
percent_screened_occ05 <- which(!is.na(str_match(ride_output_occ05$NAME_, "^[a-zA-Z0-9]{4}$")[,1]))/1000000*100
percent_retrieved_occ05 <- seq(1, length(percent_screened_occ05))/10*100

ride_output_occ03 <- read_excel("C:/Schapira_Lab/git/fragment-based-virtual-screening/mdm2/ride_realall_out03_1_1000000_combined.xlsx") %>%
  arrange(desc(Norm_Score)) %>%
  filter(Norm_Score >= 0.65)
percent_screened_occ03 <- which(!is.na(str_match(ride_output_occ03$NAME_, "^[a-zA-Z0-9]{4}$")[,1]))/1000000*100
percent_retrieved_occ03 <- seq(1, length(percent_screened_occ03))/10*100





plot(x=percent_screened_occ10, 
     y=percent_retrieved_occ10, 
     xlim=c(0.000001*100,0.1*100),
     ylim=c(0*100, 1*100),
     log='x',
     type='s',
     xlab="% of screened compounds (out of 1,000,000)", 
     ylab="% of retrieved active compounds (out of 10)",
     col=1,
     lwd=2)
lines(x=percent_screened_occ07, 
      y=percent_retrieved_occ07,
      type='s',
      col=2, 
      lwd=2)
lines(x=percent_screened_occ05, 
      y=percent_retrieved_occ05,
      type='s',
      col=3,
      lwd=2)
lines(x=percent_screened_occ03, 
      y=percent_retrieved_occ03,
      type='s',
      col=4,
      lwd=2)
legend(x = "topright",          # Position
       legend = c("Occupancy = 1", "Occupancy = 0.7", "Occupancy = 0.5", "Occupancy = 0.3"),  # Legend texts
       lty = 1,
       lwd=2,
       col = c(1, 2, 3, 4))          # Line colors           






'''
#Calculates the percentage of known ligands retrieved given a set proportion
known_lig_num <- function(df, proportion){
  top_index <- 1000000*proportion
  str_match(ride_output$NAME_, "^[a-zA-Z0-9]{4}$")[,1][1:top_index] %>%
    na.omit() %>% #Removes all NA values which do not match the pattern
    length()/10 *100 #Number of known ligands retrieved/total number of known ligands, multiply by 100 to convert to percentage
}
#Generate x and y values for the plot
top_proportion <- c(0.00001, 0.0001, 0.001, 0.01) #Convert to percentage scale
top_percent <- top_proportion*100
y_vals <- c()
for (i in 1:length(top_proportion)){
  y_vals[length(y_vals)+1] <- known_lig_num(ride_output, top_proportion[i])
}

plot(x=top_percent, 
     y=y_vals, 
     xlim=c(0.000001*100,0.1*100),
     ylim=c(0*100, 1*100),
     log='x',
     xlab="% of screened compounds (out of 1,000,000)", 
     ylab="% of retrieved active compounds (out of 10)")
lines(top_percent, y_vals, xlim=range(top_percent), ylim=range(y_vals), pch=16)

plot(x=top_percent, y)
'''
