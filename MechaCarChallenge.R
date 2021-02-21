
library(dplyr)
MechaCar_mpg <- read.csv("MechaCar_mpg.csv")
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
     ground_clearance + AWD, MechaCar_mpg)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
             ground_clearance + AWD, MechaCar_mpg))

Suspension_Coil <- read.table("Suspension_Coil.csv", header = TRUE, sep= ",")
total_summary <- Suspension_Coil %>% summarize(Mean= mean(PSI), Median= median(PSI), Variance =var(PSI), SD=sd(PSI))
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean= mean(PSI), Median= median(PSI), Variance =var(PSI), SD=sd(PSI))

t.test(x= Suspension_Coil$PSI, mu=1500)
t.test(lot_summary$Mean, subset= Manufacturing_Lot == "Lot1", mu=1500)
t.test(lot_summary$Mean, subset= Manufacturing_Lot == "Lot2", mu=1500)
t.test(lot_summary$Mean, subset= Manufacturing_Lot == "Lot3", mu=1500)
