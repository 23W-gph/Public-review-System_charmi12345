# installing packages-----------------------------------------

install.packages("pacman")
library(pacman)
p_load(rio,here,lubridate,tidyverse)
install.packages("janitor")
library(janitor)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(readr)
# calling my dataset---------------------
setwd("C:/Users/C/Desktop/new project")
patient_review_system_data <- read_csv("C:/Users/C/Desktop/new project/hospital data.csv",
                                       
                                       col_types = cols(`Poor Sanitation` = col_number(), 
                                                        `Bureaucracy and Administration Malfunction` = col_number(),        
                                                        `Staff shortage` = col_number(), 
                                                        `Low quality care` = col_number(), 
                                                        Overcrowding = col_number(),
                                                        `Overuse of health facilities` = col_number(), 
                                                        `Increased out-pocket payment` = col_number(),
                                                        `Rude and un-professional behaviour` = col_number(), 
                                                        `Medical Negligence` = col_number(), 
                                                        `Inadequate facilities for treatment` = col_number(), 
                                                        `availability of drugs` = col_number(), 
                                                        `Lack of essential Medicine` = col_number(), 
                                                        `Inequal access of healthcare or Racism` = col_number(), 
                                                        `Poor food quality` = col_number(),
                                                        `Poor emergency services` = col_number(), 
                                                        `Lack of tranperancy regarding treatment` = col_number(), 
                                                        `language barrier` = col_number()))



# cleaning my data------------------------------------------

review_system_data <- patient_review_system_data |>
  janitor::clean_names()
review_system_data <- review_system_data %>%
  rename(hospital = patient_type)
#filtering and editing dataframe into 2---------------------------------------------

sir_t_hospital <- filter(review_system_data, hospital == "Sir.T hospital")
hcg_hospital <- filter(review_system_data, hospital == "HCG hospital")

ds_c <- filter(sir_t_hospital, department == "Cardiology")
ds_n <- filter(sir_t_hospital, department == "Neurology")
ds_o <- filter(sir_t_hospital, department == "Orthopedics")
ds_p <- filter(sir_t_hospital, department == "Pediatrics")
ds_d <- filter(sir_t_hospital, department == "Dermatology")

dh_c <- filter(hcg_hospital, department == "Cardiology")
dh_n <- filter(hcg_hospital, department == "Neurology")
dh_o <- filter(hcg_hospital, department == "Orthopedics")
dh_p <- filter(hcg_hospital, department == "Pediatrics")
dh_d <- filter(hcg_hospital, department == "Dermatology")

da_c <- filter(review_system_data, department == "Cardiology")
da_n <- filter(review_system_data, department == "Neurology")
da_o <- filter(review_system_data, department == "Orthopedics")
da_p <- filter(review_system_data, department == "Pediatrics")
da_d <- filter(review_system_data, department == "Dermatology")


# processing dataframes to make a final dataframe--------------

avg_sc <- colMeans(ds_c[, 3:19], na.rm = TRUE)
avg_sn <- colMeans(ds_n[, 3:19], na.rm = TRUE)
avg_so <- colMeans(ds_o[, 3:19], na.rm = TRUE)
avg_sp <- colMeans(ds_p[, 3:19], na.rm = TRUE)
avg_sd <- colMeans(ds_d[, 3:19], na.rm = TRUE)


avg_hc <- colMeans(dh_c[, 3:19], na.rm = TRUE)
avg_hn <- colMeans(dh_n[, 3:19], na.rm = TRUE)
avg_ho <- colMeans(dh_o[, 3:19], na.rm = TRUE)
avg_hp <- colMeans(dh_p[, 3:19], na.rm = TRUE)
avg_hd <- colMeans(dh_d[, 3:19], na.rm = TRUE)


avg_ac<-  colMeans(da_c[, 3:19], na.rm = TRUE)
avg_an<-  colMeans(da_n[, 3:19], na.rm = TRUE)
avg_ao<-  colMeans(da_o[, 3:19], na.rm = TRUE)
avg_ap<-  colMeans(da_p[, 3:19], na.rm = TRUE)
avg_ad<-  colMeans(da_d[, 3:19], na.rm = TRUE)


Code_of_issues <- c( "01","02",
                     "03","04","05",
                     "06","07",
                     "08","9","10",
                     "11","12",
                     "13","14",
                     "15","16","17")
Code_of_Issues<- as.numeric(Code_of_issues)

df1.c <- data.frame(hospital = rep("Sir.T hospital", 17))
df1.c$department <- rep("Cardiology", 17)
df1.c$Avg_Rating <- avg_sc
df1.c$Code_of_Issues <- Code_of_Issues

df1.n <- data.frame(hospital = rep("Sir.T hospital", 17))
df1.n$department <- rep("Neurology", 17)
df1.n$Avg_Rating <- avg_sn
df1.n$Code_of_Issues <- Code_of_Issues

df1.o <- data.frame(hospital = rep("Sir.T hospital", 17))
df1.o$department <- rep("Orthopedics", 17)
df1.o$Avg_Rating <- avg_so
df1.o$Code_of_Issues <- Code_of_Issues

df1.p <- data.frame(hospital = rep("Sir.T hospital", 17))
df1.p$department <- rep("Pediatrics", 17)
df1.p$Avg_Rating <- avg_sp
df1.p$Code_of_Issues <- Code_of_Issues

df1.d <- data.frame(hospital = rep("Sir.T hospital", 17))
df1.d$department <- rep("Dermatology", 17)
df1.d$Avg_Rating <- avg_sd
df1.d$Code_of_Issues <- Code_of_Issues



df2.c <- data.frame(hospital = rep("HCG hospital", 17))
df2.c$department <- rep("Cardiology", 17)
df2.c$Avg_Rating <- avg_hc
df2.c$Code_of_Issues <- Code_of_Issues

df2.n <- data.frame(hospital = rep("HCG hospital", 17))
df2.n$department <- rep("Neurology", 17)
df2.n$Avg_Rating <- avg_hn
df2.n$Code_of_Issues <- Code_of_Issues

df2.o <- data.frame(hospital = rep("HCG hospital", 17))
df2.o$department <- rep("Orthopedics", 17)
df2.o$Avg_Rating <- avg_ho
df2.o$Code_of_Issues <- Code_of_Issues

df2.p <- data.frame(hospital = rep("HCG hospital", 17))
df2.p$department <- rep("Pediatrics", 17)
df2.p$Avg_Rating <- avg_hp
df2.p$Code_of_Issues <- Code_of_Issues

df2.d <- data.frame(hospital = rep("HCG hospital", 17))
df2.d$department <- rep("Dermatology", 17)
df2.d$Avg_Rating <- avg_hd
df2.d$Code_of_Issues <- Code_of_Issues




df3.c <- data.frame(hospital = rep("All", 17))
df3.c$department <- rep("Cardiology", 17)
df3.c$Avg_Rating <- avg_ac
df3.c$Code_of_Issues <- Code_of_Issues

df3.n <- data.frame(hospital = rep("All", 17))
df3.n$department <- rep("Neurology", 17)
df3.n$Avg_Rating <- avg_an
df3.n$Code_of_Issues <- Code_of_Issues

df3.o <- data.frame(hospital = rep("All", 17))
df3.o$department <- rep("Orthopedics", 17)
df3.o$Avg_Rating <- avg_ao
df3.o$Code_of_Issues <- Code_of_Issues

df3.p <- data.frame(hospital = rep("All", 17))
df3.p$department <- rep("Pediatrics", 17)
df3.p$Avg_Rating <- avg_ap
df3.p$Code_of_Issues <- Code_of_Issues

df3.d <- data.frame(hospital = rep("All", 17))
df3.d$department <- rep("Dermatology", 17)
df3.d$Avg_Rating <- avg_ad
df3.d$Code_of_Issues <- Code_of_Issues


# merging both dataframe into final one-----------------------------------------------

df_final <- rbind(df1.c, df1.n, df1.o, df1.p, df1.d, df2.c, df2.n, df2.o, df2.p, df2.d, df3.c, df3.n, df3.o, df3.p, df3.d)




setwd("C:/Users/C/Desktop/new project/")
write.csv(df_final, file = "C:/Users/C/Desktop/new project/df_final.csv", row.names = FALSE)






