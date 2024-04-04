library(tidyverse)
library(readxl)
library(compare)
library(rmarkdown)
library (dplyr)

#Calculate Electric Costs & Gas Benefits of Res,NR Heat Pumps

#****Data and Vars

#Selections
ClimateZone <- 12
HeatPumpHVAC_res <- 'SWHC045'  
HPWH_res <- 'SWWH025'
EU_HP_HVAC<-'DEER:HVAC_Eff_HP'
EU_HPWH<-'DEER:Res_ClothesDishWasher'
EUL_Claims_HeatPumpHVAC_res <- 15
EUL_Claims_HPWH_res <- 10
#EUL_New_HeatPumpHVAC_res <- 20
#EUL_New_HPWH_res <- 20
#Constants
PGE_WACC_A <- 0.0734 #Source:ACC, discount Rate
PGE_WACC_Q <- PGE_WACC_A/4
Divider_discount <- PGE_WACC_Q + 1
Inflation <- 0.02  #Source:ACC

#Read in Fuel Sub Claims 2023 Q1-Q4
FS23 <- read_excel("C:\\Users\\mmh\\R  Programming\\23_FS_Claims_PGE.xlsx") #Normal Replacement Only (No New Construction)
FS23 <- as.data.frame(FS23)
#Read in Load Shapes (accessible at https://file.ac/l1-GqhWF8OU/)
kWh_LS_QRT <- read_excel("C:\\Users\\mmh\\R  Programming\\E_LS_QRT.xlsx") #Quarterly LS

#Read in Claim Yr Quarters
Quarters <- read_excel("C:\\Users\\mmh\\R  Programming\\Qrt.xlsx")

#Total Energy: by Measure & CZ
df_HeatPumpHVAC_res<- filter(FS23, CZ == ClimateZone, MeasureID == HeatPumpHVAC_res )
HeatPumpHVAC_res_kWh<- c(sum(df_HeatPumpHVAC_res$TotalkWhFirstBaseline))
HeatPumpHVAC_res_therm<- c(sum(df_HeatPumpHVAC_res$TotalThermFirstBaseline))
df_HPWH_res<- filter(FS23, CZ == ClimateZone, MeasureID == HPWH_res)
HPWH_res_kWh<- c(sum(df_HPWH_res$TotalkWhFirstBaseline))
HPWH_res_therm<- c(sum(df_HPWH_res$TotalThermFirstBaseline))
#Claimed Electric Costs and Gas Benefits
df_HeatPumpHVAC_res <- lapply(df_HeatPumpHVAC_res,as.numeric)
df_HPWH_res <- lapply(df_HPWH_res,as.numeric)

Claims_ElecCost_HeatPumpHVAC_res<- c(sum(df_HeatPumpHVAC_res$`Electric Supply Cost`))
Claims_ElecCost_HPWH_res<- c(sum(df_HPWH_res$`Electric Supply Cost`))
Claims_GasBen_HeatPumpHVAC_res<- c(sum(df_HeatPumpHVAC_res$`Gas Benefits Gross`))
Claims_GasBen_HPWH_res<- c(sum(df_HPWH_res$`Gas Benefits Gross`))

#Calculated Quarterly Electric ACs (energy x LS) over EUL_Claims
#Filter for CZ, End Use, CLaimYearQrt
EUL_Claims_HeatPumpHVAC_res <-  list(Quarters$EUL_20) 
EUL_Claims_HeatPumpHVAC_res<- as.data.frame(EUL_Claims_HeatPumpHVAC_res)
colnames(EUL_Claims_HeatPumpHVAC_res) <- c('CYQtr')
LS_HeatPumpHVAC_res <- filter(kWh_LS_QRT, CZ == ClimateZone, EU == EU_HP_HVAC, Qtr %in% EUL_Claims_HeatPumpHVAC_res$CYQt) 
LS_HeatPumpHVAC_res <-  LS_HeatPumpHVAC_res %>% mutate(Qtr_step = 1:n())  
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(Qtr_ID = LS_HeatPumpHVAC_res$'Qtr_step'-1) #this is the exponent
EUL_Claims_HPWH_res <-  list(Quarters$EUL_20) 
EUL_Claims_HPWH_res<- as.data.frame(EUL_Claims_HPWH_res)
colnames(EUL_Claims_HPWH_res) <- c('CYQtr')
LS_HPWH_res <- filter(kWh_LS_QRT, CZ == ClimateZone, EU == EU_HPWH, Qtr %in% EUL_Claims_HPWH_res$CYQt) 
LS_HPWH_res <-  LS_HPWH_res %>% mutate(Qtr_step = 1:n())  
LS_HPWH_res <- LS_HPWH_res %>% mutate(Qtr_ID = LS_HPWH_res$'Qtr_step'-1)


#Calculate Electric Costs, Discount the Qrt Costs, Multiply & Sum 

LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(Gen_Discounted = (LS_HeatPumpHVAC_res$'Gen'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(TD_Discounted = (LS_HeatPumpHVAC_res$'TD'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))
LS_HeatPumpHVAC_res <- LS_HeatPumpHVAC_res %>% mutate(CO2_Discounted = (LS_HeatPumpHVAC_res$'CO2'/(Divider_discount^LS_HeatPumpHVAC_res$'Qtr_ID')))
AC_Sum_Gen_E_HeatPumpHVAC_res <- c(sum(LS_HeatPumpHVAC_res$'Gen_Discounted'*HeatPumpHVAC_res_kWh))
AC_Sum_TD_E_HeatPumpHVAC_res <- c(sum(LS_HeatPumpHVAC_res$'TD_Discounted'*HeatPumpHVAC_res_kWh))
AC_SUM_CO2_E_HeatPumpHVAC_res <- c(sum(LS_HeatPumpHVAC_res$'CO2_Discounted'*HeatPumpHVAC_res_kWh))
Calc_ACSum_ElecCost_HeatPumpHVAC_res <- (AC_Sum_Gen_E_HeatPumpHVAC_res + AC_Sum_TD_E_HeatPumpHVAC_res + AC_SUM_CO2_E_HeatPumpHVAC_res)*(-1)




LS_HPWH_res <- LS_HPWH_res %>% mutate(Gen_Discounted = (LS_HPWH_res$'Gen'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))
LS_HPWH_res <- LS_HPWH_res %>% mutate(TD_Discounted = (LS_HPWH_res$'TD'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))
LS_HPWH_res <- LS_HPWH_res %>% mutate(CO2_Discounted = (LS_HPWH_res$'CO2'/(Divider_discount^LS_HPWH_res$'Qtr_ID')))
AC_Sum_Gen_E_HPWH_res <- c(sum(LS_HPWH_res$'Gen_Discounted'*HPWH_res_kWh))
AC_Sum_TD_E_HPWH_res <- c(sum(LS_HPWH_res$'TD_Discounted'*HPWH_res_kWh))
AC_SUM_CO2_E_HPWH_res <- c(sum(LS_HPWH_res$'CO2_Discounted'*HPWH_res_kWh))
Calc_ACSum_ElecCost_HPWH_res <- (AC_Sum_Gen_E_HPWH_res + AC_Sum_TD_E_HPWH_res + AC_SUM_CO2_E_HPWH_res)*(-1)





print(Calc_ACSum_ElecCost_HeatPumpHVAC_res)
print(Calc_ACSum_ElecCost_HPWH_res)

#Next Steps 
#1 discount by qrtly discount rate
#2 if that does not work, look at ACC data from RECURVE
#3 ask Tara about Gas GHG Adder in teh EE tables







