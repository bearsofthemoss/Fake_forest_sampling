
# Wrangle matlab code to produce .csv output for visualization ####


# Uni forest ####

## Uni_resultsModel1 #################################

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))


# read in our data
### Here Alex changed 'J_resultsModel1_Folder' to Uni_resultsModel1_Folder
Uni_resultsModel1_Folder <-readMat(here::here( "Data_folder","matlab_output",
                                               "Uni_resultsModel1_v7.mat"))

# Address to save outputs
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/1_Uni_resultsModel1")
if(!exists(here::here("Data_folder","forest_distributions","1_Uni_resultsModel1"))){
  dir.create(here::here("Data_folder","forest_distributions","1_Uni_resultsModel1"))}

setwd(here::here("Data_folder","forest_distributions","1_Uni_resultsModel1"))


##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
##print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
##print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
##print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
##print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
##print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- Uni_resultsModel1_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

## Alex ran up till here-  is this where the .csv is written for Uni model 1 results?

 
## Uni_resultsModel3 #################################

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# # functions
# setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
# source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
#J_resultsModel3_Folder <- readMat("Uni_resultsModel3.mat")
Uni_resultsModel3_Folder <-readMat(here::here( "Data_folder","matlab_output",
                                               "Uni_resultsModel3_v7.mat"))

# Address to read inputs and save outputs
if(!exists(here::here("Data_folder","forest_distributions","2_Uni_resultsModel3"))){
  dir.create(here::here("Data_folder","forest_distributions","2_Uni_resultsModel3"))}

setwd(here::here("Data_folder","forest_distributions","2_Uni_resultsModel3"))



##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- Uni_resultsModel3_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))


## Uni_resultsModel4 ####

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
Uni_resultsModel4_Folder <-readMat(here::here( "Data_folder","matlab_output",
                                               "Uni_resultsModel4_v7.mat"))

if(!exists(here::here("Data_folder","forest_distributions","3_Uni_resultsModel4"))){
  dir.create(here::here("Data_folder","forest_distributions","3_Uni_resultsModel4"))}


# Address to read inputs and save outputs
setwd(here::here("Data_folder","forest_distributions","3_Uni_resultsModel4"))


##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- Uni_resultsModel4_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))



# J forest #### 

## J_resultsModel1 ####

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
#source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
#J_resultsModel1_Folder <- readMat("J_resultsModel1.mat")
J_resultsModel1_Folder <-readMat(here::here("Data_folder","matlab_output",
                                            "J_resultsModel1_v7.mat"))

# Address to read inputs and save outputs
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/4_J_resultsModel1")
if(!exists(here::here("Data_folder","forest_distributions","4_J_resultsModel1"))){
  dir.create(here::here("Data_folder","forest_distributions","4_J_resultsModel1"))}

setwd(here::here("Data_folder","forest_distributions","4_J_resultsModel1"))


##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- J_resultsModel1_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- J_resultsModel1_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- J_resultsModel1_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- J_resultsModel1_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- J_resultsModel1_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- J_resultsModel1_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- J_resultsModel1_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))


## J_resultsModel3 ####

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# # functions
# setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
# source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# # read in our data
# J_resultsModel3_Folder <- readMat("J_resultsModel3.mat")
J_resultsModel3_Folder <-readMat(here::here("Data_folder","matlab_output",
                                            "J_resultsModel3_v7.mat"))

# # Address to read inputs and save outputs
# setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/5_J_resultsModel3")
if(!exists(here::here("Data_folder","forest_distributions","5_J_resultsModel3"))){
  dir.create(here::here("Data_folder","forest_distributions","5_J_resultsModel3"))}
setwd(here::here("Data_folder","forest_distributions","5_J_resultsModel3"))


##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- J_resultsModel3_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- J_resultsModel3_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- J_resultsModel3_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- J_resultsModel3_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- J_resultsModel3_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- J_resultsModel3_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- J_resultsModel3_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))



## J_resultsModel4 ####

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
# setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
# source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
# J_resultsModel4_Folder <- readMat("J_resultsModel4.mat")
J_resultsModel4_Folder <-readMat(here::here("Data_folder","matlab_output",
                                            "J_resultsModel4_v7.mat"))

# Address to read inputs and save outputs
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/6_J_resultsModel4")
if(!exists(here::here("Data_folder","forest_distributions","6_J_resultsModel4"))){
  dir.create(here::here("Data_folder","forest_distributions","6_J_resultsModel4"))}

##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- J_resultsModel4_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- J_resultsModel4_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- J_resultsModel4_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- J_resultsModel4_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- J_resultsModel4_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- J_resultsModel4_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- J_resultsModel4_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))



# Arb Forest ####

## Arb_resultsModel1 ####
rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
#source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
# Arb_resultsModel1_Folder <- readMat("Arb_resultsModel1.mat")
Arb_resultsModel1_Folder <-readMat(here::here("Data_folder", "matlab_output",
                                            "Arb_resultsModel1_v7.mat"))

# Address to read inputs and save outputs
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/7_Arb_resultsModel1")
if(!exists(here::here("Data_folder","forest_distributions","7_Arb_resultsModel1"))){
  dir.create(here::here("Data_folder","forest_distributions","7_Arb_resultsModel1"))}

setwd(here::here("Data_folder","forest_distributions","7_Arb_resultsModel1"))


##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- Arb_resultsModel1_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))



## Arb_resultsModel3 ####


rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
#source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
#Arb_resultsModel3_Folder <- readMat("Arb_resultsModel3.mat")
Arb_resultsModel3_Folder <-readMat(here::here("Data_folder", "matlab_output",
                                              "Arb_resultsModel3_v7.mat"))

# Address to read inputs and save outputs
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/8_Arb_resultsModel3")
if(!exists(here::here("Data_folder","forest_distributions","8_Arb_resultsModel3"))){
  dir.create(here::here("Data_folder","forest_distributions","8_Arb_resultsModel3"))}

setwd(here::here("Data_folder","forest_distributions","8_Arb_resultsModel3"))

##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- Arb_resultsModel3_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))


## Arb_resultsModel4 ####

rm(list=ls(all=TRUE))

# library to read Matlab data formats into R
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(here)

# functions
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF")
#source("FunctionsASMA.R")
source(here::here("Code_folder","data processing","FunctionsASMA.R"))

# read in our data
#Arb_resultsModel4_Folder <- readMat("Arb_resultsModel4.mat")
Arb_resultsModel4_Folder <-readMat(here::here("Data_folder", "matlab_output",
                                              "Arb_resultsModel4_v7.mat"))

# Address to read inputs and save outputs
#setwd("C:/Users/oswaldo/Desktop/SilvaCarbon/2 SILVACARBON-QUERCA/Silvacarbon/3 AsistenciaTécnica/1 ASMA/2023/JuneRun-Craig/PDF/9_Arb_resultsModel4")
if(!exists(here::here("Data_folder","forest_distributions","9_Arb_resultsModel4"))){
  dir.create(here::here("Data_folder","forest_distributions","9_Arb_resultsModel4"))}

setwd(here::here("Data_folder","forest_distributions","9_Arb_resultsModel4"))

##################################### 1 strategy


# Sampling allocation strategies
Est1<-rep(c("E1"), times=10)
EstT1<-rep(Est1, times=10000)
length(EstT1)

# Sampling sizes
SamSize1<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=1)
SamSizeT1<-rep(SamSize1, times=10000)
length(SamSizeT1)

# Distribution types and sampling size
EstE1<-c("E1")
SamSizeV1<-c(10,15,30,45,80,130,215,360,600,1000)

# PROPORTIONAL
Dist<-"Proportional"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecProportional
VecProportional<- print(fun_DataBase(DataBase,1,EstT1,SamSizeT1))
#print(fun_MultyHist(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_ProbTables(Dist,1,VecProportional, EstE1, SamSizeV1))
print(fun_MeanTables(Dist,1,VecProportional, EstE1, SamSizeV1))


##################################### 8 strategies


# Sampling allocation strategies
Est8<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8"), times=10)
EstT8<-rep(Est8, times=10000)
length(EstT8)

# Sampling sizes
SamSize8<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=8)
SamSizeT8<-rep(SamSize8, times=10000)
length(SamSizeT8)

# Distribution types and sampling size
EstE8<-c("E1","E2","E3","E4","E5","E6","E7","E8")
SamSizeV8<-c(10,15,30,45,80,130,215,360,600,1000)

# PARABOLIC
Dist<-"Parabolic"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecParabolic
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR LEFT
Dist<-"Triangular_Left"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecTriangularLeft
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR RIGHT
Dist<-"Triangular_Rigth"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecTriangularRight
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))

# TRIANGULAR U_SAHPED
Dist<-"Triangular_Ushaped"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecTriangularUshaped
VecParabolic<- print(fun_DataBase(DataBase,8,EstT8,SamSizeT8))
#print(fun_MultyHist(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_ProbTables(Dist,8,VecParabolic, EstE8, SamSizeV8))
print(fun_MeanTables(Dist,8,VecParabolic, EstE8, SamSizeV8))


##################################### 16 strategies


Est16<-rep(c("E1","E2","E3","E4","E5","E6","E7","E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16"), times=10)
EstT16<-rep(Est16, times=10000)
length(EstT16)

SamSize16<-rep(c(10,15,30,45,80,130,215,360,600,1000), each=16)
SamSizeT16<-rep(SamSize16, times=10000)
length(SamSizeT16)

# Distribution types and sampling size
EstE16<-c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15","E16")
SamSizeV16<-c(10,15,30,45,80,130,215,360,600,1000)

# TRUNCATED TRIANGLES
Dist<-"Truncated_Triangles"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecTruncatedTriangles
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))

# TRUNCATED UNIFORM
Dist<-"Truncated_Uniform"
DataBase <- Arb_resultsModel4_Folder$UncertaintyVecTruncatedUniform
VecParabolic<- print(fun_DataBase(DataBase,16,EstT16,SamSizeT16))
#print(fun_MultyHist(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_ProbTables(Dist,16,VecParabolic, EstE16, SamSizeV16))
print(fun_MeanTables(Dist,16,VecParabolic, EstE16, SamSizeV16))



