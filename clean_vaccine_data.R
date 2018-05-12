library(install.load)
install_load('readxl')
install_load('tidyverse')

state_names <- c("Alabama", "Alaska", "Arizona", "Arkansas", 
                 "California", "Colorado", "Connecticut",
                 "Delaware", "District of Columbia", "Florida",
                 "Georgia", "Hawaii", "Idaho", "Illinois", 
                 "Indiana", "Iowa", "Kansas", "Kentucky", 
                 "Louisiana", "Maine", "Maryland", 
                 "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska",
                 "Nevada", "New Hampshire", "New Jersey",
                 "New Mexico", "New York", "North Carolina",
                 "North Dakota", "Ohio", "Oklahoma",
                 "Oregon", "Pennsylvania", "Rhode Island",
                 "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Utah", "Vermont", "Virginia",
                 "Washington", "West Virginia", "Wisconsin",
                 "Wyoming")

###
### Clean MMR vaccine data
###

MMR <- read_excel('data/MMR.xlsx', skip=2)

# We just want to extract data of percent populations with
# >=1 MMR Vaccination for the 19-35 month age group
columns_of_interest <- c("Names", as.character(1995:2016))

MMR <- MMR[, columns_of_interest]

# Convert character vectors to numerics, but not the "Names" column
MMR[, c(-1)] <- MMR[, c(-1)] %>% mutate_all(funs(as.numeric(.)))

MMR <- MMR %>% filter(Names %in% state_names) %>% rename(state = Names)

# Simplify
MMR <- as.data.frame(MMR)

# Write out new cleaned data frame
save(MMR, file="data/MMR.Rda")

###
### Clean Hepatitis A vaccine data
###

HepA <- read_excel('data/HepA.xlsx', skip=2)

# We just want to extract data of percent populations with
# >=1 Hepatitis A Vaccination for the 19-35 month age group
columns_of_interest <- c("Names", as.character(2000:2016))

HepA <- HepA[, columns_of_interest]

# Convert character vectors to numerics, but not the "Names" column
HepA[, c(-1)] <- HepA[, c(-1)] %>% mutate_all(funs(as.numeric(.)))

HepA <- HepA %>% filter(Names %in% state_names) %>% rename(state = Names)

# Simplify
HepA <- as.data.frame(HepA)

# Write out new cleaned data frame
save(HepA, file="data/HepA.Rda")

###
### Clean DTaP A vaccine data
###

DTaP <- read_excel('data/DTaP.xlsx', skip=2)

# This data file is slightly different from the others.
# The data for percent populations with
# >=1 DTaP Vaccination in the 19-35 month age group
# is not the first block of columns, it's the third,
# so we have to add the __2 suffix since
# R - by default - adds numbers to identical column names.
columns_of_interest <- c("Names", paste(as.character(1995:2016), "__2", sep=""))
#columns_of_interest <- 254:(254 + 150 - 1)
#columns_of_interest <- c(1, columns_of_interest)  # 1 is the "Names" column

DTaP <- DTaP[, columns_of_interest]

# Convert character vectors to numerics, but not the "Names" column
DTaP[, c(-1)] <- DTaP[, c(-1)] %>% mutate_all(funs(as.numeric(.)))

DTaP <- DTaP %>% filter(Names %in% state_names)

# Simplify
DTaP <- as.data.frame(DTaP)

colnames(DTaP) <- c("state", as.character(1995:2016))

# Write out new cleaned data frame
save(DTaP, file="data/DTaP.Rda")


