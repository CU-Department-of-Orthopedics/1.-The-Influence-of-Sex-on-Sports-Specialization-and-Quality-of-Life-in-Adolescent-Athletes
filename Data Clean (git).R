## Data Clean 

library(readxl)
library(tidyverse)

dat <- read_excel(file.choose())

# Recode 1/0 sex as M/F; spec. level as 1/2/3
dat <- dat %>% 
  mutate(
    sex = case_when(
      sex1m == 1 ~ "Male",
      sex1m == 0 ~ "Female"
    ),
    spec_level = case_when(
      spec_LoMedHi == 1 ~ "Low",
      spec_LoMedHi == 2 ~ "Medium",
      spec_LoMedHi == 3 ~ "High"
    )
  )

# Remove uncoded vars
dat <- dat[, -c(2,3)]

# Select relevant data 
dat <- dat[, c(1, 22, 23, 2:21)]

