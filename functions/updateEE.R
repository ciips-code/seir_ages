readxl::excel_sheets("data/EEParams2.xlsx")

EEParams2 <- list()

for (s in readxl::excel_sheets("data/EEParams2.xlsx")) {
  EEParams2[[s]] <- readxl::read_xlsx("data/EEParams2.xlsx", sheet = s) 
}

save(EEParams2,file="data/EEParams2.RData")
