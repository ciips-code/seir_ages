readxl::excel_sheets("c:users/adrian/desktop/EEParams2.xlsx")

EEParams2 <- list()

for (s in readxl::excel_sheets("c:users/adrian/desktop/EEParams2.xlsx")) {
  EEParams2[[s]] <- readxl::read_xlsx("c:users/adrian/desktop/EEParams2.xlsx", sheet = s) 
}

save(EEParams2,file="data/EEParams2.RData")
