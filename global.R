library(stringr)
library(readxl)
library(writexl)
OpisMetod_list <- list()
OpisMetod_list[[1]] <- "Próba losowa to metoda, która dla zadanego pliku traktuje wszystkie wierwsze w równy sposób i losuje dokładnie zadany % wierszy. W przypadku wyboru 5% wylosuje dokładnie 5% (zaookrąglone do góry) wierszy"
OpisMetod_list[[2]] <- "Próba losowa momentarna to metoda, która dla zadanego pliku traktuje wszystkie wierwsze w równy sposób i losuje dokładnie zadany % wierszy. W przypadku wyboru 5% wylosuje dokładnie 5% (zaookrąglone do góry) wierszy"
OpisMetod_list[[3]] <- "Znajdź conajmniej 1 nieprawidłowość to metoda, która dla zadanego pliku traktuje wszystkie wierwsze w równy sposób i losuje dokładnie zadany % wierszy. W przypadku wyboru 5% wylosuje dokładnie 5% (zaookrąglone do góry) wierszy"
OpisMetod_list[[4]] <- "Próba losowa w grupach to metoda, która dla zadanego pliku traktuje wszystkie wierwsze w równy sposób i losuje dokładnie zadany % wierszy. W przypadku wyboru 5% wylosuje dokładnie 5% (zaookrąglone do góry) wierszy"
OpisMetod_list[[5]] <- "Próba losowa z percentylami to metoda, która dla zadanego pliku traktuje wszystkie wierwsze w równy sposób i losuje dokładnie zadany % wierszy. W przypadku wyboru 5% wylosuje dokładnie 5% (zaookrąglone do góry) wierszy"