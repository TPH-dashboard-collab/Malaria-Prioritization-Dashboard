
library(readr)
library(dplyr)
library(data.table)

# Carregar novo dataset
data_new <- read_csv("TZ_subset_10regions_1seed.csv", show_col_types = FALSE)

# Ver estrutura básica
cat("=== ESTRUTURA BÁSICA ===\n")
cat("Linhas:", nrow(data_new), "\n")
cat("Colunas:", ncol(data_new), "\n")
cat("\nNomes das colunas:\n")
print(names(data_new))

# Verificar colunas importantes
cat("\n=== VERIFICAÇÕES ===\n")
cat("Tem 'admin_2'?", "admin_2" %in% names(data_new), "\n")
cat("Tem 'age_group'?", "age_group" %in% names(data_new), "\n")
cat("Tem 'scenario_name'?", "scenario_name" %in% names(data_new), "\n")
cat("Tem 'plan'?", "plan" %in% names(data_new), "\n")

# Ver planos
cat("\n=== PLANOS ===\n")
print(table(data_new$plan))

# Ver exemplo de scenario_name
cat("\n=== SCENARIO_NAME (primeiros 20) ===\n")
print(head(unique(data_new$scenario_name), 20))

# Ver colunas de intervenções (devem ser lógicas: TRUE/FALSE)
interv_cols <- grep("^(LSM|IRS|IPTSc|Vaccine|ICCM|CM|STD_Nets|PBO_Nets|IG2_Nets|SMC|PMC)$", 
                    names(data_new), value = TRUE)
cat("\n=== COLUNAS DE INTERVENÇÕES ===\n")
print(interv_cols)

# Ver exemplo de dados
cat("\n=== EXEMPLO (5 linhas) ===\n")
print(head(data_new %>% select(admin_2, age_group, scenario_name, plan, nUncomp), 5))
