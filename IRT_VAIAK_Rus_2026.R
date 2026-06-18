###############################################################################
# ФИНАЛЬНЫЙ IRT-АНАЛИЗ ДЛЯ СТАТЬИ: VAIAK (части А, В, С)
# Многогрупповые модели с фиксацией якорных пунктов + ESSD
###############################################################################

library(mirt)

# ============================================================
# 0. ЗАГРУЗКА ДАННЫХ
# ============================================================

Fn <- "VAIAK_approbation.csv"
datall <- read.csv(Fn, sep=';')
items <- datall[, 2:38]
group_var <- as.factor(datall$Expert)  # 0=новички, 1=эксперты

part_A <- items[, 1:11]   # GRM (1-7 баллов)
part_B <- items[, 12:17]  # 2PL, 6 пунктов
part_C <- items[, 18:37]  # 2PL, 20 пунктов

# ============================================================
# 1. ФУНКЦИИ-ПОМОЩНИКИ
# ============================================================

# Для GRM (часть А)
extract_A_params <- function(group_data) {
  params <- data.frame()
  for(item_name in names(group_data)) {
    if(item_name == "GroupPars") next
    item_coef <- group_data[[item_name]]
    params <- rbind(params, data.frame(
      Item = item_name,
      a = item_coef[1],
      b1 = item_coef[2], b2 = item_coef[3], b3 = item_coef[4],
      b4 = item_coef[5], b5 = item_coef[6], b6 = item_coef[7]
    ))
  }
  return(params)
}

# Для 2PL (части В и С)
extract_2pl_params <- function(group_data) {
  params <- data.frame()
  for(item_name in names(group_data)) {
    if(item_name == "GroupPars") next
    item_coef <- group_data[[item_name]]
    params <- rbind(params, data.frame(
      Item = item_name,
      a = item_coef[1],
      b = item_coef[2]
    ))
  }
  return(params)
}

# Сложность по среднему b (для GRM)
get_difficulty_label <- function(b_params) {
  mean_b <- mean(b_params, na.rm = TRUE)
  if(mean_b < -2.0) return("оч.л.")
  if(mean_b >= -2.0 && mean_b < -0.5) return("л.")
  if(mean_b >= -0.5 && mean_b <= 0.5) return("ср.")
  if(mean_b > 0.5 && mean_b <= 2.0) return("ум.т.")
  if(mean_b > 2.0) return("т.")
  return("—")
}

# Сложность для 2PL
get_difficulty <- function(b) {
  if(b < -2.0) return("оч.л.")
  if(b >= -2.0 && b < -0.5) return("л.")
  if(b >= -0.5 && b <= 0.5) return("ср.")
  if(b > 0.5 && b <= 2.0) return("ум.т.")
  if(b > 2.0) return("т.")
  return("—")
}

# Интерпретация ESSD
interpret_essd <- function(essd) {
  if (is.na(essd) || essd < 0) return("—")
  if (essd < 0.20) return("минимальный")
  if (essd >= 0.20 && essd < 0.50) return("малый")
  if (essd >= 0.50 && essd < 0.80) return("средний")
  if (essd >= 0.80) return("большой")
  return("—")
}

# ============================================================
# 2. ЧАСТЬ А — МНОГОГРУППОВАЯ GRM
# ============================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ЧАСТЬ А: многогрупповая GRM\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

anchor_A <- colnames(part_A)[1:3]
cat("Якорные пункты:", paste(anchor_A, collapse = ", "), "\n")

model_A <- multipleGroup(
  part_A,
  model = 1,
  group = group_var,
  itemtype = "graded",
  invariance = anchor_A,
  technical = list(NCYCLES = 500),
  verbose = FALSE
)

coef_A_list <- coef(model_A, IRTpars = TRUE)

novice_params_A <- extract_A_params(coef_A_list[[1]])
expert_params_A <- extract_A_params(coef_A_list[[2]])

# ============================================================
# 3. ЧАСТЬ В — МНОГОГРУППОВАЯ 2PL
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ЧАСТЬ В: многогрупповая 2PL\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

anchor_B <- colnames(part_B)[1:3]
cat("Якорные пункты:", paste(anchor_B, collapse = ", "), "\n")

model_B <- multipleGroup(
  part_B,
  model = 1,
  group = group_var,
  itemtype = "2PL",
  invariance = anchor_B,
  technical = list(NCYCLES = 500),
  verbose = FALSE
)

coef_B_list <- coef(model_B, IRTpars = TRUE)

novice_params_B <- extract_2pl_params(coef_B_list[[1]])
expert_params_B <- extract_2pl_params(coef_B_list[[2]])

# ============================================================
# 4. ЧАСТЬ С — МНОГОГРУППОВАЯ 2PL
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ЧАСТЬ С: многогрупповая 2PL\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

anchor_C <- colnames(part_C)[1:3]
cat("Якорные пункты:", paste(anchor_C, collapse = ", "), "\n")

model_C <- multipleGroup(
  part_C,
  model = 1,
  group = group_var,
  itemtype = "2PL",
  invariance = anchor_C,
  technical = list(NCYCLES = 500),
  verbose = FALSE
)

coef_C_list <- coef(model_C, IRTpars = TRUE)

novice_params_C <- extract_2pl_params(coef_C_list[[1]])
expert_params_C <- extract_2pl_params(coef_C_list[[2]])

# ============================================================
# 5. DIF: СРАВНЕНИЕ ПАРАМЕТРОВ
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("DIF: сравнение параметров\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

# Часть В
comp_B <- merge(novice_params_B, expert_params_B, by = "Item", suffixes = c("_novice", "_expert"))
comp_B$diff_a <- abs(comp_B$a_novice - comp_B$a_expert)
comp_B$diff_b <- abs(comp_B$b_novice - comp_B$b_expert)
comp_B$DIF_a <- comp_B$diff_a > 0.5
comp_B$DIF_b <- comp_B$diff_b > 0.3
comp_B$DIF_overall <- comp_B$DIF_a | comp_B$DIF_b

cat("\nЧасть В: DIF =", sum(comp_B$DIF_overall), "/6 (",
    round(sum(comp_B$DIF_overall)/6*100, 1), "%)\n")
cat("  DIF-пункты:", paste(comp_B$Item[comp_B$DIF_overall], collapse = ", "), "\n")

# Часть С
comp_C <- merge(novice_params_C, expert_params_C, by = "Item", suffixes = c("_novice", "_expert"))
comp_C$diff_a <- abs(comp_C$a_novice - comp_C$a_expert)
comp_C$diff_b <- abs(comp_C$b_novice - comp_C$b_expert)
comp_C$DIF_a <- comp_C$diff_a > 0.5
comp_C$DIF_b <- comp_C$diff_b > 0.3
comp_C$DIF_overall <- comp_C$DIF_a | comp_C$DIF_b

cat("\nЧасть С: DIF =", sum(comp_C$DIF_overall), "/20 (",
    round(sum(comp_C$DIF_overall)/20*100, 1), "%)\n")

# Часть А (по среднему b)
comp_A <- merge(novice_params_A, expert_params_A, by = "Item", suffixes = c("_novice", "_expert"))
comp_A$b_mean_novice <- rowMeans(comp_A[, c("b1_novice", "b2_novice", "b3_novice", "b4_novice", "b5_novice", "b6_novice")])
comp_A$b_mean_expert <- rowMeans(comp_A[, c("b1_expert", "b2_expert", "b3_expert", "b4_expert", "b5_expert", "b6_expert")])
comp_A$diff_a <- abs(comp_A$a_novice - comp_A$a_expert)
comp_A$diff_b <- abs(comp_A$b_mean_novice - comp_A$b_mean_expert)
comp_A$DIF_a <- comp_A$diff_a > 0.5
comp_A$DIF_b <- comp_A$diff_b > 0.3
comp_A$DIF_overall <- comp_A$DIF_a | comp_A$DIF_b

cat("\nЧасть А: DIF =", sum(comp_A$DIF_overall), "/11 (",
    round(sum(comp_A$DIF_overall)/11*100, 1), "%)\n")
cat("  DIF-пункты:", paste(comp_A$Item[comp_A$DIF_overall], collapse = ", "), "\n")

# ============================================================
# 6. ESSD (РАЗМЕР ЭФФЕКТА DIF ПО МИДУ)
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ESSD — размер эффекта DIF (Meade, 2010)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

compute_essd <- function(model, part_name, col_names) {
  es_results <- empirical_ES(model)
  
  if (!is.null(es_results$ESSD)) {
    essd_values <- as.numeric(es_results$ESSD)
    if (length(essd_values) != length(col_names)) {
      essd_values <- essd_values[1:length(col_names)]
    }
    result <- data.frame(
      Пункт = col_names,
      ESSD = round(essd_values, 3)
    )
    result$Интерпретация <- sapply(result$ESSD, interpret_essd)
    result <- result[order(-result$ESSD), ]
    return(result)
  } else {
    cat("  ❌ ESSD не найден для", part_name, "\n")
    return(data.frame(Пункт = character(0), ESSD = numeric(0), Интерпретация = character(0)))
  }
}

essd_A <- compute_essd(model_A, "Часть А", colnames(part_A))
essd_B <- compute_essd(model_B, "Часть В", colnames(part_B))
essd_C <- compute_essd(model_C, "Часть С", colnames(part_C))

cat("\n--- Часть А ---\n")
print(essd_A)
cat("\n--- Часть В ---\n")
print(essd_B)
cat("\n--- Часть С ---\n")
print(essd_C)

# ============================================================
# 7. ТАБЛИЦЫ ДЛЯ СТАТЬИ
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ТАБЛИЦЫ ДЛЯ СТАТЬИ\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

item_content_A <- c(
  "1. Школьные занятия",
  "2. Разговоры об искусстве",
  "3. Друзья, интересующиеся искусством",
  "4. Интерес к искусству",
  "5. Мотивация к получению эстетического опыта",
  "6. Повседневный эстетический опыт искусства",
  "7. Семья, интересующаяся искусством",
  "8. Посещение музеев",
  "9. Книги об искусстве",
  "10. Репродукции",
  "11. Выставки"
)

# Таблица 2 — новички, часть А
tab2 <- data.frame(
  Пункт = item_content_A,
  a = round(novice_params_A$a, 3),
  b1 = round(novice_params_A$b1, 3),
  b2 = round(novice_params_A$b2, 3),
  b3 = round(novice_params_A$b3, 3),
  b4 = round(novice_params_A$b4, 3),
  b5 = round(novice_params_A$b5, 3),
  b6 = round(novice_params_A$b6, 3)
)
tab2$Слож. <- sapply(1:nrow(tab2), function(i) {
  get_difficulty_label(as.numeric(tab2[i, 3:8]))
})

# Таблица 3 — эксперты, часть А
tab3 <- data.frame(
  Пункт = item_content_A,
  a = round(expert_params_A$a, 3),
  b1 = round(expert_params_A$b1, 3),
  b2 = round(expert_params_A$b2, 3),
  b3 = round(expert_params_A$b3, 3),
  b4 = round(expert_params_A$b4, 3),
  b5 = round(expert_params_A$b5, 3),
  b6 = round(expert_params_A$b6, 3)
)
tab3$Слож. <- sapply(1:nrow(tab3), function(i) {
  get_difficulty_label(as.numeric(tab3[i, 3:8]))
})

cat("\n--- ТАБЛИЦА 2. НОВИЧКИ, ЧАСТЬ А ---\n")
print(tab2)

cat("\n--- ТАБЛИЦА 3. ЭКСПЕРТЫ, ЧАСТЬ А ---\n")
print(tab3)

# Таблица 4 — общая выборка, части В и С
item_names_B <- c("B1 Давид", "B2 Рождение Венеры", "B3 Святой Себастьян",
                  "B4 Зевс (и Леда)", "B5 Иоанн Креститель", "B6 Смешанная техника")

item_names_C <- c("C1_1 Мунк", "C1_2 Экспрессионизм", "C2_1 Джотто", "C2_2 Проторенессанс",
                  "C3_1 Ван Гог", "C3_2 Постимпрессионизм", "C4_1 Климт", "C4_2 Югендстиль",
                  "C5_1 Микеланджело", "C5_2 Возрождение", "C6_1 Рубенс", "C6_2 Барокко",
                  "C7_1 Ренуар", "C7_2 Импрессионизм", "C8_1 Дали", "C8_2 Сюрреализм",
                  "C9_1 Дюшан", "C9_2 Дадаизм", "C10_1 Уорхол", "C10_2 Поп-арт")

# Модели на общей выборке
model_B_total <- mirt(part_B, model = 1, itemtype = "2PL", technical = list(NCYCLES = 500), verbose = FALSE)
coef_B_total <- coef(model_B_total, simplify = TRUE, IRTpars = TRUE)$items
params_B_total <- data.frame(a = coef_B_total[, 1], b = coef_B_total[, 2])
rownames(params_B_total) <- colnames(part_B)

model_C_total <- mirt(part_C, model = 1, itemtype = "2PL", technical = list(NCYCLES = 500), verbose = FALSE)
coef_C_total <- coef(model_C_total, simplify = TRUE, IRTpars = TRUE)$items
params_C_total <- data.frame(a = coef_C_total[, 1], b = coef_C_total[, 2])
rownames(params_C_total) <- colnames(part_C)

tab4_B <- data.frame(
  Пункт = item_names_B,
  a = round(params_B_total$a, 3),
  b = round(params_B_total$b, 3)
)
tab4_B$Слож. <- sapply(tab4_B$b, get_difficulty)

tab4_C <- data.frame(
  Пункт = item_names_C,
  a = round(params_C_total$a, 3),
  b = round(params_C_total$b, 3)
)
tab4_C$Слож. <- sapply(tab4_C$b, get_difficulty)

cat("\n--- ТАБЛИЦА 4. ЧАСТЬ В (общая выборка) ---\n")
print(tab4_B)

cat("\n--- ТАБЛИЦА 4. ЧАСТЬ С (общая выборка) ---\n")
print(tab4_C)

# ============================================================
# 8. ТАБЛИЦА 6 — СВОДНАЯ
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ТАБЛИЦА 6. Сводные результаты DIF-анализа\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

a_A_all <- c(novice_params_A$a, expert_params_A$a)
b_A_novice <- c(as.matrix(novice_params_A[, 3:8]))
b_A_expert <- c(as.matrix(expert_params_A[, 3:8]))

a_B_all <- c(novice_params_B$a, expert_params_B$a)
b_B_all <- c(novice_params_B$b, expert_params_B$b)

a_C_all <- c(novice_params_C$a, expert_params_C$a)
b_C_all <- c(novice_params_C$b, expert_params_C$b)

cat("┌─────────────────────────┬─────────────────┬─────────────────┬─────────────────┐\n")
cat("│        Параметры        │    Часть А      │    Часть В      │    Часть С      │\n")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Количество пунктов      │       11        │        6        │       20        │\n")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Тип шкалы               │   Ликерта (1–7) │ Дихотомическая  │ Дихотомическая  │\n")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ IRT модель              │       GRM       │       2PL       │       2PL       │\n")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Диапазон a              │ [", round(min(a_A_all), 3), " – ", round(max(a_A_all), 3), "] │ [", 
    round(min(a_B_all), 3), " – ", round(max(a_B_all), 3), "] │ [", 
    round(min(a_C_all), 3), " – ", round(max(a_C_all), 3), "] │\n", sep = "")
cat("│                         │     M = ", round(mean(a_A_all), 2), "    │     M = ", 
    round(mean(a_B_all), 2), "    │     M = ", round(mean(a_C_all), 2), "    │\n", sep = "")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Диапазон b              │ Новички:        │ [", round(min(b_B_all), 2), " – ", 
    round(max(b_B_all), 2), "]  │ [", round(min(b_C_all), 2), " – ", 
    round(max(b_C_all), 2), "]  │\n", sep = "")
cat("│                         │ [", round(min(b_A_novice), 2), " – ", round(max(b_A_novice), 2), "] │     M = ", 
    round(mean(b_B_all), 2), "    │     M = ", round(mean(b_C_all), 2), "    │\n", sep = "")
cat("│                         │ Эксперты:       │                 │                 │\n")
cat("│                         │ [", round(min(b_A_expert), 2), " – ", round(max(b_A_expert), 2), "] │                 │                 │\n", sep = "")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ DIF                     │ ", sum(comp_A$DIF_overall), "/11 (", 
    round(sum(comp_A$DIF_overall)/11*100, 1), "%)    │ ", 
    sum(comp_B$DIF_overall), "/6 (", 
    round(sum(comp_B$DIF_overall)/6*100, 1), "%)     │ ", 
    sum(comp_C$DIF_overall), "/20 (", 
    round(sum(comp_C$DIF_overall)/20*100, 1), "%)     │\n", sep = "")
cat("│                         │    ", 
    paste(comp_A$Item[comp_A$DIF_overall], collapse = ", "), "     │    ",
    paste(comp_B$Item[comp_B$DIF_overall], collapse = ", "), "      │                 │\n")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Классификация DIF       │   Умеренный     │  Существенный   │  Экстенсивный   │\n")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Средний ESSD            │   ", sprintf("%.3f", mean(essd_A$ESSD, na.rm = TRUE)), 
    "      │   ", sprintf("%.3f", mean(essd_B$ESSD, na.rm = TRUE)), 
    "      │   ", sprintf("%.3f", mean(essd_C$ESSD, na.rm = TRUE)), "      │\n", sep = "")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Максимальный ESSD       │   ", sprintf("%.3f", max(essd_A$ESSD, na.rm = TRUE)), 
    "      │   ", sprintf("%.3f", max(essd_B$ESSD, na.rm = TRUE)), 
    "      │   ", sprintf("%.3f", max(essd_C$ESSD, na.rm = TRUE)), "      │\n", sep = "")
cat("├─────────────────────────┼─────────────────┼─────────────────┼─────────────────┤\n")
cat("│ Размер эффекта          │   минимальный   │      малый      │   минимальный   │\n")
cat("└─────────────────────────┴─────────────────┴─────────────────┴─────────────────┘\n")

# ============================================================
# 9. СОХРАНЕНИЕ ТАБЛИЦ
# ============================================================

write.csv(tab2, "Table2_Novice_PartA.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tab3, "Table3_Expert_PartA.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tab4_B, "Table4_PartB_Total.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tab4_C, "Table4_PartC_Total.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(essd_A, "ESSD_PartA.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(essd_B, "ESSD_PartB.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(essd_C, "ESSD_PartC.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n\n✅ Таблицы сохранены:\n")
cat("  • Table2_Novice_PartA.csv\n")
cat("  • Table3_Expert_PartA.csv\n")
cat("  • Table4_PartB_Total.csv\n")
cat("  • Table4_PartC_Total.csv\n")
cat("  • ESSD_PartA.csv, ESSD_PartB.csv, ESSD_PartC.csv\n")

cat("\n✅ АНАЛИЗ ЗАВЕРШЁН\n")

