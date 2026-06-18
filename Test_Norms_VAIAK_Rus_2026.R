###############################################################################
# РАСЧЁТ ТЕСТОВЫХ НОРМ (СТЭНЫ) ДЛЯ ВСЕХ ШКАЛ VAIAK
###############################################################################

library(dplyr)

# Загрузка данных
Fn <- "VAIAK_approbation.csv"
datall <- read.csv(Fn, sep=';')

# Сырые баллы по шкалам
part_A <- datall[, 2:12]       # 11 пунктов
part_BC <- datall[, 13:38]     # 26 пунктов (B + C)

raw_A <- rowSums(part_A, na.rm = TRUE)
raw_BC <- rowSums(part_BC, na.rm = TRUE)
raw_total <- raw_A + raw_BC

group <- datall$Expert  # 0 = новички, 1 = эксперты

# ============================================================
# 1. ФУНКЦИЯ ДЛЯ РАСЧЁТА СТЭНОВ
# ============================================================

compute_stens <- function(scores) {
  # Сортируем баллы
  sorted_scores <- sort(scores)
  n <- length(sorted_scores)
  
  # Границы процентилей для 10 стэнов (от 1 до 10)
  # Стэн 1: 0-4%, 2: 4-11%, 3: 11-23%, 4: 23-40%, 5: 40-60%, 
  # 6: 60-77%, 7: 77-89%, 8: 89-96%, 9: 96-99%, 10: 99-100%
  percentiles <- c(0, 4, 11, 23, 40, 60, 77, 89, 96, 99, 100)
  
  # Вычисляем границы стэнов
  cut_points <- quantile(scores, probs = percentiles / 100, na.rm = TRUE)
  
  # Присваиваем стэны
  stens <- cut(scores, 
               breaks = cut_points, 
               labels = 1:10, 
               include.lowest = TRUE, 
               right = TRUE)
  
  return(as.numeric(as.character(stens)))
}

# ============================================================
# 2. ФУНКЦИЯ ДЛЯ РАСЧЁТА ДИАПАЗОНОВ СТЭНОВ (ДЛЯ ТАБЛИЦЫ)
# ============================================================

compute_sten_ranges <- function(scores) {
  stens <- compute_stens(scores)
  
  ranges <- data.frame(
    Стэн = 1:10,
    Min = NA,
    Max = NA
  )
  
  for(i in 1:10) {
    vals <- scores[stens == i]
    if(length(vals) > 0) {
      ranges$Min[i] <- min(vals, na.rm = TRUE)
      ranges$Max[i] <- max(vals, na.rm = TRUE)
    } else {
      ranges$Min[i] <- NA
      ranges$Max[i] <- NA
    }
  }
  
  return(ranges)
}

# ============================================================
# 3. РАСЧЁТ НОРМ ДЛЯ ВСЕХ ГРУПП И ШКАЛ
# ============================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ТЕСТОВЫЕ НОРМЫ (СТЭНЫ) ДЛЯ ШКАЛЫ ИНТЕРЕСА К ИСКУССТВУ\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 3.1. Шкала интереса (Часть А)
cat("--- НОВИЧКИ (N =", sum(group == 0), ") ---\n")
scores_novice_A <- raw_A[group == 0]
ranges_novice_A <- compute_sten_ranges(scores_novice_A)
print(ranges_novice_A)

cat("\n--- ЭКСПЕРТЫ (N =", sum(group == 1), ") ---\n")
scores_expert_A <- raw_A[group == 1]
ranges_expert_A <- compute_sten_ranges(scores_expert_A)
print(ranges_expert_A)

cat("\n--- ОБЩАЯ ВЫБОРКА (N =", length(raw_A), ") ---\n")
ranges_total_A <- compute_sten_ranges(raw_A)
print(ranges_total_A)

# ============================================================
# 4. ШКАЛА ХУДОЖЕСТВЕННЫХ ЗНАНИЙ (ЧАСТЬ BC)
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ТЕСТОВЫЕ НОРМЫ (СТЭНЫ) ДЛЯ ШКАЛЫ ХУДОЖЕСТВЕННЫХ ЗНАНИЙ\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("--- НОВИЧКИ (N =", sum(group == 0), ") ---\n")
scores_novice_BC <- raw_BC[group == 0]
ranges_novice_BC <- compute_sten_ranges(scores_novice_BC)
print(ranges_novice_BC)

cat("\n--- ЭКСПЕРТЫ (N =", sum(group == 1), ") ---\n")
scores_expert_BC <- raw_BC[group == 1]
ranges_expert_BC <- compute_sten_ranges(scores_expert_BC)
print(ranges_expert_BC)

cat("\n--- ОБЩАЯ ВЫБОРКА (N =", length(raw_BC), ") ---\n")
ranges_total_BC <- compute_sten_ranges(raw_BC)
print(ranges_total_BC)

# ============================================================
# 5. ОБЩИЙ БАЛЛ
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ТЕСТОВЫЕ НОРМЫ (СТЭНЫ) ДЛЯ ОБЩЕГО БАЛЛА\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("--- НОВИЧКИ (N =", sum(group == 0), ") ---\n")
scores_novice_total <- raw_total[group == 0]
ranges_novice_total <- compute_sten_ranges(scores_novice_total)
print(ranges_novice_total)

cat("\n--- ЭКСПЕРТЫ (N =", sum(group == 1), ") ---\n")
scores_expert_total <- raw_total[group == 1]
ranges_expert_total <- compute_sten_ranges(scores_expert_total)
print(ranges_expert_total)

cat("\n--- ОБЩАЯ ВЫБОРКА (N =", length(raw_total), ") ---\n")
ranges_total_total <- compute_sten_ranges(raw_total)
print(ranges_total_total)

# ============================================================
# 6. СОХРАНЕНИЕ ТАБЛИЦ
# ============================================================

# Формируем финальную таблицу для шкалы интереса
norm_table_A <- data.frame(
  Стэн = 1:10,
  Новички = paste0(ranges_novice_A$Min, "–", ranges_novice_A$Max),
  Эксперты = paste0(ranges_expert_A$Min, "–", ranges_expert_A$Max),
  Общая_выборка = paste0(ranges_total_A$Min, "–", ranges_total_A$Max)
)
norm_table_A[is.na(norm_table_A)] <- "—"

# Формируем финальную таблицу для шкалы знаний
norm_table_BC <- data.frame(
  Стэн = 1:10,
  Новички = paste0(ranges_novice_BC$Min, "–", ranges_novice_BC$Max),
  Эксперты = paste0(ranges_expert_BC$Min, "–", ranges_expert_BC$Max),
  Общая_выборка = paste0(ranges_total_BC$Min, "–", ranges_total_BC$Max)
)
norm_table_BC[is.na(norm_table_BC)] <- "—"

# Формируем финальную таблицу для общего балла
norm_table_total <- data.frame(
  Стэн = 1:10,
  Новички = paste0(ranges_novice_total$Min, "–", ranges_novice_total$Max),
  Эксперты = paste0(ranges_expert_total$Min, "–", ranges_expert_total$Max),
  Общая_выборка = paste0(ranges_total_total$Min, "–", ranges_total_total$Max)
)
norm_table_total[is.na(norm_table_total)] <- "—"

# Вывод финальных таблиц
cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("ИТОГОВЫЕ ТАБЛИЦЫ ТЕСТОВЫХ НОРМ\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

cat("\n--- ШКАЛА ИНТЕРЕСА К ИСКУССТВУ (Часть А) ---\n")
print(norm_table_A)

cat("\n--- ШКАЛА ХУДОЖЕСТВЕННЫХ ЗНАНИЙ (Часть BC) ---\n")
print(norm_table_BC)

cat("\n--- ОБЩИЙ БАЛЛ ---\n")
print(norm_table_total)

# Сохранение
write.csv(norm_table_A, "Norm_Table_Interest.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(norm_table_BC, "Norm_Table_Knowledge.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(norm_table_total, "Norm_Table_Total.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 7. ДОПОЛНИТЕЛЬНО: РАСПРЕДЕЛЕНИЕ СТЭНОВ ПО ГРУППАМ
# ============================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("РАСПРЕДЕЛЕНИЕ СТЭНОВ ПО ГРУППАМ (%)\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

distribution <- function(scores, group, group_name) {
  stens <- compute_stens(scores[group == group_name])
  tab <- table(stens)
  pct <- round(tab / sum(tab) * 100, 1)
  return(pct)
}

cat("\n--- Шкала интереса (Часть А) ---\n")
cat("Новички:\n")
print(distribution(raw_A, group, 0))
cat("\nЭксперты:\n")
print(distribution(raw_A, group, 1))

cat("\n--- Шкала знаний (Часть BC) ---\n")
cat("Новички:\n")
print(distribution(raw_BC, group, 0))
cat("\nЭксперты:\n")
print(distribution(raw_BC, group, 1))

cat("\n✅ РАСЧЁТ ТЕСТОВЫХ НОРМ ЗАВЕРШЁН\n")