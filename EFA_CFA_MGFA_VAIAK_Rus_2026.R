###############################################################################
# ЭКСПЛОРАТОРНЫЙ ФАКТОРНЫЙ АНАЛИЗ (EFA) 
###############################################################################

library(psych)

# ============================================================
# 0. ЗАГРУЗКА ДАННЫХ
# ============================================================

Fn <- "sample_300.csv"
datall <- read.csv(Fn, sep=',')

# Все 37 пунктов (столбцы 2:38)
all_items <- datall[, 2:38]
cat("Размер данных:", dim(all_items), "\n")

# Определяем типы переменных
# Столбцы 1-11 (в all_items) — политомические (A_1...A_11)
# Столбцы 12-37 (в all_items) — бинарные (B_1...C10_2)
poly_items <- colnames(all_items)[1:11]
binary_items <- colnames(all_items)[12:37]

cat("\nПолитомические пункты (", length(poly_items), "):", 
    paste(head(poly_items, 5), collapse=", "), "...\n")
cat("Бинарные пункты (", length(binary_items), "):",
    paste(head(binary_items, 5), collapse=", "), "...\n")

# ============================================================
# 1. СМЕШАННАЯ КОРРЕЛЯЦИОННАЯ МАТРИЦА
# ============================================================

cat("\n=== ПОСТРОЕНИЕ СМЕШАННОЙ КОРРЕЛЯЦИОННОЙ МАТРИЦЫ ===\n")

# Смешанная матрица: политомические (polychoric) + бинарные (tetrachoric)
mixed_cor <- mixedCor(all_items, 
                      c = which(names(all_items) %in% binary_items))
cor_matrix <- mixed_cor$rho

cat("Размер матрицы:", dim(cor_matrix), "\n")
cat("Матрица положительно определена:", all(eigen(cor_matrix)$values > 1e-10), "\n")

# ============================================================
# 2. ПРОВЕРКА ПРИГОДНОСТИ ДАННЫХ (KMO и тест Бартлетта)
# ============================================================

cat("\n=== ПРОВЕРКА ПРИГОДНОСТИ ДАННЫХ ===\n")

# --- KMO (ручной расчёт, работает с любой матрицей) ---

# Функция для ручного расчёта KMO
compute_kmo <- function(r) {
  # Проверяем, что матрица квадратная
  if(nrow(r) != ncol(r)) stop("Матрица должна быть квадратной")
  
  # Инвертируем матрицу для получения частичных корреляций
  R_inv <- tryCatch({
    solve(r)
  }, error = function(e) {
    # Если матрица сингулярна, используем псевдоинверсию
    MASS::ginv(r)
  })
  
  diag_R_inv <- diag(R_inv)
  
  # Частичные корреляции
  pcor <- -R_inv / sqrt(outer(diag_R_inv, diag_R_inv))
  diag(pcor) <- 0
  
  # Квадраты корреляций и частичных корреляций
  r2 <- r^2
  diag(r2) <- 0
  pcor2 <- pcor^2
  diag(pcor2) <- 0
  
  # KMO = сумма квадратов корреляций / (сумма квадратов корреляций + сумма квадратов частичных корреляций)
  kmo <- sum(r2) / (sum(r2) + sum(pcor2))
  
  # MSA для каждого пункта
  msa <- apply(r2, 2, sum) / (apply(r2, 2, sum) + apply(pcor2, 2, sum))
  
  return(list(KMO = kmo, MSA = msa))
}

# Вычисляем KMO
kmo_result <- compute_kmo(cor_matrix)
kmo_value <- kmo_result$KMO

cat("KMO =", round(kmo_value, 3), "\n")

# Интерпретация
if(kmo_value >= 0.9) {
  cat("  → Отличная пригодность данных для факторного анализа\n")
} else if(kmo_value >= 0.8) {
  cat("  → Хорошая пригодность данных для факторного анализа\n")
} else if(kmo_value >= 0.7) {
  cat("  → Удовлетворительная пригодность данных для факторного анализа\n")
} else if(kmo_value >= 0.6) {
  cat("  → Посредственная пригодность данных для факторного анализа\n")
} else {
  cat("  → Неудовлетворительная пригодность данных для факторного анализа\n")
}

# Выводим MSA для пунктов с низкими значениями (первые 10)
msa_sorted <- sort(kmo_result$MSA)
cat("\nMSA для пунктов (самые низкие значения):\n")
print(round(head(msa_sorted, 10), 3))
# ============================================================
# 3. ПАРАЛЛЕЛЬНЫЙ АНАЛИЗ
# ============================================================

cat("\n=== ПАРАЛЛЕЛЬНЫЙ АНАЛИЗ ===\n")
cat("Выполняется... (может занять 1–2 минуты)\n")

parallel_result <- fa.parallel(all_items, 
                               cor = "mixed",
                               fm = "minres",
                               n.iter = 100, 
                               fa = "fa", 
                               show.legend = FALSE)

n_factors <- parallel_result$nfact
cat("Параллельный анализ рекомендует:", n_factors, "факторов\n")

# Собственные значения
eigenvals <- eigen(cor_matrix)$values
cat("\nКритерий Кайзера (λ>1):", sum(eigenvals > 1), "факторов\n")
cat("Первые 10 собственных значений:\n")
print(round(eigenvals[1:min(10, length(eigenvals))], 3))

# ============================================================
# 4. EFA С 3 ФАКТОРАМИ
# ============================================================

cat("\n=== EFA С 3 ФАКТОРАМИ ===\n")
cat("Извлекаем 3 фактора с oblimin вращением...\n")

efa_3 <- fa(all_items, 
            nfactors = 3, 
            cor = "mixed", 
            rotate = "oblimin", 
            fm = "minres")

cat("\nОбъясненная дисперсия (3 фактора):\n")
print(round(efa_3$Vaccounted * 100, 1))

cat("\nСуммарная объясненная дисперсия:", 
    round(sum(efa_3$Vaccounted[2, 1:3]) * 100, 1), "%\n")

cat("\nФакторные нагрузки (нагрузки > 0.3):\n")
print(efa_3$loadings, sort = TRUE, cutoff = 0.3, digits = 2)

# ============================================================
# 5. ТАБЛИЦА С ФАКТОРНЫМИ НАГРУЗКАМИ
# ============================================================

loadings_3 <- as.data.frame(unclass(efa_3$loadings))
colnames(loadings_3) <- paste0("Фактор_", 1:3)
loadings_3$Общность <- round(efa_3$communality, 3)
loadings_3$Уникальность <- round(efa_3$uniquenesses, 3)

loadings_3$Основной_фактор <- apply(loadings_3[, 1:3], 1, function(x) which.max(abs(x)))
loadings_3$Макс_нагрузка <- apply(loadings_3[, 1:3], 1, function(x) max(abs(x)))

final_table <- loadings_3[order(loadings_3$Основной_фактор, -loadings_3$Макс_нагрузка), ]
final_table <- cbind(ПП = 1:nrow(final_table), Пункты = rownames(final_table), 
                     final_table[, c("Фактор_1", "Фактор_2", "Фактор_3", "Общность")])

cat("\n=== ТАБЛИЦА 1. ФАКТОРНЫЕ НАГРУЗКИ ===\n")
print(final_table)

# ============================================================
# 6. КОРРЕЛЯЦИИ МЕЖДУ ФАКТОРАМИ
# ============================================================

cat("\n=== КОРРЕЛЯЦИИ МЕЖДУ ФАКТОРАМИ ===\n")
factor_correlations <- efa_3$Phi
colnames(factor_correlations) <- rownames(factor_correlations) <- paste0("Фактор_", 1:3)
print(round(factor_correlations, 3))

# ============================================================
# 7. СОХРАНЕНИЕ
# ============================================================

write.csv(final_table, "efa_3_factors_sorted.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n✅ EFA ЗАВЕРШЁН\n")



# ============================================================
# 5. КОНФИРМАТОРНЫЙ ФАКТОРНЫЙ АНАЛИЗ (CFA)
# ============================================================

cat("\n=== КОНФИРМАТОРНЫЙ ФАКТОРНЫЙ АНАЛИЗ ===\n")

# 5.1. Часть А — однофакторная модель
cat("\n--- Часть А (Интерес), 1 фактор ---\n")
model_A1 <- 'Interest =~ A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + A_7 + A_8 + A_9 + A_10 + A_11'
fit_A1 <- cfa(model_A1, data = part_A, estimator = "WLSMV", ordered = colnames(part_A))

# 5.2. Часть А — двухфакторная модель (активный vs социальный интерес)
cat("\n--- Часть А (Интерес), 2 фактора ---\n")
model_A2 <- '
  Interest_Active =~ A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + A_7
  Interest_Social =~ A_8 + A_9 + A_10 + A_11
  Interest_Active ~~ Interest_Social
'
fit_A2 <- cfa(model_A2, data = part_A, estimator = "WLSMV", ordered = colnames(part_A))

# 5.3. Часть BC — однофакторная модель
cat("\n--- Часть BC (Знания), 1 фактор ---\n")
model_BC1 <- '
  Knowledge =~ B_1 + B_2 + B_3 + B_4 + B_5 + B_6 + 
               C1_1 + C1_2 + C2_1 + C2_2 + C3_1 + C3_2 + C4_1 + C4_2 + 
               C5_1 + C5_2 + C6_1 + C6_2 + C7_1 + C7_2 + C8_1 + C8_2 + 
               C9_1 + C9_2 + C10_1 + C10_2
'
fit_BC1 <- cfa(model_BC1, data = part_BC, estimator = "WLSMV", ordered = colnames(part_BC))

# 5.4. Часть BC — двухфакторная модель (B vs C)
cat("\n--- Часть BC (Знания), 2 фактора ---\n")
model_BC2 <- '
  F1 =~ B_1 + B_2 + B_3 + B_4 + B_5 + B_6
  F2 =~ C1_1 + C1_2 + C2_1 + C2_2 + C3_1 + C3_2 + C4_1 + C4_2 + 
        C5_1 + C5_2 + C6_1 + C6_2 + C7_1 + C7_2 + C8_1 + C8_2 + 
        C9_1 + C9_2 + C10_1 + C10_2
  F1 ~~ F2
'
fit_BC2 <- cfa(model_BC2, data = part_BC, estimator = "WLSMV", ordered = colnames(part_BC))

# 5.5. Общая модель — двухфакторная (Интерес + Знания)
cat("\n--- Общая модель (2 фактора: Интерес + Знания) ---\n")
model_total <- '
  Interest =~ A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + A_7 + A_8 + A_9 + A_10 + A_11
  Knowledge =~ B_1 + B_2 + B_3 + B_4 + B_5 + B_6 + 
               C1_1 + C1_2 + C2_1 + C2_2 + C3_1 + C3_2 + C4_1 + C4_2 + 
               C5_1 + C5_2 + C6_1 + C6_2 + C7_1 + C7_2 + C8_1 + C8_2 + 
               C9_1 + C9_2 + C10_1 + C10_2
  Interest ~~ Knowledge
'
fit_total <- cfa(model_total, data = datall, estimator = "WLSMV",
                 ordered = c(colnames(part_A), colnames(part_BC)))

# 5.6. Общая модель — однофакторная (для сравнения)
cat("\n--- Общая модель (1 фактор: Art Involvement) ---\n")
model_onefactor <- '
  Art_Involvement =~ A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + A_7 + A_8 + A_9 + A_10 + A_11 +
                     B_1 + B_2 + B_3 + B_4 + B_5 + B_6 + 
                     C1_1 + C1_2 + C2_1 + C2_2 + C3_1 + C3_2 + C4_1 + C4_2 + 
                     C5_1 + C5_2 + C6_1 + C6_2 + C7_1 + C7_2 + C8_1 + C8_2 + 
                     C9_1 + C9_2 + C10_1 + C10_2
'
fit_onefactor <- cfa(model_onefactor, data = datall, estimator = "WLSMV",
                     ordered = c(colnames(part_A), colnames(part_BC)))

# ============================================================
# 6. ФУНКЦИЯ ДЛЯ ИЗВЛЕЧЕНИЯ ПОКАЗАТЕЛЕЙ
# ============================================================

get_fit <- function(fit) {
  m <- fitMeasures(fit)
  data.frame(
    χ2 = round(m["chisq"], 2),
    df = round(m["df"], 0),
    `χ2/df` = round(m["chisq"] / m["df"], 2),
    CFI = round(m["cfi"], 3),
    TLI = round(m["tli"], 3),
    RMSEA = round(m["rmsea"], 3),
    RMSEA_CI = paste0("[", round(m["rmsea.ci.lower"], 3), "–", round(m["rmsea.ci.upper"], 3), "]"),
    SRMR = round(m["srmr"], 3),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 7. СВОДНАЯ ТАБЛИЦА CFA
# ============================================================

cat("\n=== СВОДНАЯ ТАБЛИЦА CFA ===\n\n")

results_cfa <- rbind(
  data.frame(Модель = "Часть А (1 фактор)", get_fit(fit_A1)),
  data.frame(Модель = "Часть А (2 фактора)", get_fit(fit_A2)),
  data.frame(Модель = "Часть BC (1 фактор)", get_fit(fit_BC1)),
  data.frame(Модель = "Часть BC (2 фактора)", get_fit(fit_BC2)),
  data.frame(Модель = "Общая (1 фактор)", get_fit(fit_onefactor)),
  data.frame(Модель = "Общая (2 фактора)", get_fit(fit_total))
)

print(results_cfa)
write.csv(results_cfa, "CFA_results.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 8. ДОПОЛНИТЕЛЬНО: КОРРЕЛЯЦИЯ МЕЖДУ ФАКТОРАМИ (ОБЩАЯ МОДЕЛЬ)
# ============================================================

cat("\n=== КОРРЕЛЯЦИЯ МЕЖДУ ФАКТОРАМИ В ОБЩЕЙ МОДЕЛИ ===\n")
if(lavInspect(fit_total, "converged")) {
  cor_factors <- lavInspect(fit_total, "cor.lv")
  cat("Корреляция Interest ~~ Knowledge:", round(cor_factors["Interest", "Knowledge"], 3), "\n")
  
  param_est <- parameterEstimates(fit_total)
  cor_info <- param_est[param_est$lhs == "Interest" & param_est$rhs == "Knowledge" & param_est$op == "~~", ]
  cat("Статистика корреляции:\n")
  cat("  Оценка:", round(cor_info$est, 3), "\n")
  cat("  Std.err:", round(cor_info$se, 3), "\n")
  cat("  Z-value:", round(cor_info$z, 3), "\n")
  cat("  P-value:", round(cor_info$pvalue, 3), "\n")
}

# ============================================================
# 9. ω-МАКДОНАЛЬДА
# ============================================================

cat("\n=== ω-МАКДОНАЛЬДА ===\n")
omega_result <- omega(all_items, nfactors = 4, cor = "mixed", rotate = "oblimin", fm = "minres")
cat("Ω-total =", round(omega_result$omega.tot, 3), "\n")
cat("Ω-hierarchical =", round(omega_result$omega_h, 3), "\n")

# ============================================================
# 10. ω ПОСЛЕ УДАЛЕНИЯ ПУНКТОВ
# ============================================================

cat("\n=== ω ПОСЛЕ УДАЛЕНИЯ ПУНКТОВ ===\n")

get_omega_total <- function(data, cor_type) {
  suppressWarnings({
    res <- omega(data, nfactors = 1, cor = cor_type, fm = "minres")
  })
  return(res$omega.tot)
}

# Часть А
omega_A_original <- get_omega_total(part_A, "poly")
results_A <- data.frame(Пункт = names(part_A), Omega_total_after = NA, Change = NA)
for(i in 1:ncol(part_A)) {
  temp <- get_omega_total(part_A[, -i], "poly")
  results_A$Omega_total_after[i] <- round(temp, 4)
  results_A$Change[i] <- round(temp - omega_A_original, 4)
}
results_A <- results_A[order(-results_A$Omega_total_after), ]
cat("\nЧасть А (исходная ω =", round(omega_A_original, 4), "):\n")
print(results_A)

# Часть BC
omega_BC_original <- get_omega_total(part_BC, "tet")
results_BC <- data.frame(Пункт = names(part_BC), Omega_total_after = NA, Change = NA)
for(i in 1:ncol(part_BC)) {
  temp <- get_omega_total(part_BC[, -i], "tet")
  results_BC$Omega_total_after[i] <- round(temp, 4)
  results_BC$Change[i] <- round(temp - omega_BC_original, 4)
}
results_BC <- results_BC[order(-results_BC$Omega_total_after), ]
cat("\nЧасть BC (исходная ω =", round(omega_BC_original, 4), "):\n")
print(results_BC)

write.csv(results_A, "omega_total_removal_A.csv", row.names = FALSE)
write.csv(results_BC, "omega_total_removal_BC.csv", row.names = FALSE)

# ============================================================
# 11. МНОГОГРУППОВОЙ CFA
# ============================================================

cat("\n=== МНОГОГРУППОВОЙ CFA ===\n")

Fn_mg <- "sample_remaining.csv"
datall_mg <- read.csv(Fn_mg, sep=',')

items_mg <- datall_mg[, 2:38]
group_factor <- factor(datall_mg$Expert, levels = c(0, 1), labels = c("Novice", "Expert"))
data_mg <- cbind(items_mg, Group = group_factor)

model_mg <- '
  Interest =~ A_1 + A_2 + A_3 + A_4 + A_5 + A_6 + A_7 + A_8 + A_9 + A_10 + A_11
  Knowledge =~ B_1 + B_2 + B_3 + B_4 + B_5 + B_6 + 
               C1_1 + C1_2 + C2_1 + C2_2 + C3_1 + C3_2 + C4_1 + C4_2 + 
               C5_1 + C5_2 + C6_1 + C6_2 + C7_1 + C7_2 + C8_1 + C8_2 + 
               C9_1 + C9_2 + C10_1 + C10_2
  Interest ~~ Knowledge
'

fit_configural <- cfa(model_mg, data = data_mg, group = "Group",
                      estimator = "WLSMV", ordered = colnames(items_mg))
fit_metric <- cfa(model_mg, data = data_mg, group = "Group",
                  estimator = "WLSMV", ordered = colnames(items_mg),
                  group.equal = c("loadings"))
fit_scalar <- cfa(model_mg, data = data_mg, group = "Group",
                  estimator = "WLSMV", ordered = colnames(items_mg),
                  group.equal = c("loadings", "intercepts"))

get_stats <- function(fit) {
  list(
    chisq = round(fitMeasures(fit, "chisq"), 2),
    df = round(fitMeasures(fit, "df"), 0),
    cfi = round(fitMeasures(fit, "cfi"), 3),
    rmsea = round(fitMeasures(fit, "rmsea"), 3),
    rmsea_ci = paste0("[", round(fitMeasures(fit, "rmsea.ci.lower"), 3), 
                      "–", round(fitMeasures(fit, "rmsea.ci.upper"), 3), "]"),
    srmr = round(fitMeasures(fit, "srmr"), 3)
  )
}

cfg <- get_stats(fit_configural)
met <- get_stats(fit_metric)
sca <- get_stats(fit_scalar)

delta_cfi_met <- met$cfi - cfg$cfi
delta_rmsea_met <- met$rmsea - cfg$rmsea
delta_cfi_sca <- sca$cfi - met$cfi
delta_rmsea_sca <- sca$rmsea - met$rmsea

results_mgcfa <- data.frame(
  Модель = c("Конфигуральная", "Метрическая", "Скалярная"),
  χ2 = c(cfg$chisq, met$chisq, sca$chisq),
  df = c(cfg$df, met$df, sca$df),
  CFI = c(cfg$cfi, met$cfi, sca$cfi),
  RMSEA = c(paste0(cfg$rmsea, " ", cfg$rmsea_ci),
            paste0(met$rmsea, " ", met$rmsea_ci),
            paste0(sca$rmsea, " ", sca$rmsea_ci)),
  SRMR = c(cfg$srmr, met$srmr, sca$srmr),
  ΔCFI = c("-", round(delta_cfi_met, 3), round(delta_cfi_sca, 3)),
  ΔRMSEA = c("-", round(delta_rmsea_met, 3), round(delta_rmsea_sca, 3))
)

cat("\n=== ТАБЛИЦА 2. МНОГОГРУППОВОЙ CFA ===\n")
print(results_mgcfa)
write.csv(results_mgcfa, "MGCFA_results.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 12. СРАВНЕНИЕ ГРУПП
# ============================================================

cat("\n=== СРАВНЕНИЕ ГРУПП ===\n")

datall_comp <- read.csv("VAIAK_approbation.csv", sep=';')
datall_comp$Group <- ifelse(datall_comp$Expert == 0, "Novice", "Expert")

datall_comp$Interest_Score <- rowSums(datall_comp[, c("A_1","A_2","A_3","A_4","A_5",
                                                      "A_6","A_7","A_8","A_9","A_10","A_11")], na.rm=TRUE)

knowledge_items <- c("B_1","B_2","B_3","B_4","B_5","B_6","C1_1","C1_2","C2_1","C2_2",
                     "C3_1","C3_2","C4_1","C4_2","C5_1","C5_2","C6_1","C6_2",
                     "C7_1","C7_2","C8_1","C8_2","C9_1","C9_2","C10_1","C10_2")
datall_comp$Knowledge_Score <- rowSums(datall_comp[, knowledge_items], na.rm=TRUE)

novice <- datall_comp[datall_comp$Expert == 0, ]
expert <- datall_comp[datall_comp$Expert == 1, ]

hedges_g <- function(nov, exp) {
  n1 <- length(nov); n2 <- length(exp)
  m1 <- mean(nov, na.rm=TRUE); m2 <- mean(exp, na.rm=TRUE)
  sd1 <- sd(nov, na.rm=TRUE); sd2 <- sd(exp, na.rm=TRUE)
  pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
  d <- (m2 - m1) / pooled_sd
  J <- 1 - (3 / (4*(n1+n2) - 9))
  return(d * J)
}

t_int <- t.test(novice$Interest_Score, expert$Interest_Score, var.equal=FALSE)
t_know <- t.test(novice$Knowledge_Score, expert$Knowledge_Score, var.equal=FALSE)

cat("\nИнтерес к искусству:\n")
cat(sprintf("  t(%.1f) = %.3f, p = %s, g = %.3f\n", 
            t_int$parameter, t_int$statistic,
            if(t_int$p.value < 0.001) "< 0.001" else round(t_int$p.value, 4),
            hedges_g(novice$Interest_Score, expert$Interest_Score)))

cat("\nХудожественные знания:\n")
cat(sprintf("  t(%.1f) = %.3f, p = %s, g = %.3f\n", 
            t_know$parameter, t_know$statistic,
            if(t_know$p.value < 0.001) "< 0.001" else round(t_know$p.value, 4),
            hedges_g(novice$Knowledge_Score, expert$Knowledge_Score)))

# ============================================================
# 13. ГРАФИКИ
# ============================================================

cat("\n=== ПОСТРОЕНИЕ ГРАФИКОВ ===\n")

eigenvals <- eigen(cor_matrix)$values

png("scree_parallel_plots.png", width = 1200, height = 600)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(eigenvals, type = "b", pch = 19, col = "blue", lwd = 2,
     main = "График каменистой осыпи", 
     xlab = "Номер компоненты", 
     ylab = "Собственное значение",
     ylim = c(0, max(eigenvals) + 1))
abline(h = 1, col = "red", lty = 2, lwd = 2)
grid()
points(n_factors, eigenvals[n_factors], col = "red", cex = 2, lwd = 3)
legend("topright", inset = c(0.02, 0.02), cex = 0.6,
       legend = c("Данные", "λ=1", paste("Реком.", n_factors, "факт.")),
       col = c("blue", "red", "red"), 
       pch = c(19, NA, 1), 
       lty = c(1, 2, 3),
       lwd = 2, bg = "white")

plot(parallel_result$fa.values, type = "b", pch = 19, col = "darkgreen", lwd = 2,
     main = "Параллельный анализ", 
     xlab = "Факторы", 
     ylab = "Собственные значения")
lines(parallel_result$fa.sim, type = "b", pch = 17, col = "orange", lwd = 2)
abline(h = 1, col = "gray", lty = 2)
abline(v = parallel_result$nfact, col = "red", lty = 3, lwd = 2)
legend("topright", inset = c(0.02, 0.02), cex = 0.6,
       legend = c("Данные", "Паралл.", "λ=1", paste(parallel_result$nfact, "факт.")),
       col = c("darkgreen", "orange", "gray", "red"), 
       pch = c(19, 17, NA, NA), 
       lty = c(1, 1, 2, 3),
       lwd = 2, bg = "white")

par(mfrow = c(1, 1))
dev.off()

cat("✅ Графики сохранены в 'scree_parallel_plots.png'\n")
cat("\n✅ ФАКТОРНЫЙ АНАЛИЗ ЗАВЕРШЁН\n")