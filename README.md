# VAIAK: Psychometric Validation of the Russian Version  
**Vienna Art Interest and Art Knowledge Questionnaire — IRT, CFA, EFA, and DIF Analysis**

---

## 📌 Overview

This repository contains the complete R code and analysis pipeline for the psychometric validation of the **Russian version of the Vienna Art Interest and Art Knowledge Questionnaire (VAIAK)**. The project includes:

- **Exploratory Factor Analysis (EFA)** with mixed correlation matrices (polychoric/tetrachoric)
- **Confirmatory Factor Analysis (CFA)** for the two-factor structure (Interest & Knowledge)
- **Omega McDonald's coefficients** (total and hierarchical) with item-deletion diagnostics
- **Multi-Group Confirmatory Factor Analysis (MGCFA)** to test configural, metric, and scalar invariance across Novice vs. Expert groups
- **Item Response Theory (IRT)** analysis:
  - Graded Response Model (GRM) for the Interest scale (Likert-type items)
  - 2-Parameter Logistic Model (2PL) for the Knowledge scale (dichotomous items)
  - Multi-group IRT with anchor items for metric linking
  - Differential Item Functioning (DIF) detection using parameter comparison (|Δa| > 0.5, |Δb| > 0.3)
  - Effect size estimation via **ESSD (Expected Score Standardized Difference)** (Meade, 2010)
- **Group comparisons** (Novices vs. Experts) with Welch's t-test and Hedges' g
- Full reproducibility of all tables from the published manuscript

---

## 🧠 Theoretical Framework

The VAIAK consists of two scales:

| Scale | Items | Response Format | IRT Model |
| :--- | :--- | :--- | :--- |
| **Art Interest** | 11 items (A1–A11) | 7-point Likert | Graded Response Model (GRM) |
| **Art Knowledge** | 26 items (B1–B6, C1_1–C10_2) | Dichotomous (0/1) | 2-Parameter Logistic (2PL) |

The questionnaire was originally developed by Specker et al. (2020). This project provides the first comprehensive IRT-based validation of the Russian adaptation.

---

## 📁 Repository Structure

