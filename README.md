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




How to Run
Step 1: Clone the repository
bash
git clone https://github.com/yourusername/VAIAK_Russian_version_approbation.git
cd VAIAK_Russian_version_approbation
Step 2: Run the EFA/CFA pipeline
r
source("scripts/EFA_CFA_MGFA_VAIAK_Rus_2026.R")
This script will:

Build a mixed correlation matrix (polychoric/tetrachoric)

Run EFA with parallel analysis (3-factor solution)

Perform CFA for Part A, Part BC, and the full model

Compute Omega McDonald's (total and hierarchical)

Test configural, metric, and scalar invariance (MGCFA)

Compare Novices vs. Experts (t-test + Hedges' g)

Generate scree and parallel analysis plots

Step 3: Run the IRT/DIF pipeline
r
source("scripts/IRT_VAIAK_Rus_2026.R")
This script will:

Fit multi-group GRM for Part A (Interest)

Fit multi-group 2PL for Part B and Part C (Knowledge)

Compare item parameters (a and b) between Novices and Experts

Detect DIF using |Δa| > 0.5 and |Δb| > 0.3 criteria

Compute ESSD effect size (Meade, 2010)

Generate Tables 2–6 from the manuscript

Step 4: Calculate test norms
r
source("scripts/Test_Norms_VAIAK_Rus_2026.R")
This script will:

Compute sten scores for all scales

Generate normative tables for Novices, Experts, and total sample

📊 Key Results (Summary)
Analysis	Key Finding
EFA	3-factor solution; KMO = 0.91; 55.3% variance explained
CFA (2-factor)	CFI = 0.990; RMSEA = 0.042; TLI = 0.990; SRMR = 0.086
Omega (Interest)	ω-total = 0.896; ω-hierarchical = 0.726
Omega (Knowledge)	ω-total = 0.911
MGCFA	Configural, metric, and scalar invariance supported (ΔCFI < 0.01)
IRT (Interest)	a: 0.84–4.01 (M = 2.01); b: -2.83–3.50
IRT (Knowledge)	a: 0.27–4.46 (M = 2.73); b: -2.78–2.32
DIF	Part A: 2/11 (18.2%); Part B: 2/6 (33.3%); Part C: 11/20 (55%)
ESSD	Minimal effect size for Parts A (0.083) and C (0.091); large for Part B (0.433)
Group comparison	Experts significantly higher on both scales (p < 0.001; Hedges' g > 0.35)
📝 Citation
If you use this code or data in your research, please cite:

Shestova, M. A. (2026). Psychometric validation of the Russian version of the Vienna Art Interest and Art Knowledge Questionnaire (VAIAK): IRT, CFA, EFA, and DIF analysis. [Manuscript submitted for publication].

Original VAIAK paper:

Specker, E., Forster, M., Brinkmann, H., Boddy, J., Pelowski, M., Rosenberg, R., & Leder, H. (2020). The Vienna Art Interest and Art Knowledge Questionnaire (VAIAK): A unified and validated measure of art interest and art knowledge. Psychology of Aesthetics, Creativity, and the Arts, 14(2), 172–185. https://doi.org/10.1037/aca0000205

📧 Contact
Maria A. Shestova
Laboratory of Art Psychology and Experimental Aesthetics
Moscow Institute of Psychoanalysis
Email: shestova-ma@inpsycho.ru
ORCID: 0000-0002-0750-1989

📜 License
This project is licensed under the MIT License — see the LICENSE file for details.

🙏 Acknowledgments
Eva Specker and colleagues for developing the original VAIAK

Faculty of Psychology, University of Vienna, for the Post-Doc Award (PA-21/1/02)

All participants who took part in the study

🔬 Methodology References
Baker, F. B., & Kim, S. H. (2004). Item response theory: Parameter estimation techniques. CRC Press.

De Ayala, R. J. (2013). The theory and practice of item response theory. Guilford Publications.

Meade, A. W. (2010). A taxonomy of effect size measures for the differential functioning of items and scales. Journal of Applied Psychology, 95(4), 728–743.

Zumbo, B. D. (2007). Three generations of DIF analyses. Language Assessment Quarterly, 4(2), 223–233.

Chalmers, R. P. (2012). mirt: A multidimensional item response theory package for the R environment. Journal of Statistical Software, 48(6), 1–29.

text

---

### Шаг 4. Нажмите **"Commit changes..."**

В правом верхнем углу (как вы делали в прошлый раз).

---

### Шаг 5. Напишите сообщение и подтвердите

В поле "Commit message" напишите:
Full README.md with complete project description

text

Нажмите **"Commit changes"**.

---

## ✅ Готово!

Теперь ваш README.md будет содержать **полный текст** со всеми разделами: Overview, Theoretical Framework, Repository Structure, Requirements, How to Run, Key Results, Citation, Contact, Acknowledgments, и References.
