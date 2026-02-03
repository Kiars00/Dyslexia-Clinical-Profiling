# Dyslexia-Clinical-Profiling
Statistical pipeline for clinical profiling of cognitive load in dyslexia. Features Crawford’s Test, IES normalization, and phenotypic mapping in R.

# Clinical Profiling of Stereotype-Driven Cognitive Load in Dyslexia

## Project Overview
This repository contains the statistical pipeline developed for my MA Thesis in **Data, Methods, and Models for Language Sciences**. The research investigates how social stereotypes act as a "cognitive barrier" for adults with **Developmental Dyslexia**, saturating their limited executive resources during morphosyntactic integration.

## Key Skills Demonstrated
- **Advanced Diagnostics:** Implementation of the **Crawford-Howell Test** for single-case vs. control group comparisons (N=67).
- **Metric Innovation:** Use of **Log-transformed Inverse Efficiency Score (IES)** to resolve speed-accuracy trade-offs in clinical populations.
- **Statistical Rigor:** Normality testing (Shapiro-Wilk, Q-Q Plots), outlier management, and effect size calculation (**Cohen’s d**).
- **Data Storytelling:** Creation of **Phenotypic Maps** to visualize individual "collapse points" during high-conflict linguistic tasks.

## Analysis Pipeline
The R script follows a modular logic:
1. **Normative Modeling:** Calculating population parameters from a sample of 67 neurotypical controls.
2. **Clinical Integration:** Processing reaction times (RT) and accuracy from 8 dyslexic participants across 4 experimental conditions (Match/Mismatch x Gender Bias).
3. **Single-Case Profiling:** Applying Crawford’s Test to define individual diagnostic categories: *Baseline Crash*, *Mismatch Collapse*, and *Task Failure*.
4. **Group Comparison:** Heterogeneity analysis via **Welch T-test** and **F-test** on variances to highlight the diverse cognitive strategies in dyslexia.

## Sample Visualizations
*(Pro tip: Carica qui i tuoi PNG e inseriscili nel README!)*
- **Interaction Plot:** Visualizes the slope of cognitive cost as conflict increases.
- **Phenotypic Map:** A 2D synthesis of executive slowing (Z-score) vs. accuracy loss (Δ Errors).

##  Privacy & Reproducibility
The datasets included (`data/`) are fully masked and anonymized (Masked ID, Mail & Location) to comply with ethical guidelines and GDPR, while maintaining the exact structure required to reproduce the statistical results.

---
**Author:** Chiara Mancuso  
**Thesis Supervisor:** Prof.ssa Gloria Gagliardi  
**University of Bologna** (Expected March 2026)
