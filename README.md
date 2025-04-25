# 🎬 MovieLens Report
**HarvardX PH125.9x: Data Science Capstone Project**

This repository contains the complete submission for the first of two capstone projects within the **HarvardX Data Science Professional Certificate** program, hosted on edX.

## 📌 Project Overview

The objective of this project was to design, implement, and validate a movie recommendation algorithm using the **MovieLens 10M dataset**. The goal was to achieve a **Root Mean Square Error (RMSE)** below **0.86490** on a hold-out test set, as specified in the project requirements.

The solution involved:

- Exploratory data analysis of user, movie, genre, and time-based features  
- Development of progressive models using bias correction (movie, user, genre, year, review date)  
- Regularization to reduce overfitting and improve generalization  
- Final model validation using a hold-out dataset (`validation`)

## 📂 Repository Structure

- `movielens.Rmd` – Main R Markdown file containing:
  - The complete modeling pipeline  
  - Inline commentary and visualizations  
  - Code to reproduce results (including RMSE calculations)  
  - A disabled (`eval=FALSE`) code chunk that loads and partitions the dataset using the official HarvardX approach

- `preamble.tex` – LaTeX configuration to control figure/table floating in the PDF output.

- `references.bib` – BibTeX file with all sources cited in the report.

> ⚠️ Note: The `.Rmd` assumes the existence of a `fulldata.RData` file containing pre-wrangled and partitioned `edx` and `validation` datasets. This file is not included here due to size constraints. Instructions and code to generate this file are included (but not evaluated) within the R Markdown file.

## 📖 License & Attribution

This project was completed as part of the **HarvardX PH125.9x: Data Science Capstone**, in accordance with the [edX Honor Code](https://courses.edx.org/edx-terms-service).
