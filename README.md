# NBA Draft Career Longevity Analysis
Data mining and machine learning analysis of NBA Draft picks (1989–2021), examining how draft position, team, conference, and education relate to career longevity and productivity.

# Overview

This project is a graduate-level data mining and machine learning analysis examining how NBA Draft–related factors influence long-term career outcomes. Using NBA Draft data from 1989–2021, the analysis explores relationships between draft position, team and conference, educational background, and career longevity and productivity.

The project was completed as the final project for CUS 610 (Data Science Concepts and Methods) and emphasizes end-to-end analytical thinking: data preparation, model selection, evaluation, and interpretation of results.

---

# Objectives

The primary goals of this project were to:

- Identify which draft-related factors are most predictive of long-term NBA career longevity
- Compare the relative influence of draft position, team, conference, and education
- Apply multiple data mining and machine learning techniques to a real-world dataset
- Translate analytical results into clear, interpretable insights

---

## Dataset
- **Source:** [NBA Draft Basketball Player Data 1989-2021 — MattOp, Kaggle](https://www.kaggle.com/datasets/mattop/nba-draft-basketball-player-data-19892021/data)
- **Observations:** NBA draft picks with career outcome metrics  
- **Features:** Draft position, draft team, conference, college background, years active, and productivity indicators (BPM, VORP, Win Shares, etc.)

---

## Methods & Techniques
The following techniques were applied throughout the analysis:

- **Association Rule Mining**
  - Apriori algorithm
- **Correlation Analysis**
  - Pearson and Spearman correlations
- **Regression Modeling**
  - Multiple linear regression for career outcome estimation
- **Classification**
  - Decision Trees
  - Random Forest models
- **Model Evaluation**
  - Confusion matrices
  - Accuracy, precision, recall, sensitivity, and specificity

All models were evaluated with an emphasis on interpretability and comparative performance.

---

## Key Findings
- **Draft position emerged as the strongest and most consistent predictor** of career longevity and productivity, but draft data is insufficient for high-accuracy prediction.
- Educational background and draft team showed **weaker and less consistent associations** with long-term success.
- Ensemble-based models, such as Random Forests, reinforced the dominance of draft position as a predictive feature while providing a more stable model structure compared to single decision tree approaches.

---

## Project Structure
```text
nba-draft-career-longevity/
│
├── data/
│   ├── raw/              # Original dataset
│   └── processed/        # Cleaned datasets used for each analysis
│
├── src/
│   └── analysis.R        # Main analysis script
│
├── environment/
│   └── packages.R        # Reproducible R package setup
│
├── visualizations/
│   ├── raw/              # Generated plots and charts
│   └── screenshots/      # Screenshots used in report for each analysis
│
├── report/
│   └── Final_Report.pdf  # Final written paper
│
├── README.md
└── .gitignore

```

--- 

## Reproducibility

To reproduce the analysis:

1. **Clone the repository**
   ```bash
   git clone https://github.com/tomassantos484/nba-draft-career-longevity.git

2. Open the project in RStudio or desired IDE

3. Install and load required packages
   ```r
   source("src/analysis.R")

4. Run analysis
   ```r
   source("src/project_script.R")

--- 

## Tools and Technologies
- R
- RStudio
- arules, arulesViz
- dplyr
- caret
- rpart, rpart.plot
- randomForest

---

## Author

[**Tomas Santos Yciano (Connect with me on LinkedIn!)**](https://www.linkedin.com/in/tjsy/)

## License

This project is released under the MIT License.  
The dataset is provided by Kaggle and subject to its original licensing terms.
