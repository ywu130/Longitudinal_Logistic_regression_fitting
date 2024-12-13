# **Longitudinal Logistic Regression Fitting**

This repository showcases logistic regression model fitting for two-dimensional cell growth with limited growing space (e.g., tissue culture areas).  

---

### **Project Overview**
The primary objective of this project is to:
- Identify if a treatment affects cell growth post-confluence.
- Analyze growth dynamics using a logistic regression model to fit cell growth data over time.  

**Key focus:**  
Increased fitted maximum cell growth (asymptote) may indicate a loss of contact inhibition, which is a hallmark of oncogenesis.

---

### **Background**
- **Cell growth in limited space:** Cells in a tissue culture grow until they reach confluence (the maximum capacity of the culture area).  
- **Contact inhibition:** Normal cells stop dividing upon confluence. Loss of contact inhibition is associated with tumorigenic behavior.  
- **Logistic regression fitting:** The logistic growth model is used to assess changes in growth patterns and to determine whether treatments alter the asymptote (maximum growth capacity).

---

### **Analysis Pipeline**
1. **Data Preparation:**  
   - Time-series data of cell growth in treated and control groups.
   - Normalization and preprocessing to ensure model accuracy.
2. **Model Fitting:**  
   - Logistic regression is applied to fit growth curves for each condition.
   - Extract parameters such as the growth rate, maximum growth (asymptote), and inflection point.
3. **Interpretation:**  
   - Compare parameters between treated and control groups.
   - Assess whether the treatment leads to increased asymptote values, indicating loss of contact inhibition.

---

### **Repository Contents**
- **Scripts:** R scripts for data preprocessing, model fitting, and visualization.
- **Data:** Sample or representative datasets used for analysis.
- **Results:** Plots and statistical summaries of fitted logistic regression models.

---

### **How to Use This Repository**
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/longitudinal-logistic-regression.git
