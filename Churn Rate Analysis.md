# Analysing Customer Churn Rate & Prediction Models

**View detailed report and analysis results:** [Churn Rate Analysis Report](https://github.com/helen030/DataProject/blob/96825a710f1a111a37822c6e2943bf01ed46137e/Dashboards/Churn%20Rate%20Analysis%20Report.pdf)

**View full R code:** [Churn Rate Analysis R Script](https://github.com/helen030/DataProject/blob/ed5a5b3d4b1dc5747ef500187d4f533c7fba9ed9/Dashboards/Churn%20Rate%20Analysis%20%26%20Prediction%20Model.R)

### Introduction

The project aims to analyse data of 30K+ customers of a telecommunication giant in the US and provide recommendations on how to predict customer churn and establish a target customer profile. 

### Setup - Data Manipulation & Cleaning

The first step is to remove any data that wasn't relevant to the analysis, such as handsets, equipment, and vehicle details. Next, I converted all categorical values into dummy variables (1 for “Yes”, 0 for “No”) and ensured that  all numbers were stored as numeric for easier analysis. Finally, I replaced N/A values with the average instead of removing them, since there weren't many and each customer record can provide useful insights to the analysis. 

### Descriptive Analysis - What Contributes to Customer Decision to Leave?

Initial analysis revealed that nearly 30% of customers have churned, an alarmingly high rate given the intense competition in the telecommunications industry. This indicates the company is not targeting the right audiences or leveraging effective strategies to keep its customers.

Notably, customers who stay with the company generated around 1.5x more monthly revenue than those who leave, highlighting the importance of reducing churn to maximise profitability for the company.

<img width="400" height="450" alt="image" src="https://github.com/user-attachments/assets/4297abe9-57f5-4914-a957-48f446d9a3f0" />

*Fig 1. Numbers of Churned and Not Churned Customers.*

Customer income has a strong influence on both spending behaviour and the likelihood of switching providers. Income group 0 has the highest churn rate, suggesting lower-income customers are more likely to leave. Interestingly, group 6 has the second-highest number of churned customers. Based on the assumption, this group is medium-income earners with stable financial and potentially better credit scores. 

<img width="840" height="350" alt="image" src="https://github.com/user-attachments/assets/c44a9872-067f-4b83-a5fd-e7773d24d33d" />

*Fig 2. Income Groups of Churned and Unchurned Customers.*

Credit rating also matters. Customers with higher credit scores are more likely to churn, implying that financial stability gives customers more choices and makes them more value-conscious. Overall, lower-income customers are sensitive to cost, while higher-income ones are harder to retain due to their higher expectations and wider access to alternatives.

<img width="640" height="370" alt="image" src="https://github.com/user-attachments/assets/bec03e2a-3d0d-44e6-bf12-0be443aea848" />

*Fig 3. Credit Ratings for Unchurned and Churned Customers.*

### Predictive Modelling – Identifying At-Risk Customers

Using a 70/30 train-test split on 30,000 customer records, I trained three models: a Random Forest, KNN, and Naïve Bayes, to predict which customers are likely to churn. Initial results showed all models achieved reasonable accuracy but had significantly low sensitivity, meaning they struggled to correctly identify churned customers. In fact, they were better at identifying customers who are likely NOT to churn.

Upon further investigation, I found that the issue is due to a major class imbalance, with only 29% data reflecting churned customers. To address this, I applied different balancing techniques such as random oversampling and undersampling. As a result, the random undersampling Random Forest achieved the best trade-off between accuracy and sensitivity, making it the most suitable model for this task.

<img width="875" height="166" alt="image" src="https://github.com/user-attachments/assets/9f9f081f-8f44-4b20-9ab0-a56c004f9c85" />

*Fig 4. Performance measures of Random Forest Models.*

Further evaluation using Decile-wise lift chart and ROC curves confirmed that the Random Forest model performs moderately well (AUC = 56.9%), meaning it can identify high-risk customers better than random chance but still has room for improvement. This suggests that incorporating more behavioural variables or external data could strengthen future churn prediction accuracy.

<img width="900" height="350" alt="image" src="https://github.com/user-attachments/assets/be5fdfa2-e9eb-4815-9bab-a44c695d3a57" />

*Fig 5. Assessing Random Forest Model Performance.*

### Conclusion

The analysis revealed that retention calls, household age, and income level are the three strongest predictors of customer churn. These insights can support the company in designing more targeted and data-driven retention strategies.
