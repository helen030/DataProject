# Analysing Customer Churn Rate & Prediction Models

**View Full Report:** [Churn Rate Analysis](https://github.com/helen030/DataProject/blob/bc43fa480910259d9968d9aee3b4b05a45c7a99f/Dashboards/Kenya%20GBV%20Dashboard.pbix)

**View R Code:** [Analysis Script](https://github.com/helen030/DataProject/blob/bc43fa480910259d9968d9aee3b4b05a45c7a99f/Dashboards/Kenya%20GBV%20Dashboard.pbix)

### Goals

The dashboard aims to identify hotspots and trends in GBV incidents by pinpointing geographical areas and temporal patterns. It also investigates gender vulnerability by connecting different variables to understand contributing factors and assesses the effectiveness of existing prevention programs and responses. A set of actionable recommendations is then provided to combat the situations and allocate resources more effectively.

### Dataset Information

The dataset contains reported GBV cases in Kenya from 2017 to 2019, which were collected from slum areas where the organisation operates. It includes sensitive information such as victim demographics, residential areas, case categories, reporting methods, and witness details.

### Setup

The first step after loading the data into Power BI is data cleaning. This included imputing missing values where all null values were populated with the most common value in their respective columns. Furthermore, I verified that each field had the correct data type and that there are no duplicate entries. Data profiling was also used to ensure the data met predefined quality standards. The data volume was within expected limits at over 1000 rows for the fact table and ranging from 2 to 13 fields across various dimension tables. 

<img width="800" alt="GBV_ETL process" src="https://github.com/user-attachments/assets/33bfd3d9-150f-4ed0-8c23-ee3e6fba86e4">

*Fig 1. Dataset after it was loaded and cleaned.*

Then, I split the dataset into different dimensions and fact tables to create a data model based on the Star Schema approach. This enhanced the data integrity and allowed for more accurate querying, filtering and slicing of data. 

<img width="400" height="400" alt="Screenshot 2024-09-18 152146" src="https://github.com/user-attachments/assets/4b78d83a-4512-4610-81e2-f87d1e03cced">

*Fig 2. Star Schema data model.*

### Dashboard Design and Creation

With the processed data, I created a GBV dashboard that presents key insights on hotspots and temporal patterns, victim vulnerability, perpetrator profiles, and program impacts. The dashboard's visualisations help inform the organisation's decision-making process and guide targeted efforts to reduce case frequency, better victim support, and more effectively use resources.

<img width="850" height="450" alt="Screenshot 2024-09-18 152547" src="https://github.com/user-attachments/assets/19f2b36c-fe62-4ee9-a8cc-955da8ffbe56">

*Fig 3. Overview page of the dashboard.*

Additionally, I added slicers and navigation buttons between pages to the dashboard. These slicers allow the user to filter the data by various criteria such as time, the victim's gender, and slum areas. These provide a more dynamic and interactive user experience, allowing the user to explore the data in greater detail and gain deeper insights.

<img width="600" height="400" alt="image" src="https://github.com/user-attachments/assets/c2e76246-cb40-4386-bd1a-cab84ff7499e" />

*Fig 4. Slicers in the dashboard.*

### Findings and Conclusion

The dashboard uncovers that women aged 25–34 face the highest burden of domestic abuse, with Kibera emerging as a hotspot of reported gender-based violence. Social and cultural norms in Kenya appear to reinforce gendered vulnerabilities, placing young women in harm’s way, while boys face a greater risk of abuse during early childhood. Notably, the lack of well-wisher intervention suggests a powerful stigma within communities, where silence is often chosen over reporting signs of violence.

<img width="770" height="350" alt="image" src="https://github.com/user-attachments/assets/6b25f5ac-86a1-4c8e-b711-fda004833882" />

*Fig 5. Key insights.*

Overall, the dashboard turns raw GBV case data into actionable insights and delivers a data-driven foundation that allow stakeholders to quickly spot high-risk locations, vulnerable demographics, and recurring temporal patterns. With interactive filters, dynamic visuals, and multi-page navigation, it empowers users to explore causes, evaluate intervention effectiveness, and guide targeted prevention strategies.

