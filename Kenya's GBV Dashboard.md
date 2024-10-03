# Kenya's Gender-Based Violence Dashboard

**Code:** [KenyaPulse GBV Dashboard](https://app.powerbi.com/groups/me/reports/49d2855c-8a61-40f8-9d9a-d3f427f68b62/ReportSectione90a9dd27c477665e969?experience=power-bi&bookmarkGuid=Bookmark83fa3b35183e0e2b2b26)

### Project Description

This dashboard helps the organisation understand factors contributing to gender-based violence (GBV) incidents in Kenya better. Through assessing different factors, a set of actionable recommendations is provided for the organisation to combat the situations and allocate resources more effectively.

### Dataset Information

The dataset contains reported GBV cases in Kenya from 2017 to 2019, which was collected from slum areas where the organisation operates. It includes sensitive information such as victim demographics, residential areas, case categories, reporting methods, and witness details.

### ETL Process

The first step after loading the data into Power BI is data cleaning. This included imputing missing values where all null values were populated with the most common value in their respective columns. Furthermore, I verified that each field had the correct data type and there is no duplicate entries. Data profiling was also used to ensure the data met predefined quality standards. The data volume was within expected limits at over 1000 rows for the fact table and ranging from 2-13 fields across various dimension tables. 

<img width="959" alt="GBV_ETL process" src="https://github.com/user-attachments/assets/33bfd3d9-150f-4ed0-8c23-ee3e6fba86e4">

*Fig 1. Dataset after it was loaded and cleaned.*

Then, I split the dataset into different dimension and fact tables to create a data model based on the Star Schema approach. This enhanced the data integrity and allowed for more accurate querying, filtering and slicing of data. 

<img width="504" alt="Screenshot 2024-09-18 152146" src="https://github.com/user-attachments/assets/4b78d83a-4512-4610-81e2-f87d1e03cced">

*Fig 2. Star Schema data model.*

### Dashboard Building

With the processed data, I created a GBV dashboard that presents key insights on hotspots and temporal patterns, victim vulnerability, perpetrator profiles, and program impacts. The dashboard's visualisations help inform the organisation's decision-making process and guide targeted efforts to reduce case frequency, better victim support, and more effective use of resources.

<img width="959" alt="Screenshot 2024-09-18 152547" src="https://github.com/user-attachments/assets/19f2b36c-fe62-4ee9-a8cc-955da8ffbe56">

*Fig 3. Overview page of the dashboard.*

In addition, I also added slicers and navigation buttons between pages to the dashboard. These slicers allow the user to filter the data by various criteria such as time, victim's gender, and slum areas. These provide a more dynamic and interactive user experience, allowing the user to explore the data in greater detail and gain deeper insights.

<img width="755" alt="Screenshot 2024-09-18 154017" src="https://github.com/user-attachments/assets/a938009d-8b2f-4dd3-b92a-36db2ee3f283">

*Fig 4. Slicers in the dashboard.*
