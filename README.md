# Business Problem
 “Global Mart” is an online store that caters to customers from across the globe. The goal is to figure out the most frequently occurring combination of the items that are bought together. This would enable you to recommend the related items to a customer, once he makes a purchase in the store. 
 
### Data Understanding
The Global Mart dataset and its data dictionary is included in the repo. Each row of the dataset represents an item of the order. However, the Order ID is not unique. Thus, the different items ordered at a time figure in different rows with the same Order ID. The number of unique products is too large a number to provide meaningful insight. Thus, the most relevant attribute to analyse would be the "Sub-Category" of the products.
 
### Data Preparation
Market Basket Analysis can be done on the transaction level data, where each row represents the items that are bought in a single transaction. To be able to use the "arules" package for association rule mining, data needs to be converted into transactions format. 
 
### Data Mining & Evaluation
Once, we have the transaction level data, where each row represents the items bought in a single transaction, we can explore the association rules from the data using apriori principle. Here the aim is to find the most business-relevant association rules, while also maximising the support, confidence or lift. we also put a cap on  transactions by considering only those which have more than a threshold itemsets.

### Important steps for Analysis

#### Step 1: (Data Understanding & Data Preparation) 
* Transform the original dataset into transaction level dataset 
* Convert the transaction level data into transaction format using "arules" package 
 
#### Step 2: (Association Rule Mining) 
* Mine association rules from the data 
* Optimise the minimum support, confidence, lift threshold level or the minimum floor on the number of items required in a transaction to qualify for consideration 
 
#### Step 3: (Rule Relevance/Evaluation) 
* The numerical value of the support, confidence and the lift level for the itemsets/rule 
* Check how logical/relevant/viable are the rules from the store's point of view 
* Explain and analyse the business implications of the rule 

Further, a presentation is also included in the repo which further amplifies and explains the code.
