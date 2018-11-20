#Checkpoint-1
#load the required libraries
library(plyr)
library(arules)
library(arulesViz)
library(dplyr)
library(tidyr)
library(reshape)
library(arulesViz)
library(vcd)
library(seriation)
library(igraph)
library(grid)
library(cluster)
library(TSP)
library(gclus)
library(scatterplot3d)
library(colorspace)
library(lubridate)





#load the dataset
globalmart <- read.csv("Global Superstore.csv",stringsAsFactors = FALSE)
#View the dataset
View(globalmart)
#Delete the columns not required for analysis
globalmart <- globalmart[,c(2,3,17)]
#see the structure of the dataset
str(globalmart)
#convert data to POSIXlt object
globalmart$Order.Date <- as.POSIXlt(as.Date(globalmart$Order.Date, "%d-%m-%Y"))
#convert sub category to factor
globalmart$Sub.Category <- as.factor(globalmart$Sub.Category)
str(globalmart)


#As the data is in the correct format required for creating transactions
# write it to a csv file,
write.csv(globalmart,"ItemList.csv",row.names = FALSE,quote=FALSE)


#Read the csv file written to disk as a transaction object
#Enable the remove duplicates argument. It will remove duplicates
#present in the transactions.
#As the data is in single format use the argument single and give
#appropriate column number for order.Id and subcategory
globalmart_transactions <- read.transactions(file="ItemList.csv",
                                             rm.duplicates = TRUE,
                                             format='single',
                                             sep=',',
                                             cols=c(1,3),
                                             skip=1)

#View the summary of transactions                                             
summary(globalmart_transactions)                                             
#Results of summary is as follows:-
#25035 transactions for 17 items
#Mean size of transactions is 1.893
#upto 75% of the transactions are of size 2


#see the first five transactions
inspect(globalmart_transactions[1:5])
#View the frequency of items in transactions
itemFrequency(globalmart_transactions)
#plot the item freqeuncy
itemFrequencyPlot(globalmart_transactions)
#plot the item frequency with minimum support of 10%
itemFrequencyPlot(globalmart_transactions,support=0.1)
#plot top 5 transactions
itemFrequencyPlot(globalmart_transactions,topN=5)
#plot all the transactions in descending order
itemFrequencyPlot(globalmart_transactions,topN=17,cex.names=0.8)

##############################################################################################
#Checkpoint-2

#create rules with default settings 
#Default values for support is 0.1 and
#confidence is 0.8
item_rules <- apriori(globalmart_transactions)
#No rules are created with default values, as the support and
#confidence levels are very high

#changing the default support and confidence levels
#As there is no meaning in creating rules with single items
#minlen is set for 2 items
item_rules <- apriori(globalmart_transactions,
                       parameter = list(support=0.05,confidence=0.1,
                                        minlen=2))
#After reducing the support and confidence also no rules are created.


#support and confidence are further reduced
item_rules <- apriori(globalmart_transactions,
                      parameter = list(support=0.02,confidence=0.09,
                                       minlen=2))
#22 rules created 


#Further reducing support and confidence to see how many rules are created
item_rules <- apriori(globalmart_transactions,
                      parameter = list(support=0.01,confidence=0.01,
                                       minlen=2))
#Further reducing the support and confidence increased the
#number of rules to 152.Dealing with too many rules created by very low support
#and confidence is not fruitful for the business of stores.
#so finally selecting the support and confidence which led to creation of
#22 rules

#Final ruleset
item_rules <- apriori(globalmart_transactions,
                      parameter = list(support=0.02,confidence=0.09,
                                       minlen=2))

###################################################################################
#checkpoint - 3

#Rules can be sorted by support, confidence or lift.
#sort rules by lift
rules_sorted <- sort(item_rules,by="lift")
#inspect the sorted rules
inspect(rules_sorted)

#The rules created needs to be pruned as 
#some rules are redundant and meaningless in the presence of other rules.
#For example Rule 21 and 22,
#Rule 17 and 18, convey the same information
#So further pruning the rules
subset.matrix <- is.subset(rules_sorted,rules_sorted)
subset.matrix[lower.tri(subset.matrix,diag = T)] <- NA
redundant <- colSums(subset.matrix,na.rm=T) >= 1
#View the redundant rules
which(redundant) # 11 rules are rendered redundant

#sort the pruned rules
rules_pruned <- rules_sorted[!redundant]
#Inspect the pruned rules
inspect(rules_pruned)

#plot the pruned rules
plot(rules_pruned)
#An interactive scatter plot
#End interaction after exploring the plot
interactive_plot <- plot(rules_pruned,measure=c("support","lift"),
                         shading="confidence",interactive=TRUE)
plot(rules_pruned,method="grouped")
plot(rules_pruned,method="graph")
plot(rules_pruned,method="Paracoord",measure='lift')


# The business implications of the rules mined are as follows:-
# The items which are recommended for purchase along with Art are Storage and Binders.
# The items which are recommended for purchase along with Accessories is Binders
# The items which are recommended for purchase along with storage are phones and Binders
# The items which are recommended for purchase along with phone are Binders
# The items which are recommended for purchase along with Furnishings is Binders
# The items which are recommended for purchase along with paper are storage and Binder
# The items which are recommended for purchase along with Chairs are Binders
# Binders should be recommended for purchase along with Art, Accessories, storage, phone,
#Furnishing, paper and chairs.

######################################################################################

# As Binders,storage and art are turning up in most of the rules and their
#item frequency  is also very high, so trying to
#find some rules without them which will help the business

#create a set of rules for the given support and confidence
item_rules_2 <- apriori(globalmart_transactions,
                      parameter = list(support=0.01,confidence=0.01,
                                       minlen=2))

summary(item_rules_2)

#subset rules without Binders, storage and art
rules_without_fis <- subset(item_rules_2,(subset= rhs %in% c('Accessories',
                                                                'Appliances',
                                                                 'Bookcases',
                                                                'Chairs',
                                                                'Copiers',
                                                                'Envelopes',
                                                                'Fasteners',
                                                                'Furnishings',
                                                                'Labels',
                                                                'Machines',
                                                                'Paper',
                                                                'Phones',
                                                                'Supplies',
                                                                'Tables')) & 
                              (subset= lhs %in% c('Accessories',
                                                  'Appliances',
                                                  'Bookcases',
                                                  'Chairs',
                                                  'Copiers',
                                                  'Envelopes',
                                                  'Fasteners',
                                                  'Furnishings',
                                                  'Labels',
                                                  'Machines',
                                                  'Paper',
                                                  'Phones',
                                                  'Supplies',
                                                  'Tables')))


                                                           
#inspect the subsetted rules                                                
inspect(rules_without_fis) #68 rules created.

#Rules can be sorted by support, confidence or lift.
#sort rules by lift
rules_sorted_fis <- sort(rules_without_fis,by="lift")
#inspect the sorted rules
inspect(rules_sorted_fis)


#The rules created needs to be pruned as 
#some rules are redundant and meaningless 
#So further pruning the rules
subset.matrix.2 <- is.subset(rules_sorted_fis,rules_sorted_fis)
subset.matrix.2[lower.tri(subset.matrix.2,diag = T)] <- NA
redundant.2 <- colSums(subset.matrix.2,na.rm=T) >= 1
#View the redundant rules
which(redundant.2) 

#sort the pruned rules
rules_pruned_2 <- rules_sorted_fis[!redundant.2]
#Inspect the pruned rules
inspect(rules_pruned_2[1:10])

#plot the pruned rules
plot(rules_pruned_2)
#An interactive scatter plot
#End interaction after exploring the plot
interactive_plot <- plot(rules_pruned_2,measure=c("support","lift"),
                         shading="confidence",interactive=TRUE)
plot(rules_pruned_2,method="grouped")
plot(rules_pruned_2,method="graph")
plot(rules_pruned_2,method="Paracoord",measure='lift')

# The business implications of the rules mined are as follows:-
# The items which are recommended for purchase along with Appliances is Paper.
# The items which are recommended for purchase along with Phones are Accessories and Furnishings
# The items which are recommended for purchase along with supplies is chairs
# The items which are recommended for purchase along with Envelopes are Accessories and chairs
# The items which are recommended for purchase along with Chairs is Copiers
# The items which are recommended for purhcase along with Fasteners is chairs
# The items which are recommended for purchase along with Accessories are paper and Furnishings


############################################(END of Assignment)##################################









