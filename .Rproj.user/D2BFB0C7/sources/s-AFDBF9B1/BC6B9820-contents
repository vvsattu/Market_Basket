#--------------------Food Recommendation System for a Cafe ---------------------------
#Arules - to provide frequent itemset and association rules

install.packages("arules")
library(arules)
#Plyr - to split and mash data together
install.packages("plyr", dependencies= TRUE)
library('plyr')
#ARulesViz - visualization of assosiation rules and item set
library(arulesViz)
#read and load data set
Bakery = read.csv("C:/Users/Hp/Desktop/Predictive
analytics/BreadBasket_DMS.csv")
Bakery
str(Bakery)
#95 unique items
unique(Bakery$Item)

#min transacion is 1 and max 9684

summary(Bakery$Transaction)

#frequeny of each data item

summary(Bakery$Item)

sort(table(Bakery$Item))

11 | P a g e

#sort transactions in ascending order
df_sorted <- Bakery[order(Bakery$Transaction),]
df_sorted$Transaction <- as.numeric(df_sorted$Transaction)
str(df_sorted)
#pivots the item descriptions with same date and same member number in one line,
df_itemList <- ddply(Bakery, c("Transaction",
                               "Date"),function(df1)paste(df1$Item,
                                                          collapse =","))
df_itemList$Transaction <- NULL
df_itemList$Date <- NULL
#Rename column headers for ease of use
colnames(df_itemList) <- c("itemList")
#write.csv used to export data set
write.csv(df_itemList, "ItemList.csv", quote = FALSE, row.names = TRUE)
#Generating association rules - Converting to transaction format
txn=read.transactions(file="ItemList.csv", rm.duplicates = TRUE,
                      format = "basket", sep = ",", cols=1);
txn@itemInfo$labels <- gsub("\"","", txn@itemInfo$labels)
txn
basket_rules <-apriori(txn,parameter = list(sup=0.01, conf=0.01));

12 | P a g e

if(sessionInfo()['basePkgs']== "tm" | sessionInfo()['otherPkgs'] == "tm"){
  detach(package:tm, unload=TRUE)
}
inspect(basket_rules)
df_basket <- as(basket_rules, "data.frame")
View(df_basket)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method= "graph", control=list(type="items"))
plot(basket_rules, method= "paracoord", control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)
itemFrequencyPlot(txn, topN=5)
