setwd("C:/Users/kelvi/OneDrive/Documents/Data Mining/Assignment Phase 2")
library(arules)
library(stringr)
# Read .csv file for list of items and assign the column classes
goods <- read.csv("goods.csv", colClasses = c("numeric", "character", "character","numeric", "character"))

# Merge the columns and remove the "'" and "spaces"
goods$Item_name <- str_c(goods$flavor, goods$food, sep = " ")
goods$Item_name <- gsub("'", "", goods$Item_name)

# Remove unnecesary columns from data frame
goods$flavor <- NULL
goods$type <- NULL
goods$price <- NULL
goods$drink <- NULL
goods$food <- NULL

# Read .csv file for list of transactions
bakery <- read.csv("1000i.csv", header = FALSE)

# Rename header for newly created data frame
colnames(bakery) <- c("Transaction_ID","Item_count","Item_ID")

# Merge both data frames based on their Item_ID (similar header name)
bakery_merge <- merge(bakery, goods, by = "Item_ID")

# Read the merged data frame as a transaction
bakery_transaction <- as(split(bakery_merge[, "Item_name"], bakery_merge[, "Transaction_ID"]), "transactions")
inspect(bakery_transaction)

# Find rules associated to the transaction data frame (using apriori)
rules <- apriori(bakery_transaction, parameter=list(support=0.005, confidence=0.5))
inspect(rules)

# Sort the rules by lift
sort_rules <- sort(rules, by = "lift")

# Find redundant rules
subset.matrix <- is.subset(sort_rules,sort_rules)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant <- colSums(subset.matrix,na.rm = T) >= 1
which(redundant)

# Remove the redundant rules
pruned_rules <- sort_rules[!redundant]
inspect(pruned_rules)
