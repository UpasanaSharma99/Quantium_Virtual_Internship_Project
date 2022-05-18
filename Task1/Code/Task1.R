# Loading all necessary libraries
library(data.table)
library(readr)
library(ggplot2)
library(ggmosaic)

# Opening dataset files
filePath <- "C:/Richa/Internships/Quantium/Task1/"
transactionData <- fread(paste0(filePath,"QVI_transaction_data.csv"))
customerData <- fread(paste0(filePath,"QVI_purchase_behaviour.csv"))

# View the structure of transactionData table
str(transactionData)
head(transactionData)

# Converting the date from integer format to date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
head(transactionData)

# Checking the column PROD_NAME by its summary
transactionData[, .N, by = PROD_NAME]

# Checking for incorrect entries other than chips
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), "
")))
setnames(productWords, 'words')

# Filtering data for digits and special characters

# Removing digits
productWords <- productWords[grepl("\\d", words) == FALSE, ]

# Removing special characters
productWords <- productWords[grepl("[:alpha:]", words), ]

# Check the data
head(transactionData)

# Let's look at the most common words by counting the number of times a word appears and sorting them by this frequency in order of highest to lowest frequency
productWords[, .N, words][order(N, decreasing = TRUE)]

# Check for nulls
transactionData[is.null(PROD_NAME), .N]

# Remove salsa from the items list
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]
summary(transactionData)

# Filter the data to find the outlier 
transactionData[PROD_QTY == 200]

# See if this customer has had any other transactions
transactionData[LYLTY_CARD_NBR == 226000]

# Filtering the customer based on the loyalty card number
transactionData = transactionData[LYLTY_CARD_NBR != 226000]
summary(transactionData)

# Count the number of transaction by date
transactionData[, .N, by = DATE]

# Creating a sequence of dates and join this the count of transactions by date
transactions_by_date <- as.data.table(transactionData)
transactions_by_date <- data.frame(transactions_by_date[, .N, by = DATE])

# Setting plot theme to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Plot transactions over time
ggplot(transactions_by_date, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Filter to December and look at individual days
ggplot(transactions_by_date[transactions_by_date$DATE >= "2018-12-10" & transactions_by_date$DATE <= "2018-12-31", ], aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Date", y = "Number of transactions", title = "Transactions over time") +  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Create pack size
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

# Checking for any outliers
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

# Let's check the output of the first few rows to see if we have indeed picked out pack size.
transactionData

# Plotting a histogram showing the number of transactions by pack size
hist(transactionData[, PACK_SIZE])

# Creating a column of brand names from the first word of column Product names
transactionData$BRAND <- gsub("([A-Za-z]+).*", "\\1", transactionData$PROD_NAME)
View(transactionData)

# Identify unique brand names
transactionData[, .N, by = BRAND][order(-N)]

# Clean Brand Names
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUSIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]

# Checking Again
transactionData[, .N, by = BRAND][order(-N)]

# View the structure of customerData table
str(customerData)
summary(customerData)
head(customerData)

# Checking customer data by lifestage column
customerData[, .N, by = LIFESTAGE][order(-N)]

# Plotting a graph 
ggplot(customerData[, .N, by = LIFESTAGE][order(N)], aes(x = LIFESTAGE, y = N)) +
  geom_bar(stat = "identity", fill = "blue") + labs(x= "Life Stage", y = "Number of Customers", title = "Number of customers on each lifestage") + 
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5))

# Checking customer data by permium customers column
customerData[, .N, by = PREMIUM_CUSTOMER][order(-N)]

# Plotting a graph
ggplot(customerData[, .N, by = PREMIUM_CUSTOMER][order(N)], aes(x = PREMIUM_CUSTOMER, y = N)) +
  geom_bar(stat = "identity", fill = "blue") + labs(x= "Budget type", y = "Number of Customers", title = "Number of customers on each budget type") + 
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5))

# Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)
View(data)

# Checking for Nulls in data
data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]

# Saving this dataset as a csv file
fwrite(data, paste0(filePath,"QVI_data.csv"))

## Data Analysis on Customer Segment

# calculating total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

# Plotting a graph
ggplot(data = sales) +
  geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
  fill = PREMIUM_CUSTOMER)) + 
  labs(x = " Lifestage", y = "Premium Customers", title = "Proportion of sales") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Calculating number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

# Plotting a graph
ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) + 
  labs(x = " Lifestage", y = "Premium Customers", title = "Proportion of customers") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVERAGE = sum(PROD_QTY) / uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVERAGE)]

# Plotting a graph
ggplot(data = avg_units, aes(weight = AVERAGE, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + 
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data[, .(AVERAGE = sum(TOT_SALES) / sum(PROD_QTY)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVERAGE)]

# Plotting a graph
ggplot(data = avg_price, aes(weight = AVERAGE, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) + 
  geom_bar(position = position_dodge()) +
  labs(x = "Lifestage", y = "Avg price per transaction", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

priceperunit <- data[, price := TOT_SALES / PROD_QTY]

# Performing an independent t-test between mainstream vs premium and budget midage and young singles and couples
t.test(data[LIFESTAGE %in% c("YOUNG SINGELS/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGELS/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER != "Mainstream", price], 
       alternative = "greater")
# --------- p-value < 2.2e-16 ---------

# Deep dive into Mainstream, young singles/couples
segment_1 <- data[LIFESTAGE == "YOUNG SINGELS/COUPLES" & PREMIUM_CUSTOMER == "Mainstream", ]
others <- data[!(LIFESTAGE == "YOUNG SINGELS/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"), ]

# Comparing brand affinity to the rest of the population
quantity_segment_1 <- segment_1[, sum(PROD_QTY)]
quantity_others <- others[, sum(PROD_QTY)]

quantity_segment_1_by_brand <- segment_1[, .(targetSegment = sum(PROD_QTY) / quantity_segment_1), by = BRAND]
quantity_others_by_brand <- others[, .(others = sum(PROD_QTY) / quantity_others), by = BRAND]
brand_proportions <- merge(quantity_segment_1_by_brand, quantity_others_by_brand)[, affinityToBrand := targetSegment / others]
brand_proportions[order(-affinityToBrand)]

# Preferred pack size compared to the rest of the population
quantity_segment_1_by_pack <- segment_1[, .(targetSegment = sum(PROD_QTY) / quantity_segment_1), by = PACK_SIZE]
quantity_others_by_pack <- others[, .(others = sum(PROD_QTY) / quantity_others), by = PACK_SIZE]
pack_proportions <- merge(quantity_segment_1_by_pack, quantity_others_by_pack)[, affinityToPack := targetSegment / others]
pack_proportions[order(-affinityToPack)]

data[PACK_SIZE == 270, unique(PROD_NAME)]
