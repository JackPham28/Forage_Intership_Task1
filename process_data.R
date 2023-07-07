library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
filepath <- "E:/LearningSpace/Quantium Project Intership/Task 1/"
#load data from csv file
behaviourData = fread(paste0(filepath,"QVI_purchase_behaviour.csv"))
transactionData = fread(paste0(filepath,"QVI_transaction_data.csv"))
#explore data

print(str(transactionData))
print(str(behaviourData))
print(head(transactionData,15))
print(head(behaviourData,15))
#Transform data
#Date column in transaction table is not in datetime type.
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
print(head(transactionData))

#explore PROD_name column
print(str(transactionData$PROD_NAME))

#remove extra space:
transactionData$PROD_NAME = gsub("\\s+"," ",transactionData$PROD_NAME)

#Sort to get the words of product.
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME])," ",)))
productWords
setnames(productWords,"V1","Words")
print(productWords)
#remove digit and special characters:
x= productWords[!grepl("[[:digit:]]|[[:punct:]]",productWords$Words,perl = TRUE),]
#take a look about frequency of words. (recommend to use this code)
y= x[, .N, Words]
y[order(y$N, decreasing = TRUE)]
#or: only applied with the data frame single column or simple data frames.
y = table(x)
sort(y, decreasing = TRUE)

#remove all rows with Salsa product:
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME),perl = TRUE)]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]
#another way to remove SALSA column:
transactionData$SALSA <- NULL
#Summarize the data:
summary(transactionData)
summary(transactionData$PROD_QTY)
sum(is.na(transactionData))

#investigate the case packets of chips are 200 in a transaction.
transactionData[PROD_QTY == 200,]

#the result that customer 226000 make this transaction, explore this customer:
transactionData[LYLTY_CARD_NBR == 226000]

#Filter out loyalty card number 226000 because they buy for the business purpose
transactionData[, Num := grepl(226000, LYLTY_CARD_NBR,perl = TRUE)]
transactionData <- transactionData[Num == FALSE, ][, Num := NULL]

#Re-examine transaction_Data:
summary(transactionData)
sum(is.na(transactionData))
 
#Count transaction by date: the frequency column with name is num_transactions
transactions_by_date <- transactionData[, .(num_transactions = length(TXN_ID)), by = DATE]
#or this syntax: the frequency column with name is N
transactions_by_date <- transactionData[, .N, DATE]
print(transactions_by_date)

#Create a DATE data frame:
df_date <- data.table(date = seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day"))
print(df_date)
#Create a data frame merged to explore the missing data date:
df_date_merge <- merge(df_date,transactions_by_date, by.x= "date",by.y = "DATE",all.x = TRUE)
print(df_date_merge)
#get the row with missing data date:
df_date_merge[is.na(df_date_merge$N),]

#Setting plot thếm to format graphs:
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#Plot transactions over time:
ggplot(df_date_merge, aes(x = date, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
#zoom in December 2018 by subset
subset_data <- df_date_merge[df_date_merge$date >= as.Date("2018-12-01")&df_date_merge$date <= as.Date("2018-12-31"),]
ggplot(data = subset_data) +
  geom_line(aes(x = date, y = N)) +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Create pack size:
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
#or:
transactionData[, "PACK_SIZE"] <- parse_number(transactionData$PROD_NAME)

transactionData$PACK_SIZE <-parse_number(transactionData$PROD_NAME)
#Take a look about pack size summary:
summary(transactionData$PACK_SIZE)
transactionData[, .N, PACK_SIZE][order(PACK_SIZE, decreasing = TRUE)]
#Or:
table(transactionData$PACK_SIZE)

#Plot a histogram showing the number of the transaction per pack_size:
#Create a data set to plot
subset_packsize = transactionData %>% count(PACK_SIZE)
subset_packsize[order(subset_packsize$PACK_SIZE),]
#Plot with the order of pack_size by pack_size:
ggplot(subset_packsize, aes(x= (factor(-PACK_SIZE)),y= n))+ #rearrange with "-" or "+"
  geom_bar(stat = "identity",fill= "blue",col = "black", width = 0.8) + coord_flip()+
  labs(title = "the number of the transaction per pack_size",x = "pack_size", y = "number of transaction")+
  geom_text(aes(label = round(n)), hjust = 1.1,color = "black")
#Another way:(recommended)
ggplot(subset_packsize, aes(x= reorder(PACK_SIZE,-PACK_SIZE),y= n))+
  geom_bar(stat = "identity",fill= "blue",col = "black", width = 0.8) + coord_flip()+
  labs(title = "the number of the transaction per pack_size",x = "pack_size", y = "number of transaction")+
  geom_text(aes(label = round(n)), hjust = 1.1,color = "black")

#Create brand name columns:
class(transactionData$PROD_NAME)
typeof(transactionData$PROD_NAME)
transactionData[, BRAND := word(transactionData$PROD_NAME,1)]
#another ways: use sapply function
transactionData[, BRAND := sapply(strsplit(transactionData$PROD_NAME," "), "[[",1)]
#Other ways:
transactionData[, BRAND := (substr(PROD_NAME, 1, regexpr(pattern = ' ', PROD_NAME)-1))]

#Take a look brand name:
unique(transactionData$BRAND)
table(transactionData$BRAND)
#Chang some wrong name
transactionData
transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
transactionData[BRAND == "GrnWves"|BRAND == "Grain", BRAND := "Grainwaves"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Natural", BRAND := "Naturals"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
table(transactionData$BRAND)
#Behaviour Data:
summary(behaviourData)
head(behaviourData)
#Merge data:
data <- merge(transactionData, behaviourData, all.x = TRUE)
class(data)
data[is.na(LIFESTAGE),LYLTY_CARD_NBR]

#write data to csv:
write.csv(data, file = "QVI_data.csv")
fwrite(data, "QVI_data.csv")

#Total sales by customer life stage and premium_customer:
Analytic_Segment_Customer <- data[, .(Sum_tot_sales = sum(TOT_SALES)
                                  ,num_of_cus = uniqueN(LYLTY_CARD_NBR)
                                  ,unit_per_segment = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)
                                  ,price_per_unit= sum(TOT_SALES)/sum(PROD_QTY))
                                  ,by = c("LIFESTAGE","PREMIUM_CUSTOMER")]
Analytic_Segment_Customer$SEGMENTS <- paste(Analytic_Segment_Customer$LIFESTAGE,Analytic_Segment_Customer$PREMIUM_CUSTOMER,sep = " & ")
Analytic_Segment_Customer[, LIFESTAGE := NULL][, PREMIUM_CUSTOMER := NULL]
Analytic_Segment_Customer <- Analytic_Segment_Customer[,c(3,2,1)]
#Plot1 sum of sales: 
ggplot(data = Analytic_Segment_Customer, aes(x = reorder(LIFESTAGE, Sum_tot_sales, decreasing = TRUE), y = Sum_tot_sales, fill = PREMIUM_CUSTOMER))+
  geom_bar(stat = "identity", position = "dodge",width = 0.8 )+
  labs( x = "Segments customer", y = "Sum of sales by Segment customer")+
  coord_flip()+
  geom_text(aes(label = round(Sum_tot_sales,2)), hjust = 1.1,color = "black", position = position_dodge(width = 0.8))

#Plot2 number of customer:
ggplot( data = Analytic_Segment_Customer, aes(x = reorder(LIFESTAGE, num_of_cus, decreasing = TRUE), y = num_of_cus,fill = PREMIUM_CUSTOMER))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.8)+
  labs(x= "SEGMENT Customer", y = "Number Of customer by Segment")+
  coord_flip()+
  geom_text(aes(label = num_of_cus), hjust = 1.1,color = "black", position = position_dodge2(width = 0.8))
  #guides(fill = "none")

#Plot3 unit_per_segment:
ggplot( data = Analytic_Segment_Customer, aes(x = reorder(LIFESTAGE, unit_per_segment, decreasing = TRUE), y = unit_per_segment, fill = PREMIUM_CUSTOMER))+
  geom_col(position = "dodge", width = 0.8)+
  labs(x= "Customer Segment", y = "Average Unit per Customer",title = "Average Unit Per Customer by Segment")+
  coord_flip()+
  theme(plot.title = element_text(size = 25,hjust = 0.5,face =("bold.italic")))+
  geom_text(aes(label = round(unit_per_segment,2)), hjust = 1.1,color = "black",position = position_dodge(width = 0.8))
  #guides(fill = "none")

#Plot4 Average price per unit by segment customer:
ggplot(data= Analytic_Segment_Customer, aes(x= reorder(LIFESTAGE, price_per_unit, decreasing = TRUE),y= price_per_unit, fill = PREMIUM_CUSTOMER))+
  geom_col(position = "dodge", width = 0.8)+ # 0.8 to make a space between groups
  coord_flip()+
  labs(x = "Customer Segment", y = "Price Per Unit", title = "Price Per Unit by Customer Segment", fill = "Premium customer")+
  geom_text(aes(label = round(price_per_unit,3)),hjust = 1.1, color = "black",position = position_dodge(width = 0.8))+
  theme(plot.title= element_text(size=25, hjust=0.5, face = "bold"),legend.position = "bottom")

#scale_fill_gradient(low = "blue", high = "red")+
#turn off legend:  theme(legend.position = "none")
#T-test mainstream vs premium,budget:
data[, unitprice := TOT_SALES/PROD_QTY]
#create new table to t-test
t_test <- data  %>% 
  select (LIFESTAGE, PREMIUM_CUSTOMER,unitprice) %>% #get the needed columns from data table
  mutate(group = case_when(
                ((LIFESTAGE == "YOUNG SINGLES/COUPLES" | LIFESTAGE == "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER =="Mainstream") ~ 1,
                ((LIFESTAGE == "YOUNG SINGLES/COUPLES" | LIFESTAGE == "MIDAGE SINGLES/COUPLES") & (PREMIUM_CUSTOMER =="Premium"|PREMIUM_CUSTOMER == "Budget")) ~ 2,
                TRUE ~ 0  
          )) %>%          #create group column to category 
  filter(group != 0) %>%  #filter values except 0
  select("group","unitprice")
# or:
data[,group := case_when(
       ((LIFESTAGE == "YOUNG SINGLES/COUPLES" | LIFESTAGE == "MIDAGE SINGLES/COUPLES") & PREMIUM_CUSTOMER =="Mainstream") ~ 1,
       ((LIFESTAGE == "YOUNG SINGLES/COUPLES" | LIFESTAGE == "MIDAGE SINGLES/COUPLES") & (PREMIUM_CUSTOMER =="Premium"|PREMIUM_CUSTOMER == "Budget")) ~ 2,
       TRUE ~ 0  
   )][group != 0]
#T-test:
t.test(t_test$unitprice~ t_test$group)
# p< 2.2e-16 so that mainstream is significantly higher Premium and Budget.

#Get top brands in every customer segment.
brand_group <- data[,.(count_brand = length(TXN_ID)), by =c("LIFESTAGE","PREMIUM_CUSTOMER","BRAND")]
brand_group %>%
    group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
    summarise(top_3_brands = toString(BRAND[order(-count_brand)][1:3]))%>%
    data.table()
#Kettle is most popular brand.
#Get top brand pack_size in every customer segment:
size_group  <- data[,.(count_size = length(TXN_ID)), by =c("LIFESTAGE","PREMIUM_CUSTOMER","PACK_SIZE")]
size_group  [,.("top_3_size(g)" = toString(PACK_SIZE[order(-count_size)][1:3])), by =c("LIFESTAGE","PREMIUM_CUSTOMER")][order(LIFESTAGE),]
size_group %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise("top_3_size(g)" = toString(PACK_SIZE[order(-count_size)][1:3]),
            "respective_quantity" = toString(count_size[order(-count_size)][1:3]))%>%
  data.table()
#175g the most pack size popular.
