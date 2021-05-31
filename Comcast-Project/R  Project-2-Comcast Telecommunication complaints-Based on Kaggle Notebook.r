rm(list=ls())

getwd()

comcast_dat = read.csv("Comcast Telecom Complaints data.csv")

head(comcast_dat)

str(comcast_dat)

#library(tibble)

comcast_dat_df <- data.frame(comcast_dat)


colnames(comcast_dat_df)

# Finding NA's in data-set
na_vector <- is.na(comcast_dat_df)
length(na_vector[na_vector==T])

library(lubridate) # Library makes it easier to work with dates and times.

comcast_dat_df$Date_new <- dmy(comcast_dat_df$Date)

head(comcast_dat_df$Date_new)

# Check if you have data of single year or multiple year

unique(as.integer(year(comcast_dat_df$Date_new)))

unique(as.integer(month(comcast_dat_df$Date_new)))

# Storing month in column 'Month' in the dataframe created
comcast_dat_df$Month = as.integer(month(comcast_dat_df$Date_new))

# Calculate daily and monthly number of complaints

library(dplyr)

Count_d  <- summarise(group_by(comcast_dat_df,Date_new), Count=n())
head(Count_d)

count_m <- summarise(group_by(comcast_dat_df,Month), Count=n())
count_m

library(ggplot2)

ggplot(data = count_m, aes(x= Month, y= Count))+
  geom_line(color="slateblue4")+
  geom_point()+
  geom_text(label= count_m$Count, nudge_x = 0.35, nudge_y = 0.45, check_overlap = T)+
  scale_x_continuous(breaks = count_m$Month)+
  labs(title = "Monthly Trend for the Number of Complaints",x= "Months",y ="No. of Complaints")

#Daily trend chart for the number of complaints

ggplot(data = Count_d, aes(x= as.POSIXct(Date_new), y= Count))+
  geom_line(color="green")+
  geom_point()+
  scale_x_datetime(breaks = "1 weeks", date_labels = "%d/%m")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title = "Daily Trend for the Number of Complaints 2015", x = "Date", y ="No. of Complaints")

# install.packages('tm', dependencies = T)


# using NLP to get highest frequency words
library(tm)
library(SnowballC)


#convert Customer.Complaint column into a corpus (collection of documents)

corp_com = Corpus(VectorSource(comcast_dat_df$Customer.Complaint))

#remove punctuations

corp_com <- tm_map(corp_com, removePunctuation)

#to lowercase

corp_cpm <- tm_map(corp_com, PlainTextDocument)
corp_com <- tm_map(corp_com, tolower)

#remove stopwords and the word "comcast"
corp_com <- tm_map(corp_com, removeWords, c("comcast", stopwords("english")))

#creating term document matrix and calculating tags frequency

m <- TermDocumentMatrix(corp_com)
mat <- as.matrix(m)
f <- sort(rowSums(mat),decreasing=TRUE)
df1 <- data.frame(word = names(f),freq=f)
head(df1, 25)

internet <- contains(comcast_dat_df$Customer.Complaint, match = 'internet',ignore.case = T)
internet

billing <- contains(comcast_dat_df$Customer.Complaint, match = 'bill',ignore.case = T)
billing

data <- contains(comcast_dat_df$Customer.Complaint, match = 'data',ignore.case = T)
data

cap <- contains(comcast_dat_df$Customer.Complaint, match = 'cap',ignore.case = T)
cap

throttle <- contains(comcast_dat_df$Customer.Complaint, match = 'throttl',ignore.case = T)
throttle

price <- contains(comcast_dat_df$Customer.Complaint, match = 'price',ignore.case = T)
price

charge <- contains(comcast_dat_df$Customer.Complaint, match = 'charge',ignore.case = T)
charge

cable <- contains(comcast_dat_df$Customer.Complaint, match = 'cable',ignore.case = T)
cable

connect <- contains(comcast_dat_df$Customer.Complaint, match = 'connect',ignore.case = T)
connect

contract <- contains(comcast_dat_df$Customer.Complaint, match = 'contract',ignore.case = T)
contract

account <- contains(comcast_dat_df$Customer.Complaint, match = 'account',ignore.case = T)

email <- contains(comcast_dat_df$Customer.Complaint, match = 'email',ignore.case = T)

xfinity <- contains(comcast_dat_df$Customer.Complaint, match = 'xfinit',ignore.case = T)

hbo <- contains(comcast_dat_df$Customer.Complaint, match = 'hbo',ignore.case = T)

modem <- contains(comcast_dat_df$Customer.Complaint, match = 'modem',ignore.case = T)

service <- contains(comcast_dat_df$Customer.Complaint, match = 'service',ignore.case = T)

comcast_dat_df$Complaint.Types[internet]

comcast_dat_df$Complaint.Types[internet] <- "Internet"
comcast_dat_df$Complaint.Types[internet]

comcast_dat_df$Complaint.Types[billing] <- ifelse(is.na(comcast_dat_df$Complaint.Types[billing]),"Billing",comcast_dat_df$Complaint.Types[billing])

comcast_dat_df$Complaint.Types[billing]

comcast_dat_df$Complaint.Types[data] <- ifelse(is.na(comcast_dat_df$Complaint.Types[data]),"Data",comcast_dat_df$Complaint.Types[data])

comcast_dat_df$Complaint.Types[cap] <- ifelse(is.na(comcast_dat_df$Complaint.Types[cap]),"Usage Cap",comcast_dat_df$Complaint.Types[cap])

comcast_dat_df$Complaint.Types[throttle] <- ifelse(is.na(comcast_dat_df$Complaint.Types[throttle]),"Throttling",comcast_dat_df$Complaint.Types[throttle])

comcast_dat_df$Complaint.Types[price] <- ifelse(is.na(comcast_dat_df$Complaint.Types[price]),"Price",comcast_dat_df$Complaint.Types[price])

comcast_dat_df$Complaint.Types[charge] <- ifelse(is.na(comcast_dat_df$Complaint.Types[charge]),"Charges",comcast_dat_df$Complaint.Types[charge])

comcast_dat_df$Complaint.Types[cable] <- ifelse(is.na(comcast_dat_df$Complaint.Types[cable]),"Cable",comcast_dat_df$Complaint.Types[cable])

comcast_dat_df$Complaint.Types[connect] <- ifelse(is.na(comcast_dat_df$Complaint.Types[connect]),"Connect",comcast_dat_df$Complaint.Types[connect])

comcast_dat_df$Complaint.Types[contract] <- ifelse(is.na(comcast_dat_df$Complaint.Types[contract]),"Contract",comcast_dat_df$Complaint.Types[contract])

comcast_dat_df$Complaint.Types[account] <- ifelse(is.na(comcast_dat_df$Complaint.Types[account]),"Account",comcast_dat_df$Complaint.Types[account])

comcast_dat_df$Complaint.Types[email] <- ifelse(is.na(comcast_dat_df$Complaint.Types[email]),"Email",comcast_dat_df$Complaint.Types[email])

comcast_dat_df$Complaint.Types[xfinity] <- ifelse(is.na(comcast_dat_df$Complaint.Types[xfinity]),"Xfinity",comcast_dat_df$Complaint.Types[xfinity])

comcast_dat_df$Complaint.Types[hbo] <- ifelse(is.na(comcast_dat_df$Complaint.Types[hbo]),"HBO",comcast_dat_df$Complaint.Types[hbo])

#assigning "Others" for rest of the complaints
comcast_dat_df$Complaint.Types[-c(internet,billing,data,cap,throttle,price,charge,cable,connect,contract,account,email,xfinity,hbo,modem,service)]<- "Others"

#final table with the frequency of all complaint types
table(comcast_dat_df$Complaint.Types)

#new categorical variable with value as Open and Closed
open_complaints <-(comcast_dat_df$Status == "Open" | comcast_dat_df$Status == "Pending")


closed_complaints <- (comcast_dat_df$Status == "Closed" | comcast_dat_df$Status == "Solved")


comcast_dat_df$complaint_status[open_complaints] <- "Open" 
comcast_dat_df$complaint_status[closed_complaints] <- "Closed"

# Statewise status of complaints
# Few states are in upper-lower case mix (e.g. District of Columbia)

comcast_dat_df$state_lower <- tolower(comcast_dat_df$State)

# Creating a count of complaint status on each state

count_states <- summarise(group_by(comcast_dat_df,state_lower,complaint_status),Count=n())

head(count_states)

# Plotting 

ggplot(as.data.frame(count_states), aes(x = state_lower, y = Count))+
  geom_col(aes(fill = complaint_status))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Complaint Status By States Stacked Bar Chart", x = "States", y = "No. of Complaints", fill= "Status")

#State having the maximum complaints (total)
max_comp <- arrange(summarise(group_by(comcast_dat_df, state_lower), Count = n()), desc(Count))
max_comp[1,]

# idetifying the state that has maximum number of Open complaints

max_open_camp <- arrange(summarise(group_by(filter(comcast_dat_df,complaint_status == "Open"), state_lower),Count=n()),desc(Count))

max_open_camp[1,]

#State having the highest percentage of unresolved complaints

count_by_states <- merge(x=max_comp,y=max_open_camp,by="state_lower", all.x = TRUE)

head(count_by_states)

# Replacing NA values by 0
count_by_states$Count.y <- ifelse(is.na(count_by_states$Count.y),0,count_by_states$Count.y)

head(count_by_states)

count_by_states <- cbind(count_by_states,"% unresolved"=(count_by_states$Count.y / count_by_states$Count.x)*100)

count_by_states <- arrange(count_by_states, desc(count_by_states["% unresolved"]))

count_by_states[1,]

#percentage of complaints resolved till date through the Internet and customer care calls

levels(as.factor(comcast_dat_df$Received.Via))

df2 <- table(comcast_dat_df$Received.Via, comcast_dat_df$complaint_status)
df2

df2 <- cbind(df2, Total = rowSums(df2))

df2

# Pie Chart for Total Closed and Open Complaints

slices <- c(843,262)
names <- c("Total Closed", "Total Open")
slices_per <- round(slices/sum(slices)*100)
slices_per <- as.character(slices_per)
slices_names_per <- paste(names, slices_per,"%")
pie(x=slices,labels = slices_names_per, col=rainbow(length(names)),
    main="Pie Chart of Total Open and Closed Complaints")

# Pie Chart for Closed and Open Complaints received via Customer Care Calls
slices1 <- c(864, 255)
names1 <- c("Closed", "Open")
slices_per1 <- round(slices1/sum(slices1)*100)
slices_per1 <- as.character(slices_per1)
slices_names_per1 <- paste(names1, slices_per1,"%")
pie(x=slices1,labels = slices_names_per1, col=rainbow(length(names1)),
    main="Closed and Open Complaints received via CC Calls")

# Pie Chart for Total Closed Complaints received via Internet and Customer Care Call

slices3 <- c(864, 843)
names3 <- c("Customer Call","Internet")
slices_per3 <- round(slices3/sum(slices3)*100)
slices_per3 <- as.character(slices_per3)
slices_names_per3 <- paste(names3, slices_per3,"%")
pie(x=slices3,labels = slices_names_per3, col=rainbow(length(names3)),
    main="Total Closed Complaints received via Internet and CC Calls")


