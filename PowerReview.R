#Ref: working with dates:
#https://blog.exploratory.io/5-most-practically-useful-operations-when-working-with-date-and-time-in-r-9f9eb8a17465

#go to GRU download file and import dataset
#https://www.gru.com/

#read file to smaller name:
#dfgru <- `gru.billing.history.(3)`

library(tidyverse) #import the needed libraries
library(lubridate)
library(stringr)


#location of files and folders:
code_folder <- "/Users/matt/R Code" #location of R code
input_folder <- "/Users/matt/R csvData"  #location of csv data
output_folder <- "/Users/matt/R Output"  #where output goes

###############
code_file = "GRU1.R"
input_data = "GRU.csv"
###############

lookup = paste(input_folder,'/',input_data, sep="") #folder + file for lookup

dfgru <- read.csv(lookup) 

######reads from imported dataset
#dfgru <- `gru.billing.history.(3)`

######write the original file to the csv folder
#write_csv(dfgru ,lookup)


##########functions:

#strip dollar function
strip_dollars = function(x) { 
  as.numeric(sub("\\$","",x)) 
}

#print function - looks for output folder
printGraph <- function(gName){
  ggsave(paste(output_folder,'/',gName, ".png",sep=""), width = 24, height = 16, units = "cm")
}



df <- dfgru %>%
  mutate(Namount = as.numeric(strip_dollars(Amount))) %>% #strip dollar
  mutate(Ndate = mdy(Date))%>%
  mutate(month_name = month(Ndate, label = TRUE))%>% #get the month names
  select(Ndate, month_name,Namount) 
#df

#change to tibble
df <- tbl_df(df)
#df

df <- df%>%
  mutate(solar = Ndate > '2016-09-20')

sol <- df%>%
  filter(Ndate == '2016-10-20')


g <- ggplot(data= df) 
g +  geom_point(aes(x = Ndate, y = Namount, group= Ndate, col=solar), size = 2) +
geom_smooth(aes(x = Ndate, y = Namount,group=1), method = "loess") +
geom_line(aes(x = Ndate, y = Namount,group=1), linetype = "solid",color = "coral1") +
  labs(title = "GRU Monthly Bill",
       subtitle = paste("Bill by Month:",min(dfb$Ndate),"to",max(dfb$Ndate), sep=" "),
       x = "Date", y = "$ Amount")
printGraph("GRUsmooth")

######### - use geom_col for bar of values

g <- ggplot(data= df)
g +  geom_col(aes(x = Ndate, y = Namount, group = month(Ndate), 
              col= year(Ndate), fill= year(Ndate))) +
     #geom_point(x=sol$Ndate, y=sol$Namount + 25,size = 2, col="red",alpha = 0.7) +
  geom_text(x=sol$Ndate, y= -10,label = "x",size = 6, col="red",alpha = 0.7) +
  labs(title = "GRU Monthly Bill",
       subtitle = paste("Bill by Month:",min(dfb$Ndate),"to",max(dfb$Ndate), sep=" ",
                        ", Solar Install: red 'x'"),
       x = "Date", y = "$ Amount",
       col='Year', fill = 'Year') +
  scale_x_date(date_labels = "%b", date_breaks = "2 month") 
printGraph("GRUbar")



#group by months

dfa <- df %>%
  group_by(month_name) %>%
  mutate(meanAmount = mean(Namount)) %>% #gets the mean for each month
  select (meanAmount, month_name, Ndate, Namount)

#position dodge allowed the mean values, without it the values were added
g <- ggplot(data= dfa)
g +  geom_col(aes(x = month_name, y = meanAmount, group = month_name, 
          col= month_name, fill= month_name),position = "dodge") +
  theme(legend.position="none") +
  labs(title = "GRU Monthly Bill",
  subtitle = paste("Average Bill by Month:",min(dfa$Ndate),"to",max(dfa$Ndate), sep=" "),
  x = "Date", y = "$ Amount (mean)",
  col='Avg', fill = 'Avg')
printGraph("GRUbarAvg")

#group by month - oldest dates first

dfb <- dfa%>%
  arrange(Ndate)

g <- ggplot(data= dfb)
g +  geom_col(aes(x = reorder(month_name,Ndate), y = Namount, group = year(Ndate), 
                  col= year(Ndate), fill= year(Ndate)),position = "dodge") +
  labs(title = "GRU Monthly Bill",
       subtitle = paste("Bill Grouped by Month:",min(dfb$Ndate),"to",max(dfb$Ndate), sep=" "),
       x = "Date", y = "$ Amount (mean)",
       col='Avg', fill = 'Avg')
printGraph("GRUbarGroup")

################ try to do a monte carlo simulation on the power bill

gru_log_bill <- df %>%
  mutate(logAmount = log(Namount))
  
#Applying the log-transformation, we can visually see that the bills 
# are approximately normally distributed:

# Plot the log-return, needs more data points, but trying to be normal  
gru_log_bill %>%    
  ggplot(aes(x = logAmount)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5) 


gru_log_bill %>% 
  mutate(transAmount = trans_log(logAmount))

#We can examine the distribution of log returns by applying the 
#quantile() function.

probs <- c(.005, .025, .25, .5, .75, .975, .995)

dist_log_bills <- gru_log_bill$logAmount %>%  
  quantile(probs = probs, na.rm = TRUE)

dist_log_bills

#proabilities for bills converted back from log:
dist_log_bills %>% exp()

mean_log_bills <- mean(dist_log_bills, na.rm = TRUE)
sd_log_bills <- sd(dist_log_bills, na.rm = TRUE)

mean_log_bills
sd_log_bills

#Finally, to get the actual returns, we need to re-transform the log 
#returns. Pipe the mean of the log returns (mean_log_returns) to exp():

mean_log_bills %>% exp()
sd_log_bills %>% exp()

#MONTE CARLO SIMULATION - for future trial...

  
  

