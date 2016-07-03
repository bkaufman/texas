### finance hw1 ###
getwd()

if(!require(dplyr))
{
  install.packages('dplyr')
  library(dplyr)
}

if(!require(readr))
{
  install.packages('readr')
  library(readr)
}

#import data set
df <- read_csv('HW1 Data.csv')

#convert dates to readable format. 
df$stock_ID <- as.factor(df$stock_ID)
df$date <- as.Date(df$date, "%d%b%Y")

#df <- slice(df,1:10000)

#remove nulls
df2 <- df[!is.na(df$price),]
df2 <- df[!is.na(df$return_incl_divs),]
df2$dividend_declared <- ifelse(df2$return_ex_divs==df2$return_incl_divs,0,1)

#remove duplicate rows
df3 <- unique(df2)
df4 <- df3

#cumulative sum and all dividend payouts
bmk = df4 %>% group_by(stock_ID) %>% arrange(date) %>% mutate(cs = cumsum(dividend_declared))
bmk$div <- ifelse(bmk$dividend_declared == 1, ifelse(bmk$dividend_declared==bmk$cs,1,0),0)

## group by apply
#if cum sum is more than 3 times, you change it to one
## assumption is stocks can at most pay yearly dividends
bmk2 = bmk %>% 
  group_by(stock_ID) %>% 
  arrange(date) %>% 
  transform(newdiv = inverse.rle(within.list(rle(cs), 
                                          values[c(FALSE,(lengths >12)[-length(lengths)])] <- 1)))

#put in the flag
bmk2$divfin <- ifelse(bmk2$dividend_declared == 1, ifelse(bmk2$dividend_declared==bmk2$newdiv,1,0),0)

#put in the 12 1's
bmk3 = bmk2 %>% 
  group_by(stock_ID) %>% 
  arrange(date) %>% 
  mutate(rn = row_number(cs)) %>% 
  transform(div_own = c(stats::filter(divfin,c(rep(0,12),0,seq(1,12)),circ=T)))

###### stock splits  ######

spli <- subset(bmk3, , -c(dividend_declared, cs, div, newdiv, divfin))
spli$split <- ifelse(is.na(spli$shrout_adj_factor),0,1)

spli_2 <- spli %>% 
  group_by(stock_ID) %>% 
  arrange(date) %>% 
  transform(split_final = c(stats::filter(split,c(rep(0,12),0,seq(1,12)),circ=T)))

#final clean up
spli_2 <- spli_2 %>%
  group_by(stock_ID) %>%
  arrange(date, shrout_adj_factor) %>%
  distinct(date)

spli_2[spli_2$rn <= spli_2$split_final,]$split_final <- 0
spli_2[spli_2$rn <= spli_2$div_own,]$div_own <- 0
spli_2[spli_2$split_final>0,]$split_final <- 1
spli_2[spli_2$div_own>0,]$div_own <- 1
spli_2$split <- NULL
spli_2$both <- spli_2$div_own + spli_2$split_final
spli_2[spli_2$both>0,]$both <- 1
spli_2$rn <- NULL

### average returns ###

### dividend only portfolio ####
head(spli_2)
colnames(spli_2)
spli_3 <- spli_2
spli_3$div_ret <- spli_3$div_own * spli_3$return_incl_divs
spli_3$mktcap <- spli_3$price * spli_3$shrout
spli_group <- group_by(spli_3, date, div_own)
div_returns <- spli_group %>% 
  summarise(ret_month = mean(div_ret), val_month = weighted.mean(div_ret,mktcap), MktRF = mean(MktRF),
            SMB = mean(SMB), HML = mean(HML), RF = mean(RF), MOM = mean(MOM)) %>%
  mutate(excess_ret = ret_month - RF, excess_ret_val = val_month - RF) %>%
  arrange(desc(div_own)) %>% distinct(date)
div_returns
colnames(div_returns)

## Average Returns ##
avret_equ_div <- mean(div_returns$ret_month) * 12
avret_val_div <- mean(div_returns$val_month) * 12

##  Standard Deviation ##
sd_equ_div <- sd(div_returns$ret_month) * sqrt(12)
sd_val_div <- sd(div_returns$val_month) * sqrt(12)

##  Sharpe Ratio ##
sharpe_equ_div <- (mean(div_returns$excess_ret) * 12 - mean(div_returns$RF)) / (sd(div_returns$excess_ret) * sqrt(12))
sharpe_val_div <- (mean(div_returns$excess_ret_val) * 12 - mean(div_returns$RF)) / (sd(div_returns$excess_ret_val) * sqrt(12))


### split only portfolio ####
spli_3$spl_ret <- spli_3$split_final * spli_3$return_incl_divs
spli_group <- group_by(spli_3, date, split_final)
split_returns <- spli_group %>% 
  summarise(ret_month = mean(spl_ret), val_month = weighted.mean(spl_ret,mktcap), MktRF = mean(MktRF),
            SMB = mean(SMB), HML = mean(HML), RF = mean(RF), MOM = mean(MOM)) %>%
  mutate(excess_ret = ret_month - RF, excess_ret_val = val_month - RF) %>%
  arrange(desc(split_final)) %>% distinct(date)

## Average Returns ##
avret_equ_spl <- mean(split_returns$ret_month) * 12
avret_val_spl <- mean(split_returns$val_month) * 12

##  Standard Deviation ##
sd_equ_spl <- sd(split_returns$ret_month) * sqrt(12)
sd_val_spl <- sd(split_returns$val_month) * sqrt(12)

##  Sharpe Ratio ##
sharpe_equ_spl <- (mean(split_returns$excess_ret) * 12 - mean(split_returns$RF)) / (sd(split_returns$excess_ret) * sqrt(12))
sharpe_val_spl <- (mean(split_returns$excess_ret_val) * 12 - mean(split_returns$RF)) / (sd(split_returns$excess_ret_val) * sqrt(12))

### div or split ####
spli_3$dsboth <- spli_3$both * spli_3$return_incl_divs
spli_group <- group_by(spli_3, date, both)
both_returns <- spli_group %>% 
  summarise(ret_month = mean(dsboth), val_month = weighted.mean(dsboth,mktcap), MktRF = mean(MktRF),
            SMB = mean(SMB), HML = mean(HML), RF = mean(RF), MOM = mean(MOM)) %>%
  mutate(excess_ret = ret_month - RF, excess_ret_val = val_month - RF) %>%
  arrange(desc(both)) %>% distinct(date)

## Average Returns ##
avret_equ_bth <- mean(both_returns$ret_month) * 12
avret_val_bth <- mean(both_returns$val_month) * 12

##  Standard Deviation ##
sd_equ_bth <- sd(both_returns$ret_month) * sqrt(12)
sd_val_bth <- sd(both_returns$val_month) * sqrt(12)

##  Sharpe Ratio ##
sharpe_equ_bth <- (mean(both_returns$excess_ret) * 12 - mean(both_returns$RF)) / (sd(both_returns$excess_ret) * sqrt(12))
sharpe_val_bth <- (mean(both_returns$excess_ret_val) * 12 - mean(both_returns$RF)) / (sd(both_returns$excess_ret_val) * sqrt(12))

both_returns <- data.frame(both_returns)
split_returns <- data.frame(split_returns)
div_returns <- data.frame(div_returns)

### regressions ###
regDe <- lm(excess_ret~MktRF,data=div_returns)
regDv <- lm(excess_ret_val~MktRF,data=div_returns)
regDfe <- lm(excess_ret~MktRF+SMB+HML+MOM,data=div_returns)
regDfv <- lm(excess_ret_val~MktRF+SMB+HML+MOM,data=div_returns)
regSe <- lm(excess_ret~MktRF,data=split_returns)
regSv <- lm(excess_ret_val~MktRF,data=split_returns)
regSfe <- lm(excess_ret~MktRF+SMB+HML+MOM,data=split_returns)
regSfv <- lm(excess_ret_val~MktRF+SMB+HML+MOM,data=split_returns)
regBe <- lm(excess_ret~MktRF,data=both_returns)
regBv <- lm(excess_ret_val~MktRF,data=both_returns)
regBfe <- lm(excess_ret~MktRF+SMB+HML+MOM,data=both_returns)
regBfv <- lm(excess_ret_val~MktRF+SMB+HML+MOM,data=both_returns)

####  sharpe ratio ####
half <- nrow(div_returns)/2
half1D <- slice(div_returns, 1:half)
half2D <- slice(div_returns, (half+1):nrow(split_returns))

half1S <- slice(split_returns, 1:half)
half2S <- slice(split_returns, (half+1):nrow(split_returns))

half1B <- slice(both_returns, 1:half)
half2B <- slice(both_returns, (half+1):nrow(both_returns))

shrp_1D <- (mean(half1D$excess_ret_val) * 12 - mean(half1D$RF)) / (sd(half1D$excess_ret_val) * sqrt(12))
shrp_2D <- (mean(half2D$excess_ret_val) * 12 - mean(half2D$RF)) / (sd(half2D$excess_ret_val) * sqrt(12))
shrp_1S <- (mean(half1S$excess_ret_val) * 12 - mean(half1S$RF)) / (sd(half1S$excess_ret_val) * sqrt(12))
shrp_2S <- (mean(half2S$excess_ret_val) * 12 - mean(half2S$RF)) / (sd(half2S$excess_ret_val) * sqrt(12))
shrp_1B <- (mean(half1B$excess_ret_val) * 12 - mean(half1B$RF)) / (sd(half1B$excess_ret_val) * sqrt(12))
shrp_2B <- (mean(half2B$excess_ret_val) * 12 - mean(half2B$RF)) / (sd(half2B$excess_ret_val) * sqrt(12))

