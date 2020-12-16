########### CODE TO SCRAPE DATA FROM GOV WEBSITE ###########

library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)

# Importing website URL
URL <- 'https://www.camara.leg.br/deputados/quem-sao/resultado?search=&partido=&uf=&legislatura=48&sexo='

# Defining first page
FirstPage <- read_html(URL)

# Finding last page
GetPageInfo <- function(htmlPage){
  
  # Step 1: read all paragraphs in page
  ParagraphText <- FirstPage %>%
    html_nodes("p") %>%
    # Getting only the paragraphs ("p") from the html code
    html_text()
  # Converts html language to text
  
  # Step 2: get text that contains total items
  TotalItems <- ParagraphText[grepl(pattern='^.*encontrados\\.',x=ParagraphText)]
  
  # Step 3: splitting into words
  Words <- unlist(strsplit(TotalItems,split=" "))
  
  # Step 4: getting numbers only
  Numbers <- Words[grepl(pattern="^[0-9][0-9]",x=Words)]
  Numbers <- as.numeric(Numbers)
  
  # Step 5: calculating total number of pages
  MaxResultsPerPage <- Numbers[1]
  TotalResults <- Numbers[2]
  TotalNumberOfPages <- ceiling(TotalResults/MaxResultsPerPage)
  
  c(MaxResultsPerPage,TotalResults,TotalNumberOfPages)
}

PageInfo <- GetPageInfo(FirstPage)
MaxResultsPerPage <- PageInfo[1]
TotalResults <- PageInfo[2]
TotalNumberOfPages <- PageInfo[3]

# Defining last page
LastPage <- TotalNumberOfPages

# Getting URLs of all pages
ListofURLs <- str_c(URL, '&pagina=', 1:LastPage)
head(ListofURLs)

# Creating empty dataframe to store deputy information
Deputies_df <- data.frame(Name=character(),Party=character(),State=character())

# Function that extracts deputy names from page
GetDeputies <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.lista-resultados__cabecalho') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

# Get the result of the queried page and extract its information into a dataframe
DataframeFromPage <- function(queryResultHTML){
  
  ListOfDeputies <- GetDeputies(queryResultHTML)
  
  # Creating dataframe
  Deputy_df <- data.frame(ListOfDeputies)
  
  # Removing "ex-deputado"
  Deputy_df <- separate(data=Deputy_df, col=ListOfDeputies, sep="\n",into="Deputies")
  
  # Splitting columns into "Name" and "Party"
  Deputy_df <- separate(data = Deputy_df, col = Deputies, into = c("Name", "Party"), sep = "\\(|\\)")
  
  # Splitting columns into "Name", "Party", and "State"
  Deputy_df <- separate(data = Deputy_df, col = Party, into = c("Party", "State"), sep = "-")
  Deputy_df
}

# Repeat EACH PAGE
for(page in 1:TotalNumberOfPages){
  # Query the current page information
  CurrentPage <- read_html((ListofURLs[page]))
  CurrentPage_df <- DataframeFromPage(CurrentPage)
  
  Deputies_df <- rbind(Deputies_df,CurrentPage_df)
}

##########################################################################################

########### LOAD DATA ###########
Data <- read.csv("/Users/larafernandez/Desktop/ASM/Project 3/Project3_Data.csv")
Data <- Data[1:32,]

########### VISUALIZATIONS ###########
library(ggplot2)
library(stargazer)

# Summary stats
Data2 <- Data[, c("Party_Fragmentation", "Coalition_Fragmentation", 
                  "Change_GDP_per_Capita", "Debt_pct_GDP","IPCA_Yearly",
                  "Exchange_Rate","Revenue_pct_GDP","Expenditures_pct_GDP",
                  "Terms_of_Trade", "RealGDP_Growth","Current_Account",
                  "Unemployment_Rate")]

stargazer(Data2, type = "html",title = "Summary Statistics")

# Debt over time
plot(Data$Year, Data$Debt_pct_GDP,type="o",
     xlab="Year",ylab="Public Debt (as % of GDP)",main="Brazil's Public Debt, 1987-2018",
     xaxt='n')
axis(1, at = seq(1987L, 2018L, by = 1L))

# Political fragmentation over time
plot(Data$Year,Data$Party_Fragmentation,type="o",col="red",
     ylim=c(0,1.5),
     main="Fragmentation in Brazilian Congress, 1987-2018",
     xlab="Year",
     ylab="Fragmentation (%)",
     xaxt='n')
axis(1, at = seq(1987L, 2018L, by = 1L))
lines(Data$Year,Data$Coalition_Fragmentation,col="blue")
points(Data$Year, Data$Coalition_Fragmentation,col="blue")
legend("topright", legend=c("Party Frag.", "Coalition Frag."),
       col=c("red", "blue"), lty=1, cex=0.8)

# Exchange rate
plot(Data$Year,Data$Change_Exchange_Rate,type="o",
     main="Brazil's Exchange Rate, 1987-2018",
     xlab="Year",
     ylab="BRL/USD (% change)",
     xaxt='n')
axis(1, at = seq(1987L, 2018L, by = 1L))

# Independent/Dependent relationship:

plot(Data$Party_Fragmentation,Data$Debt_pct_GDP,
     main="Relationship between Party Fragmentation and Public Debt",
     cex.main=1,
     xlab="Party Fragmentation (%)",
     ylab="Public Debt as % of GDP",
     abline(lm(Data$Debt_pct_GDP ~ Data$Party_Fragmentation)))

plot(Data$Coalition_Fragmentation,Data$Debt_pct_GDP,
     main="Relationship between Coalition Fragmentation and Public Debt",
     cex.main=1,
     xlab="Coalition Fragmentation (%)",
     ylab="Public Debt as % of GDP",
     abline(lm(Data$Debt_pct_GDP ~ Data$Coalition_Fragmentation)))

########### MODEL ###########
# Baseline (no control variables):
m0 <- lm(data=Data, Debt_pct_GDP ~ Party_Fragmentation + Coalition_Fragmentation)
plot(m0)

# Correcting for non-linearity:
m0_revised <- lm(data=Data, Debt_pct_GDP ~ log(Party_Fragmentation) + log(Coalition_Fragmentation))
plot(m0_revised)
  # Did not improve

m0_revised2 <- lm(data=Data, log(Debt_pct_GDP) ~ Party_Fragmentation + Coalition_Fragmentation)
  # Did not improve

# Switching to percent change in debt as outcome variable:
m1 <- lm(data=Data, Change_Debt_pct_GDP ~ Party_Fragmentation + Coalition_Fragmentation)
plot(m1)
  # Moving forward with this model

stargazer(m1,title="Revised Baseline Model, No Controls",type='html',align=TRUE)

# Adding control variables:
m2 <- lm(data=Data, Change_Debt_pct_GDP ~ Party_Fragmentation + 
           Coalition_Fragmentation+Left+Center+Right+
           Change_GDP_per_Capita + Change_IPCA_Yearly + RealGDP_Growth + Change_Revenue_pct_GDP +
           Change_Expenditures_pct_GDP +
           Change_Current_Account + Change_Unemployment_Rate)
plot(m2)

# Comparing models:
stargazer(m1, m2,title="Baseline Model vs Full Model",type='html',align=TRUE)