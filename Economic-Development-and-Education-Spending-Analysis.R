# Load all packages used
library("data.table")
library("dplyr")
library("moments")
library("tidyverse")
library("corrplot")
library("Hmisc")
library("readxl")
library("ggstatsplot")
library("forecast")
library("descr")
library("summarytools")

# Reading the file1 into the application
ed <- read_excel("Economic Development.xlsx")
head(ed, 4)

# pivot a column
ed <- ed %>% gather("Years", "Values", 3:14, na.rm = TRUE)
head(ed)

# unpivot a column to columns
ed <- ed %>% spread(`Series Name`, Values)
head(ed)
View(ed)

# rename some of the columns
ed <- ed %>%
  rename(
    "country" = "Country Name",
    "years" = "Years",
    "gdp growth (% annual)" = "GDP growth (annual %)",
    "gdp per capita (US$)" = "GDP per capita (current US$)",
    "gdp per capita growth (% annual)" = "GDP per capita growth (annual %)",
    "gov exp on edu (% of gdp)" = "Government expenditure on education, total (% of GDP)",
    "gov exp on edu (% gov exp)" = "Government expenditure on education, total (% of government expenditure)",
    "gov exp per stu pri (% gdp pc)" = "Government expenditure per student, primary (% of GDP per capita)",
    "gov exp per stu sec (% gdp pc)" = "Government expenditure per student, secondary (% of GDP per capita)",
    "gov exp per stu ter (% gdp pc)" = "Government expenditure per student, tertiary (% of GDP per capita)"
  )
head(ed)
View(ed)


# Change data types of some columns
ed[,3:10] <- sapply(ed[,3:10], as.numeric)
str(ed)


# Check for missing values
colSums(is.na(ed[, c(6:10)]))


# Fill missing values with median value
NAcolumns <- colnames(ed)[apply(ed, 2, anyNA)]

colMean <- apply(ed[,colnames(ed) %in% NAcolumns],
                 2, mean, na.rm =  TRUE)


ed <- ed %>% mutate(
  `gov exp on edu (% of gdp)` = ifelse(is.na(`gov exp on edu (% of gdp)`), colMean[1], `gov exp on edu (% of gdp)`),
  `gov exp on edu (% gov exp)` = ifelse(is.na(`gov exp on edu (% gov exp)`), colMean[2], `gov exp on edu (% gov exp)`),
  `gov exp per stu pri (% gdp pc)` = ifelse(is.na(`gov exp per stu pri (% gdp pc)`), colMean[3], `gov exp per stu pri (% gdp pc)`),
  `gov exp per stu sec (% gdp pc)` = ifelse(is.na(`gov exp per stu sec (% gdp pc)`), colMean[4], `gov exp per stu sec (% gdp pc)`),
  `gov exp per stu ter (% gdp pc)` = ifelse(is.na(`gov exp per stu ter (% gdp pc)`), colMean[5], `gov exp per stu ter (% gdp pc)`),
)

# Check for missing values
colSums(is.na(ed[, c(6:10)]))


# Reading the file2 into the application
es <- read_excel("Education Spending.xlsx")
head(es, 4)


# pivot a column
es <- es %>% gather("Years", "Values", 3:14, na.rm = TRUE)
head(es)

# unpivot a column to columns
es <- es %>% spread(`Series Name`, Values)
head(es)
View(es)


# rename some of the columns
es <- es %>%
  rename(
    "country" = "Country Name",
    "years" = "Years",
    "adj net enrollment rate (% pri sch age)" = "Adjusted net enrollment rate, primary (% of primary school age children)",
    "edu attainment bac lev (% tot cum)" = "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)",
    "edu attainment lw sec lev (% tot cum)" = "Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)",
    "edu attainment up sec lev (% tot cum)" = "Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)",
    "edu attainment mas lev (% tot cum)" = "Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)",
    "edu attainment doc lev (% tot cum)" = "Educational attainment, Doctoral or equivalent, population 25+, total (%) (cumulative)",
    "literacy rate (% age 15 & abv)" = "Literacy rate, adult total (% of people ages 15 and above)",
    "literacy rate (% age 15-24)" = "Literacy rate, youth total (% of people ages 15-24)",
    "school enrollment pri (% gross)" = "School enrollment, primary (% gross)",
    "school enrollment sec (% gross)" = "School enrollment, secondary (% gross)",
    "School enrollment ter (% gross)" = "School enrollment, tertiary (% gross)"
  )
head(es)
View(es)


# Change data types of some columns
es[,3:13] <- sapply(es[,3:13], as.numeric)
str(es)


# Check for missing values
colSums(is.na(es[, c(3:13)]))


# Fill missing values with median value
NAcolumns <- colnames(es)[apply(es, 2, anyNA)]

colMean <- apply(es[,colnames(es) %in% NAcolumns],
                 2, mean, na.rm =  TRUE)


es <- es %>% mutate(
  `adj net enrollment rate (% pri sch age)` = ifelse(is.na(`adj net enrollment rate (% pri sch age)`), colMean[1], `adj net enrollment rate (% pri sch age)`),
  `edu attainment bac lev (% tot cum)` = ifelse(is.na(`edu attainment bac lev (% tot cum)`), colMean[2], `edu attainment bac lev (% tot cum)`),
  `edu attainment lw sec lev (% tot cum)` = ifelse(is.na(`edu attainment lw sec lev (% tot cum)`), colMean[3], `edu attainment lw sec lev (% tot cum)`),
  `edu attainment up sec lev (% tot cum)` = ifelse(is.na(`edu attainment up sec lev (% tot cum)`), colMean[4], `edu attainment up sec lev (% tot cum)`),
  `edu attainment mas lev (% tot cum)` = ifelse(is.na(`edu attainment mas lev (% tot cum)`), colMean[5], `edu attainment mas lev (% tot cum)`),
  `edu attainment doc lev (% tot cum)` = ifelse(is.na(`edu attainment doc lev (% tot cum)`), colMean[6], `edu attainment doc lev (% tot cum)`),
  `literacy rate (% age 15 & abv)` = ifelse(is.na(`literacy rate (% age 15 & abv)`), colMean[7], `literacy rate (% age 15 & abv)`),
  `literacy rate (% age 15-24)` = ifelse(is.na(`literacy rate (% age 15-24)`), colMean[8], `literacy rate (% age 15-24)`),
  `school enrollment pri (% gross)` = ifelse(is.na(`school enrollment pri (% gross)`), colMean[9], `school enrollment pri (% gross)`),
  `school enrollment sec (% gross)` = ifelse(is.na(`school enrollment sec (% gross)`), colMean[10], `school enrollment sec (% gross)`),
  `School enrollment ter (% gross)` = ifelse(is.na(`School enrollment ter (% gross)`), colMean[11], `School enrollment ter (% gross)`)
)

# Check for missing values
colSums(is.na(es[, c(3:13)]))


# ---------------------------------------------------------------------------

df1 <- ed %>% select(country, years,
                     `gdp per capita (US$)`,
                     `gdp growth (% annual)`,
                     `gov exp on edu (% of gdp)`,
                     `gov exp on edu (% gov exp)`)
df1 <- tibble::rowid_to_column(df1, "id")
df2 <- es %>% select(`literacy rate (% age 15 & abv)`)
df2 <- tibble::rowid_to_column(df2, "id")
df <- merge(df1, df2, by = "id")

View(df)

# view the structure of the dataset
str(df)



corr_data <- df %>%
  select(-id, -country, -years)

cor(corr_data$`gov exp on edu (% gov exp)`,
    corr_data$`literacy rate (% age 15 & abv)`,
    use = "na.or.complete")

# CORRELATION ANALYSIS
# Correlation measures the relationship between two indicator variables.
corr_data <- df %>%
  select(-Country, -Years)

# find correlation between two variables
cor(corr_data$`gov exp on edu (% gov exp)`,
    corr_data$`gdp per capita (US$)`,
    use = "na.or.complete")
cor(corr_data$`gov exp on edu (% gov exp)`,
    corr_data$`literacy rate (% age 15 & abv)`,
    use = "na.or.complete")

# find correlation for all the indicators
round(
  cor(corr_data, use = "na.or.complete"), digits = 2
)


corrplot(cor(corr_data, use = "na.or.complete"),
         method = "number",
         type = "full"
)


# HYPOTHESIS TEST
# HYPOTHESIS 1
# NULL hypothesis -  there is no linear relationship between the two variables
# ALTERNATIVE hypothesis - there is a linear relationship between the two variables
# pearson correlation test
test1 <- cor.test(corr_data$`gov exp on edu (% gov exp)`,
                  corr_data$`gdp per capita (US$)`)
test1
# GEE % of GDP vs literacy
test2 <- cor.test(corr_data$`gov exp on edu (% of gdp)`,
                  corr_data$`literacy rate (% age 15 & abv)`)
test2

# Correlation matrix
res <-rcorr(as.matrix(corr_data))
round(res$P, 3)

ggscatterstats(
  data = corr_data,
  x = `gov exp on edu (% gov exp)`,
  y = `literacy rate (% age 15 & abv)`,
  bf.message = FALSE,
  marginal = FALSE
)


# SIMPLE REGRESSION
hist(corr_data$`literacy rate (% age 15 & abv)`[corr_data$`literacy rate (% age 15 & abv)` > 85])


plot(corr_data$`gov exp on edu (% of gdp)` ~ `literacy rate (% age 15 & abv)`, data = corr_data, subset = c(corr_data$`literacy rate (% age 15 & abv)` > 85))


# check if the dependent variable is normally distributed using a histogram
hist(corr_data$GDP)


# create the model
model <-lm(`gdp per capita (US$)` ~ `literacy rate (% age 15 & abv)`, data = corr_data)
summary(model)

# multiple regression
mmodel <- lm(`gdp per capita (US$)` ~ `literacy rate (% age 15 & abv)` + `gov exp on edu (% gov exp)` + `gov exp on edu (% of gdp)`, data = corr_data)
summary(mmodel)
plot(mmodel)

# time series forcasting
forecast.data <- ts(df$`gdp per capita (US$)`, frequency = 12, start=c(2006,1))
plot.ts(forecast.data)



