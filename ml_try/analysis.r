
require(Amelia)
require(colorspace)

data <- readData()
data$SEX <- as.factor(tolower(as.character(data$SEX)))


table(data$SEX)[1]/dim(data)[1]
table(data$SEX)[2]/dim(data)[1]

colors_A <- sequential_hcl(2)

missmap(data, col = colors_A, legend=FALSE)


# require(magrittr)

# show missed values
require(dplyr)
# missed1 <- data %>% apply(., 2, function(column) sum(is.na(column)))
# missed1 <- data.frame(cols=names(missed), missed)

missed <- colSums(is.na(data))
missed <- data.frame(cols=names(missed), missed)
missed <- missed %>% filter(missed > 0) %>% arrange(desc(missed))

str(missed)
missed

# replace all columns mathed with 'DIFF' with 0
library(car)
data6 <- head(data)

data <- data %>% select(contains("DIFF")) %>% 
      apply(., 2, function(column) recode(column, "NA = 0")) %>%  
      as.data.frame() %>% 
      data.frame(select(data, -contains("DIFF")))



str(data)
dim(data)

mutate()

"*DIFF* replaced by 0"
missed <- missed %>% filter(!grepl("DIFF", cols))
missed
# ----------------------------------------------------------------

# AVG_DAYS_BETW_REF_6M
# Average days between refill 6m
# (Date of the LAST refill - Date of the FIRST refill during the last 6 months)/(Number of refills during the last 6 months - 1)
# Decided: replace NA with 180

library(ggplot2)
hist(data$AVG_DAYS_BETW_REF_6M)
qplot(SEX, AVG_DAYS_BETW_REF_6M, data = data, geom = "boxplot")

data %>% filter(AVG_DAYS_BETW_REF_6M == 0) %>% dim()
data1 <- data %>% filter(AVG_DAYS_BETW_REF_6M == 0)

ggbar <- ggplot(data = data) + geom_bar(stat = "bin") 
g1 <- ggbar + aes(x = factor(SEX))
g1

ggbar <- ggplot(data = data) + geom_bar(stat = "identity") 
g1 <- ggbar + aes(x = factor(SEX), y= AVG_ONNET_INC_MOU_6M)
g1

str(data)

data <- data %>% select(AVG_DAYS_BETW_REF_6M) %>% 
  apply(., 2, function(column) recode(column, "NA = 180")) %>%  
  as.data.frame() %>% 
  data.frame(select(data, -AVG_DAYS_BETW_REF_6M))

# ----------------------------------------------------------------

missed

hist(data$PSTN_VOICE_ARMU)
qplot(SEX, AVG_BALANCE_BEFORE_REF_6M, data = data, geom = "boxplot")

ggbar <- ggplot(data = data) + geom_bar(stat = "identity") 
g1 <- ggbar + aes(x = factor(SEX), y= AVG_BALANCE_BEFORE_REF_6M)
g1

data %>% filter(is.na(AVG_BALANCE_BEFORE_REF_1M)) %>% dim()

data2 <- data %>% filter(is.na(AVG_BALANCE_BEFORE_REF_1M))

ggbar <- ggplot(data = data) + geom_bar(stat = "bin") 
g1 <- ggbar + aes(x = AVG_BALANCE_BEFORE_REF_6M)
g1

mean(data$AVG_BALANCE_BEFORE_REF_1M, na.rm = T)


summary(rf_0.06_log$finalModel)


d <- imputeNA_Bag_after_DIFF(data)


# -------------------------------------


data <- readData()
data <- preBasic_noImpute(data)

nz <- nearZeroVar(data, saveMetrics = T)
nz <- nz %>% mutate(colName = rownames(nz))
filter(nz, nzv == T)




res_cor <- cor(select(data, -SEX))
summary(res_cor[upper.tri(res_cor)])

fc <- findCorrelation(select(data, -SEX))
fc <- fc[!is.na(fc)] 
data_uncor <- data[,-fc]
descrCor2 <- cor(select(data_uncor, -SEX))
summary(descrCor2[upper.tri(descrCor2)])

#--------------------------------------------


dt <- runScenario(readData, preBasic_noImpute, imputeNA_as0_DIFF, reduce_corrPredictors, makeParts_006, imputeNA_Bag_after_DIFF)
data <- readData()
data <- preBasic_noImpute(data)
data <- imputeNA_as0_DIFF(data)
d <- reduce_corrPredictors(data)

