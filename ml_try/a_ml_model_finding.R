
source("a_ml_functions.R")

dt <- preProcess(readData, preBasic, preLog, makeParts_006, reduceTrain_rf006_log_20)

testMethods(dt, c("glm","rpart"))

# FAILED: "wsrf"
# DONE: 

testMethods(dt, c( "gbm","bagFDA", "gcvEarth", "ranger", "earth", "fda", "xgbTree"
                   ,"xgbLinear", "C5.0", "RRFglobal", "rfRules", "treebag"
                   ,"evtree", "AdaBoost.M1", "J48", "OneR", "JRip", "extraTrees"
                   ,"RRF", "LMT", "C5.0Tree", "C5.0Rules", "ctree2"))

