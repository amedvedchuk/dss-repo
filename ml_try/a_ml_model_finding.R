
source("a_ml_functions.R")

dt <- preProcess(readData, preBasic, preLog, makeParts_006, reduceTrain_rf006_log_20)

testMethods(dt, c("glm","rpart"))

# FAILED: "wsrf"
# TOO SLOW: rfRules evtree
# DONE: "gbm","bagFDA", "gcvEarth", "ranger", "earth", "fda", "xgbTree"
#    ,"xgbLinear", "C5.0", "RRFglobal", "treebag", "AdaBoost.M1"

testMethods(dt, c("J48", "OneR", "JRip", "extraTrees"
                   ,"RRF", "LMT", "C5.0Tree", "C5.0Rules", "ctree2"))


# TODO:
# "blackboost", "gamboost", "glmboost", "rotationForest", "rotationForestCp", "bartMachine", "rFerns", "dwdLinear", "rmda", "dwdPoly", "dwdRadial", "binda", "logicBag", "LogitBoost", "logreg", "plr", "polr"