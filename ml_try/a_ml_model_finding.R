
source("a_ml_functions.R")

# dt <- preProcess(readData, preBasic, preLog, makeParts_006, reduceTrain_rf006_log_20)
# dt <- preProcess(readData, preBasic, makeParts_006, reduceTrain_rf006_log_20)

dt <- preProcess(readData, preBasic, makeParts_006)


testMethods(dt, c("glm","rpart", "enet"))
testMethods(dt, c("enet"))

# FAILED: "wsrf" bartMachine polr svmLinear lssvmLinear amdai "bag" "awnb","awtan","brnn" "chaid","enet",
  # binda, logicBag, LogitBoost logreg - Some of the values of the predictors are not 0 or 1

# TOO SLOW: rfRules evtree dwdPoly dwdRadial "Boruta", "gam" "SLAVE", 

# DONE: 
# "gbm","bagFDA", "gcvEarth", "ranger", "earth", "fda", "xgbTree"
# ,"xgbLinear", "C5.0", "RRFglobal", "treebag", "AdaBoost.M1"
# "J48", "OneR", "JRip", "extraTrees"
# ,"RRF", "LMT", "C5.0Tree", "C5.0Rules", "ctree2"
# "blackboost", "gamboost", "glmboost", "rotationForest", "rotationForestCp"
# "rf", "rpart", "rpart2", "glm", "rFerns", "dwdLinear", "rmda","plr",
# "lda", "ada", "nb", "bayesglm", 
# "ORFlog", "ORFpls", "ORFridge", "avNNet", "bstTree", "bagEarth", 
# "bagEarthGCV", "BstLm", "bstSm", "nodeHarvest"
# "AdaBag", "bagFDAGCV", "bdk",
# "CSimca", "dnn", "elm",
# "gamLoess", "gamSpline", 
# "gaussprLinear", "GFS.GCCL", "glmnet", "glmStepAIC", "gpls", "hda", 
# "hdda", "kernelpls", "kknn", "knn"

# "ownn", "pam", "parRF", "PART", "partDSA", "pcaNNet", "pda", "pda2", 
# "RSimca", "sda", "sddaLDA", "sddaQDA", "sdwd", "simpls", 
# "slda", "smda", "snn", "sparseLDA", "svmPoly", "svmRadial", 
# "svmRadialCost", "svmRadialWeights"

# "rf", "glm", "rpart", "rpart2", "AdaBag", "gbm", "bagFDA", "gcvEarth", 
# "ranger", "earth", "fda", "xgbTree", "xgbLinear", "C5.0", "RRFglobal", 
# "treebag", "AdaBoost.M1", "J48", "OneR", "JRip", "extraTrees", "RRF", 
# "LMT", "C5.0Tree", "C5.0Rules", "ctree2", "blackboost", "gamboost", 
# "glmboost", "rotationForest", "rotationForestCp", "rFerns", "dwdLinear", 
# "LogitBoost", "plr", "lda", "ada", "svmLinear", "nb", "bayesglm", 
# "avNNet", "bstTree", "bagEarth", "bagEarthGCV", "BstLm", "bstSm", 
# "nodeHarvest", "bagFDAGCV", "bdk", "CSimca", "dnn", "elm", "gamLoess", 
# "gamSpline", "glmnet", "glmStepAIC", "hda", "hdda", "kernelpls", "knn", 
# "pam", "parRF", "PART", "partDSA", "RSimca", "sda", "sdwd", "simpls", 
# "slda", "sparseLDA", "svmPoly", "svmRadial", "svmRadialCost", "svmRadialWeights"

testMethods(dt, c(
                  "rf", "glm", "rpart", "rpart2", "AdaBag", "gbm", "bagFDA", "gcvEarth", 
                  "ranger", "earth", "fda", "xgbTree", "xgbLinear", "C5.0", "RRFglobal", 
                  "treebag", "AdaBoost.M1", "J48", "OneR", "JRip", "extraTrees", "RRF", 
                  "LMT", "C5.0Tree", "C5.0Rules", "ctree2", "blackboost", "gamboost", 
                  "glmboost", "rotationForest", "rotationForestCp", "rFerns", "dwdLinear", 
                  "LogitBoost", "plr", "lda", "ada", "svmLinear", "nb", "bayesglm", "avNNet", 
                  "bstTree", "bagEarth", "bagEarthGCV", "BstLm", "bstSm", "nodeHarvest", 
                  "bagFDAGCV", "bdk", "CSimca", "dnn", "elm", "gamLoess", "gamSpline", 
                  "glmnet", "glmStepAIC", "hda", "hdda", "kernelpls", "knn", "pam", "parRF", 
                  "PART", "partDSA", "RSimca", "sda", "sdwd", "simpls", "slda", "sparseLDA", 
                  "svmPoly", "svmRadial", "svmRadialCost", "svmRadialWeights"
                  ))


# TODO:

