
require(caret)
# library(combinat)

setClass("train")

MModel <- setRefClass("MModel",
                      fields=list(
                        datasets = "list",
                        ensemble = "list",
                        imputingInfo = "data.frame",
                        combFits = "list",
                        result = "data.frame",
                        combFitIndex = "numeric",
                        predictFits = "list",
                        result_df = "data.frame"
                      ),
                      methods=list(
                        # +++++++++++++++ getDescription +++++++++++++++++
                        getDescription = function(){
                          desc <- capture.output(cat(sep="\n", 
                                                     "dim(datasets): ",         capture.output(t(data.frame(lapply(datasets, function(ds){c(dim(ds),dim(ds[!complete.cases(ds),])[1])}), row.names =c("nrow","ncol","NAsCnt")))), 
                                                     "\nimputingInfo: ",        capture.output(imputingInfo),
                                                     "\nTRAINING vars: ",       capture.output(names(datasets$training)),
                                                     "\nEnsemble accuracy on train: ",        capture.output(data.frame(t(sapply(ensemble,function(x){list(Model = x$method, Accuracy = max(x$result$Accuracy))})))),
                                                     "\nEnsembling result: ",        capture.output(result),
                                                     "\n======= Ensamble FIT calls: ========",  capture.output(sapply(ensemble,function(x){x$call})),
                                                     "\n======= Combined FIT calls: ========",  capture.output(sapply(combFits,function(x){x$fit$call})),
                                                     "======================================",
                                                     "\ncombFitIndex: ",        capture.output(combFitIndex),
                                                     "\nPredict FIT calls: ",      capture.output(lapply(predictFits, function(x){x$fit$call})), 
                                                     "\n\n FINAL PREDICTION MODEL SETTINGS: ",    capture.output(predictFits),
                                                     "\n\npredicted df head: ", capture.output(head(result_df, 10))
                          ))
                          desc
                        },
                        # +++++++++++++++ addImputingInfo +++++++++++++++++
                        addImputingInfo = function(infoRow=NULL){
                          imputingInfo <<- rbind(imputingInfo, infoRow)
                        },
                        # +++++++++++++++ addDataset +++++++++++++++++
                        addDataset = function(name=NULL, dataset=NULL){
                          datasets[[name]] <<- dataset
                        },
                        # +++++++++++++++ trainCombined +++++++++++++++++
                        trainCombined = function(data=NULL){
                          print(str(data))
                          #                           train(y~., data=data, method="rpart2",
                          #                                 trControl = trainControl(method = "cv", verboseIter = T, number = 5))
                          train(y~., data=data, method="rf",
                                           trControl = trainControl(method = "cv", verboseIter = T, number = 5))
                        },
                        # +++++++++++++++ calcComb +++++++++++++++++
                        calcComb = function(){
                          
                          # Prepare grid
                          ensemble_len <- length(ensemble) 
                          grid <- expand.grid(data.frame(matrix(rep(c(T,F),ensemble_len),nrow = 2,ncol=ensemble_len)))
                          grid <- grid[apply(grid, 1, any),]
                          colnames(grid) <- sapply(ensemble, function(x){x$method})
                          
                          print("grid dimensions: ")
                          print(dim(grid))
                          
                          result <<- grid
                          
                          # the result accuracieas:
                          accuracy <- apply(grid, 1, function(gridRow){
                            
                            combinedDf<-NULL
                            
                            if(any(gridRow)){
                              
                              modcomb <- ensemble[unlist(gridRow)]
                              
                              print("ensemble combination:")
                              print(sapply(modcomb,function(x){x$method}))
                              
                              # prepare combinedDF
                              pred <- lapply(modcomb, predict, newdata = na.omit(datasets$testing))
                              combinedDf <- data.frame(pred, na.omit(datasets$testing)$y)
                              colnames(combinedDf) <- c(names(gridRow[unlist(gridRow)]),"y")
                              
                              
                              if(length(modcomb) > 1){
                                combFit <- trainCombined(combinedDf)
                                combinedDf <- data.frame(combinedDf, combPred = predict(combFit, combinedDf))
                                combFits[length(combFits)+1] <<- list(list(fit=combFit, usePredv = TRUE, predvCnames = colnames(combinedDf)))
                              } else {
                                # only one model, so do not need to combine. Just take accuracy from underlying assemple model
                                combFit <- modcomb[[1]]
                                combFits[length(combFits)+1] <<- list(list(fit=combFit, usePredv = FALSE, predvCnames = colnames(datasets$testing)))
                                combinedDf <- data.frame(combinedDf, combPred = combinedDf[,1])
                              }
                              
                              # prepare accuracy result
                              res <- data.frame(lapply(names(gridRow), function(x){
                                if(gridRow[[x]]){
                                  confusionMatrix(combinedDf[[x]], na.omit(datasets$testing)$y)$overall[1]
                                } else {
                                  NA
                                }
                              }))
                              colnames(res)<-names(gridRow)
                              
                              # add accuracy for combinedDf on testing dataset
                              res$combOwnAcc <- max(combFit$result$Accuracy)
                              res$testAcc <- confusionMatrix(combinedDf$combPred, na.omit(datasets$testing)$y)$overall[1]
                              res 
                              
                            }
                            
                          })
                          
                          # result$combOwnAcc <<- sapply(combFits, function(x){max(x$fit$result$Accuracy)})
                          acc <- t(sapply(accuracy, rbind))
                          result <<- cbind(result, acc)
                          
                          result
                          
                        },
                        # +++++++++++++++ calcValidation +++++++++++++++++
                        calcValidation = function(){
                          
                          combPredVs <- predictComb(seq_along(combFits), newdata = datasets$validation)
                          
                          res <- lapply(combPredVs, function(x){
                            print(c(length(x),length(na.omit(datasets$validation)$y)))
                            confusionMatrix(x, na.omit(datasets$validation)$y)$overall[1]
                          })
                          
                          valAcc <- data.frame(valAcc = unlist(res))
                          
                          result <<- cbind(result, valAcc)
                          # order by valAcc desc
                          orderByValAcc <- order(result$valAcc, decreasing = T)
                          result <<- result[orderByValAcc,]
                          combFits <<- combFits[orderByValAcc]
                          
                          result
                        },
                        # +++++++++++++++ predictComb +++++++++++++++++
                        predictComb = function(combFitIndex, newdata){
                          predVs <- data.frame(lapply(ensemble, predict, newdata = na.omit(newdata)))
                          colnames(predVs) <- sapply(ensemble, function(x){x$method})
                          predVs
                          
                          combFitIndex <<- combFitIndex
                          predFits <- combFits[combFitIndex]
                          predictFits <<- predFits
                          
                          combPredVs <- lapply(predFits, function(combFit){
                            print(sprintf("prediction on model [%s] with Estimated Accuracy = %f ..."
                                          , combFit$fit$modelInfo$label
                                          , max(combFit$fit$results$Accuracy)))
                            if(combFit$usePredv){
                              print("usePredv = T")
                              print(colnames(predVs))
                              predict(combFit$fit, predVs)
                            } else {
                              print("usePredv = F")
                              print(colnames(predVs))
                              unlist(predict(combFit$fit, na.omit(newdata)))
                            }
                          })
                          
                        }
                      )
)
