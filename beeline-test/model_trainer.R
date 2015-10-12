
require(caret)
# library(combinat)

setClass("train")

MModel <- setRefClass("MModel",
                      fields=list(
                        final = "train",
                        ensemble = "list",
                        datasets = "list",
                        imputingInfo = "data.frame",
                        result_df = "data.frame",
                        combFits = "list",
                        result = "data.frame"
                      ),
                      methods=list(
                        #                         initialize = function(..., final=NULL, ensambled=NULL, datasets=NULL,imputingInfo=NULL, result_df=NULL){
                        #                           callSuper(...,final=final, ensambled=ensambled, datasets=datasets,imputingInfo=imputingInfo, result_df=result_df)
                        #                         },
                        getDescription = function(){
                          desc <- capture.output(cat(sep="\n", 
                                                     "dim(datasets): ",         capture.output(t(data.frame(lapply(mmod$datasets, dim), row.names =c("nrow","ncol")))), 
                                                     "\nimputingInfo: ",        capture.output(imputingInfo),
                                                     "\nensembling result: ",        capture.output(result),
                                                     "\n======= Ensamble FIT calls: ========",  capture.output(sapply(ensemble,function(x){x$call})),
                                                     "\n======= Combined FIT calls: ========",  capture.output(sapply(combFits,function(x){x$fit$call})),
                                                     "====================================",
                                                     "\nFinal FIT call: ",      ifelse(exists("final"), capture.output(final$call), "N/A"),
                                                     "\nTRAINING vars: ",       capture.output(names(datasets$training)),
                                                     "\n\nMODEL SETTINGS: ",    ifelse(exists("final"), capture.output(final), "N/A"),
                                                     "\n\npredicted df head: ", capture.output(head(result_df, 10))
                          ))
                          desc
                        },
                        addImputingInfo = function(infoRow=NULL){
                          imputingInfo <<- rbind(imputingInfo, infoRow)
                        },
                        calcComb = function(){
                          
                          # Prepare grid
                          ensemble_len <- length(ensemble) 
                          grid <- expand.grid(data.frame(matrix(rep(c(T,F),ensemble_len),nrow = 2,ncol=ensemble_len)))
                          grid <- grid[apply(grid, 1, any),]
                          colnames(grid) <- sapply(ensemble, function(x){x$method})
                          
                          result <<- grid
                          
                          # the result:
                          preds1 <- apply(grid, 1, function(gridRow){
                            
                            df<-NULL
                            
                            if(any(gridRow)){
                              
                              modcomb <- ensemble[unlist(gridRow)]
                              
                              print("ensemble combination:")
                              print(sapply(modcomb,function(x){x$method}))
                              
                              pred <- lapply(modcomb, predict, newdata = na.omit(datasets$testing))
                              
                              df <- data.frame(pred, na.omit(datasets$testing)$y)
                              
                              colnames(df) <- c(names(gridRow[unlist(gridRow)]),"y")
                              
                              #                               combFit <- train(y~., data=df, method="rf",
                              #                                                trControl = trainControl(method = "cv", verboseIter = T, number = 5))
                              
                              if(length(modcomb) > 1){
                                combFit <- train(y~., data=df, method="rpart2",
                                                 trControl = trainControl(method = "cv", verboseIter = T, number = 5))
                                df <- data.frame(df, combPred = predict(combFit,df))
                                combFits[length(combFits)+1] <<- list(list(fit=combFit, usePredv = TRUE))
                              } else {
                                # only one model, so do not need to combine. Just take accuracy from underlying assemple model
                                combFit <- modcomb[1]
                                combFits[length(combFits)+1] <<- list(list(fit=combFit, usePredv = FALSE))
                                df <- data.frame(df, combPred = df[,1])
                              }
                              
                              
                              
                              # prepare accuracy result
                              res <- data.frame(lapply(names(gridRow), function(x){
                                # df[,-grep("y",names(df))]
                                if(gridRow[[x]]){
                                  confusionMatrix(df[[x]], na.omit(datasets$testing)$y)$overall[1]
                                } else {
                                  NA
                                }
                              }))
                              colnames(res)<-names(gridRow)
                              res$testAcc <- confusionMatrix(df$combPred, na.omit(datasets$testing)$y)$overall[1]
                              res 
                              
                            }
                            
                          })
                          
                          acc <- t(sapply(preds1, rbind))
                          result <<- cbind(result, acc)
                          acc
                          
                        },
                        # ++++++++++++++++++++++++++++++++
                        calcValidation = function(){
                          
                          predVs <- data.frame(lapply(ensemble, predict, newdata = na.omit(datasets$validation)))
                          #                           colnames(predVs) <- paste("var", seq_along(ensemble), sep="")
                          colnames(predVs) <- sapply(ensemble, function(x){x$method})
                          predVs
                          
                          # combPredVs <- lapply(combFits,predict,predVs)
                          combPredVs <- lapply(combFits, function(combFit){
                            if(combFit$usePredv){
                              print("usePredv = T")
                              predict(combFit$fit, predVs)
                            } else {
                              print("usePredv = F")
                              unlist(predict(combFit$fit, na.omit(datasets$validation)))
                            }
                          })
                          
                            res <- lapply(combPredVs, function(x){
                              print(length(x))
                              print(length(na.omit(datasets$validation)$y))
                              confusionMatrix(x, na.omit(datasets$validation)$y)$overall[1]
                            })
                          
                          valAcc <- data.frame(valAcc = unlist(res))
                          
                          result <<- cbind(result, valAcc)
                          result
                          
                          #                           pred1V <- predict(modelFitAsIs,validation); 
                          #                           pred2V <- predict(fit2,validation)
                          #                           predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
                          #                           combPredV <- predict(combFit,predVDF)
                          
                          
                        }
                      )
)

# mod <- MModel$new()
# mod$ensemble <- list(modelFitAsIs, fit2, fit2)

# 
# ac1 <- mod$calcComb()
# ac1


# sapply(ac, function(blok){
#   lapply(blok, function(row){
#     print(blok)
# #     df<<-rbind(df,row)
#     #sapply(mod$ensemble[row], function(x){df<<-rbind(df,x$method)})
#     })
# })

# ac[[3]][3,]

# apply(data.frame(matrix(1:6,nrow = 2)),3,print)

# expand.grid(c(1,NA), c(2,NA), c(3,NA))
# expand.grid(data.frame(c(1,NA), c(2,NA), c(3,NA)))
# 
# eg <- expand.grid(c(1,NA), c(2,NA), c(3,NA))
# apply(ac, function(x){ mod$ensemble[x]$method})
# 
# ac
# 
# apply(ac, 1, function(x){
#   # print(x[1])
#   # sapply(x, function)
#   modcomb <- mod$ensemble[x]
# 
#   print("row...")
#   print(modcomb[[1]]$method)
#   print(modcomb[[2]]$method)
#   print(modcomb[[3]]$method)
#   strDesc <-""
#   desc <- sapply(modcomb, function(m){
#     strDesc<-paste(strDesc,m$method,sep="")
#   })
# })
# 
# ac[1,]
# mod$ensemble[[c(ac[1,])]]
# 
# mod$final
# mod$final <- fit2
# 
# mod$addImputingInfo(data.frame(ds="imputeSetName11",imMetod = "imputeMethod11"))
# 
# mod$getDescription()
# write(mod$getDescription(), file="test.txt")
# 
# 
# length(mod$ensemble)
# 
# 
# en<- list()

