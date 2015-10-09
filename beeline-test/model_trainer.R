
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
                        combFits = "list"
                      ),
                      methods=list(
                        #                         initialize = function(..., final=NULL, ensambled=NULL, datasets=NULL,imputingInfo=NULL, result_df=NULL){
                        #                           callSuper(...,final=final, ensambled=ensambled, datasets=datasets,imputingInfo=imputingInfo, result_df=result_df)
                        #                         },
                        getDescription = function(){
                          desc <- capture.output(cat(sep="\n", 
                                                     "dim(datasets): ",         capture.output(t(data.frame(lapply(mmod$datasets, dim), row.names =c("nrow","ncol")))), 
                                                     "\nimputingInfo: ",        capture.output(imputingInfo),
                                                     "\n======= Ensamble FIT calls: ========",  capture.output(sapply(ensemble,function(x){x$call})),
                                                     "====================================",
                                                     "\nFinal FIT call: ",      ifelse(exists("final"), capture.output(final$call), "N/A"),
                                                     "\nTRAINING vars: ",       capture.output(names(datasets$training)),
                                                     "\n\nMODEL SETTINGS: ",    ifelse(exists("final"), capture.output(final), "N/A"),
                                                     "\n\npredicted df head: ", ifelse(exists("result_df"), capture.output(head(result_df, 10)),"N/A")
                          ))
                          desc
                        },
                        addImputingInfo = function(infoRow=NULL){
                          imputingInfo <<- rbind(imputingInfo, infoRow)
                        },
                        calcComb = function(){
                          
                          #TODO
                          grid <- expand.grid(c(T,F), c(T,F), c(T,F))
                          
                          grid
                          
                          # mmod$ensemble[unlist(pr1[3,])]
                          
                          preds1 <- apply(grid, 1, function(x){
                            # print(x[1])
                            # sapply(x, function)
                            
                            df<-NULL
                            
                            if(any(x)){
                              
                              modcomb <- ensemble[unlist(x)]
                              
                              print("ensemble combination:")
                              print(sapply(modcomb,function(x){x$method}))
                              
                              pred <- lapply(modcomb, predict, newdata = na.omit(datasets$testing))
                              
                              df <- data.frame(pred, na.omit(datasets$testing)$y)
                              colnames(df) <- c(paste("var", seq_along(pred), sep=""),"y")
                              df
                              
                              #                               combFit <- train(y~., data=df, method="rf",
                              #                                                trControl = trainControl(method = "cv", verboseIter = T, number = 5))
                              combFit <- train(y~., data=df, method="rpart2",
                                               trControl = trainControl(method = "cv", verboseIter = T, number = 5))
                              df <- data.frame(df, combPred = predict(combFit,df))
                              
                              combFits[length(combFits)+1] <<- list(combFit)
                              df
                              
                              # dd1[,-grep("y",names(dd1))]
                              
                              res <- lapply(df[,-grep("y",names(df))], function(x){
                                  confusionMatrix(x, na.omit(datasets$testing)$y)$overall[1]
                                # )

                              })
                              
                              # names(res)<-c(sapply(modcomb,function(x){x$method}) ,"combPredV")
                              data.frame(res)
                              
                             
                              
                            }
                            
                          })
                          
                          # data.frame(preds1)
                          
                          # TODO
                          #                           lapply(preds1[1:7], function(p){
                          #                             df <- data.frame(p, na.omit(datasets$testing)$y)
                          #                             colnames(df) <- c(seq_along(p),"y")
                          #                             df
                          #                           })
                          
                        },
                        calcValidation = function(){
                          
                          
                          
                          pred1V <- predict(modelFitAsIs,validation); 
                          pred2V <- predict(fit2,validation)
                          predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
                          combPredV <- predict(combFit,predVDF)
                          
                          
                        }
                      )
)

mod <- MModel$new()
mod$ensemble <- list(modelFitAsIs, fit2, fit2)

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

