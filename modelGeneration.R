library(caret)
library(caretEnsemble)
library(RWeka)

train_JRIP <- function(data, class){
  times <- 10
  accuracy <- 0
  rules <- 0
  conditions <- 0
  
  set.seed(1234)
  folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
  
  for (j in 1:times){
    train.idx <- c(folds[[mod10(j+1)]], folds[[mod10(j+2)]], folds[[mod10(j+3)]], folds[[mod10(j+4)]], folds[[mod10(j+5)]], folds[[mod10(j+6)]], folds[[mod10(j+7)]], folds[[mod10(j+8)]], folds[[mod10(j+9)]])
    eval.idx <- folds[[mod10(j)]]
    colnames(data)[class] <- "class"
  
    trained.model <- JRip(class~., data = data[train.idx,])
  
    eval <- evaluate_Weka_classifier(trained.model, newdata = data[eval.idx,])
    
    accuracy <- accuracy + eval$details["pctCorrect"]
    
    trained.model.rules <- extract.model.rules(trained.model)
    rules <- rules + length(trained.model.rules)
    
    for(rule in trained.model.rules){
      conditions <- conditions + length(rule$conditions)
    }
    conditions <- conditions - 1
  }
  returnVector <- c(accuracy/times, rules/times, conditions/times)
  
  return (returnVector)
}

train_OneR <- function(data, class){
  times <- 10
  accuracy <- 0
  rules <- 0
  conditions <- 0
  
  set.seed(1234)
  folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
  
  for (j in 1:times){
    train.idx <- c(folds[[mod10(j+1)]], folds[[mod10(j+2)]], folds[[mod10(j+3)]], folds[[mod10(j+4)]], folds[[mod10(j+5)]], folds[[mod10(j+6)]], folds[[mod10(j+7)]], folds[[mod10(j+8)]], folds[[mod10(j+9)]])
    eval.idx <- folds[[mod10(j)]]
    colnames(data)[class] <- "class"
    
    trained.model <- OneR(class~., data = data[train.idx,])
    
    
    eval <- evaluate_Weka_classifier(trained.model, newdata = data[eval.idx,])
    
    accuracy <- accuracy + eval$details["pctCorrect"]
  
  
    trained.model.rules <- extract.oneR.rules(trained.model)
    rules <- rules + length(trained.model.rules)
  
    for(rule in trained.model.rules){
      conditions <- conditions + length(rule$conditions)
    }
  }
  
  returnVector <- c(accuracy/times, rules/times, conditions/times)
  
  return (returnVector)
}

train_cover_oneR <- function(data, class){
  times <- 10
  accuracy <- 0
  transformed.accuracy <- 0
  rules <- 0
  conditions <- 0
  
  set.seed(1234)
  folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
  
  for (j in 1:times){
    train.idx <- c(folds[[mod10(j+1)]], folds[[mod10(j+2)]], folds[[mod10(j+3)]], folds[[mod10(j+4)]], folds[[mod10(j+5)]], folds[[mod10(j+6)]], folds[[mod10(j+7)]], folds[[mod10(j+8)]], folds[[mod10(j+9)]])
    eval.idx <- folds[[mod10(j)]]
    
    pairWize.dataset <- create.pairwize.dataset(data[train.idx,], class)
    c.rules <- list()
    
    for (i in 1:length(pairWize.dataset)){
      current.dataset <- pairWize.dataset[[i]]
      pw.model <- OneR(class~., data = current.dataset)
      
      
      
      trained.model.rules <- extract.oneR.rules(pw.model)
      if(!length(trained.model.rules) == 1){
        c.rules <- c(c.rules, trained.model.rules[1:length(trained.model.rules)])
      }
    }
    
    print(c.rules)
    
    covers <- as.data.frame(matrix(, nrow = nrow(data), ncol = length(c.rules) + 1))
    
    for (i in 1:length(train.idx)){
      example <- train.idx[[i]]
      covers[i, ] <- classifier.covers(data[example,], c.rules)
    }
    
    colnames(covers)[ncol(covers)] <- "class"
    
    covers[,ncol(covers)] <- as.factor(covers[,ncol(covers)])
    
    cover.classifier <- JRip(class~., covers)
    
    evaluate.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = length(c.rules) + 1))
    
    
    
    for (i in 1:length(eval.idx)){
      example <- eval.idx[[i]]
      evaluate.frame[i, ] <- classifier.covers(data[example,], c.rules)
    }
    

    
    colnames(evaluate.frame)[ncol(evaluate.frame)] <- "class"
    
    evaluate.frame[,ncol(evaluate.frame)] <- as.factor(evaluate.frame[,ncol(evaluate.frame)])
    
    eval <- evaluate_Weka_classifier(cover.classifier, newdata = evaluate.frame, seed = 100)
    
    transformed.classifier <- transform.meta.classifier(cover.classifier, c.rules)

    
    compare.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = 2))
    correct <- 0
     
    for(i in 1:nrow(compare.frame)){
       example <- eval.idx[[i]]
       compare.frame[i,1] <- transform.cover.predict(data[example,], transformed.classifier)
       compare.frame[i,2] <- as.character(predict(cover.classifier, evaluate.frame[i, ]))
       compare.frame[i,3] <- data[example,"class"]
    }

    for(i in 1:nrow(compare.frame)){
      correct <- correct + ifelse(compare.frame[i,1] == compare.frame[i,3], 1, 0)
    }


    transformed.accuracy <-  transformed.accuracy + ((correct/length(eval.idx))*100)
    
    # cover.rules <- extract.model.rules(cover.classifier)
    # 
    # for(rule in transformed.classifier){
    #   conditions <- conditions + length(rule$conditions)
    # }
    
    #print(data[eval.idx,
    
    #return (list(cover.classifier, transformed.classifier, c.rules))
    
    
    
    accuracy <- accuracy + eval$details["pctCorrect"]
    cover.rules <- extract.model.rules(cover.classifier)
    rules <- rules + length(cover.rules)
    if(!length(cover.rules) == 1){
      for(i in 1:(length(cover.rules)-1)){
        conditions <- conditions + length(cover.rules[[i]]$conditions)
      }
      
    }

    
  }
  
  perf <- c(accuracy/times, transformed.accuracy/times, rules/times, conditions/times)
  
  return (perf)
}

train_pw_oneR <- function(data, class){
  times <-10
  accuracy <- 0
  rules <- 0
  conditions <- 0
  
  set.seed(1234)
  folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:times){
    train.idx <- c(folds[[mod10(i+1)]], folds[[mod10(i+2)]], folds[[mod10(i+3)]], folds[[mod10(i+4)]], folds[[mod10(i+5)]], folds[[mod10(i+6)]], folds[[mod10(i+7)]], folds[[mod10(i+8)]], folds[[mod10(i+9)]])
    eval.idx <- c(folds[[mod10(i)]])
    
    pairWize.dataset <- create.pairwize.dataset(data[train.idx,], class)
    
    pw.models <- list(7)
    
    predictions <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = length(pairWize.dataset) +1 ))
    
    
    for (i in 1:length(pairWize.dataset)){
      current.dataset <- pairWize.dataset[[i]]
      pw.models[[i]] <- OneR(class~., data = current.dataset)
      
      predict <- predict(pw.models[i], newdata = data[eval.idx,])
      predictions[i] <- predict
      trained.model.rules <- extract.oneR.rules(pw.models[[i]])
      rules <- rules + length(trained.model.rules)
      
      for(rule in trained.model.rules){
        conditions <- conditions + length(rule$conditions)
      }
      
    }
    
    
    
    predictions[length(pw.models)+1] <- data[eval.idx, class]
    
    colnames(predictions)[ncol(predictions)] <- "class"
    
    class.vector <- unlist(lapply(data[,class], function(x) as.character(x)))
    
    
    compare.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = 2))
    vote.pred <- numeric(length(eval.idx))
    
    correct <- 0
    for (i in 1:length(eval.idx)){
      
      factor.vector <- predictions[i,1:(ncol(predictions)-1)]
      
      predict.vector <- unlist(lapply(factor.vector, function(x) as.character(x)))
      
      prediction <- pw_predict(predict.vector, class.vector)
      vote.pred[i] <- prediction
    }
    
    
    compare.frame[,1] <- vote.pred
    compare.frame[,2] <- as.character(data[eval.idx, class])
    
    for(i in 1:nrow(compare.frame)){
      correct <- correct + ifelse(compare.frame[i,1] == compare.frame[i,2], 1, 0)
    }
    
    
    accuracy <-  accuracy + ((correct/length(eval.idx))*100)
  }
  
  
  perf <- c(accuracy/times, rules/times, conditions/times)
  
  return (perf)
}

train_cover <- function(data, class){
  times <- 10
  accuracy <- 0
  transformed.accuracy <- 0
  rules <- 0
  conditions <- 0
  
  set.seed(1234)
  folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
  
  for (j in 1:times){
    train.idx <- c(folds[[mod10(j+1)]], folds[[mod10(j+2)]], folds[[mod10(j+3)]], folds[[mod10(j+4)]], folds[[mod10(j+5)]], folds[[mod10(j+6)]], folds[[mod10(j+7)]], folds[[mod10(j+8)]], folds[[mod10(j+9)]])
    eval.idx <- folds[[mod10(j)]]
    
    pairWize.dataset <- create.pairwize.dataset(data[train.idx,], class)
    c.rules <- list()
    
    for (i in 1:length(pairWize.dataset)){
      current.dataset <- pairWize.dataset[[i]]
      pw.model <- JRip(class~., data = current.dataset)
      
      trained.model.rules <- extract.model.rules(pw.model)
      if(!length(trained.model.rules) == 1){
        c.rules <- c(c.rules, trained.model.rules[1:(length(trained.model.rules)-1)])
      }
    }
    
    covers <- as.data.frame(matrix(, nrow = nrow(data), ncol = length(c.rules) + 1))
    
    
    for (i in 1:length(train.idx)){
      example <- train.idx[[i]]
      covers[i, ] <- classifier.covers(data[example,], c.rules)
    }
    
    colnames(covers)[ncol(covers)] <- "class"
    
    covers[,ncol(covers)] <- as.factor(covers[,ncol(covers)])

    
    cover.classifier <- JRip(class~., covers)
  
    
    evaluate.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = length(c.rules) + 1))
    
    
    
    for (i in 1:length(eval.idx)){
      example <- eval.idx[[i]]
      evaluate.frame[i, ] <- classifier.covers(data[example,], c.rules)
    }
    
    
    colnames(evaluate.frame)[ncol(evaluate.frame)] <- "class"
  
    
    evaluate.frame[,ncol(evaluate.frame)] <- as.factor(evaluate.frame[,ncol(evaluate.frame)])
    
    eval <- evaluate_Weka_classifier(cover.classifier, newdata = evaluate.frame, seed = 100)
    
    transformed.classifier <- transform.meta.classifier(cover.classifier, c.rules)

    #print(transformed.classifier)
    
    #print(evaluate.frame)
    
    compare.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = 2))
    correct <- 0
    
    for(i in 1:nrow(compare.frame)){
      example <- eval.idx[[i]]
      compare.frame[i,1] <- transform.cover.predict(data[example,], transformed.classifier)
      compare.frame[i,2] <- as.character(predict(cover.classifier, evaluate.frame[i, ]))
      compare.frame[i,3] <- data[example,"class"]
    }
    
    for(i in 1:nrow(compare.frame)){
      correct <- correct + ifelse(compare.frame[i,1] == compare.frame[i,3], 1, 0)
    }
    
    transformed.accuracy <-  transformed.accuracy + ((correct/length(eval.idx))*100)
    
    cover.rules <- extract.model.rules(cover.classifier)
    
    for(rule in transformed.classifier){
      conditions <- conditions + length(rule$conditions)
    }
    
    conditions <- conditions - 1
    
    #print(data[eval.idx,
    
    #return (list(cover.classifier, transformed.classifier, c.rules))
    
    accuracy <- accuracy + eval$details["pctCorrect"]
    
    rules <- rules + length(transformed.classifier)
  }
  
  perf <- c(accuracy/times, transformed.accuracy/times, rules/times, conditions/times)
  
  return (perf)
  
  
  # model1 <- JRip(class~., data = data[train.idx1, ])
  # model2 <- JRip(class~., data = data[train.idx2, ])
  # model1.rules <- extract.model.rules(model1)
  # model2.rules <- extract.model.rules(model2)
  # rule1.noDefault <- model1.rules[1:(length(model1.rules)-1)]
  # rule2.noDefault <- model2.rules[1:(length(model2.rules)-1)]
  # 
  # c.rules <- c(rule1.noDefault, rule2.noDefault)
  # 
  # covers <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = length(c.rules) +1 ))
  # 
  # for (i in 1:length(eval.idx)){
  #   example <- eval.idx[[i]]
  #   covers[i, ] <- classifier.covers(data[example,], c.rules)
  # }
  # 
  # colnames(covers)[ncol(covers)] <- "class"
  # 
  # covers[,ncol(covers)] <- as.factor(covers[,ncol(covers)])
  # 
  # 
  # cover.classifier <- JRip(class~., covers)
  # 
  # eval <- evaluate_Weka_classifier(cover.classifier, numFolds = 10, seed = 100)
  # 
  # cover.rules <- extract.model.rules(cover.classifier)
  # 
  # for(rule in cover.rules){
  #   conditions <- conditions + length(rule$conditions)
  # }
  # print(cover.classifier)
  # 
  # accuracy <- eval$details["pctCorrect"]
  # 
  # rules <- length(rules)
  # return (c(accuracy, rules, conditions))
}

transform.cover.predict <- function(example, rules){
  if(!length(rules) == 1){
    for(i in 1:(length(rules)-1)){
      if(conditions_cover(example, rules[[i]]))
        return(rules[[i]]$conclusion$class)
    }
  }

  
  return(rules[[length(rules)]]$conclusion$class)
  
}

conditions_cover <- function(example, conditions.in){
  conditions <- conditions.in[[1]]

  
  for (i in 1:length(conditions)){
    if(length(conditions[[i]]) > 0 && grepl("!", conditions[[i]][[1]])){
      if(eval_negated_rule(example, conditions[[i]]) == FALSE){
        return (FALSE)
      }
    }
    
    else{
      for(j in 1:length(conditions[[i]])){
        
        if (eval_condition(example, conditions[[i]][[j]]) == FALSE){
          return (FALSE)
        }
      }
    }
  }

  return (TRUE)
}

eval_condition <- function(example, condition) {
  # if(grepl("!", condition)){
  #   boolean <- rule_eval(example, substr(condition, 2, nchar(condition)))
  #   return (!boolean)
  # }
  # else{
   return (rule_eval(example, condition))
  #}
}

eval_negated_rule <- function(example, conditions){
  for(i in 1:length(conditions)){
    boolean <- rule_eval(example, substr(conditions[[i]], 2, nchar(conditions[[i]])))
    if(!boolean){
      return(TRUE)
    }
  }

  return(FALSE)
  
}

mod10 <- function(index){
  if (index <= 10){
    return (index)
  }
  else{
    return ((index%%11) + 1)
  }
  
}

train_PW_stacking <- function(data, class){
  
  times <-10
  accuracy <- 0
  rules <- 0
  conditions <- 0
  
  set.seed(1234)
  folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:times){
    train.idx <- c(folds[[mod10(i+1)]], folds[[mod10(i+2)]], folds[[mod10(i+3)]], folds[[mod10(i+4)]], folds[[mod10(i+5)]], folds[[mod10(i+6)]], folds[[mod10(i+7)]], folds[[mod10(i+8)]], folds[[mod10(i+9)]])
    eval.idx <- c(folds[[mod10(i)]])
    
    pairWize.dataset <- create.pairwize.dataset(data[train.idx,], class)
    
    pw.models <- list(7)
    
    predictions <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = length(pairWize.dataset) +1 ))
    
    
    for (i in 1:length(pairWize.dataset)){
      current.dataset <- pairWize.dataset[[i]]
      pw.models[[i]] <- JRip(class~., data = current.dataset)
      
      predictions[i] <- predict(pw.models[i], newdata = data[eval.idx,])
      
      trained.model.rules <- extract.model.rules(pw.models[[i]])
      rules <- rules + length(trained.model.rules)
      
      for(rule in trained.model.rules){
        conditions <- conditions + length(rule$conditions)
      }
      conditions <- conditions - 1
      
    }
    
    
    predictions[length(pw.models)+1] <- data[eval.idx, class]
    
    colnames(predictions)[ncol(predictions)] <- "class"
    class.vector <- unlist(lapply(data[,class], function(x) as.character(x)))

    
    compare.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = 2))
    vote.pred <- numeric(length(eval.idx))
    
    correct <- 0
    for (i in 1:length(eval.idx)){
        
        factor.vector <- predictions[i,1:(ncol(predictions)-1)]
        
        predict.vector <- unlist(lapply(factor.vector, function(x) as.character(x)))
        
        prediction <- pw_predict(predict.vector, class.vector)
        vote.pred[i] <- prediction
    }
    
    
    compare.frame[,1] <- vote.pred
    compare.frame[,2] <- as.character(data[eval.idx, class])
    
    for(i in 1:nrow(compare.frame)){
      correct <- correct + ifelse(compare.frame[i,1] == compare.frame[i,2], 1, 0)
    }
  
    
    accuracy <-  accuracy + ((correct/length(eval.idx))*100)
  }
  
  
  perf <- c(accuracy/times, rules/times, conditions/times)
  
  return (perf)
}

pw_predict <- function(predictions, classes){
  prediction <- 0
  
  freq <- as.data.frame(table(predictions))

  sorted <- freq[order(-freq$Freq),]

  filtered <- sorted[sorted$Freq == sorted[1,2],]
  
  if(nrow(filtered) > 1){
    class.frame <- as.data.frame(table(classes))
    class.filter <- class.frame[class.frame$classes %in% filtered[,1],]
    class.sorted <- class.filter[order(-class.filter$Freq),]
    
    class.freq.filtered <- class.sorted[class.sorted$Freq == class.sorted[1,2],]
    
    if (nrow(class.freq.filtered) > 1){
      filtered.filtered <- filtered[filtered$predictions %in% class.freq.filtered[,1],]
      
      random <- sample(filtered.filtered, 1)
      
      prediction <- random[1,1]
    }
    else{
      prediction <- filtered[filtered$predictions %in% class.freq.filtered[1,1], 1]
    }
      
  }
  else{
    prediction <- filtered[1,1]
  }
  
  return (as.character(prediction))
  
}

train.conditions.cover <- function(data, class){
    times <- 10
    accuracy <- 0
    transformed.accuracy <- 0
    rules <- 0
    conditions <- 0
    
    set.seed(1234)
    
    folds <- createFolds(data[,class], k = 10, list = TRUE, returnTrain = FALSE)
    
    for (j in 1:times){
      train.idx <- c(folds[[mod10(j+1)]], folds[[mod10(j+2)]], folds[[mod10(j+3)]], folds[[mod10(j+4)]], folds[[mod10(j+5)]], folds[[mod10(j+6)]], folds[[mod10(j+7)]], folds[[mod10(j+8)]], folds[[mod10(j+9)]])
      eval.idx <- folds[[mod10(j)]]
      
      pairWize.dataset <- create.pairwize.dataset(data[train.idx,], class)
      c.conditions <- list()
      
      for (i in 1:length(pairWize.dataset)){
        current.dataset <- pairWize.dataset[[i]]
        pw.model <- JRip(class~., data = current.dataset)
        
        trained.model.rules <- extract.model.rules(pw.model)
        if(!length(trained.model.rules) == 1){
          c.conditions <- c(c.conditions, extract.conditions(trained.model.rules[1:(length(trained.model.rules)-1)]))
        }
      }
      
      c.conditions <- unique(c.conditions)
      
      covers <- as.data.frame(matrix(, nrow = length(train.idx), ncol = length(c.conditions) + 1))
      
      
      for (i in 1:length(train.idx)){
        example <- train.idx[[i]]
        covers[i, ] <- classifier.conditions.covers(data[example,], c.conditions)
      }
      
      colnames(covers)[ncol(covers)] <- "class"
      
      
      
      covers[,ncol(covers)] <- as.factor(covers[,ncol(covers)])
      
      cover.classifier <- JRip(class~., covers)
      
      evaluate.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = length(c.conditions) + 1))
      
      
      for (i in 1:length(eval.idx)){
        example <- eval.idx[[i]]
        evaluate.frame[i, ] <- classifier.conditions.covers(data[example,], c.conditions)
      }
      
      
      colnames(evaluate.frame)[ncol(evaluate.frame)] <- "class"
      
      evaluate.frame[,ncol(evaluate.frame)] <- as.factor(evaluate.frame[,ncol(evaluate.frame)])
      
      eval <- evaluate_Weka_classifier(cover.classifier, newdata = evaluate.frame, seed = 100)
      
      cover.model.rules <- extract.model.rules(cover.classifier)
      
      transformed.classifier <- transform.meta.conditions.classifier(cover.classifier, c.conditions)
      
      compare.frame <- as.data.frame(matrix(, nrow = length(eval.idx), ncol = 2))
      correct <- 0
      
      for(i in 1:nrow(compare.frame)){
        example <- eval.idx[[i]]
        compare.frame[i,1] <- transform.cover.predict(data[example,], transformed.classifier)
        compare.frame[i,2] <- as.character(predict(cover.classifier, evaluate.frame[i, ]))
        compare.frame[i,3] <- data[example,"class"]
      }
      
      for(i in 1:nrow(compare.frame)){
        correct <- correct + ifelse(compare.frame[i,1] == compare.frame[i,3], 1, 0)
      }
      
      transformed.accuracy <-  transformed.accuracy + ((correct/length(eval.idx))*100)
      
      for(rule in cover.model.rules){
        conditions <- conditions + length(rule$conditions)
      }
      conditions <- conditions - 1
      
      accuracy <- accuracy + eval$details["pctCorrect"]
      
      rules <- rules + length(cover.model.rules)
    }
    
    perf <- c(accuracy/times, transformed.accuracy/times, rules/times, conditions/times)
    
    return (perf)
    
}


