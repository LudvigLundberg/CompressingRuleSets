library(lazyeval)

classifier.covers <- function(example, classifier.rules, class.index = length(example)){
  return.vector <- list(length(classifier.rules))
  
  
  for (i in 1:length(classifier.rules)){
    return.vector[[i]] <- rule.covers(example, classifier.rules[[i]]$conditions)
    return.vector[[i]] <- as.numeric(return.vector[[i]])
  }
  
  return.frame <- data.frame(return.vector)

  return.frame[, length(return.vector) + 1] <- as.character(example[, "class"])

  return (return.frame)
}

classifier.conditions.covers <- function(example, classifier.condtitions, class.index = length(example)){
  
  return.vector <- list(length(classifier.condtitions))
  
  for (i in 1:length(classifier.condtitions)){
    return.vector[[i]] <- rule_eval(example, classifier.condtitions[[i]])
    return.vector[[i]] <- as.numeric(return.vector[[i]])
  }
  
  return.frame <- data.frame(return.vector)
  
  return.frame[, length(return.vector) + 1] <- as.character(example[, "class"])
  
  return (return.frame)
}

rule.covers <- function(example, rule){
  covers = 1
  for (condition in rule){
    if(rule_eval(example, condition) == FALSE ){
      covers = 0
    }
  }
  return (covers)
}

rule_eval <- function(example, condition){
  eval(parse(text = condition), envir = example)
}

transform.meta.classifier <- function(classifier, rules){
  classifier.rules <- extract.model.rules(classifier)
  
  if(length(classifier.rules) == 1){
    return (classifier.rules)
  }
  else{
    for (i in  1:(length(classifier.rules)-1)){
      classifier.rules[[i]] <- convert.rule(classifier.rules[[i]], rules)
    }
    
    return (classifier.rules)
  }

}

transform.meta.conditions.classifier <- function(classifier, conditions){
  classifier.rules <- extract.model.rules(classifier)
  
  if(length(classifier.rules) == 1){
      return (classifier.rules)
  }
  else{
    for (i in  1:(length(classifier.rules)-1)){
      classifier.rules[[i]] <- convert.condition(classifier.rules[[i]], conditions)
    }
    
    return (classifier.rules)
  }
}

pairwize.datasplit.vector <- function(level.vector){
  return(combn(level.vector, 2, simplify = FALSE))
}

create.pairwize.dataset <- function(dataframe, class.index){
  datasplit.vector <- pairwize.datasplit.vector(levels(dataframe[, class.index]))
  
  return.set = list(length(datasplit.vector))
  
  i = 1
  
  for(pair in datasplit.vector){
    
    sub.set <- subset(dataframe, dataframe[,class.index] %in% pair)
    
    return.set[[i]] <-  sub.set
    
    i = i + 1
  }
  
  return(return.set)
  
}
