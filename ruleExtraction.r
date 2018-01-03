library(rJava)
library(stringr)

convert.rule <- function(rule, original.rules = NULL){
  
  conditions.list <- list()
  index <- 1
  for(condition in rule$conditions){
    splitted.rule <- unlist(strsplit(condition, " "))
    
    if (splitted.rule[3] == 1){
      conditions.list[[index]] <- get.rule(splitted.rule[1], original.rules)
    }
    else {
      conditions.list[[index]] <- get.negated.rule(splitted.rule[1], original.rules)
    }
    
    index = index + 1
  }
  
  rule$conditions <- conditions.list
  
  return (rule)
}

get.rule <- function(condition, original.rules){
  rule.index <- substr(condition, 2, nchar(condition))
  
  rule.index <- as.numeric(rule.index)
  
  return (original.rules[[rule.index]]$conditions)
}

get.negated.rule <- function(condition, original.rules){
  rule.index <- substr(condition, 2, nchar(condition))
  
  rule.index <- as.numeric(rule.index)
  
  new.conditions <- original.rules[[rule.index]]$conditions
  
  for (i in 1:length(new.conditions)){
    new.conditions[[i]] <- paste0("!", new.conditions[[i]])
  }
  
  return (new.conditions)
}

convert.condition <- function(rule, conditions){
  conditions.list <- list()
  index <- 1
  for(condition in rule$conditions){
    splitted.rule <- unlist(strsplit(condition, " "))
    
    if (splitted.rule[3] == 1){
      conditions.list[[index]] <- get.condition(splitted.rule[1], conditions)
    }
    else {
      conditions.list[[index]] <- get.negated.condition(splitted.rule[1], conditions)
    }
    
    index = index + 1
  }
  
  rule$conditions <- conditions.list
  
  return (rule)
}

get.condition <- function(condition, original.conditions){
  condition.index <- substr(condition, 2, nchar(condition))
  
  condition.index <- as.numeric(condition.index)
  
  return (original.conditions[[condition.index]])
}

get.negated.condition <- function(condition, original.conditions){
  condition.index <- substr(condition, 2, nchar(condition))
  
  condition.index <- as.numeric(condition.index)
  
  new.conditions <- original.conditions[[condition.index]]
  
  for (i in 1:length(new.conditions)){
    new.conditions[[i]] <- paste0("!", new.conditions[[i]])
  }
  
  return (new.conditions)
}

# Print the rules of a classifier model

print.model.rules <- function(model){
  model.string <- .jstrVal(model$classifier)
  
  splitted.model <- strsplit(model.string, "\n")
  for (rule in splitted.model){
    print(rule)
  }
}

extract.conditions <- function(rules){
  conditions <- list()
  for(i in 1:length(rules)){
    conditions <- c(conditions, rules[[i]]$conditions)
  }
  
  return(conditions)
}

# Extract Rules from a OneR model
extract.oneR.rules <- function(model){
  model.string <- .jstrVal(model$classifier)
  
  splitted.model <- strsplit(model.string, "\n")
  
  variable <- splitted.model[[1]][1]
  rules <- list(length(splitted.model[[1]])-1)
  index <- 1
  for(i in 2:(length(splitted.model[[1]])-1)){
    rules[[index]] <- create.oneR.rule.with.condition(variable, splitted.model[[1]][i] )
    index <- index + 1
  }
  
  return (rules)
}

create.oneR.rule.with.condition <- function(variable.string, condition ){
  splitted.condition <- strsplit(condition, "->")
  
  variable.string <- gsub(":", "", variable.string)
  
  class.string <- str_trim(splitted.condition[[1]][2], "both")
  

  
  splitted.tabs <- strsplit(splitted.condition[[1]][1], "\t")
  
  condition <- splitted.tabs[[1]][2]
  

  
  split.condition <- strsplit(condition, " ")
  
  if(length(split.condition[[1]]) == 1){
    
    if(is.na(as.numeric(condition))){
      condition <- paste0("\"", condition, "\"")
    }
    
    rule <- paste0(variable.string, " == ", condition)
  }
  else{
    converted.condition <- split.condition[[1]][2]
    
    if(is.na(as.numeric(converted.condition))){
      converted.condition <- paste0("\"", converted.condition, "\"")
    }
    
    rule <- paste0(variable.string, " ", split.condition[[1]][1], " ", converted.condition)
  }
  
  conditions <- list(rule)
  
  return(list(conditions = conditions, class = class.string))
}

# Extract rules from a JRip model
# Returns a list of all rules

extract.model.rules <- function(model){
  
  model.string <- .jstrVal(model$classifier)
  
  splitted.model <- strsplit(model.string, "\n")
  
  rule.list <- splitted.model[[1]]
  
  model.rules <- rule.list[4:(length(rule.list)-2)]
  
  formated.rules <- create.rules.with.conditions(model.rules)
  
  return(formated.rules)
  
}


#Create a list of  "object" rules, each containing attributes conditions and conclusion
create.rules.with.conditions <- function(unformated.rule.list){
  list.length <- length(unformated.rule.list)
  
  rules <- list(list.length)
  
  for (i in 1:(list.length)){
    rules[[i]] <-  create.rule(unformated.rule.list[i])
  }
  
  return (rules)
}


#Create a rule "object", each containing attributes conditions and conclusion
create.rule <- function(unformated.rule.string){
  
  split.conditions.conclusion <- strsplit(unformated.rule.string, "=>")
  # if(length(split.conditions.conclusion) == 0){
  #   return ()
  # }
# 
#   if(!is.empty(split.conditions.conclusion[[1]][1])){
#  
#   }   
#   
  
  conditions.string <- split.conditions.conclusion[[1]][1]
  conclusion.string <- split.conditions.conclusion[[1]][2]
  
  rule <- list(conditions = create.conditions(conditions.string), conclusion = create.conclusion(conclusion.string))
  
  
  return (rule)
}


# Format conditions to be a list of each condition as a string, clears whitespace and removes paranthesis

create.conditions <- function(unformated.condition.string){
  
  
  split.conditions <- strsplit(unformated.condition.string, "and")
  
  nr.conditions <- length(split.conditions[[1]])
  
  conditions <- vector("list", nr.conditions)
  
  
  for (i in 1:nr.conditions){
    trim.string <- str_trim(split.conditions[[1]][i], "both")
    formated.string <- substr(trim.string, 2, nchar(trim.string)-1)
    formated.string2 <- gsub(" = ", " == ", formated.string)
    conditions[[i]] <- replace.characters(formated.string2)
  }
  return (conditions)
}


replace.characters <- function(character.string){
  if(!grepl("==", character.string)){
    return (character.string)
  }
  split.condition <- strsplit(character.string, " == ")
  equals.variable <- split.condition[[1]][2]
  
  if(is.na(as.numeric(equals.variable))){
    replacement <- paste0("\"", equals.variable, "\"")
    return (gsub(equals.variable, replacement, character.string ))
  }
  else{
    return (character.string)
  }
}

#Creates a condition with 
create.condition <- function(formated.condition.string){
  
  split.condition <- strsplit(formated.condition.string, " ")
  condition <- list(variable = split.condition[[1]][1], operator = split.condition[[1]][2], value = split.condition[[1]][3])
  return (condition)
  
}

create.conclusion <- function(unformated.conclusion.string){
  trim.string <- str_trim(unformated.conclusion.string, "both")
  split.conclusion <- strsplit(trim.string, " ")
  
  unformated.class <- split.conclusion[[1]][1]
  formated.class <- strsplit(unformated.class, "=")[[1]][2]
  
  formated.accuracy <- split.conclusion[[1]][2]
  
  conclusion <- list(class = formated.class, accuracy = formated.accuracy)
  
}
