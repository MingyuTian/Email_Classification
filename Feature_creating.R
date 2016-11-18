load("TrainingMessages.rda")

#isSpam
isSpam = grepl("spam", names(message))

#isRe
isRe = vector('numeric', length(message))
for(i in 1:length(message)){
  b = strsplit(message[[i]]$body, '\n')[[1]]
  isRe[i] = any(grepl('^Re', b))
}

#numLinesInBody
numLinesInBody = vector('numeric', length(message))
for(i in 1:length(message)){
  b = strsplit(message[[i]]$body, '\n')[[1]]
  numLinesInBody[i] = length(b)
}

#bodyCharacterCount
bodyCharacterCount = sapply(1:length(message), function(i)
  nchar(message[[i]]$body, type = 'bytes'))

#subjectExclamationCount
subjectExclamationCount = sapply(1:length(message), function(i)
  length(gregexpr("!", message[[i]]$body)[[1]]))

#subjectQuestCount
subjectQuestCount = sapply(1:length(message), function(i)
  length(gregexpr("\\?", message[[i]]$body)[[1]]))

#numAttachments
numAttachments = sapply(1:length(message), function(i) length(message[[i]]$attachment))

#priority
priority = sapply(1:length(message), function(i) 
  any(names(message[[i]]$head[ , ]) == "X-Priority" | 
        names(message[[i]]$head[ , ]) == "X-Msmail-Priority"))

#isInReplyTo
isInReplyTo = sapply(1:length(message), function(i)
  any(names(message[[i]]$head[ , ]) == "In-Reply-To"))

#percentCapitals
alpha = sapply(1:length(message), function(i)
  length(gregexpr("[[:alpha:]]", message[[i]]$body)[[1]]))
upper = sapply(1:length(message), function(i)
  length(gregexpr("[[:upper:]]", message[[i]]$body)[[1]]))
percentCapitals = upper/alpha

#subjectPunctuationCheck
subjectPunctuationCheck = vector('logical', length(message))
index =  sapply(1:length(message), function(i)
  any(names(message[[i]]$head[ , ]) == "Subject"))
index = which(index == TRUE)
subjectPunctuationCheck[index] = sapply(index, function(i) 
  grepl('[[:alpha:]][[:punct:][:digit:]][[:alpha:]]', message[[i]]$head[ , "Subject"]))

#hourSent
hourSent = sapply(1:length(message), function(i) 
  strsplit(message[[i]]$head[ , "Date"], "[ :]")[[1]][5])
i = which(hourSent > 24)
hourSent[i] = -1

#percentSubjectBlanks
percentSubjectBlanks = vector('numeric', length(message))
index =  sapply(1:length(message), function(i)
  any(names(message[[i]]$head[ , ]) == "Subject"))
index = which(index == TRUE)
percentSubjectBlanks[index] = sapply(index, function(i) 
  length(grepl(' ', message[[i]]$head[ , "Subject"]))/nchar(
    message[[i]]$head[ , "Subject"], type = "bytes"))

#isYelling
isYelling = vector('logical', length(message))
i = which(percentCapitals == 1)
isYelling[i] = TRUE

#subjectSpamWords
subjectSpamWords = vector('logical', length(message))
index =  sapply(1:length(message), function(i)
  any(names(message[[i]]$head[ , ]) == "Subject"))
index = which(index == TRUE)
subjectSpamWords[index] = sapply(index, function(i) 
  grepl('(viagra|pounds|free|weight|guarantee|millions|dollars|
        credit|risk|prescription|generic|drug|money back|credit card)',
        message[[i]]$head[ , "Subject"]))

#percentForwards
percentForwards = vector("numeric", length(message))
for(i in 1:length(message)){
  a = strsplit(message[[i]]$body, "\n")
  b = grepl("^>", a[[1]])
  percentForwards[i] = length(b[b == TRUE])/length(a[[1]])
}

#isOriginalMessage
isOriginalMessage = sapply(1:length(message), function(i) 
  grepl("original", message[[i]]$body))

#isDear
isDear = sapply(1:length(message), function(i) 
    grepl("(Dear|Mr|Mrs|Miss|Ms)", message[[i]]$body))

#numDollarSigns
numDollarSigns = sapply(1:length(message), function(i) 
  length(gregexpr("\\$", message[[i]]$body)[[1]]))

#percentDigits the percentage of digits in the body of the email 
digit = sapply(1:length(message), function(i)
    length(gregexpr("[[:digit:]]", message[[i]]$body)[[1]]))
percentDigits = digit/(digit+alpha)

#to establish the data frame
myemail = data.frame(isSpam, isRe, numLinesInBody, bodyCharacterCount,
                     subjectExclamationCount, subjectQuestCount, numAttachments,
                     priority, isInReplyTo, percentCapitals, subjectPunctuationCheck,
                     hourSent, percentSubjectBlanks, isYelling, subjectSpamWords,
                     percentForwards, isOriginalMessage, isDear, numDollarSigns,
                     percentDigits)

##################################################################################################
#clean the data
myemail[,12] = as.numeric(myemail[,12])
index = which(is.na(myemail$percentForwards))
myemail$percentForwards[index] = 0
index1 = which(myemail$percentSubjectBlanks > 1)
myemail$percentSubjectBlanks[index1] = 0

#The correlation of them
correlation = cor(myemail)
sort(correlation[,1])

#tables of some of the cloumns
with(myemail, table(isSpam, isInReplyTo))
with(myemail, table(isSpam, priority))
with(myemail, table(isSpam, isDear))

#scatter plot
attach(myemail)
par(mfrow = c(1,2))
CapSpam = split(percentCapitals, isSpam)
boxplot(CapSpam, main = "percentCapitals and isSpam")
plot(percentCapitals, col = factor(isSpam), main = "percentCapitals and isSpam")
ForSpam = split(percentForwards, isSpam)
boxplot(ForSpam, main = "percentForwards and isSpam")
plot(percentForwards, col = factor(isSpam), main = "percentForwards and isSpam")
ExcSpam = split(subjectExclamationCount, isSpam)
boxplot(ExcSpam, ylim = c(0,100), main = "subjectExclamationCount and isSpam")
plot(subjectExclamationCount, ylim = c(0,100), col = factor(isSpam), main = "subjectExclamationCount and isSpam")
