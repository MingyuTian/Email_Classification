#main function to get the output
getalloutput = function(){
  setwd("G:/email/") # the place to store emails
  dirs = list.files()
  files =  list.files(dirs, full.names = TRUE) #get all the email's name
  output = vector('list', length(files))
  output = lapply(files, function(files.element) reademails(files.element)) #get the output
  names(output) = files #give it the name
  return(output)
}

#function of read a single email
reademails = function(name){
  con = file(name, open = 'rt')
  on.exit(close(con))
  a = readLines(con)
  if(!grepl("From", a[1]) & !grepl(":", a[1])) 
    return(list(header = "wrong", body = "wrong", attachment = "wrong"))
  inspace = which(a == '')[1]
  #get the head
  raw = a[1:(inspace - 1)] 
  if(grepl("From", raw[1])) raw = raw[-1]
  head = textConnection(raw)
  header = read.dcf(head)
  close(head)
  ends = length(a)
  #to get the signal and end signal of attachment
  for( j in inspace:length(a)){
    if(grepl("^--", a[j])) {
      signal = a[j]
      endsig = paste(signal, "--", sep = "")
      if(any(a == endsig)){
        ends = j 
        break
      }
    }
  }
  #to get body
  bod = ""
  attachment = list()
  if(inspace + 1 < j) bod = c(a[(inspace+1):(j-1)], a[ends:length(a)])
  body = paste(bod, collapse= "\n")
  #to get attachment
  if(j != length(a)){ 
    attach = a[j:length(a)]
    sig = which(attach == signal)
    ends = which(attach == endsig)
    end = c(sig[-1], ends)
    if(length(sig) == 0 | length(ends) == 0) 
      return(list(header = header, body = body, attachment = "wrong"))
    ww=sapply(1:length(sig), function(i) return(list(attach[(sig[i]+1):(end[i]-1)])))
    attachment = lapply(ww, function(ww.element) getheadbody(ww.element))        
  }
  return(list(header = header, body = body, attachment = attachment))
}
#function to get head and body of the attachment
getheadbody = function(a){
  head = textConnection(a[1])
  header = read.dcf(head)
  bod = a[2:length(a)]
  body = paste(bod, collapse = "\n")
  close(head)
  return(list(header,body))
}

#use the function to get the output
message = getalloutput()
save(message, file = "TrainingMessages.rda")
