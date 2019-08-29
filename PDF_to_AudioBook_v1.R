####Installing and Loading Packages ####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pdftools,stringr, plyr, dplyr, tidyr, readtext)

library(stringr,pdftools,plyr,dplyr,tidyr,readtext)


### READING IN FILES ####
# Creating a list of file names
dir <- "C:/Users/Jason Cook/Box Sync/Misc/audio_articles" #CHANGE THIS DIRECTORY TO YOUR FOLDER OF PDFs
filenames <- list.files(dir,pattern = '*.pdf', full.names = TRUE) #creating list of all txt files



# The loop will grab each sentance within each file that contains the keyword(s) and then will add the results to the Variable dataframe created above
for (i in filenames){
  text <- readtext(i)
  doc_id <- text$doc_id
  doc_id <- gsub("pdf","txt",doc_id)
  outname <- paste(dir,doc_id, sep="/")
  text$text <- tolower(text$text)
  
  #Remove references
  text$text <-  gsub("references.*","",text$text)
  
  #See if NBER Working Paper
  if (length(grep("nber working paper series",text$text))==1){
    #Toss first page
    text$text <-  gsub("^.*?\r\n\n","",text$text)
    
    #Title
    title <- str_extract(text$text, "^.*?\r\n")
    text$text <-  gsub("^.*?\r\n","",text$text)
    
    #First author name
    name <- str_extract(text$text, "^[a-z]+\\s")
    
    #Scrape Abstract (from word "abstract" to the first author's first name)
    match <- paste0("abstract.*?", name)
    m <- regexpr(match, text$text)
    abstract <-  regmatches(text$text, m)
    abstract <-  gsub(name,"",abstract)
    abstract <-  gsub("abstract","abstract.",abstract)
    abstract <-  str_replace_all(abstract, "\r\n"," ")
    
    #Delete second page
    text$text <-  gsub("^.*?\r\n\n","",text$text)
    
    #Add abstract back into front
    text$text <- paste0(abstract, " ", text$text)
  }
  
  #AEJ-specific stuff
  if (length(grep("american economic journal",text$text))==1){
    #For AEJs the first two lines are just the first page header
    text$text <-  gsub("^.*?\r\n","",text$text)
    text$text <-  gsub("^.*?\r\n","",text$text)
  }
  
  #Remove title page footnotes
  text$text <-  gsub("\r\n\\s*\\*.*?(\r\n\n)","\\1",text$text)
  text$text <-  gsub("\r\n\\s*???.*?(\r\n\n)","\\1",text$text)
  
  #text$text <-  gsub("it is also 91%.*","",text$text)
  
  #Remove footnotes
  text$text <-  gsub("\r\n\\s*[0-9]+\r\n\\s*([a-z].*?)(\r\n\n)","\\2\\3",text$text)
  
  #AEJ-specific stuff
  if (length(grep("american economic journal",text$text))==1){
    #Remove paper header
    text$text <-  gsub("\r\n\n.*?\r\n"," ",text$text)
  }

  
  #Remove paretheneses to remove citations
  text$text <-  gsub("\\(.*?\\)","",text$text)
  
  #Remove hyphens near line spaces
  text$text <-  str_replace_all(text$text, "-\r\n","")
  
  #Remove foot note numbers from text
  text$text <-  gsub("\\.[0-9]+ ([a-z])","\\. \\1",text$text)
  text$text <-  gsub(",[0-9]+",", ",text$text)
  
  #Remove Tables and Figures
  text$text <-  gsub("table [0-9].*?notes.*?\\.\r\n([a-z])"," \\1",text$text) #remove table notes because it usually starts the text with a lower case letter after
  text$text <-  gsub("table [0-9].*?notes"," ",text$text) #For rest, just remove table and stop at notes heading
  
  text$text <-  str_replace_all(text$text, "\r\n"," ")
  #text$text <-  str_replace_all(text$text, "-","-")
 
  text$text <-  str_replace_all(text$text, "[^[:ascii:]]", "") #keeps only ascii characters
  
  #Basic Cleaning
  text$text <-  str_replace_all(text$text, "\\.+","\\.")
  text$text <-  str_replace_all(text$text, "\\s+\\.","\\.")
  text$text <-  str_replace_all(text$text, "\\s+,",",")
  text$text <-  str_replace_all(text$text, "\\s+;",";")
  text$text <-  str_replace_all(text$text, "\\s*$" , "")
  text$text <-  str_replace_all(text$text, "^\\s*" , "")
  text$text <-  str_replace_all(text$text, "\\s+" , " ") #replace all duplicate spaces with singe space
 
  write(text$text,outname)
}



