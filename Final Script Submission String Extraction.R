library(readr)
library(stringr)
library(dplyr)

train_data <- read_csv("train.csv") # loading training data
test_data <- read_csv("test.csv") # loading test data

#extracting strings following most common pattern in description
string.extracted<-str_extract_all(train_data$description, "(?<!\\.)\\b[a-z\\d-_]+\\b(?=\\.([a-z\\d]+\\.){0,4}(ecnahcdroffilc)\\.(net|com))")

# au-per-06a-stwp-01.per.asia.ecnahcdroffilc.com, this is the most common pattern where "au-per-06a-stwp-01" is the desired string
# This pattern in a particular description occurs more than one time.
# so keeping track on which nth pattern string matches with StringToExtract in training data.

index.df<-data.frame()
for(j in 1:4){
  for(i in 1:length(string.extracted)){
    index.df[i,j] = (train_data$StringToExtract[i] == string.extracted[[i]][j])
  }  
}

# binding columns of index.df with train_data
train_data<-train_data%>%
  bind_cols(index.df)

# merging train_data and test_data
whole.data<- train_data%>%
  bind_rows(test_data)%>%
  # mutate(description = )%>%
  mutate(description2 = description%>%
           str_replace_all(pattern = "\\d+",replacement = "")%>%
           str_replace_all(pattern = "[[:punct:]]",replacement = "")
  )%>%
  mutate(nchars = nchar(description2))%>%
  arrange(description2)

# grouping similar descriptions in whole.data i.e. merged dataset with certain fixed parameters.
# taking character counts into account
grouping.array<-array()
for(i in 2:(length(whole.data$description2))){
  nchar.prev.ind<-whole.data$description2[i-1]%>%nchar
  nchar.pres.ind<- whole.data$description2[i]%>%nchar
  if(nchar.pres.ind > 1500){
    if(nchar.prev.ind - 200 <= nchar.pres.ind &
       nchar.pres.ind <= nchar.prev.ind + 200 ){
      grouping.array[i] = F
    } else {
      grouping.array[i] = T
    }  
  } else if(nchar.pres.ind > 700){ 
    if(nchar.prev.ind - 30 <= nchar.pres.ind &
       nchar.pres.ind <= nchar.prev.ind + 30){
      grouping.array[i] = F
    } else {
      grouping.array[i] = T
    }
  } else {
    if(nchar.prev.ind - 16 <= nchar.pres.ind &
       nchar.pres.ind <= nchar.prev.ind + 16){
      grouping.array[i] = F
    } else {
      grouping.array[i] = T
    }
  }
}

grouping.array[1]<-T

# merging grouping.array in whole.data
whole.data<-whole.data%>%
  mutate(groups = grouping.array%>%cumsum
  )
# removing some undesired patterns which might create trouble in further analysis
whole.data$description<-str_replace_all(whole.data$description, pattern = "mdm\\.ecnahcdroffilc\\.com","")

# separating some unique set of descriptions whose extracted string might have ip in it.
# treatment will be done separately
whole.data.leftover1.ip<-
  whole.data%>%
  filter(description%>%str_detect("^oionr|^cisco|^sim"))

whole.data<- whole.data%>%
  filter(!(id %in% whole.data.leftover1.ip$id))

### Method Algorithm 1
##### extracting StringToExtract following most commonly found pattern at nth number position.
pattern<- "(?<!\\.)\\b[a-z\\d-_]+\\b(?=\\.([a-z\\d]+\\.){0,4}(ecnahcdroffilc|tnauqe)\\.(net|com))"
V1.ext1<-whole.data%>%
  filter(groups %in% groups[V1])%>%
  mutate(ext1 = description%>%
           str_extract(pattern))

V2.ext1<-whole.data%>%
  filter(groups %in% groups[V1==F & V2 == T])%>%
  mutate(ext1 = description%>%
           str_extract_all(pattern)%>%
           sapply(function(x){x[2]}))


V3.ext1<-whole.data%>%
  filter(groups %in% groups[V1==F & V2 == F & V3 == T])%>%
  mutate(ext1 = description%>%
           str_extract_all(pattern)%>%
           sapply(function(x){x[3]}))

V4.ext1<-whole.data%>%
  filter(groups %in% groups[V1==F & V2 == F & V3 == T])%>%
  mutate(ext1 = description%>%
           str_extract_all(pattern)%>%
           sapply(function(x){x[4]}))

count.1<-V1.ext1%>%
  bind_rows(V2.ext1%>%
              filter(!(id %in% V1.ext1$id)))%>%
  bind_rows(V3.ext1%>%
              filter(!(id %in% V1.ext1$id)&!(id %in% V2.ext1$id)))%>%
  bind_rows(V4.ext1%>%
              filter(!(id %in% V1.ext1$id)&!(id %in% V2.ext1$id)&!(id %in% V3.ext1$id)))

count.1<- count.1%>%
  filter(is.na(StringToExtract)) ### count.1 is a dataset of seggregated entries


### whole.data.leftover ip treatment
# merging whole.data.leftover1.ip and whole.data for common treatment of leftover entries.
whole.data<-whole.data%>%
  bind_rows(whole.data.leftover1.ip)

whole.data<- whole.data%>%
  filter(!(id %in% count.1$id))

### Method Algorithm 1
# creating a function to extract same strings which are already available in train_data, grouping wise.

sorting.function<- function(x= StringToExtract, y = description){
  string1<- x
  if(length(string1)==1){
    return(string1)
  }
  description1<- y
  find.out<-is.na(string1)
  already.have<- !find.out
  logical.val <- str_detect(string1, "\\.")
  logical.val2<- str_detect(string1, "will")
  logical.val[is.na(logical.val)]<-F
  logical.val2[is.na(logical.val2)]<-F
  if((c(T) %in% find.out) & (F %in% find.out)){
    check <- F
    if((T %in% logical.val)){
      check<-T
      already.have[logical.val]<- F
    }
    if((T %in% logical.val2)){
      already.have[logical.val2]<- F
    }
    
    pattern1<- string1[already.have]%>%
      str_replace_all(pattern = "\\d+", replacement = "[a-z\\\\d-]+")%>%
      str_replace_all("(?<=\\\\d\\+)[a-z](?=\\\\d\\+)","[a-z]")%>%
      table()%>%
      sort(decreasing = T)%>%
      names()%>%
      unique()
    if(!is.null(pattern1)){
      pattern1<- str_c("\\b",pattern1,"\\b")
    }
    if(check == T){
      pattern1<- c(pattern1, "\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b")
    }
    
    for(i in pattern1){
      string1[is.na(string1)] = str_extract(description1[is.na(string1)],i)
    }
    
  } else {
    return(string1)
  }
  return(string1)
} 

#########################################################
### catching all same entries if present in the particular group

whole.data<-whole.data%>%
  group_by(groups)%>%
  mutate(ext1 = sorting.function(x = StringToExtract, 
                                 description))  #ext1 column refers to finally extracted entries from description
count.1<- count.1%>%
  bind_rows(whole.data%>%
              filter(!is.na(ext1) & (id %in% test_data$id)))

### updating whole.data with only entries to extract for

whole.data<- whole.data%>%
  filter(!(id %in% count.1$id))

####################################################
### removing ext1 from whole.data
whole.data<-whole.data%>%
  select(-ext1)

### Method Algorithm 3 (sandwich algorithm i.e. using nearby text of extracted string in description to extract further strings)

trimmed.STE<-function(x){
  na.ind<-is.na(x)
  x[na.ind]<-""
  ips<-str_detect(x, "\\.")
  x[ips]<-NA
  x[!ips]<-str_replace_all(x[!ips], pattern = "\\d+|[[:punct:]]", replacement = "")
  x[na.ind]<-NA
  x
}
# binding column consisting of trimmed StringToExtract i.e. numbers and punctuation removed
whole.data<-
  whole.data%>%
  mutate(trimmedSTE = trimmed.STE(StringToExtract))

# function to extract text present before/after of StringToExtract in description.
sandwich.function<- function(description2, trimmedSTE, char.before = 10, char.after = 15, before = T){
  m<- str_locate(pattern = trimmedSTE[!is.na(trimmedSTE)], string = description2[!is.na(trimmedSTE)])
  
  if(before){
    before<-array(data = NA, trimmedSTE%>%length())
    before[!is.na(trimmedSTE)]<- substr(description2[!is.na(trimmedSTE)], m[,1]-char.before, m[,1]-1)
    before
  } else {
    after<-array(data = NA, trimmedSTE%>%length())
    after[!is.na(trimmedSTE)]<- substr(description2[!is.na(trimmedSTE)], m[,2]+1, m[,2]+char.after)
    after
  }
}

# extracting and binding before and after columns
whole.data<-whole.data%>%
  ungroup()%>%
  mutate(
    before = sandwich.function(
      description2, trimmedSTE, char.before = 10 
    ),
    after = sandwich.function(
      description2, trimmedSTE, before = F, char.after = 10
    )
  )

# Looking up characters squeezed between sandwich text i.e. before/after
sandwich.lookup.chars<- function(before , after, description2){
  StringToExtract<- rep(NA, length(before))
  if(before%>%is.na()%>%all()){
    return(StringToExtract)
  }
  before<-str_replace_all(before, "\\+","\\\\+")
  after<-str_replace_all(after, "\\+", "\\\\+")
  lookupregex<-
    paste0("(?<=", before[!is.na(before)&!is.na(after)],")[a-z=]{1,31}(?=",after[!is.na(before)&!is.na(after)], ")")%>%
    table()%>%
    sort(decreasing = T)%>%
    names()%>%
    unique()
  
  for(i in lookupregex){
    print(i)
    StringToExtract[is.na(StringToExtract)]<-str_extract(description2[is.na(StringToExtract)], i)
  }
  StringToExtract
}

# extracting and binding ext i.e. just characters of the StringToExtract without punctuation and numbers
whole.data<-whole.data%>%group_by(groups)%>%
  mutate(ext = sandwich.lookup.chars(before,
                                     after,
                                     description2))


# function for extracting final strings in leftover entries.
extract.ext1 <- function(ext, description){
  temp<-str_extract_all(ext, ".") 
  patterns<-lapply(temp,function(x){
    paste0(x,"[\\=\\_\\d-]{0,}",collapse = "")
  })%>%unlist
  temp<-str_extract(description, patterns)
  temp<-temp%>%str_replace("-+$|_+$", "")
  temp%>%str_replace_all("[_]{2,}","_")
}

results1<-whole.data%>%
  filter(!is.na(ext))%>%
  mutate(ext1 = extract.ext1(ext, description))

whole.data.leftover1<-
  results1%>%filter(is.na(ext1))

### binding extracted results in count.1
results1<-results1%>%
  filter(!is.na(ext1))
results1<-results1%>%
  filter(id %in% test_data$id & !(id %in% count.1$id))

### binding in count.1
count.1<- count.1%>%
  bind_rows(results1)

### update whole.data

whole.data<- whole.data%>%
  filter(!(id %in% count.1$id))

whole.data<-whole.data%>%
  filter(!(id %in% whole.data.leftover1$id))

### whole.data.leftover1 treatment function three regular expressions

str.ext1<- function(x){
  ext1<-rep(NA, length(x))
  ext1[is.na(ext1)]<- str_extract(x[is.na(ext1)], "[a-z\\d-]+(?=(\\.internal))")
  ext1[is.na(ext1)]<- str_extract(x[is.na(ext1)], "[a-z\\d-]+(?=(\\.[a-z\\d]+\\.ecnahcdroffilc))")
  ext1[is.na(ext1)]<- str_extract(x[is.na(ext1)], "[a-z\\d-]+(?=(\\.ecnahcdroffilc))")
  ext1
}

whole.data.leftover1<-whole.data.leftover1%>%
  mutate(ext1 = str.ext1(description))

### whole.data.leftover1 in 
results1<- whole.data.leftover1%>%
  filter(id %in% test_data$id)

### updating count.1
count.1<- count.1%>%
  bind_rows(results1)

### whole.data.leftover2 
whole.data.leftover2<- whole.data%>%
  filter(is.na(StringToExtract))


### whole.data.leftover
extraction.function<- function(StringToExtract, description){
  pattern.replace<-"ddl-msg-mgmt03\\.internal|ddl-oionr-01\\.internal|mad-10-asw1-03\\.internal|mad-10-printers-03\\.internal|rom-ug-asw1-02\\.internal|rom-ug-asw1-01\\.internal|rom-ug-dsw1-01\\.internal|rom-ug-dsw1-02\\.internal|rom-ug-ilo-01\\.internal"
  logicalval<- str_detect(description,"^oionr|^cisco")
  description[logicalval]<-description[logicalval]%>%str_replace_all(pattern.replace, " ")
  patterns<-c("serverfqdn=[a-z\\d-]+",
              "dc=internal",
              "\\bddlsql\\d+\\b",
              "(?<=(router\\:\\:))[a-z\\d-_]+",
              "\\b[a-z\\d]+(?=\\.controlnet)",
              "(?<=(target name: ))[a-z]+(?=[[:cntrl:]])",
              "(?<=item:\\s{0,3})[a-z\\d-_]+",
              "(?<=(item:\\s{0,3}|host:\\s{0,3}))[a-z\\d-_]+",
              "(?<=(router ))[a-z\\d-_]+",
              "(?<=(ciscotrap_))[a-z\\d-_]+",
              "(?<=(client: ))[a-z\\d-]+\\b",
              "(?<=(target_host\\: ))[a-z\\d-]+\\b",
              "(?<=(computer:))[a-z\\d-]+\\b",
              "[a-z\\d-]+(?=\\.[a-z]+\\.[a-z]+\\.ecnahcdroffilc)",
              "[a-z\\d-]+(?=\\.[a-z]+\\.ecnahcdroffilc)",
              "[a-z\\d-]+(?=\\.internal)",
              "[a-z\\d-]+(?=\\.ecnahcdroffilc)",
              "(?:\\d{1,3}\\.){3}\\d{1,3}",
              "(?<=(computer ))[a-z\\d-]+\\b(?= was)"
  )
  for(i in patterns){
    print(i)
    StringToExtract[is.na(StringToExtract)]<-
      description[is.na(StringToExtract)]%>%str_extract(i)
  }
  StringToExtract
}

# results1
results1<- whole.data.leftover2%>%
  mutate(ext1 = extraction.function(StringToExtract,description))

results1<- results1%>%
  filter(id %in% test_data$id)

### binding results1 to count.1 again
count.1<-
  count.1%>%
  bind_rows(results1)

c1=count.1%>%ungroup()%>%select(id, StringToExtract = ext1)

### output
c1
