library(dplyr)
library(stringr)
### preparing whole.data 
whole.data<- train_data%>%
  bind_rows(test_data)%>%
  mutate(description2 = description%>%
           # str_replace_all(pattern = StringToExtract ,replacement = "")%>%
           str_replace_all(pattern = "\\d+",replacement = "")%>%
           str_replace_all(pattern = "[[:punct:]]",replacement = "")
  )%>%
  arrange(description2)

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
    if(nchar.prev.ind - 20 <= nchar.pres.ind &
       nchar.pres.ind <= nchar.prev.ind + 20  ){
      grouping.array[i] = F
    } else {
      grouping.array[i] = T
    }
  } else {
    if(nchar.prev.ind - 14 <= nchar.pres.ind &
       nchar.pres.ind <= nchar.prev.ind + 14  ){
      grouping.array[i] = F
    } else {
      grouping.array[i] = T
    }
    }
}

grouping.array[1]<-T

whole.data<-whole.data%>%
  mutate(groups = grouping.array%>%cumsum
  )
#########################################################
# sorting.function
sorting.function<- function(x= StringToExtract, y = description){
  string1<- x
  if(length(string1)==1){
    return(string1)
  }
  description1<- y
  find.out<-is.na(string1)
  already.have<- !find.out
  # logical.val <- str_detect(string1, "\\.|will")
  logical.val <- str_detect(string1, "\\.")
  logical.val2<- str_detect(string1,"will")
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
      str_replace_all(pattern = "\\d+", replacement = "\\\\d+")%>%
      table()%>%
      sort(decreasing = T)%>%
      names()%>%
      unique()
    pattern1<- str_c("\\b",pattern1,"\\b")
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
                                 description))
count.1<- whole.data%>%
  filter(!is.na(ext1) & id %in% test_data$id)

### updating whole.data with only entries to search for

whole.data<- whole.data%>%
  filter(!(id %in% count.1$id))

### removing ext1
whole.data<-whole.data%>%
  select(-ext1)

trimmed.STE<-function(x){
  na.ind<-is.na(x)
  x[na.ind]<-""
  ips<-str_detect(x, "\\.")
  x[ips]<-NA
  x[!ips]<-str_replace_all(x[!ips], pattern = "\\d+|[[:punct:]]", replacement = "")
  x[na.ind]<-NA
  x
}

whole.data<-
  whole.data%>%
  mutate(trimmedSTE = trimmed.STE(StringToExtract))


### extracting strings 
whole.data%>%
  filter( groups %in% count.1$groups)

whole.data%>%
  mutate(ext1 = sorting.function5(trimmedSTE, description2))

whole.data$trimmedSTE[whole.data$trimmedSTE==""]<-NA

### determining groups in which trimmed ste followed by internal


# remove.false.entries<-whole.data%>%
#   filter(StringToExtract%>%str_detect("\\."))%>%
#   mutate(str_filter = description%>%str_extract("^[a-z]+"))
# 
# internal.groups<-(whole.data%>%
#                     mutate(str_filter = description%>%str_extract("^[a-z]+"))%>%
#                     filter(!(str_filter %in% remove.false.entries$str_filter))%>%
#                     mutate(internal = str_extract(description2,
#                                                   paste0(trimmedSTE,"internal")))%>%
#                     filter(!is.na(internal))%>%
#                     select(groups))[[1]]%>%unique
# 
# 
# whole.data.internal <- whole.data%>%
# filter(groups%in%internal.groups)

sandwich.function<- function(description2, trimmedSTE, char.before = 10, char.after = 10, before = T){
  m<- str_locate(pattern = trimmedSTE[!is.na(trimmedSTE)], string = description2[!is.na(trimmedSTE)])
  
   if(before){
    before<-array(data = NA, trimmedSTE%>%length())
    before[!is.na(trimmedSTE)]<- substr(description2[!is.na(trimmedSTE)], m[,1]-char.before, m[,1]-1)
    # before[is.na(before)]<-""
    before
  } else {
    after<-array(data = NA, trimmedSTE%>%length())
    after[!is.na(trimmedSTE)]<- substr(description2[!is.na(trimmedSTE)], m[,2]+1, m[,2]+char.after)
      # after[is.na(after)]<-""
    after
  }
  
}

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

  # emptyind<- is.na(StringToExtract)
  for(i in lookupregex){
    print(i)
    StringToExtract[is.na(StringToExtract)]<-str_extract(description2[is.na(StringToExtract)], i)
  }
  # StringToExtract[emptyind][StringToExtract[emptyind]%>%str_detect("error|will")]<-NA
  StringToExtract
}

whole.data<-whole.data%>%group_by(groups)%>%
  mutate(ext = sandwich.lookup.chars(before,
                                     after,
                                     description2))

### filtering out all ip and their groups
## since they create a lot of mess
# ipsonly<- whole.data%>%
  # filter(groups %in% groups[str_detect(StringToExtract,"\\.")])

# whole.data<-whole.data%>%
  # filter(!(id %in% ipsonly$id))

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

results1<-results1%>%
  filter(!is.na(ext1))
results1<-results1%>%
  filter(id %in% test_data$id)

### binding in count.1
count.1<- count.1%>%
  bind_rows(results1)

### update whole.data

whole.data<- whole.data%>%
  filter(!(id %in% count.1$id))

### 
whole.data<-whole.data%>%
  filter(!(id %in% whole.data.leftover1$id))
### whole.data.leftover1 function


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

### binding whole.data.leftover2 entries with matching in whole.data
whole.data.leftover2<-whole.data%>%
  filter(groups %in% whole.data.leftover2$groups & !is.na(StringToExtract))%>%
  bind_rows(whole.data.leftover2)

### whole.leftover ips

whole.data.leftover3<-
  whole.data.leftover2%>%
  filter(str_detect(description2, "^oionr|^cisco"))

### update data.leftover2

whole.data.leftover2<- 
  whole.data.leftover2%>%
  filter(!(id %in% whole.data.leftover3$id))

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
              # "[a-z\\d]{8,8}(?= \\(\\))",
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

# results1<-
 results1<- whole.data.leftover2%>%
  mutate(ext1 = extraction.function(StringToExtract,description))


 results1<- results1%>%
   filter(id %in% test_data$id)
### remember to filter it please
count.1<-
  count.1%>%
  bind_rows(results1)

### whole.data.leftover3 treatment


extraction.function2<- function(StringToExtract, description){
  pattern<-"ddl-msg-mgmt03\\.internal|ddl-oionr-01\\.internal|mad-10-asw1-03\\.internal|mad-10-printers-03\\.internal|rom-ug-asw1-02\\.internal|rom-ug-asw1-01\\.internal|rom-ug-dsw1-01\\.internal|rom-ug-dsw1-02\\.internal|rom-ug-ilo-01\\.internal"
  StringToExtract<-StringToExtract%>%str_replace_all(pattern, replacement = "")
   StringToExtract[is.na(StringToExtract)]<-
    description[is.na(StringToExtract)]%>%str_extract("\\b[a-z]+(?=\\.controlnet)")
   StringToExtract[is.na(StringToExtract)]<-
     description[is.na(StringToExtract)]%>%str_extract("[a-z\\d-]+(?=\\.[a-z]+\\.[a-z]+\\.ecnahcdroffilc)")
   StringToExtract[is.na(StringToExtract)]<-
     description[is.na(StringToExtract)]%>%str_extract("(?<=(ciscotrap_))[a-z\\d-]+")
   StringToExtract[is.na(StringToExtract)]<-
     description[is.na(StringToExtract)]%>%str_extract("\\bservice\\b")
  StringToExtract[is.na(StringToExtract)]<-
    description[is.na(StringToExtract)]%>%str_extract("\\b(?:\\d{1,3}\\.){3}\\d{1,3}\\b")
  StringToExtract
}

results1<-whole.data.leftover3%>%
  mutate(ext1 = extraction.function2(StringToExtract, description))
results1<-results1%>%filter(id %in% test_data$id)
count.1<- count.1%>%
  bind_rows(results1)

count.1<- count.1%>%
  filter(!(id %in% train_data$id))

count.1<- count.1%>%
  select(id, StringToExtract = ext1)

count.1<- count.1%>%
  ungroup()%>%
  select(-groups)



####
description2[174]%>%
  str_extract(paste0("(?<=", before[174],")[a-z]{0,32}(?=", after[174],")"))%>%
  str_extract_all(".")%>%unlist%>%
  paste0("[\\=\\_\\d-]{0,}", collapse = "")->pattern

for(i in 1:100){
str_extract(description[174], pattern)%>%print
}




# #### distace dtm 
# t.f<- function(x){
#   temp<-rep(T,14)
#   c(temp, rep(F,(length(x)-14)))
# }
# library(tidytext)
# whole.data%>%
#   ungroup()%>%
#   unnest_tokens(output = "tokens",input = description2)%>%
#   group_by(id, tokens)%>%
#   summarise(n=n())->trial.dtm
# 
# trial.dtm<-trial.dtm%>%
#   mutate(top14 = t.f(id))%>%
#   filter(top14== T)%>%
#   select(-top14)%>%
#   cast_dtm(document = id, term = tokens, value = n)
# 
# 
# new.mat<-as.matrix(trial.dtm)
# 
# dist(new.mat)->distand

# check cross

c1 = read_csv(file = file.choose(),
              col_names = T)
c2 = read_csv(file = file.choose(),
              col_names = T)
c1%>%
  left_join(c2,"id")%>%
  filter(StringToExtract.x != StringToExtract.y)%>%
  left_join(test_data, by = "id")%>%
    View()
