train<- read.csv("./train.csv",quote = "",sep = "\"", header = T, stringsAsFactors = F)
#1、情感正向词，词组+打“+1”-label
pos <- read.csv("./pos.csv", header = T, sep = ",", stringsAsFactors = F)
weight <- rep(1, length(pos[,1]))
pos <- cbind(pos, weight)

#2、情感负向词，词组+打“-1”-label
neg <- read.csv("./neg.csv", header = T, sep = ",", stringsAsFactors = F)
weight <- rep(-1, length(neg[,1]))
neg <- cbind(neg, weight)

#3、正、负向词组合并
posneg <- rbind(pos, neg)  #正负词典合并
names(posneg) <- c("term", "weight")
posneg <- posneg[!duplicated(posneg$term), ]#`duplicated`函数的作用和`unique`函数比较相似，它返回重复项的位置编号

dict <- posneg[, "term"]
#library(Rwordseg)
#listDict()  #查看已有词库
#uninstallDict() #删除安装的词典  
insertWords(dict)

sentence <- as.vector(train.test$msg) #文本内容转化为向量sentence
sentence <- gsub("[[:digit:]]*", "", sentence) #清除数字[a-zA-Z]
sentence <- gsub("[a-zA-Z]", "", sentence)   #清除英文字符
sentence <- gsub("\\.", "", sentence)      #清除全英文的dot符号
train.test <- train.test[!is.na(sentence), ]          #清除一些空值文本（文本名）
sentence <- sentence[!is.na(sentence)]   #清除对应sentence里面的空值（文本内容），要先执行文本名
train.test <- train.test[!nchar(sentence) < 2, ]  #筛选字符数小于2的文本
sentence <- sentence[!nchar(sentence) < 2] #`nchar`函数对字符计数，英文叹号为R语言里的“非”函数
temp <- lapply(x, length)                       #每一个元素的长度,即文本分出多少个词
temp <- unlist(temp)                            #lapply返回的是一个list，所以3行unlist

id <- rep(train.test[, "id"], temp)             #将每一个对应的id复制相应的次数，就可以和词汇对应了

label <- rep(train.test[, "label"], temp)       #id对应的情感倾向标签复制相同的次数
term <- unlist(x)                               #6行将list解散为向量

testterm <- as.data.frame(cbind(id, term, label), stringsAsFactors = F) #生成一个单词-文档-数据框
stopword <- read.csv("./stopword.csv", header = T, sep = ",", stringsAsFactors = F)
stopword <- stopword[!stopword$term %in% posneg$term,]#函数`%in%`在posneg$term中查找stopword的元素，如果查到了就返回真值，没查到就返回假
testterm <- testterm[!testterm$term %in% stopword,]#去除停用词

testterm <- join(testterm, posneg)
testterm <- testterm[!is.na(testterm$weight), ]
head(testterm)
#2、计算情感指数
dictresult <- aggregate(weight ~ id, data = testterm, sum)
dictlabel <- rep(-1, length(dictresult[, 1]))
dictlabel[dictresult$weight > 0] <- 1          #很有技巧地把情感词语正负赋值到情感得分表中
dictresult <- as.data.frame(cbind(dictresult, dictlabel), stringsAsFactors = F)




###模型评价
###temp <- unique(testterm[, c("id", "label")])
###dictresult <- join(dictresult, temp)
###evalue <- table(dictresult$dictlabel, dictresult$label)