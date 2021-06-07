#########################################################################################################################################################
#########################################################################################################################################################
############################################################## Nuvem ################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

# Limpar texto:


dados$rownumber = 1:dim(dados)[1]
review_corpus = VCorpus(VectorSource(dados$texto))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
# Remover Stopwords 
review_corpus = tm_map(review_corpus, removeWords, c(
  "que"                 ,
  "nao"                 ,
  "para"                ,
  "com"                 ,
  "mais"                ,
  "tem"                 ,
  "uma"                 ,
  "muito"               ,
  "quando"              ,
  "por"                 ,
  "porque"              ,
  "eles"                ,
  "gente"               ,
  "esta"                ,
  "tenho"               ,
  "ter"                 ,
  "vezes"               ,
  "pra"                 ,
  "voce"                ,
  "ser"                 ,
  "isso"                ,
  "fica"                ,
  "mas"                 ,
  "fazer"               ,
  "sem"                 ,
  "minha"               ,
  "aqui"                ,
  "como"                ,
  "acho"                ,
  "foi"                 ,
  "hora"                ,
  "outra"               ,
  "meu"                 ,
  "ate"                 ,
  "sao"                 ,
  "outro"               ,
  "ela"                 ,
  "era"                 ,
  "mes"                 ,
  "estou"               ,
  "coisa"               ,
  "tinha"               ,
  "pessoa"              ,
  "depois"              ,
  "nos"                 ,
  "dos"                 ,
  "tudo"                ,
  "sempre"              ,
  "pelo"                ,
  "vem"                 ,
  "mesmo"               ,
  "temos"               ,
  "esse"                ,
  "tive"                ,
  "ele"                 ,
  "querer"              ,
  "pouco"               ,
  "pois"                ,
  "exemplo"             ,
  "nem"                 ,
  "nunca"               ,
  "hoje"                ,
  "parte"               ,
  "vez"                 ,
  "entao"               ,
  "essa"                ,
  "questao"             ,
  "vai"                 ,
  "ninguem"             ,
  "toda"                ,
  "bem"                 ,
  "tambem"              ,
  "das"                 ,
  "seria"               ,
  "nada"                ,
  "poderia"             ,
  "seja"                ,
  "todo"                ,
  "agora"               ,
  "sei"                 ,
  "trocar"              ,
  "estao"               ,
  "maior"               ,
  "muita"               ,
  "todos"               ,
  "estava"              ,
  "veio"                ,
  "pessoas"             ,
  "pela"                ,
  "dar"                 ,
  "menos"               ,
  "ainda"               ,
  "faz"                 ,
  "novo"                ,
  "alto"                ,
  "quem"                ,

  stopwords("portuguese")))  
review_corpus = tm_map(review_corpus, stripWhitespace)

# Criar matriz  
b=DocumentTermMatrix(review_corpus,control = list (weighting =  weightBin))
ap_td <- tidy(b)
PB_dados <- merge(dados,ap_td, by.x = "rownumber", by.y = "document")

rm(ap_td);rm(b);rm(review_corpus);rm(dados)


write.table(PB_dados, file = "C:\\Users\\A0067426\\Desktop\\TX Mining Barros\\PB_dados_1802.csv",row.names=FALSE, na="",col.names=TRUE, sep=";")

