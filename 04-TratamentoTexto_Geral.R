#########################################################################################################################################################
#########################################################################################################################################################
#               TRATAMENTO DO TEXTO PARA MARCAÇÃO DE PERSONAGENS E ATRIBUTOS 
#########################################################################################################################################################
#########################################################################################################################################################



dados$texto<-tolower(dados$texto)

dados$texto<-gsub(pattern = ",", replace = " ", x = dados$texto)
dados$texto<-gsub(pattern = paste("\\b\\s?",'tv',"\\s?\\b",sep=""), replace = " televisao ", x = dados$texto)
dados$texto<-gsub(pattern = paste("\\b\\s?",'4g',"\\s?\\b",sep=""), replace = " dadosmovel ", x = dados$texto)
dados$texto<-gsub(pattern = paste("\\b\\s?",'3g',"\\s?\\b",sep=""), replace = " dadosmovel ", x = dados$texto)
dados$texto<-gsub(pattern = paste("\\b\\s?",'2g',"\\s?\\b",sep=""), replace = " dadosmovel ", x = dados$texto)
dados$texto<-gsub(pattern = paste("\\b\\s?",'wifi',"\\s?\\b",sep=""), replace = " wireless ", x = dados$texto)
dados$texto<-gsub(pattern = paste("\\b\\s?",'wi-fi',"\\s?\\b",sep=""), replace = " wireless ", x = dados$texto)



# Limpar texto
review_corpus = Corpus(VectorSource(dados$texto))
#review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, stripWhitespace)


#########################################################################################################################################################
#               TRATANDO PALAVRAS SIMPLES
#########################################################################################################################################################

# Criar matriz  
b=TermDocumentMatrix(review_corpus)

# Substituir palavras 
gc() # limpeza memoria#

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'nao')<=1
                                      &b$dimnames$Terms!="tao"
                                      &b$dimnames$Terms!="sao"
                                      &b$dimnames$Terms!="ano"
                                      &b$dimnames$Terms!="mao"
                                      &b$dimnames$Terms!="ndo"
                                      &b$dimnames$Terms!="nas"
                                      &b$dimnames$Terms!="vao"
                                      &b$dimnames$Terms!="dao"
                                      &b$dimnames$Terms!="fao")])
                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " nao ", x = dados$texto)}
                }else {print("Palavra Inexistente")} 
###

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'megas')<=1
                                      &b$dimnames$Terms!="menas")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " mega ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 


vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'internet')<=2
                       &b$dimnames$Terms!="interna"
                       &b$dimnames$Terms!="internas"
                       &b$dimnames$Terms!="internos"
                       &b$dimnames$Terms!="interno")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " internet ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cair')<=1
                       &b$dimnames$Terms!="sair"
                       &b$dimnames$Terms!="cairo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cair ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'caindo')<=1
                       &b$dimnames$Terms!="saindo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cair ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'queda')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " queda ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 

               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'velocidade')<=2
                       &b$dimnames$Terms!="veracidade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " velocidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'lenta')<=1
                       &b$dimnames$Terms!="tenta"
                       &b$dimnames$Terms!="venta")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " lenta ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'devagar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " devagar ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'vezes')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " vezes ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'problema')<=3 
                       &b$dimnames$Terms!="programa"
                       &b$dimnames$Terms!="proxima"
                       &b$dimnames$Terms!="propoem")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " problema ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'sinal')<=1
                       &b$dimnames$Terms!="final")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " sinal ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'melhorar')<=2
                       &b$dimnames$Terms!="melhoraram"
                       &b$dimnames$Terms!="melhorou")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " melhorar ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'funciona')<=3
                       &b$dimnames$Terms!="funcionario"
                       &b$dimnames$Terms!="funcionaria"
                       &b$dimnames$Terms!="aciona"
                       &b$dimnames$Terms!="funcao"
                       &b$dimnames$Terms!="funcoes"
                       &b$dimnames$Terms!="funco")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " funciona ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'conexao')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " conexao ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'conectar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " conectar ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(b$dimnames$Terms=="casas")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " casa ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'modem')<=1
                       &b$dimnames$Terms!="podem"
                       &b$dimnames$Terms!="medem")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " modem ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'oscilar')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " oscila ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'instavel')<=2
                       &b$dimnames$Terms!="estavel"
                       &b$dimnames$Terms!="inviavel"
                       &b$dimnames$Terms!="instalei")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " instavel ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
                              
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'estabilidade')<=2
                                      &b$dimnames$Terms!="instabilidade"
                                      &b$dimnames$Terms!="postabilidade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " estabilidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'instabilidade')<=2 
                       &b$dimnames$Terms!="estabilidade" )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " instabilidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'inviabilidade')<=1
                      &b$dimnames$Terms!="estabilidade"
                      &b$dimnames$Terms!="instabilidade"
                      &b$dimnames$Terms!="viabilidade")])   
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " inviabilidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(b$dimnames$Terms=="pega")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " pegar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'contratei')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " contratei ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'contratar')<=1
                       &b$dimnames$Terms!="contatar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " contratar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'contrato')<=1
                       &b$dimnames$Terms!="contatar"
                       &b$dimnames$Terms!="contato")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " contratar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'contratado')<=1
              &b$dimnames$Terms!="contatado")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " contratar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'plano')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " plano ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'precisa')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " precisar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'conseguir')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " conseguir ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'consiga')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " conseguir ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chega')<=1
                       &b$dimnames$Terms!="cheia"
                       &b$dimnames$Terms!="cheia")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chegar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'sempre')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " sempre ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'fraca')<=1
                       &b$dimnames$Terms!="graca"
                       &b$dimnames$Terms!="praca")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " fraco ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'download')<=3
                       &b$dimnames$Terms!="downgrade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " download ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'upload')<=2
                       &b$dimnames$Terms!="load")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " upload ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'troca')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " trocar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'trocou')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " trocar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'apareceu')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " aparecer ", x = dados$texto)}
               }else {print("Palavra Inexistente")}              

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'aumentar')<=2
                       &b$dimnames$Terms!="comentar"
                       &b$dimnames$Terms!="argumentar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " aumentar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'aparelho')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " aparelho ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'computador')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " computador ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'navegar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " navegar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'acessar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " acessar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'acessando')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " acessar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'entregar')<=2
                       &b$dimnames$Terms!="entrar"
                       &b$dimnames$Terms!="enxergar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " entrega ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'picos')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " pico ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'instalar')<=3
                       &b$dimnames$Terms!="instavel"
                       &b$dimnames$Terms!="instante"
                       &b$dimnames$Terms!="igualar"
                       &b$dimnames$Terms!="reinstalar"
                       &b$dimnames$Terms!="inspirar"
                       &b$dimnames$Terms!="instruir"
                       &b$dimnames$Terms!="instalador")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " instalar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'reiniciar')<=2
                       &b$dimnames$Terms!="regoniciar"
                       &b$dimnames$Terms!="iniciar"
                       &b$dimnames$Terms!="regoniciar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " reiniciar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'atrasada')<=2
                       &b$dimnames$Terms!="travada"
                       &b$dimnames$Terms!="tracada"
                       &b$dimnames$Terms!="tratada"
                       &b$dimnames$Terms!="atrelada")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " atrasar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'travar')<=1
                       &b$dimnames$Terms!="tratar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " trava ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'qualidade')<=2
                       &b$dimnames$Terms!="quantidade"
                       &b$dimnames$Terms!="realidade"
                       &b$dimnames$Terms!="quandidade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " qualidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'quantidade')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " quantidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'quando')<=1
                       &b$dimnames$Terms!="quanto")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " quando ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'assinatuira')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " assinatura ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'rede')<=1
                       &b$dimnames$Terms!="mede"
                       &b$dimnames$Terms!="pede"
                       &b$dimnames$Terms!="reze"
                       &b$dimnames$Terms!="cede")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " rede ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'interromper')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " interromper ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'assinar')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " assinar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'entender')<=1
                                      &b$dimnames$Terms!="estender")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " entender ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                  
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'entendo')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " entender ", x = dados$texto)}
               }else {print("Palavra Inexistente")}    

               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'atende')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " atender ", x = dados$texto)}
               }else {print("Palavra Inexistente")}              
                                            
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'atendimento')<=3
                       &b$dimnames$Terms!="agendamento"
                       &b$dimnames$Terms!="entendimento"
                       &b$dimnames$Terms!="vencimento"
                       &b$dimnames$Terms!="andamento"
                       &b$dimnames$Terms!="atendente")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " atendimento ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'atendente')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " atendente ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'demora')<=2
                       &b$dimnames$Terms!="embora"
                       &b$dimnames$Terms!="memoria"
                       &b$dimnames$Terms!="dera"
                       &b$dimnames$Terms!="lembra"
                       &b$dimnames$Terms!="demostra"
                       &b$dimnames$Terms!="mora")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " demorar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'falar')<=1
                       &b$dimnames$Terms!="falhar"
                       &b$dimnames$Terms!="falta"
                       &b$dimnames$Terms!="faltar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " falar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ligar')<=1
                       &b$dimnames$Terms!="lugar"
                       &b$dimnames$Terms!="lidar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ligar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ligacao')<=1
                       &b$dimnames$Terms!="ligacai")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ligar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'central')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " central ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'eletronico')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " eletronico ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'informar')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " informar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'informacao')<=2
                       &b$dimnames$Terms!="formacao")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " informar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
                 ###
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'informacoes')<=3
                       &b$dimnames$Terms!="confirmacoes")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " informar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}


vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cobrar')<=1
                       &b$dimnames$Terms!="cobrir")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cobrar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cobranca')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cobrar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cobrado')<=1
               &b$dimnames$Terms!="sobrado")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cobrar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cobraram')<=1
              &b$dimnames$Terms!="sobraram")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cobrar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                 
               
 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'solucionar')<=2
                       &b$dimnames$Terms!="selecionar"
                       &b$dimnames$Terms!="soluciona")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " solucionar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'propaganda')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " propaganda ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'callcenter')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " callcenter ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'explicam')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " explicar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'menu')<=1
                       &b$dimnames$Terms!="meu")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " menu ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'preco')<=1
                       &b$dimnames$Terms!="perco"
                       &b$dimnames$Terms!="peco"
                       &b$dimnames$Terms!="preso"
                       &b$dimnames$Terms!="preto")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " preco ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'valores')<=1
                       &b$dimnames$Terms!="valorem"
                       &b$dimnames$Terms!="valore")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " valor ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'valor')<=1
                       &b$dimnames$Terms!="valore")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " valor ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cara')<=0)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " caro ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'caros')<=0)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " caro ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'servico')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " servico ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'conta')<=0
                                  |b$dimnames$Terms=='comnta'
                                  |b$dimnames$Terms=='aconta'
                                  |b$dimnames$Terms=='comta'
                                  |b$dimnames$Terms=='conbta'
                                  |b$dimnames$Terms=='contaa'
                                  |b$dimnames$Terms=='contae')])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " conta ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'pagar')<=1
                       &b$dimnames$Terms!="pegar"
                       &b$dimnames$Terms!="parar"
                       &b$dimnames$Terms!="apagar"
                       &b$dimnames$Terms!="paar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " pagar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'faturas')<=1
                       &b$dimnames$Terms!="faturar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " fatura ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'meses')<=1
                       &b$dimnames$Terms!="veses"
                       &b$dimnames$Terms!="vezes")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " mes ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'reais')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " reais ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'vencer')<=1
                       &b$dimnames$Terms!="vender"
                       &b$dimnames$Terms!="vende")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " vencer ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'taxa')<=1
                       |b$dimnames$Terms=="taxado"
                       |b$dimnames$Terms=="taxade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " taxa ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'endereco')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " endereco ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'imprimir')<=1
                       |b$dimnames$Terms=="imprimisse")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " imprimir ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'correio')<=1
                       &b$dimnames$Terms!="correto")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " correio ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'greve')<=1
                      &b$dimnames$Terms!="preve"
                      &b$dimnames$Terms!="breve")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " greve ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'debito')<=1
                       &b$dimnames$Terms!="feito")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " debito ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'indevida')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " indevida ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'divergente')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " divergencia ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'pagamento')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " pagar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'disponibilizasse')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " disponibilizar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'entrar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " entrar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'procurar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " procurar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'promocao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " promocao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'promocoes')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " promocao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'protocolo')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " protocolo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'atualizar')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " atualizar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'clareza')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " clareza ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'relacao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " relacao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'oferecer')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " oferecer ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'condicao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " condicao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'regiao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " regiao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'insatisfeito')<=2
                       &b$dimnames$Terms!="satisfeito"
                       &b$dimnames$Terms!="satisfeita")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " insatisfacao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'satisfeito')<=2
                       &b$dimnames$Terms!="insatisfeito"
                       &b$dimnames$Terms!="insatisfeita")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " satisfacao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'falaram')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " falar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'fazem')<=1
                       &b$dimnames$Terms!="falem")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " fazer ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'tecnico')<=2
                       &b$dimnames$Terms!="mecanico")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " tecnico ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'suporte')<=1
                       &b$dimnames$Terms!="suporta")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " suporte ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
            

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'tentei')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " tentar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'tentam')<=1
                       &b$dimnames$Terms!="atentam"
                       &b$dimnames$Terms!="testam"
                       &b$dimnames$Terms!="tenham")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " tentar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'tenta')<=0)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " tentar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'reclamar')<=2
                       &b$dimnames$Terms!="relatar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " reclamar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'disser')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " dizer ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'existe')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " existir ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'liguei')<=1
                                      &b$dimnames$Terms!="liguem")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " liguei ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'quero')<=1
                       &b$dimnames$Terms!="quebro")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " querer ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'entra')<=1
                       &b$dimnames$Terms!="extra"
                       &b$dimnames$Terms!="estra")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " entrar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'comunicao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " comunicacao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'alteracao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " alteracao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ninguem')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ninguem ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'diferente')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " diferenca ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'diferenca')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " diferenca ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'manutencao')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " manutencao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
              
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'mudanca')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " mudanca ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'deveria')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " deveria ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'pacotes')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " pacote ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'horas')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " hora ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'outras')<=1
                       &b$dimnames$Terms!="outros")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " outra ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'atraso')<=1
                       &b$dimnames$Terms!="atras")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " atraso ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'motivos')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " motivo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'competencia')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " competencia ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'resolve')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " resolver ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cumprir')<=2
                                      &b$dimnames$Terms!="suprir"
                                      &b$dimnames$Terms!="cubrir")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cumprir ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ruim')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ruim ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'tempo')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " tempo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'operadora')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " operadora ", x = dados$texto)}
               }else {print("Palavra Inexistente")}              
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'fidelidade')<=2
               &b$dimnames$Terms!="finalidade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " fidelidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'minutos')<=1 )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " minutos ", x = dados$texto)}
               }else {print("Palavra Inexistente")}              
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'concorrencia')<=1 )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " concorrencia ", x = dados$texto)}
               }else {print("Palavra Inexistente")}    
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'recarga')<=1 )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " recarga ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'torre')<=1
                &b$dimnames$Terms!="morre")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " torre ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'antena')<=1 )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " antena ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'giga')<=1 
               &b$dimnames$Terms!="liga"
               &b$dimnames$Terms!="diga"
               &b$dimnames$Terms!="gira")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " giga ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'dados')<=1 
                &b$dimnames$Terms!="danos"
                &b$dimnames$Terms!="lados"
                &b$dimnames$Terms!="dadas")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " dados ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'acaba')<=1 
                &b$dimnames$Terms!="acara"
                &b$dimnames$Terms!="acada")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " acaba ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'movel')<=1 
               &b$dimnames$Terms!="imovel"
               &b$dimnames$Terms!="movem"
               &b$dimnames$Terms!="move"
               &b$dimnames$Terms!="moves")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " moveis ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'moveis')<=1 )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " moveis ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                   
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'falando')<=1
               &b$dimnames$Terms!="falhando"
               &b$dimnames$Terms!="faltando" )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " falando ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'mensagem')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " mensagem ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                                     
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chip')<=1
                &b$dimnames$Terms!="chia" )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chip ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chiado')<=1
              &b$dimnames$Terms!="criado" )])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chiado ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'sms')<=1
               &b$dimnames$Terms!="sps"
               &b$dimnames$Terms!="sms"
               &b$dimnames$Terms!="kms"
               &b$dimnames$Terms!="ums"
              &b$dimnames$Terms!="sos" )])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " sms ", x = dados$texto)}
              }else {print("Palavra Inexistente")}               
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'via')<=0)])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " via ", x = dados$texto)}
              }else {print("Palavra Inexistente")} 
              
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'adicional')<=1)])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " adicional ", x = dados$texto)}
              }else {print("Palavra Inexistente")}
              
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ilimitada')<=1
              &b$dimnames$Terms!="limitada" )])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ilimitada ", x = dados$texto)}
              }else {print("Palavra Inexistente")}  
              
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'familia')<=1)])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " familiar ", x = dados$texto)}
              }else {print("Palavra Inexistente")}
              
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'procon')<=1)])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " procon ", x = dados$texto)}
              }else {print("Palavra Inexistente")}  
              
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'anatel')<=1)])
              vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
              if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " anatel ", x = dados$texto)}
              }else {print("Palavra Inexistente")}                 
              
                           
#Tramento Personagens MOTOR:

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'aplicativo')<=2
                       &b$dimnames$Terms!="explicativo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " aplicativo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'boleto')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " boleto ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               

               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'defeito')<=1
                       &b$dimnames$Terms!="desfeito")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " defeito ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'email')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " email ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'fatura')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " fatura ", x = dados$texto)}
               }else {print("Palavra Inexistente")}

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'papel')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
              for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " papel ", x = dados$texto)}
               }else {print("Palavra Inexistente")}   
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'detalhada')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " detalhada ", x = dados$texto)}
               }else {print("Palavra Inexistente")}  

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'fibra')<=1
                       &b$dimnames$Terms!="fimra"
                       &b$dimnames$Terms!="vibra")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " fibra ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'funcionario')<=1
                       &b$dimnames$Terms!="funcionarso"
                       &b$dimnames$Terms!="funcionaria")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " funcionario ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'gravacao')<=2
                       &b$dimnames$Terms!="ralacao")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " gravacao ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'linha')<=1
                       &b$dimnames$Terms!="vinha"
                       &b$dimnames$Terms!="minha"
                       &b$dimnames$Terms!="tinha")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " linha ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'produto')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " produto ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'reparo')<=1
                       &b$dimnames$Terms!="preparo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " reparo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'roteador')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " roteador ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'servico')<=2
                       &b$dimnames$Terms!="serio"
                       &b$dimnames$Terms!="servidor"
                       &b$dimnames$Terms!="servia")]) 
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " servico ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'site')<=1
                       &b$dimnames$Terms!="sete"
                       &b$dimnames$Terms!="cite")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " site ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'speedy')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " speedy ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'telefone')<=2
                       &b$dimnames$Terms!="telefonia"
                       &b$dimnames$Terms!="telecon"
                       &b$dimnames$Terms!="telefonua"
                       &b$dimnames$Terms!="telefonema"
                       &b$dimnames$Terms!="telemonte"
                       &b$dimnames$Terms!="telefonese")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " telefone ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'visita')<=1
                       &b$dimnames$Terms!="vista")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " visitas ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'wifi')<=2
                       &b$dimnames$Terms!="vivi"
                       &b$dimnames$Terms!="piti"
                       &b$dimnames$Terms!="lifa")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " wifi ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'credito')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " creditos ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'validade')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " validade ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'rapido')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " rapido ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'bonus')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " bonus ", x = dados$texto)}
               }else {print("Palavra Inexistente")}   
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'musiquinha')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " musiquinha ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'franquia')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " franquia ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'recarrego')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " recarregar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}              

               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chove')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chove ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chovendo')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chove ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                 

 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'facebook')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " facebook ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'youtube')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " youtube ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'respeito')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " respeito ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'lesado')<=1
                &b$dimnames$Terms!="levado"
                &b$dimnames$Terms!="pesado")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " lesado ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ddd')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ddd ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                       

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'turbo')<=1
                              &b$dimnames$Terms!="turno"
                              &b$dimnames$Terms!="turvo"
                              &b$dimnames$Terms!="tubo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " turbo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'consumidor')<=1
                               &b$dimnames$Terms!="consumidos"
                               &b$dimnames$Terms!="consumido"
                               &b$dimnames$Terms!="consumida")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " consumidor ", x = dados$texto)}
               }else {print("Palavra Inexistente")}   
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'bloqueia')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " bloquear ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'burocaracia')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " burocracia ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'telemarketing')<=3)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " telemarketing ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               

               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'portabilidade')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " portabilidade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'interurbano')<=2)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " interurbano ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               
               

 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'liberados')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " liberados ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                             
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'variedade')<=1
                                &b$dimnames$Terms!="validade"
                                &b$dimnames$Terms!="raridade"
                                &b$dimnames$Terms!="valedade")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " variedade ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                              
 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'opcoes')<=1
                                                     &b$dimnames$Terms!="pocoes"
                                                     &b$dimnames$Terms!="pcoes")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " opcoes ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                             
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'online')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " online ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                             
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'programada')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " programada ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                    
 
               
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'saldo')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " saldo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                    
               
            
                             
               
                              
#Tramento Atributos MOTOR:

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'receber')<=1
                       &b$dimnames$Terms!="recibo"
                       &b$dimnames$Terms!="receio"
                       &b$dimnames$Terms!="precebo"
                       &b$dimnames$Terms!="receba")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " receber ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'retornar')<=2
                       &b$dimnames$Terms!="tornar"
                       &b$dimnames$Terms!="retornei"
                       &b$dimnames$Terms!="retirar"
                       &b$dimnames$Terms!="estornar"
                       &b$dimnames$Terms!="reformar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " retornar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'vem')<=0
                       |b$dimnames$Terms=='ven'
                       |b$dimnames$Terms=='veem'
                       |b$dimnames$Terms=='veen')])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " vem ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chama')<=1
                                      &b$dimnames$Terms!="chata")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chamar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}                                      


vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'chamada')<=1
                       &b$dimnames$Terms!="chamava")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " chamada ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'consegue')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " consegue ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'consigo')<=1
                       &b$dimnames$Terms!="contigo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " consigo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'desconheco')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " desconheco ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'enviar')<=1
                       &b$dimnames$Terms!="enfiar")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " enviar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'enviou')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " enviar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'errado')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " errado ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'eficiente')<=1
                       &b$dimnames$Terms!="deficiente")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " eficiente ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ineficiente')<=1
                       &b$dimnames$Terms!="eficiente")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ineficiente ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'lento')<=1
                       &b$dimnames$Terms!="lendo"
                       &b$dimnames$Terms!="tento"
                       &b$dimnames$Terms!="cento")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " lento ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'lerdo')<=1
                       &b$dimnames$Terms!="lendo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " lerdo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'prazo')<=1)])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " prazo ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'segunda')<=1
                       &b$dimnames$Terms!="seguida"
                       &b$dimnames$Terms!="segundo")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " segunda ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'transfere')<=2
                       &b$dimnames$Terms!="transferi")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " transferir ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'esperar')<=1
                       &b$dimnames$Terms!="esperem")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " esperar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'esperando')<=1
                       &b$dimnames$Terms!="esperado")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " esperar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'excedente')<=1
                       &b$dimnames$Terms!="excelente")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " excedente ", x = dados$texto)}
               }else {print("Palavra Inexistente")}
               
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'banda')<=1
                                      &b$dimnames$Terms!="manda"
                                      &b$dimnames$Terms!="anda"
                                      &b$dimnames$Terms!="bando"
                                      &b$dimnames$Terms=="bandar"
                                      &b$dimnames$Terms=="bandO")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " banda ", x = dados$texto)}
                 }else {print("Palavra Inexistente")} 
                
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'larga')<=1
                                      &b$dimnames$Terms!="largam"
                                      &b$dimnames$Terms!="carga")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " larga ", x = dados$texto)}
               }else {print("Palavra Inexistente")}               
               
   
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ligacao')<=1
                                      &b$dimnames$Terms!="ligacai")])
               vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ligacao ", x = dados$texto)}
               }else {print("Palavra Inexistente")} 
               
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ajuda')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
               if (vec[1] != "\\b\\s?\\s?\\b"){ 
               for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ajudar ", x = dados$texto)}
               }else {print("Palavra Inexistente")}    

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cobertura')<=2)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cobertura ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cliente')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cliente ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'celular')<=2)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " celular ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}   
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ligacoes')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ligacoes ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'whatsapp')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " whatsapp ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}  
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'loja')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " loja ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                      
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'roaming')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " roaming ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                
                 
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'compartilhar')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " compartilhar ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'dados')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " dados ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                 
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'dependente')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " dependente ", x = dados$texto)}
                 }else {print("Palavra Inexistente")} 
                 
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'gratis')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " gratis ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                  
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'internacional')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " internacional ", x = dados$texto)}
                 }else {print("Palavra Inexistente")} 
        
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'empresa')<=1
                                                       &b$dimnames$Terms!="empresta"
                                                       &b$dimnames$Terms!="emprega")])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " empresa ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}       
 
                 
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'contato')<=1
                                                       &b$dimnames$Terms!="contrato")])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " contato ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}   
                 
                 
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'telefonia')<=1
                                                       &b$dimnames$Terms!="telefonica")])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " telefonia ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                     
                 
                 
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cidade')<=1
                                                       &b$dimnames$Terms!="idade")])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cidade ", x = dados$texto)}
                 }else {print("Palavra Inexistente")}                       
                 
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'gerente')<=1)])
                 vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                 if (vec[1] != "\\b\\s?\\s?\\b"){ 
                 for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " gerente ", x = dados$texto)}
                 }else {print("Palavra Inexistente")} 
                 
                 
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'consultor')<=1
                                        &b$dimnames$Terms!="consultar")]) 
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " consultor ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                 
                 
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'gestor')<=1
                                         &b$dimnames$Terms!="gesto")]) 
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " gestor ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                 
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'sistema')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " sistema ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                 
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'desconto')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " desconto ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}               
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'direto')<=1
                                &b$dimnames$Terms!="direito"
                                &b$dimnames$Terms!="diretor")]) 
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " direto ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                 
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'representante')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " representante ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'empresarial')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " empresarial ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                 

                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'relacionamento')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " relacionamento ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                         
                                
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'contestacao')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " contestacao ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                        
                                
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'corporativo')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " corporativo ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                        
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'negociacao')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " negociacao ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                       

                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'proposta')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " proposta ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                       
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'navegacao')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " navegacao ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                        
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cnpj')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cnpj ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                                                      

                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'terceirizada')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " terceirizada ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                                                      
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'prometida')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " prometida ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                                                      
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'rural')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " rural ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                             
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'juridico')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " juridico ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                     
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'municipio')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " municipio ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                      
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'judicial')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " judicial ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}          
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'qualificacao')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " qualificacao ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}          

vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ddi')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ddi ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                         
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'combinado')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " combinado ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'disponibilizacao')<=1)])                             
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " disponibilizacao ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'compra')<=1 
                                  &b$dimnames$Terms!="cumpra"
                                  &b$dimnames$Terms!="compara")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " compra ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                                 
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'area')<=1
                                  &b$dimnames$Terms!="arena"
                                  &b$dimnames$Terms!="areal")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " area ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}  
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'canal')<=1
                                &b$dimnames$Terms!="tarefa")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " canal ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                      

                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'tarifa')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " tarifa ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                               
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'voz')<=1
                                 &b$dimnames$Terms!="vou"
                                &b$dimnames$Terms!="vez")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " voz ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                               
                                
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'app')<=1
                                &b$dimnames$Terms!="wpp"
                                &b$dimnames$Terms!="ape")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " app ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                    
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'disponibilidade')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " disponibilidade ", x = dados$texto)}
                                }else {print("Palavra Inexistente")} 
                                
 vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'cortes')<=1
                                &b$dimnames$Terms!="cortez"
                                &b$dimnames$Terms!="cores")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " cortes ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'interrupcoes')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " interrupcoes ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}          
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'ruido')<=1
                                 &b$dimnames$Terms!="muido"
                                 &b$dimnames$Terms!="cuido")])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " ruido ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                                     
                                
vec<-as.vector(b$dimnames$Terms[which(stringdist(b$dimnames$Terms,'interferencia')<=1)])
                                vec<-paste("\\b\\s?",vec,"\\s?\\b",sep="")
                                if (vec[1] != "\\b\\s?\\s?\\b"){ 
                                for(i in 1:length(vec)){dados$texto<-gsub(pattern = vec[i], replace = " interferencia ", x = dados$texto)}
                                }else {print("Palavra Inexistente")}                        
                                
                                
                                                                                                  
#########################################################################################################################################################
#               TRATANDO PALAVRAS COMPOSTAS   
#########################################################################################################################################################

                 dados$texto<-gsub(pattern = "atendimento digital", replace = "atendimentodigital", x = dados$texto)
                 dados$texto<-gsub(pattern = "suporte tecnico", replace = "suportetecnico", x = dados$texto)
                 dados$texto<-gsub(pattern = "cara da vivo", replace = "caradavivo", x = dados$texto)
                 dados$texto<-gsub(pattern = "banda larga", replace = "bandalarga", x = dados$texto)
                 dados$texto<-gsub(pattern = "vivo turbo", replace = "vivoturbo", x = dados$texto)
                 dados$texto<-gsub(pattern = "compartilhar internet", replace = "compartilharinternet", x = dados$texto)
                 dados$texto<-gsub(pattern = "acaba rapido", replace = "acabarapido", x = dados$texto)
                 dados$texto<-gsub(pattern = "opcoes de recarga", replace = "opcoesrecarga", x = dados$texto)                 
                 dados$texto<-gsub(pattern = "variedade de valor", replace = "variedadevaloresrecarga", x = dados$texto)                 
                 dados$texto<-gsub(pattern = "pontos de venda", replace = "pontosdevenda", x = dados$texto)                 
                 dados$texto<-gsub(pattern = "meu vivo", replace = "meuvivo", x = dados$texto)                 
                 dados$texto<-gsub(pattern = "consulta de saldo", replace = "consultadesaldo", x = dados$texto)                 
                 dados$texto<-gsub(pattern = "enviar conta", replace = "enviarconta", x = dados$texto)                
                 dados$texto<-gsub(pattern = "cobrar indevida", replace = "cobrarindevida", x = dados$texto)  
                 dados$texto<-gsub(pattern = "valor indevida", replace = "valorindevida", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "fatura detalhada", replace = "faturadetalhada", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "segunda via", replace = "segundavia", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "melhorar plano", replace = "melhorarplano", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "chamadas ilimitada", replace = "chamadasilimitada", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "ligacoes ilimitada", replace = "ligacoesilimitada", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "roaming internacional", replace = "roaminginternacional", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "internet ilimitada", replace = "internetilimitada", x = dados$texto)                   
                 dados$texto<-gsub(pattern = "valor combinado", replace = "valorcombinado", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "area de cobertura", replace = "areadecobertura", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "disponibilidade de aparelho", replace = "disponibilidadedeaparelho", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "troca de aparelho", replace = "trocadeaparelho", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "entrega dos aparelho", replace = "entregadosaparelho", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "cobertura do sinal", replace = "coberturadosinal", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "estabilidade do sinal", replace = "estabilidadedosinal", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "3g", replace = "rede3g", x = dados$texto)                    
                 dados$texto<-gsub(pattern = "4g", replace = "rede4g", x = dados$texto)           
                 
                 
                 
                 
                 
                 
            
                 
                
#########################################################################################################################################################
#               REMOVENDO STOPWORDS  
#########################################################################################################################################################

dados<-as.data.frame(dados)
# Remover Stopwords 
review_corpus = Corpus(VectorSource(dados$texto))
review_corpus <- tm_map(review_corpus, removeWords, stopwords("portuguese"))
review_corpus = tm_map(review_corpus, removeWords, c( "pra","vezes","fica","ser","ter","acho",
                                                      "fazer" , "pois" , "voce", "outra", "bem","porque",
                                                      "vai","vem","mim","antes","ate","vez","sao","faz",
                                                      "quanto","uns","fiz","pido","dar","dois","duas",
                                                      "ncia","sim","qdo","rea","oes","faÃ","cil","crÃ",
                                                      "nico","ncia","vir","ssima","osÃ","sica",
                                                      "que","nao","para","com","mais","tem","uma","muito","quando",
                                                      "por" , "eles","gente","esta", "tenho","ter" , "vezes" ,"pra" ,      
                                                      "voce","ser","isso", "fica" ,"mas","fazer","sem", "ate",
                                                      "minha","aqui", "como","acho","foi", "outra", "meu",
                                                      "sao","outro", "ela","era", "estou","coisa","tinha",
                                                      "pessoa","dia","depois","dias","nos","dos","tudo",
                                                      "sempre","pelo", "vem","mesmo","temos","esse", "tive",
                                                      "ele", "querer","pouco","pois","nem","nunca",
                                                      "hoje","parte", "vez","entao","essa","vai",
                                                      "ninguem","toda","bem","tambem","das","seria","nada",
                                                      "poderia", "seja","todo","agora","sei",
                                                      "trocar","estao","maior","muita","todos","estava",
                                                      "veio",  "pessoas", "pela","dar","menos","ainda",
                                                      "faz", "novo", "alto", "quem"))
    

review_corpus = tm_map(review_corpus, stripWhitespace)
c=TermDocumentMatrix(review_corpus)




