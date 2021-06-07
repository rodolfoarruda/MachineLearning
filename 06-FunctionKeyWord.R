#########################################################################################################################################################
#########################################################################################################################################################
############################################################## Function Word Cloud ######################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################

#Função para filtrar banco de dados
bd<-function(produto,prod_unico)
{
  if (prod_unico==0)
         {
          if (produto==0){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0)
            return(dados)   } 
          else if (produto==1){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_BL > 0)
            return(dados)   } 
          else if (produto==2){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_TV > 0)
              return(dados) }
          else if (produto==3){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_Linha > 0)
            return(dados)   } 
          else if (produto==4){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_ATC > 0)
            return(dados) } 
          else if (produto==5){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_FAT > 0)
            return(dados) } 
          else if (produto==6){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_WEB > 0)
            return(dados) } 
          else if (produto==7){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_SupTec > 0)
            return(dados) } 
          else if (produto==8){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0 & banco_filtrado$key_Preco > 0)
            return(dados) } 
          else {return(0)}
           }
 else if (prod_unico==1)
        {
          if (produto==1){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL      > 0 
                                                                            & banco_filtrado$key_TV     == 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC    == 0
                                                                            & banco_filtrado$key_FAT    == 0
                                                                            & banco_filtrado$key_WEB    == 0
                                                                            & banco_filtrado$key_SupTec == 0
                                                                            & banco_filtrado$key_Preco  == 0
                          )
            return(dados)   } 
          else if (produto==2){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                            & banco_filtrado$key_TV      > 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC    == 0
                                                                            & banco_filtrado$key_FAT    == 0
                                                                            & banco_filtrado$key_WEB    == 0
                                                                            & banco_filtrado$key_SupTec == 0
                                                                            & banco_filtrado$key_Preco  == 0                        
                          )
            return(dados) }
          else if (produto==3){
              dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                              & banco_filtrado$key_TV     == 0
                                                                              & banco_filtrado$key_Linha   > 0
                                                                              & banco_filtrado$key_ATC    == 0
                                                                              & banco_filtrado$key_FAT    == 0
                                                                              & banco_filtrado$key_WEB    == 0
                                                                              & banco_filtrado$key_SupTec == 0
                                                                              & banco_filtrado$key_Preco  == 0                            
                          )
            return(dados)   } 
          else if (produto==4){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                            & banco_filtrado$key_TV     == 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC     > 0
                                                                            & banco_filtrado$key_FAT    == 0
                                                                            & banco_filtrado$key_WEB    == 0
                                                                            & banco_filtrado$key_SupTec == 0
                                                                            & banco_filtrado$key_Preco  == 0                            
                         )
            return(dados) } 
          else if (produto==5){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                            & banco_filtrado$key_TV     == 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC    == 0
                                                                            & banco_filtrado$key_FAT     > 0
                                                                            & banco_filtrado$key_WEB    == 0
                                                                            & banco_filtrado$key_SupTec == 0
                                                                            & banco_filtrado$key_Preco  == 0                            
                        )
            return(dados) } 
          else if (produto==6){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                            & banco_filtrado$key_TV     == 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC    == 0
                                                                            & banco_filtrado$key_FAT    == 0
                                                                            & banco_filtrado$key_WEB     > 0
                                                                            & banco_filtrado$key_SupTec == 0
                                                                            & banco_filtrado$key_Preco  == 0  
                          )
            return(dados) } 
          else if (produto==7){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                            & banco_filtrado$key_TV     == 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC    == 0
                                                                            & banco_filtrado$key_FAT    == 0
                                                                            & banco_filtrado$key_WEB    == 0
                                                                            & banco_filtrado$key_SupTec  > 0
                                                                            & banco_filtrado$key_Preco  == 0  
                         )
            return(dados) } 
          else if (produto==8){
            dados<-filter(banco_filtrado, banco_filtrado$SemSugestao == 0   & banco_filtrado$key_BL     == 0
                                                                            & banco_filtrado$key_TV     == 0
                                                                            & banco_filtrado$key_Linha  == 0
                                                                            & banco_filtrado$key_ATC    == 0
                                                                            & banco_filtrado$key_FAT    == 0
                                                                            & banco_filtrado$key_WEB    == 0
                                                                            & banco_filtrado$key_SupTec == 0
                                                                            & banco_filtrado$key_Preco   > 0  
                         )
            return(dados) } 
          else {return(0)}
         }
 else {return(0)} 
  
}





