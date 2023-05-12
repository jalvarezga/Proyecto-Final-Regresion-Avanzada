###
### 
datosCrudos=read.csv("crimenes2022.csv",header=TRUE)

datosCrudos$OFFENSE_DESCRIPTION=as.factor(datosCrudos$OFFENSE_DESCRIPTION)

datosCrudos <- datosCrudos %>%
                
