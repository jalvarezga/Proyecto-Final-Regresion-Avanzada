library(R6)

#--- Usar espejo CRAN del ITAM ---
options(repos="http://cran.itam.mx/")

#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

bugsModel <- R6Class("bugsModel",
  public = list(
    data = NULL,
    inits = NULL,
    parms = NULL,
    model.fileText = NULL,
    n.iter = NULL,
    n.chains = NULL,
    n.burnin = NULL,
    n.thin = NULL,
    modelSim = NULL,
    outModelSim = NULL,
    graphModelSim = NULL,
    summModel = NULL,
    dicModel = NULL,
    #Inicializamos
    initialize = function(data, inits, parms, model.fileText, n.iter=5000, n.chains=2, n.burnin=500, n.thin=1) {
      self$data <- data
      self$inits <- inits
      self$parms <- parms
      self$model.fileText <- model.fileText
      self$n.iter <- n.iter
      self$n.chains <- n.chains
      self$n.burnin <- n.burnin
      self$n.thin <- n.thin
      self$modelSim <- NULL
      self$outModelSim <- NULL
      self$graphModelSim <- NULL
      self$summModel <- NULL
      self$dicModel <- NULL
    },
    #Corremos una cadena
    runBugsModel = function() {
      require(R2OpenBUGS)
      self$modelSim <- bugs(model.file = textConnection(self$model.fileText),
      data = self$data,
      parameters.to.sav = self$parms,
      inits = self$inits,
      n.iter=self$n.iter,
      n.chains = self$n.chains,
      n.burnin = self$n.burnin,
      n.thin = self$n.thin)
      
      self$outModelSim <- self$modelSim$sims.list
      self$graphModelSim <- self$modelSim$sims.array
      self$summModel <- self$modelSim$summary
      self$dicModel <- self$modelSim$DIC
      
    },
    #Graficamos traceplot
    bugsTraceplot = function() {
      return(traceplot(self$modelSim))
    },
    #Vemos el espacio que recorre la cadena
    viewSpace = function() {
      z <- self$outModelSim$beta
      par(mfrow=c(1,1))
      plot(z)
    },
    #Vemos la convergencia de la cadena
    graphConvergence = function(parama = 1) {
      z1 <- self$graphModelSim[,1,parama]
      z2 <- self$graphModelSim[,2,parama]
      par(mfrow=c(3,2))
      plot(z1,type="l",col="grey50")
      lines(z2,col="firebrick2")
      y1<-cumsum(z1)/(1:length(z1))
      y2<-cumsum(z2)/(1:length(z2))
      ymin<-min(y1,y2)
      ymax<-max(y1,y2)
      plot(y1,type="l",col="grey50",ylim=c(ymin,ymax))
      lines(y2,col="firebrick2",ylim=c(ymin,ymax))
      hist(z1,freq=FALSE,col="grey50")
      hist(z2,freq=FALSE,col="firebrick2")
      acf(z1)
      acf(z2)
    },
    #Vemos el summary
    summaryModel = function() {
      print(self$summModel)
    },
    #Checamos el DIC 
    dicModelfunc = function() {
      print(self$dicModel)
    },
    #Creamos la tabla resumen
    resumenfunc = function() {
      out.w<-self$summModel[!grep("deviance", rownames(self$summModel)),c(1,3,7)]
      out.w<-cbind(out.w,apply(out$w,2,prob))
      dimnames(out.w)[[2]][4]<-"prob"
      print(out.w)
    }
  )
) 




