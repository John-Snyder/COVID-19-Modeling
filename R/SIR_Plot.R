

SIR.model <- function(t, b, g){
  require(deSolve)
  init <- c(S=1-1e-6,I=1e-6,R=0)
  parameters <- c(bet=b,gamm=g)
  time <- seq(0,t,by=t/(2*length(1:t)))
  
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS <- -bet*S*I
      dI <- bet*S*I-gamm*I
      dR <- gamm*I
      return(list(c(dS,dI,dR)))})}
    
    out<-ode(y=init,times=time,eqn,parms=parameters)
    out.df<-as.data.frame(out)
    
    require(ggplot2)
    mytheme4 <- theme_bw() +
      theme(text=element_text(colour="black")) +
      theme(panel.grid = element_line(colour = "white")) +
      theme(panel.background = element_rect(fill = "#B2B2B2"))
    theme_set(mytheme4)
    
    title <- bquote("SIR Model: Basic")
    subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2])))
    
    res<-ggplot(out.df,aes(x=time))+
      ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+
      geom_line(aes(y=S,colour="Susceptible"))+
      geom_line(aes(y=I,colour="Infected"))+
      geom_line(aes(y=R,colour="Recovered"))+
      ylab(label="Proportion")+
      xlab(label="Time (days)")+
      theme(legend.justification=c(1,0), legend.position=c(1,0.5))+
      theme(legend.title=element_text(size=12,face="bold"),
            legend.background = element_rect(fill='#FFFFFF',
                                             size=0.5,linetype="solid"),
            legend.text=element_text(size=10),
            legend.key=element_rect(colour="#FFFFFF",
                                    fill='#C2C2C2',
                                    size=0.25,
                                    linetype="solid"))+
      scale_colour_manual("Compartments",
                          breaks=c("Susceptible","Infected","Recovered"),
                          values=c("blue","red","darkgreen"))
    
    print(res)
    ggsave(plot=res,
           filename=paste0("SIRplot_","time",t,"beta",b,"gamma",g,".png"),
           width=8,height=6,dpi=180)
    getwd()}

#
SIR.model(100,0.4138817,0.1290300)


