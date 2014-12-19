shinyServer(function(input, output) {
  
  control<-reactiveValues(autostart=F)
  
  observe({
    if(input$run==0){return()}
    isolate({
      control$autostart<-T
    })
  })
  
  data<-reactive({CDS_Spread<-data.frame(tenor=c(1,2,3,5,7,10),
                                                 CDS=c(input$s1,input$s2,input$s3,input$s4,input$s5,input$s6))
                          data<-data.frame(Maturity=seq(0,10,by=0.5))
                          
                          rf=input$rf #risk free
                          R=input$R   #Recovery Rate
                          alpha=input$alpha #A parameter
                          
                          #Define Function to solve equation (method 2)
                          bisection<-function(Fun,a,b,c){
                            for(i in 1:10000)
                            {
                              if(abs(b-a)>=c)
                              {
                                mid=(a+b)/2;
                                if(Fun(a)*Fun(mid)<=0){b=mid}else{a=mid}
                              }else{
                                break;
                              }
                            }
                            return(b)
                          }
                          
                          #Define Function to get k lags of subject X.
                          L.<-function(x,k){
                            result<-NULL
                            n<-length(x)
                            if(n>k){
                              result<-x[1:(n-k)];
                              result<-c(rep(0,k),result)
                            }else{result<-rep(0,n)}
                            return(result)
                          }
                          
                          data$Factor<-exp(-data$Maturity*rf)#Discount Factor
                          data$delta<-c(0,diff(data$Maturity))#Delta. 0.5 in our case
                          data2<-data #for method2
                          data3<-data #for method3
                          
                          ################Method 1########################
                          l<-which(data$Maturity==1)#location. To get the location of first spread (1 year in our case)
                          
                          CC<-sum(data$Factor[1:l]*data$delta[1:l])#Formula from P6
                          BB<-CC*(1-R)#Formula from P6
                          DD<-sum(data$Factor[1:l]*data$delta[1:l]*(data$Maturity[1:l]-alpha*data$delta[1:l]/2))
                          #Formula from P6
                          
                          data$q<-NA
                          data$q[1]<-0
                          
                          data$Pt<-NA
                          data$Pt[1]<-0
                          
                          q1<-(CDS_Spread$CDS[1]*CC)/(BB+CDS_Spread$CDS[1]*DD)#Formula from P6
                          
                          data$q[l]<-q1
                          data$q[1:l]<-na.approx(data$q[1:l])
                          data$Pt[l]<-q1#The assumption of method 1 is adding q to get P(t).
                          #But here it's the first one, so P(t1)=q1.
                          data$Pt[1:l]<-na.approx(data$Pt[1:l])#interpolation
                          data$Pt_lag1<-L.(data$Pt,1)#lag1 to get P(ti-1)
                          
                          rm(CC);rm(DD);rm(BB);rm(l)
                          
                          time=c(1,2,3,5,7,10)
                          
                          CCf<-function(data,time,i,ll,LL){
                            attach(data)
                            result1<-sum( Factor[1:ll]*
                                            delta[1:ll]*
                                            ( (1-Pt[1:ll])+alpha*(Pt[1:ll]-Pt_lag1[1:ll])/2 ) )
                            result2<-sum( Factor[(ll+1):LL]*
                                            delta[(ll+1):LL]*
                                            (1-Pt[ll]) )
                            detach(data)
                            return(result1+result2)
                          }
                          #Formula from P7, C1+C
                          DDf<-function(data,time,i,ll,LL){
                            attach(data)
                            result<-sum( Factor[(ll+1):LL]*
                                           delta[(ll+1):LL]*
                                           (Maturity[(ll+1):LL]-Maturity[ll]-alpha*delta[(ll+1):LL]/2) )
                            detach(data)
                            return(result)
                          }
                          #Formula from P7
                          AAf<-function(data,time,i,R,ll,LL){
                            attach(data)
                            result<-(1-R)*sum( Factor[1:ll]*
                                                 (Pt[1:ll]-Pt_lag1[1:ll]) )
                            detach(data)
                            return(result)
                          }
                          #Formula from P7
                          BBf<-function(data,time,i,R,ll,LL){
                            attach(data)
                            result<-(1-R)*sum( Factor[(ll+1):LL]*
                                                 delta[(ll+1):LL] )
                            detach(data)
                            return(result)
                          }
                          #Formula from P7
                          
                          for(i in 2:length(time))
                          {
                            LL<-which(data$Maturity==time[i])#Location of the current Spread
                            ll<-which(data$Maturity==time[i-1])#Location of the last valid Spread
                            
                            CC<-CCf(data,time,i,ll,LL)
                            
                            DD<-DDf(data,time,i,ll,LL)
                            
                            AA<-AAf(data,time,i,R,ll,LL)
                            
                            BB<-BBf(data,time,i,R,ll,LL)
                            
                            q2<-(CDS_Spread$CDS[i]*CC-AA)/(BB+CDS_Spread$CDS[i]*DD)*(time[i]-time[i-1])
                            #Formula from P7
                            
                            data$q[LL]<-q2
                            data$Pt[LL]<-q2+data$Pt[ll]#current q2 plus the previous P(t).
                            #Since the default probability here is accumulative.
                            data$Pt[1:LL]<-na.approx(data$Pt[1:LL])#interpolation
                            data$Pt_lag1<-L.(data$Pt,1)#lag1
                          }
                          rm(AA);rm(BB);rm(CC);rm(DD);rm(LL);rm(ll);rm(i)
                          ################Method 1########################
                          data$Pt_lag1<-NULL
                          data$Factor<-NULL
                          data$delta<-NULL
                          return(data)})
  data2<-reactive({
    library(zoo)
    CDS_Spread<-data.frame(tenor=c(1,2,3,5,7,10),
                                                 CDS=c(input$s1,input$s2,input$s3,input$s4,input$s5,input$s6))
                          data<-data.frame(Maturity=seq(0,10,by=0.5))
                          
                          rf=input$rf #risk free
                          R=input$R   #Recovery Rate
                          alpha=input$alpha #A parameter
    
    #Define Function to solve equation (method 2)
    bisection<-function(Fun,a,b,c){
      for(i in 1:10000)
      {
        if(abs(b-a)>=c)
        {
          mid=(a+b)/2;
          if(Fun(a)*Fun(mid)<=0){b=mid}else{a=mid}
        }else{
          break;
        }
      }
      return(b)
    }
    
    #Define Function to get k lags of subject X.
    L.<-function(x,k){
      result<-NULL
      n<-length(x)
      if(n>k){
        result<-x[1:(n-k)];
        result<-c(rep(0,k),result)
      }else{result<-rep(0,n)}
      return(result)
    }
    
    data$Factor<-exp(-data$Maturity*rf)#Discount Factor
    data$delta<-c(0,diff(data$Maturity))#Delta. 0.5 in our case
    data2<-data #for method2
    data3<-data #for method3
    
    ################Method 1########################
    l<-which(data$Maturity==1)#location. To get the location of first spread (1 year in our case)
    
    CC<-sum(data$Factor[1:l]*data$delta[1:l])#Formula from P6
    BB<-CC*(1-R)#Formula from P6
    DD<-sum(data$Factor[1:l]*data$delta[1:l]*(data$Maturity[1:l]-alpha*data$delta[1:l]/2))
    #Formula from P6
    
    data$q<-NA
    data$q[1]<-0
    
    data$Pt<-NA
    data$Pt[1]<-0
    
    q1<-(CDS_Spread$CDS[1]*CC)/(BB+CDS_Spread$CDS[1]*DD)#Formula from P6
    
    data$q[l]<-q1
    data$q[1:l]<-na.approx(data$q[1:l])
    data$Pt[l]<-q1#The assumption of method 1 is adding q to get P(t).
    #But here it's the first one, so P(t1)=q1.
    data$Pt[1:l]<-na.approx(data$Pt[1:l])#interpolation
    data$Pt_lag1<-L.(data$Pt,1)#lag1 to get P(ti-1)
    
    rm(CC);rm(DD);rm(BB);rm(l)
    
    time=c(1,2,3,5,7,10)
    
    CCf<-function(data,time,i,ll,LL){
      attach(data)
      result1<-sum( Factor[1:ll]*
                      delta[1:ll]*
                      ( (1-Pt[1:ll])+alpha*(Pt[1:ll]-Pt_lag1[1:ll])/2 ) )
      result2<-sum( Factor[(ll+1):LL]*
                      delta[(ll+1):LL]*
                      (1-Pt[ll]) )
      detach(data)
      return(result1+result2)
    }
    #Formula from P7, C1+C
    DDf<-function(data,time,i,ll,LL){
      attach(data)
      result<-sum( Factor[(ll+1):LL]*
                     delta[(ll+1):LL]*
                     (Maturity[(ll+1):LL]-Maturity[ll]-alpha*delta[(ll+1):LL]/2) )
      detach(data)
      return(result)
    }
    #Formula from P7
    AAf<-function(data,time,i,R,ll,LL){
      attach(data)
      result<-(1-R)*sum( Factor[1:ll]*
                           (Pt[1:ll]-Pt_lag1[1:ll]) )
      detach(data)
      return(result)
    }
    #Formula from P7
    BBf<-function(data,time,i,R,ll,LL){
      attach(data)
      result<-(1-R)*sum( Factor[(ll+1):LL]*
                           delta[(ll+1):LL] )
      detach(data)
      return(result)
    }
    #Formula from P7
    
    for(i in 2:length(time))
    {
      LL<-which(data$Maturity==time[i])#Location of the current Spread
      ll<-which(data$Maturity==time[i-1])#Location of the last valid Spread
      
      CC<-CCf(data,time,i,ll,LL)
      
      DD<-DDf(data,time,i,ll,LL)
      
      AA<-AAf(data,time,i,R,ll,LL)
      
      BB<-BBf(data,time,i,R,ll,LL)
      
      q2<-(CDS_Spread$CDS[i]*CC-AA)/(BB+CDS_Spread$CDS[i]*DD)*(time[i]-time[i-1])
      #Formula from P7
      
      data$q[LL]<-q2
      data$Pt[LL]<-q2+data$Pt[ll]#current q2 plus the previous P(t).
      #Since the default probability here is accumulative.
      data$Pt[1:LL]<-na.approx(data$Pt[1:LL])#interpolation
      data$Pt_lag1<-L.(data$Pt,1)#lag1
    }
    rm(AA);rm(BB);rm(CC);rm(DD);rm(LL);rm(ll);rm(i)
    ################Method 1########################
    
    ################Method 2########################
    l<-which(data2$Maturity==1)
    data2$l1_Maturity<-L.(data2$Maturity,1)
    
    Vpayt1<-function(h1){
      result<-(1-R)*sum(data2$Factor[1:l]*
                          ( exp(-h1*data2$l1_Maturity[1:l])-exp(-h1*data2$Maturity[1:l]) )
      )
      return(result)
    }
    
    Vpremt1<-function(h1){
      
      toolong<-exp(-h1*data2$l1_Maturity[1:l])-exp(-h1*data2$Maturity[1:l])
      
      result<-CDS_Spread$CDS[1]*sum(data2$Factor[1:l]*data2$delta[1:l]*
                                      ( exp(-h1*data2$Maturity[1:l])+alpha*(toolong)/2 )
      )
      return(result)
    }
    
    equation_to_solve<-function(h1){return(Vpayt1(h1)-Vpremt1(h1))}
    
    h1<-bisection(Fun=equation_to_solve,0,1,0.00000000001)
    
    data2$h<-NA
    data2$h[1]<-0
    
    data2$Pt<-NA
    data2$Pt[1]<-0
    
    data2$h[l]<-h1
    
    data2$h[1:l]<-na.locf(data2$h[1:l],fromLast=T)
    
    data2$Pt[1:l]<-1-exp(-data2$h[1:l]*data2$Maturity[1:l])#The assumption of method 2
    #But here it's the first one, so P(t1)=q1.
    data2$Pt[1:l]<-na.approx(data2$Pt[1:l])#interpolation
    data2$Pt_lag1<-L.(data2$Pt,1)#lag1 to get P(ti-1)
    
    time=c(1,2,3,5,7,10)
    
    Vpay<-function(h2){
      
      term1<-data2$Factor[(ll+1):LL]
      term2<-rep((1-data2$Pt[ll]),length(term1))
      term3<-exp(-h2*(data2$l1_Maturity[(ll+1):LL]-data2$Maturity[ll]))
      term4<-exp(-h2*(data2$Maturity[(ll+1):LL]-data2$Maturity[ll]))
      
      result<-AA+(1-R)*sum(term1*term2*(term3-term4))
      return(result)
    }
    
    Vprem<-function(h2){
      
      s<-CDS_Spread$CDS[i]
      Pt<-data2$Pt[1:LL]
      Factor<-data2$Factor[1:LL]
      Maturity<-data2$Maturity[1:LL]
      l1_Maturity<-data2$l1_Maturity[1:LL]
      delta<-data2$delta[1:LL]
      
      Pt[(ll+1):LL]<-getPtfromH_global(Pt,Maturity,ll,c((ll+1):LL),h2)
      
      l1_Pt<-L.(Pt,1)
      
      return(s*sum(Factor*delta*(1-Pt+alpha*(Pt-l1_Pt)/2)))
    }
    
    equation_to_solve2<-function(h2){return(Vpay(h2)-Vprem(h2))}
    
    getPtfromH<-function(data2,ll,LL,h){
      return(data2$Pt[ll]+(1-data2$Pt[ll])*(1-exp(-h*(data2$Maturity[LL]-data2$Maturity[ll]))))
    }
    
    getPtfromH_global<-function(Pt,Maturity,ll,LL,h){
      return(Pt[ll]+(1-Pt[ll])*(1-exp(-h*(Maturity[LL]-Maturity[ll]))))
    }
    
    #to get 0.07929
    
    for(i in 2:length(time))
    {
      LL<-which(data2$Maturity==time[i])#Location of the current Spread
      ll<-which(data2$Maturity==time[i-1])#Location of the last valid Spread
      #CC<-CCf(data2,time,i,ll,LL)
      AA<-AAf(data2,time,i,R,ll,LL)
      #h2<-bisection(Fun=function(h){1-exp(-h*2)-0.146649},0,1,0.000001)
      h2<-bisection(Fun=equation_to_solve2,0,1,0.00000000001)
      
      data2$h[LL]<-h2
      
      data2$h[1:LL]<-na.locf(data2$h[1:LL],fromLast=T)
      
      data2$Pt[(ll+1):LL]<-getPtfromH(data2,ll,c((ll+1):LL),data2$h[(ll+1):LL])
      
      data2$Pt_lag1<-L.(data2$Pt,1)#lag1
    }
                  data2$Pt_lag1<-NULL
                  data2$Factor<-NULL
                  data2$delta<-NULL
    data2$l1_Maturity<-NULL
                  return(data2)})
  
  output$method1<-renderTable({
    input$run
    isolate({
      if(control$autostart){
return(data())
      }
    })
  },digits = 6)
  
  output$method2<-renderTable({
    input$run
    isolate({
      if(control$autostart){
        return(data2())
      }
    })
  },digits = 6)
  
  output$text1<-renderText({
    input$run
    isolate({
      if(control$autostart){
        "Method 1"
      }
    })
    })
  output$text2<-renderText({
    input$run
    isolate({
      if(control$autostart){
    "Method 2"
      }
    })
    })
  
  output$plot1<-renderPlot({
    input$run
    isolate({
      if(control$autostart){
        attach(data())
        plot(Maturity,Pt,type="o",main="PD Curve (method 1)")
        grid()
        detach(data())
      }
    })
  })
  
  output$plot2<-renderPlot({
    input$run
    isolate({
      if(control$autostart){
        attach(data2())
        plot(Maturity,Pt,type="o",main="PD Curve (method 2)")
        grid()
        detach(data2())
      }
    })
  })
  
})
