#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  observeEvent(input$click,{file.show("C:/Users/Caroline/Documents/M2/SVM/projet/notice.html")})
  data=read.csv("C:/Users/Caroline/Documents/M2/SVM/projet/creditcard.csv",header=T,sep=",")
  attach(data)
  train=(Time<150000)
  data.150000=data[!train,]
  Class.150000=Class[!train]
  glm.fit=glm(Class~V4+V10+V14,data=data,family=binomial,subset=train)
  glm.probs=predict(glm.fit,type='response',data.150000)
  glm.pred=rep(0,45614)
  glm.pred[glm.probs>.5]=1
 
  library(MASS)
  lda.fit=lda(Class~V4+V10+V14,data=data.150000,subset = train)
  lda.pred=predict(lda.fit, data[!train,])
  qda.fit=qda(Class~V4+V10+V14,data=data.150000,subset=train)
  qda.pred=predict(qda.fit,data[!train,],type='vector')
  library(e1071)
  #svm.fit=svm(Class~.,data=data,subset=train,type=NULL,kernel=input$kernel,degree=input$degree,coef0=input$coef0,cost=input$c)
  #svm.pred=predict(svm.fit, data[!train,])
  #output$tablesvm <-renderDataTable({mean(glm.pred==Class.150000)*100})
  output$text1 <- renderText({input$submit
                            'the percent of good prediction is : '
                            })
  output$predsvm <- renderText({ input$submit
                                 ex.svm=isolate(0.5)
                                  ex.svm})
  #mean(svm.pred==Class.150000)*100
  ex.svm=99.9

  
  
  
  
  
  set.seed(10111)
  x = matrix(rnorm(40), 20, 2)
  y = rep(c(-1, 1), c(10, 10))
  x[y == 1,] = x[y == 1,] + 1
  plot(x, col = y + 3, pch = 19)
  dat = data.frame(x, y = as.factor(y))
  svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE) 
  output$plot1 <- renderPlot({
    input$submit1
      if (input$kernel1=='linear'){
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE) 
        plot(svmfit, dat)
        }
    if (input$kernel1=='polynomial'){
      svmfit = svm(y ~ ., data = dat, kernel = "polynomial", degree=input$degree1, coef0=input$coef01, cost = input$c1, scale = FALSE) 
      plot(svmfit, dat)
}
    if (input$kernel1=='radial basis'){
      svmfit = svm(y ~ ., data = dat, kernel = "radial basis", cost = 10, scale = FALSE) 
      plot(svmfit, dat)
    }
    if (input$kernel1=='sigmoid'){
      svmfit = svm(y ~ ., data = dat, kernel = "sigmoid",coef0=input$coef01, cost = input$c1, scale = FALSE) 
      plot(svmfit, dat)
    }
  })
   output$explication1 <- renderText({
    'We can observe that the SVM create a line who separate data in two parts'
  })
  make.grid = function(x, n = 75) {
    grange = apply(x, 2, range)
    x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
    x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
    expand.grid(X1 = x1, X2 = x2)
  }
  xgrid = make.grid(x)
  xgrid[1:10,]
  ygrid = predict(svmfit, xgrid)
  plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
  points(x, col = y + 3, pch = 19)
  points(x[svmfit$index,], pch = 5, cex = 2)
  beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
  beta0 = svmfit$rho
  
 
  
  output$plot2 <- renderPlot({
    input$submit1
    if (input$kernel1=='linear'){
    plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
    points(x, col = y + 3, pch = 19)
    points(x[svmfit$index,], pch = 5, cex = 2)
    abline(beta0 / beta[2], -beta[1] / beta[2])
    abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
    abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)
    }
  })
  
  
  
  
  
  output$dim <- renderDataTable({
    dim(data)
    })
  output$name <- renderDataTable({
    names(data)
    })
  output$sum <- renderDataTable({
    summary(data)
  })
#  output$tablesvm <- renderDataTable({
 #   table(svm.pred$class,Class.150000)
  #})
#  output$predsvm <- renderText({
  #ex.svm=mean(svm.pred$class==Class.150000)*100
  #  ex.svm
  #})
  

  output$table <- renderDataTable({
    if (input$Model=='logistic regression'){table=table(glm.pred,Class.150000)}
    if (input$Model=='linear discriminant analysis'){table=table(lda.pred$class,Class.150000)}
    if (input$Model=='quadratic discriminant analysis'){table=table(qda.pred$class,Class.150000)}
    table})
  output$text <- renderText({'the percent of good prediction is : '})
  output$pred <- renderText({
    if (input$Model=='logistic regression'){
      ex=mean(glm.pred==Class.150000)*100
      decision=ifelse(ex>ex.svm,"Selected model prefered","SVM prefered")
      output$conclusion <- renderText({decision})
      }
    if (input$Model=='linear discriminant analysis'){ex=mean(
      lda.pred$class==Class.150000)*100
      decision=ifelse(ex>ex.svm,"Selected model prefered","SVM prefered")
      output$conclusion <- renderText({decision})
      }
    if (input$Model=='quadratic discriminant analysis'){
      ex=mean(qda.pred$class==Class.150000)*100
      decision=ifelse(ex>ex.svm,"Selected model prefered","SVM prefered")
      output$conclusion <- renderText({decision})
      }
    ex})




    
})
