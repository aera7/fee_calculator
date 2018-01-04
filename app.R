library(shiny)
library(ggplot2)
source("functions_doc.R")
#rm(list=ls())

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("FEES CALCULATOR"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("c_meth1", "Calculation method:",
                  c("1. ABS. Outperformance" = "abs_perf",
                    "2. ABS. Outperformance (GAIN)" = "gain",
                    "3. ABS. Outperformance and High Water Mark" = "abs_perf_hwm",
                    "4. ABS. Outperformance and Water Mark" = "abs_perf_wm",
                    "5. Benchmark" = "bm",
                    "6. Benchmark and High Water Mark" = "bm_hwm",
                    "7. Benchmark and Water Mark" = "bm_wm",
                    "8. Hurdle Rate" = "hr",
                    "9. Hurdle Rate and High Water Mark" = "hr_hwm",
                    "10. Hurdle Rate and Water Mark" = "hr_wm"),
                  selected = "abs_perf"
      ),
      selectInput("c_meth2", "Calculation method:",
                  c("1. ABS. Outperformance" = "abs_perf",
                    "2. ABS. Outperformance (GAIN)" = "gain",
                    "3. ABS. Outperformance and High Water Mark" = "abs_perf_hwm",
                    "4. ABS. Outperformance and Water Mark" = "abs_perf_wm",
                    "5. Benchmark" = "bm",
                    "6. Benchmark and High Water Mark" = "bm_hwm",
                    "7. Benchmark and Water Mark" = "bm_wm",
                    "8. Hurdle Rate" = "hr",
                    "9. Hurdle Rate and High Water Mark" = "hr_hwm",
                    "10. Hurdle Rate and Water Mark" = "hr_wm"),
                  selected = "gain"
      ),
      sliderInput("periods",
                  "Number of periods:",
                  min = 1,
                  max = 20,
                  value = 10),
      sliderInput("tariff_1",
                  "Tariff 1:",
                  min = 1,
                  max = 50,
                  value = 5),
      sliderInput("tariff_2",
                  "Tariff 2:",
                  min = 1,
                  max = 50,
                  value = 5),
      sliderInput("riskfree",
                  "Market trend:",
                  min = 1,
                  max = 10,
                  value = 3),
      sliderInput("var_market",
                  "Varianz Market returns:",
                  min = 1,
                  max = 10,
                  value = 7),
      sliderInput("var_port",
                  "Varianz Portfolio returns :",
                  min = 1,
                  max = 20,
                  value = 12,
                  step = 0.5),
      sliderInput("hurdle_rate",
                  "Hurdle rate:",
                  min = 1,
                  max = 10,
                  value = 3),
      checkboxInput("trendline", "Show trend line", FALSE)
      # checkboxInput("chk_hurdle", "Use Hurdle rate", FALSE),
      # selectInput("state", "Calculation method:",
      #             list("available possibilities" , c("1", "2", "3")
      #                  )
      # )
      
    ),
   
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      fluidRow(
        column(width = 8,
               plotOutput("plot1", height = 300,
                          # Equivalent to: click = clickOpts(id = "plot_click")
                          click = "plot1_click",
                          brush = brushOpts(
                            id = "plot1_brush"
                          )
               )
        )
      ),
      fluidRow(
        column(width = 6,
               h4("Points near click"),
               verbatimTextOutput("click_info")
        ),
        column(width = 6,
               h4("Brushed points"),
               verbatimTextOutput("brush_info")
        ))
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$periods + 1)
    n_of_sim = 100 # number of points in the grafik * 2
    p_ret <- c(1:n_of_sim) # portfolio retun array
    tar1 <- c(1:n_of_sim)  # tariff 1 array
    tar2 <- c(1:n_of_sim)  # tariff 2 array 
    for (i in 1:n_of_sim){ 
      
      m= simulator(periods = input$periods,
                   trf = input$tariff_1/100,
                   trf2 = input$tariff_2/100,
                   riskfree = input$riskfree,
                   bm_std = input$var_market,
                   port_std = input$var_port,
                   hurd_rate =input$hurdle_rate/100,
                   typus1=input$c_meth1,
                   typus2=input$c_meth2
      )
      tar1[i] = sum(m$tariff1)
      tar2[i] = sum(m$tariff2)
      p_ret[i] = ((tail(m$eop,n=1)-100)/100)
    }
    # print(data.frame(p_ret,tar1, tar2)) to check the tariffs and portfolio values
    # print(max(tar1,tar2)) to check the maximum
    
    # start to plot the actual image
    plot(p_ret,tar1,type="p", pch= 20 ,col="red",ylim=c(0,max(tar1,tar2)+0.02), xlab="Portfolio Retruns", ylab="Commissions",  main = "The difference in fee calculations methods")
    points(p_ret,tar2,col="green", pch= 20) # cex =2
    # legend(min(p_ret),max(tar1,tar2), c(input$c_meth2, input$c_meth2), col = c("green","red"),
    #        text.col = "green4", lty = c(2, -1), pch = c(NA, 3, 4),
    #        merge = TRUE, bg = "gray90")
    fit <- lm(tar1~p_ret)
    fit2 <- lm(tar2~p_ret)
    if(input$trendline){
      lines(p_ret, fitted(fit), col="red")
      lines(p_ret, fitted(fit2), col="green")
    }
    legend("topleft", c(input$c_meth1, input$c_meth2), col = c("red","green"),pch = 1,
           inset = .01, merge = TRUE, bg = "gray90")
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$periods + 1)
    n_of_sim = 100
    p_ret <- c(1:n_of_sim)
    tar1 <- c(1:n_of_sim)
    tar2 <- c(1:n_of_sim)
    for (i in 1:n_of_sim){
      
      m= simulator(periods = input$periods,
                   trf = input$tariff_1/100,
                   trf2 = input$tariff_2/100,
                   riskfree = input$riskfree,
                   bm_std = input$var_market,
                   port_std = input$var_port,
                   hurd_rate =input$hurdle_rate/100,
                   typus1=input$c_meth1,
                   typus2=input$c_meth2
      )
      # m = simulator(input$periods,1,"a", "b", input$tariff/100)
      tar1[i] = sum(m$tariff1)
      tar2[i] = sum(m$tariff2)
      p_ret[i] = (tail(m$eop,n=1)-100)/100
      #print(c((tail(m$eop,n=1)-100)/100,sum(m$ror), sum(m$tariff), (sum(m$tariff)/((tail(m$eop,n=1)-100)/100 ))))
      #print("a---------------")
      #s = simulator(input$periods,1,'b')
      # print(c((tail(m$eop,n=1)-100)/100,sum(m$ror), sum(m$tariff), (sum(m$tariff)/((tail(m$eop,n=1)-100)/100 ))))
      # print("b---------------")
    }
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    print(data.frame(p_ret,tar1, tar2))
    print(max(tar1,tar2))
    plot(p_ret,tar1,type="p", pch= 20 ,col="red",ylim=c(0,max(tar1,tar2)+0.02), xlab="Portfolio Retruns", ylab="Commissions",  main = "The difference in fee calculations methods")
    # plot(p_ret,tar2,type="p",col="red",ylim=c(0,.5), xlab="Portfolio Retruns", ylab="Comissions")
    points(p_ret,tar2,col="green", pch= 20) # cex =2
    # legend(min(p_ret),max(tar1,tar2), c(input$c_meth2, input$c_meth2), col = c("green","red"),
    #        text.col = "green4", lty = c(2, -1), pch = c(NA, 3, 4),
    #        merge = TRUE, bg = "gray90")
    fit <- lm(tar1~p_ret)
    fit2 <- lm(tar2~p_ret)
    if(input$trendline){
      lines(p_ret, fitted(fit), col="red")
      lines(p_ret, fitted(fit2), col="green")
    }
    legend("topleft", c(input$c_meth1, input$c_meth2), col = c("red","green"),pch = 1,
           inset = .01, merge = TRUE, bg = "gray90")
    
    # # use Oregon climate-station data [orstationc.csv]
    # opar <- par(mar=c(5,4,4,5)+0.1) # space for second axis
    # plot(p_ret, tar1)    # first plot
    # par(new=T)          # second plot is going to get added to first
    # plot(p_ret, tar2, pch=3, axes=F, ylab="")  # don't overwrite
    # axis(side=4)   # add axis
    
    
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(mtcars2, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(mtcars2, input$plot1_brush)
  })
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

##########################################################################
######################### ADDITIONAL INFORMATION #########################
##########################################################################

#     ' If cal_me = "1. ABS. Outperformance" Then
# '      If ROR > 0 Then
#     '        ret_val := AIC * ROR * TRF;
#     '      End If
#     
#     '    ElseIf cal_me = "3. ABS. Outperformance and High Water Mark" Then
#     '      If ROR > 0 And VAL_EOP > HWM Then
#     '        ret_val := (VAL_EOP - HWM) * TRF;
#     '      End If
#     
#     '    ElseIf cal_me = "4. ABS. Outperformance and Water Mark" Then
#     '      If ROR > 0 And VAL_EOP > WM Then
#     '        ret_val := (VAL_EOP - WM) * TRF;
#     '      End If
#     
#     '    ElseIf cal_me = "2. ABS. Outperformance (GAIN)" Then
#     '      If GAIN > 0 Then
#     '        ret_val := GAIN * TRF;
#     '      End If
#     '
#     '    ElseIf cal_me = "5. Benchmark" Or cal_me = "8. Hurdle Rate" Then
#     '      If ROR > L_BMRK_ROR_MARKUP Then
#     '        L_RET_VAL := AIC * (ROR - L_BMRK_ROR_MARKUP) * TRF;
#     '      End If
#     '
#     '    ElseIf cal_me = "6. Benchmark and High Water Mark" Or "9. Hurdle Rate and High Water Mark" Then
#     '      If ROR > L_BMRK_ROR_MARKUP And VAL_EOP > HWM Then
#     '        L_RET_VAL := (ROR - L_BMRK_ROR_MARKUP) * (VAL_EOP - HWM) * TRF;
#     '      End If
#     
#     '    ELSeIF CALC_MTD_ID = "7. Benchmark and Water Mark" or "10. Hurdle Rate and Water Mark")
# '    THEN
# '      If ROR > L_BMRK_ROR_MARKUP And VAL_EOP > WM Then
# '        L_RET_VAL := (ROR -  L_BMRK_ROR_MARKUP) * (VAL_EOP - WM) * TRF;
# '      END IF;


##########################################################################
##################### ADDITIONAL INFORMATION END #########################
##########################################################################
