#------------------------------- Load packages ---------------------------------

if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("shinyMatrix")) {
  install.packages("shinyMatrix")
  library(shinyMatrix)
}
if (!require("DT")) {
  install.packages("DT")
  library(DT)
}
if (!require("glue")) {
  install.packages("glue")
  library(glue)
}
if (!require("gridExtra")) {
  install.packages("gridExtra")
  library(gridExtra)
}
if (!require("egg")) {
  install.packages("egg")
  library(egg)
}

#------------------------ Import Pre-defined Functions -------------------------
source("sampleSizeCalculator.R")
source("costFunc.R")
source("optimalHybridDesignFinder.R")
source('optimalEquivalenceMarginFinder.R')
source("operatingCharacteristics.R")

shinyServer(

  function(input, output) {

    #------------------------------- Functions ---------------------------------

    # This function is used to extract randomization ratios from textInput
    convert <- function(input_str) {
      
      # Split input by commas and trim spaces
      values <- unlist(strsplit(input_str, ","))
      values <- trimws(values)  # Remove extra spaces
      
      # Check if all values are either numeric or fractions
      is_valid <- sapply(values, function(x) {
        grepl("^\\d*\\.?\\d+$", x) || grepl("^\\d+/\\d+$", x)  # Matches decimals or fractions
      })
      
      # If any value is invalid, return an error
      if (any(!is_valid)) {
        stop("Error: Invalid number format detected! Please enter decimals or fractions only.")
      }
      
      # Convert fractions to decimal
      decimal_values <- sapply(values, function(x) {
        if (grepl("/", x)) {
          eval(parse(text = x))  # Convert fraction to decimal
        } else {
          as.numeric(x)  # Convert directly to numeric
        }
      })
      
      return(decimal_values)
    }
    
    #------------------------------- Sample Size Calculator ---------------------------------
    
    result_sampleSize  <- reactiveVal(0)
    
    observeEvent(input$actionButton_sampleSizeCalculator, {
      requiredSampleSize = sampleSizeCalculator(standardDeviation = input$standardDeviation,
                                                randomizationRatio = input$randomizationRatio, 
                                                effectSize = input$effectSize,
                                                power = input$power, 
                                                typeIErrorRate = input$typeIErrorRate,
                                                twoSided = as.logical(input$twoSidedOptimalHybridDesign))
      result_sampleSize(requiredSampleSize)
    })
    
    output$sampleSizeText <- renderUI({
      HTML(glue("The required sample size to achieve statistical power of {round(input$power, 2)} for detecting an effect size of {round(input$effectSize, 2)} is:<br>"))
    })
    
    output$result_sampleSizeCalculator <- renderUI({
      strong(result_sampleSize())
    })
    
    #------------------------------- Cost Function ---------------------------------
    
    result_costFunc  <- reactiveVal(0)

    observeEvent(input$actionButton_sampleSizeCalculator, {
      costRCT = costFunc(weightPower = input$weightPower,
                         power = input$power,
                         weightTrtAllocation = input$weightTrtAllocation,
                         Nt = result_sampleSize()*input$randomizationRatio,
                         weightSampleSize = input$weightSampleSize,
                         N = result_sampleSize(),
                         weightPowerLoss = input$weightPowerLoss,
                         minPower = input$power,
                         lowerBoundaryPowerLoss = input$lowerBoundaryPowerLoss,
                         weightTypeIErrorInflation = input$weightTypeIErrorInflation,
                         maxTypeIErrorInflation = input$typeIErrorRate,
                         upperBoundaryTypeIErrorInflation = input$upperBoundaryTypeIErrorInflation)
      result_costFunc(costRCT)
    })
    
    output$costText <- renderUI({
      HTML("The cost of the previously defined conventional RCT design:")
    })
    
    output$result_costFunction <- renderUI({
      strong(result_costFunc())
    })
    
    #------------------------------- Optimal Hybrid Design ---------------------------------
    
    observeEvent(input$actionButton_optimalHybridDesign, {
      showModal(popUpOptimalHybridDesign())
      
    })
    
    popUpOptimalHybridDesign = function (failed = FALSE) {
      
      requiredSampleSize = sampleSizeCalculator(standardDeviation = input$standardDeviation,
                                                randomizationRatio = 0.5, 
                                                effectSize = input$effectSize,
                                                power = input$power, 
                                                typeIErrorRate = input$typeIErrorRate,
                                                twoSided = as.logical(input$twoSidedOptimalHybridDesign))
      
      costRCT = costFunc(weightPower = input$weightPower,
                         power = input$power,
                         weightTrtAllocation = input$weightTrtAllocation,
                         Nt = requiredSampleSize*input$randomizationRatio,
                         weightSampleSize = input$weightSampleSize,
                         N = requiredSampleSize,
                         weightPowerLoss = input$weightPowerLoss,
                         minPower = input$power,
                         lowerBoundaryPowerLoss = input$lowerBoundaryPowerLoss,
                         weightTypeIErrorInflation = input$weightTypeIErrorInflation,
                         maxTypeIErrorInflation = input$typeIErrorRate,
                         upperBoundaryTypeIErrorInflation = input$upperBoundaryTypeIErrorInflation)
      
      randomizationRatios = convert(input$randomizationRatioCandidates)
      
      optimalDesign = optimalHybridDesignFinder(randomizationRatios = randomizationRatios, 
                                                RWDRatio = input$RWDRatio, 
                                                effectSize = input$effectSize, 
                                                standardDeviation = input$standardDeviation, 
                                                typeIErrorRate = input$typeIErrorRate, 
                                                typeIErrorRateEQ = 0.2, 
                                                weightPower = input$weightPower,
                                                weightTrtAllocation = input$weightTrtAllocation,
                                                weightSampleSize = input$weightSampleSize,
                                                weightPowerLoss = input$weightPowerLoss,
                                                lowerBoundaryPowerLoss = input$lowerBoundaryPowerLoss, 
                                                weightTypeIErrorInflation = input$weightTypeIErrorInflation,
                                                upperBoundaryTypeIErrorInflation = input$upperBoundaryTypeIErrorInflation, 
                                                maxSampleSize = input$maxSampleSize, 
                                                referencePower = input$power,
                                                session = session,
                                                twoSided = as.logical(input$twoSidedOptimalHybridDesign))
      output$done <- renderText("Optimal Hybrid Design Located.")
      
      requiredSampleSizeReference = sampleSizeCalculator(standardDeviation = input$standardDeviation,
                                                         randomizationRatio = optimalDesign$randomizationRatio, 
                                                         effectSize = input$effectSize,
                                                         power = input$power, 
                                                         typeIErrorRate = input$typeIErrorRate,
                                                         twoSided = as.logical(input$twoSidedOptimalHybridDesign))
      costRCTReference = costFunc(weightPower = input$weightPower,
                                  power = input$power,
                                  weightTrtAllocation = input$weightTrtAllocation,
                                  Nt = requiredSampleSizeReference*optimalDesign$randomizationRatio,
                                  weightSampleSize = input$weightSampleSize,
                                  N = requiredSampleSizeReference,
                                  weightPowerLoss = input$weightPowerLoss,
                                  minPower = input$power,
                                  lowerBoundaryPowerLoss = input$lowerBoundaryPowerLoss,
                                  weightTypeIErrorInflation = input$weightTypeIErrorInflation,
                                  maxTypeIErrorInflation = input$typeIErrorRate,
                                  upperBoundaryTypeIErrorInflation = input$upperBoundaryTypeIErrorInflation)
      
      operatingCharacteristics = operatingCharacteristics(randomizationRatio = optimalDesign$randomizationRatio, 
                                                          N = optimalDesign$sampleSize,
                                                          RWDRatio = input$RWDRatio,
                                                          eqMargin = optimalDesign$eqMargin,
                                                          effectSize = input$effectSize, 
                                                          standardDeviation = input$standardDeviation, 
                                                          typeIErrorRate = input$typeIErrorRate, 
                                                          typeIErrorRateEQ = 0.2,
                                                          twoSided = as.logical(input$twoSidedOptimalHybridDesign))
      
      output$Plots <- renderPlot({
        
        dataPowerPlot = data.frame(bias = seq(-1, 1, by = 0.01), power = operatingCharacteristics$powerArray)
        hlineDataPower <- data.frame(
          yintercept = input$lowerBoundaryPowerLossOptimalEQ,  # Dynamic threshold
          label = "Lower Power Boundary"
        )
        
        powerPlot <- ggplot(dataPowerPlot, aes(x = bias, y = power)) +
          geom_line(aes(color = "Power Curve", linetype = "Power Curve"), size = 1) + 
          geom_hline(data = hlineDataPower, aes(yintercept = yintercept, color = label, linetype = label), size = 1) +
          scale_color_manual(name = "Legend", 
                             values = c("Power Curve" = "darkgray", 
                                        "Lower Power Boundary" = "red"),
                             breaks = c("Power Curve", "Lower Power Boundary")) +  
          scale_linetype_manual(name = "Legend",
                                values = c("Power Curve" = "solid",
                                           "Lower Power Boundary" = "dashed"),
                                breaks = c("Power Curve", "Lower Power Boundary")) +  
          labs(title = "Statistical Power of the Optimal Hybrid Design Across Different Bias", 
               x = "Bias between RCT Control Arm and RWD", 
               y = "Statistical Power") +  
          theme_minimal()
        
        datatypeIErrorPlot = data.frame(bias = seq(-1, 1, by = 0.01), typeIError = operatingCharacteristics$typeIErrorArray)
        hlineDataTypeIError <- data.frame(
          yintercept = input$upperBoundaryTypeIErrorInflationOptimalEQ,
          label = "Upper TypeIError Boundary"
        )
        
        typeIErrorPlot <- ggplot(datatypeIErrorPlot, aes(x = bias, y = typeIError)) +
          geom_line(aes(color = "TypeIError Curve", linetype = "TypeIError Curve"), size = 1) +
          geom_hline(data = hlineDataTypeIError, aes(yintercept = yintercept, color = label, linetype = label), size = 1) +
          scale_color_manual(name = "Legend", 
                             values = c("TypeIError Curve" = "darkgray", 
                                        "Upper TypeIError Boundary" = "red")) +  
          scale_linetype_manual(name = "Legend",
                                values = c("TypeIError Curve" = "solid",
                                           "Upper TypeIError Boundary" = "dashed")) +  
          labs(title = "Type I Error of the optimal hybrid design across different bias", x = "Bias between RCT control arm and RWD", y = "Type I Error") +  # Labels
          theme_minimal()
        
        dataBetaPlot = data.frame(bias = seq(-1, 1, by = 0.01), beta = operatingCharacteristics$betaArray)
        
        betaPlot <- ggplot(dataBetaPlot, aes(x = bias, y = beta)) +
          geom_line(color = "darkgray", size = 1) +
          labs(title = "Borrowing probability of the optimal hybrid design across different bias", x = "Bias between RCT control arm and RWD", y = "Borrowing probability") +  # Labels
          theme_minimal()
        
        dataBiasPlot = data.frame(bias = seq(-1, 1, by = 0.01), introBias = operatingCharacteristics$biasArray)
        
        biasPlot <- ggplot(dataBiasPlot, aes(x = bias, y = introBias)) +
          geom_line(color = "darkgray", size = 1) +
          labs(title = "Introduced bias of the optimal hybrid design across different bias", x = "Bias between RCT control arm and RWD", y = "Introduced bias") +  # Labels
          theme_minimal()
        
        grid.arrange(powerPlot, typeIErrorPlot, betaPlot, biasPlot, ncol = 1)
        egg::ggarrange(powerPlot, typeIErrorPlot, betaPlot, biasPlot,
                       ncol = 1,
                       widths = 1,
                       draw = TRUE)
      })
      
      parameterTable <- data.frame(
        Parameter = c("referencePower", "randomizationRatio", "standardDeviation", "effectSize", "RWDRatio", "typeIErrorRate",
                      "weightPower", "weightTrtAllocation", "weightSampleSize", "weightPowerLoss",
                      "weightTypeIErrorInflation", "lowerBoundaryPowerLoss", "upperBoundaryTypeIErrorInflation",
                      "maxSampleSize", "costRCT"),
        Value = c(input$power, input$randomizationRatio, input$standardDeviation, input$effectSize, input$RWDRatio, input$typeIErrorRate,
                  input$weightPower, input$weightTrtAllocation, input$weightSampleSize, input$weightPowerLoss,
                  input$weightTypeIErrorInflation, input$lowerBoundaryPowerLoss, input$upperBoundaryTypeIErrorInflation,
                  input$maxSampleSize, costRCT)
      )
      
      output$titleTableParameter = renderText({"<strong>Table 1: Predefined Parameters for Optimal Hybrid Design Selection</strong>"})
      output$tableParameter <- renderDT({
        datatable(parameterTable, 
                  class = 'cell-border compact stripe', 
                  rownames = FALSE, 
                  colnames = c("Parameters", "Values"),
                  options = list(dom = 't', ordering = FALSE))
      })
      
      optimalHybridDesignTable = data.frame(randomizationRatio = c(optimalDesign$randomizationRatio, optimalDesign$randomizationRatio, input$randomizationRatio),
                                            sampleSize = c(optimalDesign$sampleSize, requiredSampleSizeReference, requiredSampleSize),
                                            trtPatietns = c(optimalDesign$trtPatients, requiredSampleSizeReference*optimalDesign$randomizationRatio, requiredSampleSize*0.5),
                                            equivalenceBoundary = c(optimalDesign$eqMargin, "NA", "NA"),
                                            power = c(optimalDesign$power, input$power, input$power),
                                            minPower = c(optimalDesign$minPower, "NA", "NA"),
                                            maxTypeIErrorInflation = c(optimalDesign$maxTypeIErrorInflation, "NA", "NA"),
                                            cost = c(optimalDesign$cost, costRCTReference, costRCT))
      
      output$titleTableOptimalHybridDesign = renderText({"<strong>Table 2: Design Parameters and Operating Characteristics of The Optimal Hybrid Design</strong>"})
      output$tableOptimalHybridDesign <- renderDT({
        datatable(optimalHybridDesignTable, 
                  class = 'cell-border compact stripe', 
                  rownames = c("Optimal Hybrid Design", "Conventional RCT design 1", "Conventional RCT design 2"), 
                  colnames = c("r", "N", "Nt", "m", "power", "minimum power", "maximum type I error", "Cost"),
                  options = list(dom = 't', ordering = FALSE))
      })
      
      output$titleDesignCandidates = renderText({"<strong>Table 3: Candidate Designs Evaluated for Optimal Hybrid Design Selection</strong>"})
      output$tableDesignCandidates <- renderDT({
        df <- as.data.frame(optimalDesign$designCandidates)
        min_cost_index <- which.min(df$Cost)
        
        datatable(df, 
                  class = 'cell-border compact stripe', 
                  rownames = FALSE, 
                  colnames = c("r", "N", "Nt", "m", "power", "minimum power", "maximum type I error", "Cost"),
                  options = list(
                    dom = 't',
                    ordering = FALSE,
                    scrollY = "400px",
                    paging = FALSE
                  ))
      })
      
      modalDialog(
        title = HTML("<b>Design Parameters and Operating Characteristics of The Optimal Hybrid Design</b>"),
        
        withMathJax(),
        
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        output$text <- renderText({
          "The plots and tables displayed here are specific to the choice of parameters. Predefined parameters are listed below"
        }),
        
        fluidRow(
          align = 'center',
          
          htmlOutput('titleTableParameter'),
          DTOutput('tableParameter', width = "80%"),
          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 60%;"),
          
          htmlOutput('titleTableOptimalHybridDesign'),
          DTOutput('tableOptimalHybridDesign', width = "80%"),
          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 60%;"),
          
          output$text <- renderUI({
            HTML(glue(
              "<strong>Please Note:</strong> The choice of the cost function can influence the final optimal hybrid design. <br><br>",
              "The total cost of the optimal hybrid design, based on the predefined cost function, is <b>{round(optimalDesign$cost, 4)}</b>. <br><br>",
              
              "For reference, the cost of a conventional RCT with a randomization ratio of <b>{round(optimalDesign$randomizationRatio, 4)}</b> is <b>{round(costRCTReference, 4)}</b>. <br>",
              "Compared to this conventional two-arm RCT design, the hybrid design achieves a cost reduction of <b>{round(costRCTReference - optimalDesign$cost, 4)}</b> <br><br>",
              
              "And the cost of a conventional RCT with a randomization ratio of <b>0.5</b> is <b>{round(costRCT, 4)}</b>. <br>",
              "Compared to this conventional two-arm RCT design, the hybrid design achieves a cost reduction of <b>{round(costRCT - optimalDesign$cost, 4)}</b>."
            ))
          }),
          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 60%;"),
          br(),
          
          HTML("<strong>Figure 1: Statistical power, type I Error, and borrowing probability of the optimal hybrid design across different bias</strong>"),
          plotOutput('Plots', width = "80%", height = "800px"),
          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 60%;"),
          br(),
          
          fluidRow(
            align = "center",
            column(1),
            column(10,
                   HTML("<h5 style='text-align: left;'><strong>Appendix: </strong></h5>"),
                   htmlOutput('titleDesignCandidates'),
                   DTOutput('tableDesignCandidates', width = "100%")
                   ),
            column(1)
          )
          
        ),
        
        footer = modalButton("Close"),
        size = c("l")
      )
    }
    
    #------------------------------- Optimal Equivalence Boundary ---------------------------------
    
    observeEvent(input$actionButtonOptimalEQ, {
      
      requiredSampleSize = sampleSizeCalculator(standardDeviation = input$standardDeviationOptimalEQ,
                                                randomizationRatio = input$randomizationRatioOptimalEQ, 
                                                effectSize = input$effectSizeOptimalEQ,
                                                power = input$lowerBoundaryPowerLossOptimalEQ, 
                                                typeIErrorRate = input$typeIErrorRateOptimalEQ,
                                                twoSided = as.logical(input$twoSidedOptimalEQ))
      
      if (input$sampleSizeOptimalEQ < requiredSampleSize) {
        showModal(modalDialog(
          title = "Warning: Insufficient Sample Size",
          output$textOptimalEQ <- renderUI({
            HTML(glue(
              "The required sample size to achieve the specified lower power boundary is <b>{requiredSampleSize}</b>. <br>",
              "However, you entered <b>{input$sampleSizeOptimalEQ}</b>, which is insufficient. <br>",
              "As a result, the power requirement cannot be met, regardless of the equivalence boundary. <br>",
              "Please specify a sample size of at least <b>{requiredSampleSize}</b> to ensure valid results."
            ))
          }),
          size = "xl",
          easyClose = TRUE
        ))
      } else {
        showModal(popUpOptimalEQ())
      }
      
    })
    
    popUpOptimalEQ = function (failed = FALSE) {
      
      
      
      marginFinderTypeIerror <- function(m) {
        return(optimalEquivalenceMarginFinder(margin = m, 
                                              randomizationRatio = input$randomizationRatioOptimalEQ, 
                                              sampleSize = input$sampleSizeOptimalEQ,
                                              RWDRatio = input$RWDRatioOptimalEQ, 
                                              effectSize = input$effectSizeOptimalEQ, 
                                              standardDeviation = input$standardDeviationOptimalEQ, 
                                              typeIErrorRate = input$typeIErrorRateOptimalEQ, 
                                              typeIErrorRateEQ = 0.2,
                                              twoSided = as.logical(input$twoSidedOptimalEQ))$maxTypeIerror - input$upperBoundaryTypeIErrorInflationOptimalEQ)
      }
      
      marginFinderPower <- function(m) {
        return(optimalEquivalenceMarginFinder(margin = m, 
                                              randomizationRatio = input$randomizationRatioOptimalEQ,
                                              sampleSize = input$sampleSizeOptimalEQ,
                                              RWDRatio = input$RWDRatioOptimalEQ, 
                                              effectSize = input$effectSizeOptimalEQ, 
                                              standardDeviation = input$standardDeviationOptimalEQ, 
                                              typeIErrorRate = input$typeIErrorRateOptimalEQ,
                                              typeIErrorRateEQ = 0.2,
                                              twoSided = as.logical(input$twoSidedOptimalEQ))$minPower - input$lowerBoundaryPowerLossOptimalEQ)
      }
      
      marginTypeIerror <- uniroot(marginFinderTypeIerror, interval = c(0, 2), tol = 1e-12)$root
      
      marginPower <- uniroot(marginFinderPower, interval = c(0, 2), tol = 1e-12)$root
      
      eqMargin <- min(marginTypeIerror, marginPower)
      
      operatingCharacteristics = operatingCharacteristics(randomizationRatio = input$randomizationRatioOptimalEQ, 
                                                          N = input$sampleSizeOptimalEQ,
                                                          eqMargin = eqMargin,
                                                          RWDRatio = input$RWDRatioOptimalEQ,
                                                          effectSize = input$effectSizeOptimalEQ, 
                                                          standardDeviation = input$standardDeviationOptimalEQ, 
                                                          typeIErrorRate = input$typeIErrorRateOptimalEQ, 
                                                          typeIErrorRateEQ = 0.2,
                                                          twoSided = as.logical(input$twoSidedOptimalEQ))
      
      output$PlotsOptimalEQ <- renderPlot({
        
        dataPowerPlot = data.frame(bias = seq(-1, 1, by = 0.01), power = operatingCharacteristics$powerArray)
        hlineDataPower <- data.frame(
          yintercept = input$lowerBoundaryPowerLossOptimalEQ,  # Dynamic threshold
          label = "Lower Power Boundary"
        )
        
        powerPlot <- ggplot(dataPowerPlot, aes(x = bias, y = power)) +
          geom_line(aes(color = "Power Curve", linetype = "Power Curve"), size = 1) + 
          geom_hline(data = hlineDataPower, aes(yintercept = yintercept, color = label, linetype = label), size = 1) +
          scale_color_manual(name = "Legend", 
                             values = c("Power Curve" = "darkgray", 
                                        "Lower Power Boundary" = "red"),
                             breaks = c("Power Curve", "Lower Power Boundary")) +  
          scale_linetype_manual(name = "Legend",
                                values = c("Power Curve" = "solid",
                                           "Lower Power Boundary" = "dashed"),
                                breaks = c("Power Curve", "Lower Power Boundary")) +  
          labs(title = "Statistical Power of the Optimal Hybrid Design Across Different Bias", 
               x = "Bias between RCT Control Arm and RWD", 
               y = "Statistical Power") +  
          theme_minimal()
        
        datatypeIErrorPlot = data.frame(bias = seq(-1, 1, by = 0.01), typeIError = operatingCharacteristics$typeIErrorArray)
        hlineDataTypeIError <- data.frame(
          yintercept = input$upperBoundaryTypeIErrorInflationOptimalEQ,
          label = "Upper TypeIError Boundary"
        )
        
        typeIErrorPlot <- ggplot(datatypeIErrorPlot, aes(x = bias, y = typeIError)) +
          geom_line(aes(color = "TypeIError Curve", linetype = "TypeIError Curve"), size = 1) +
          geom_hline(data = hlineDataTypeIError, aes(yintercept = yintercept, color = label, linetype = label), size = 1) +
          scale_color_manual(name = "Legend", 
                             values = c("TypeIError Curve" = "darkgray", 
                                        "Upper TypeIError Boundary" = "red")) +  
          scale_linetype_manual(name = "Legend",
                                values = c("TypeIError Curve" = "solid",
                                           "Upper TypeIError Boundary" = "dashed")) +  
          labs(title = "Type I Error of the optimal hybrid design across different bias", x = "Bias between RCT control arm and RWD", y = "Type I Error") +  # Labels
          theme_minimal()
        
        dataBetaPlot = data.frame(bias = seq(-1, 1, by = 0.01), beta = operatingCharacteristics$betaArray)
        
        betaPlot <- ggplot(dataBetaPlot, aes(x = bias, y = beta)) +
          geom_line(color = "darkgray", size = 1) +
          labs(title = "Borrowing probability of the optimal hybrid design across different bias", x = "Bias between RCT control arm and RWD", y = "Borrowing probability") +  # Labels
          theme_minimal()
        
        dataBiasPlot = data.frame(bias = seq(-1, 1, by = 0.01), introBias = operatingCharacteristics$biasArray)
        
        biasPlot <- ggplot(dataBiasPlot, aes(x = bias, y = introBias)) +
          geom_line(color = "darkgray", size = 1) +
          labs(title = "Introduced bias of the optimal hybrid design across different bias", x = "Bias between RCT control arm and RWD", y = "Introduced bias") +  # Labels
          theme_minimal()
        
        grid.arrange(powerPlot, typeIErrorPlot, betaPlot, biasPlot, ncol = 1)
        egg::ggarrange(powerPlot, typeIErrorPlot, betaPlot, biasPlot,
                       ncol = 1,
                       widths = 1,
                       draw = TRUE)
      })
      
      parameterTable <- data.frame(
        Parameter = c("randomizationRatio", "sampleSize", "standardDeviation", "effectSize", "RWDRatio", "typeIErrorRate",
                      "lowerBoundaryPowerLoss", "upperBoundaryTypeIErrorInflation"),
        Value = c(input$randomizationRatioOptimalEQ, input$sampleSizeOptimalEQ, input$standardDeviationOptimalEQ, input$effectSizeOptimalEQ, input$RWDRatioOptimalEQ, input$typeIErrorRateOptimalEQ,
                  input$lowerBoundaryPowerLossOptimalEQ, input$upperBoundaryTypeIErrorInflationOptimalEQ)
      )
      
      output$titleTableParameter = renderText({"<strong>Table 1: Parameters defined in the previous step</strong>"})
      output$tableParameter <- renderDT({
        datatable(parameterTable, 
                  class = 'cell-border compact stripe', 
                  rownames = FALSE,
                  options = list(dom = 't', ordering = FALSE))
      })
      
      modalDialog(
        title = HTML("<b>Optimal Equivalence Boundary and Operating Characteristics of the Corresponding Hybrid Design</b>"),
        withMathJax(),
        
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Loading...",id="loadmessage")),
        output$text <- renderText({
          "The plots and tables displayed here are specific to the choice of parameters. Previously defined parameters are listed below"
        }),
        
        fluidRow(
          align = 'center',
          
          htmlOutput('titleTableParameter'),
          DTOutput('tableParameter',
                   width = "80%"),
          
          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 60%;"),
          
          output$text <- renderUI({
            HTML(glue(
              "The equivalence boundary that ensures the lower power boundary is <b>{round(marginPower, 4)}</b>. <br>",
              "The equivalence boundary that ensures the upper boundary on Type I error is <b>{round(marginTypeIerror, 4)}</b>. <br>",
              "Therefore, the optimal equivalence boundary that guarantees global Type I error and power control is <b>{round(eqMargin, 4)}</b>."
            ))
          }),
          
          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 60%;"),
          br(),
          
          HTML("<strong>Figure 1: Statistical power, type I Error, and borrowing probability of the optimal hybrid design across different bias</strong>"),
          plotOutput('PlotsOptimalEQ',
                     width = "80%",
                     height = "800px")
          
        ),
        
        footer = modalButton("Close"),
        size = c("l")
      )
    }
 }
) # End of Server




