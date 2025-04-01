#------------------------------- Load required packages ------------------------------

if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require("shinythemes")) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require("shinybusy")) {
  install.packages("shinybusy")
  library(shinybusy)
}

#------------------------------- Define user interface ------------------------------


shinyUI(
  fluidPage(
    div(style="padding:20px",
        img(src="SOM_logo.png", height="2.5%", width="20%"),
        titlePanel(
          title="", windowTitle="My Window Title"
        )
    ),
  navbarPage(title = strong("Optimal Two-Step Hybrid Design: A Framework for Equivalence Boundary Selection Under Type I Error and Power Constraints"),
             theme = shinytheme("paper"),
             tags$style(HTML(".navbar-header { width:100% }
                              .navbar-brand { width: 100%; text-align: center }")),
             tabPanel("Introduction", icon=icon("house", verify_fa = FALSE),
                      fluidPage(
                        br(),
                        br(),
                        wellPanel(
                          align = 'left',
                          HTML("This interactive web application offers three key features: <br>
                                <br>
                                (1) calculating the sample size for a conventional two-arm RCT design,<br>
                                (2) determining the optimal hybrid design,<br>
                                (3) selecting the optimal equivalence boundary in a hybrid design. <br>
                                <br>
                                The optimal hybrid design is chosen based on a pre-specified cost function, which balances three trial objectives: maximizing statistical power, optimizing treatment patient allocation, and minimizing the total sample size. This selection is made while ensuring global constraints on power and Type I error control.<br>
                                The second feature allows users to determine the optimal equivalence boundary that guarantee global Type I error and power control in a pre-specified hybrid design.<br>
                                The application provides a user-friendly graphical user interface (GUI), where users can input design parameters under realistic scenarios. Once the parameters are specified, clicking the action button initiates the computations and displays the results in a dedicated output tab. <br>
                                The output includes the specified scenario parameters, the optimal hybrid design or equivalence boundary, and the corresponding operating characteristics of the chosen hybrid design."),
                        ),
                        br(),
                        br(),
                        br(),
                        br(),
                        hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 120px; margin-left: 0;"),
                        br(),
                        
                        HTML("<strong>Authors Information:</strong> <br>
                               <br>
                               Jiapeng Xu (Maintainer)<br>
                               Department of Biomedical Data Science, Center for Innovative Study Designs and the Biostatistics Core, Stanfrod Cancer Insistute, School of Medicine, Stanford University, Stanford, 94305, CA, USA <br>
                               e-mail: jx2427@columbia.edu <a href=mailto:jx2427@columbia.edu>Send email</a> <br>
                               <br>
                               Ying Lu <br>
                               Department of Biomedical Data Science, Center for Innovative Study Designs and the Biostatistics Core, Stanfrod Cancer Insistute, School of Medicine, Stanford University, Stanford, 94305, CA, USA <br>
                               e-mail: ylu1@stanford.edu <br>
                               <br>
                               Arlina Shen <br>
                               Department of Biomedical Data Science, Center for Innovative Study Designs and the Biostatistics Core, Stanfrod Cancer Insistute, School of Medicine, Stanford University, Stanford, 94305, CA, USA <br>
                               e-mail: ahshen26@stanford.edu"),
                        
                      )
                      
             ),
             #------------------------------------------ Optimal Hybrid Design Finder -------------------------------------
             tabPanel(title = "Optimal Hybrid Design Finder", icon = icon("chart-line", verify_fa = FALSE),
                      fluidPage(
                        withMathJax(),
                        fluidRow(
                          column(12,
                                 align = "center",
                                 wellPanel(align = 'center',
                                           withMathJax(),
                                           br(),
                                           fluidRow(
                                             column(6,
                                                    h5("Required Sample Size Calculator (Conventional Two-Arm Randomized Controlled Trial (RCT) Design)"),
                                                    column(4,
                                                           selectInput('twoSidedOptimalHybridDesign',
                                                                       strong('One/Two-Sided Test'),
                                                                       c('One-Sided Test' = 'FALSE',
                                                                         'Two-Sided Test' = 'TRUE'),
                                                                       selected = 'Two-Sided Test',
                                                                       multiple = FALSE),
                                                           numericInput("standardDeviation", strong("Standard Deviation"), value = 1, min = 0, max = 10000, width = '400px'),
                                                           numericInput("randomizationRatio", strong("Randomization Ratio (Treatment:Total)"), value = 0.5, min = 0, max = 1, width = '400px'),
                                                           numericInput("effectSize", strong("Effect Size"), value = 0.4, min = 0, max = 10000, width = '400px'),
                                                           sliderInput("power", strong("Statistical Power"), value = 0.8, min = 0, max = 1, width = '400px'),
                                                           sliderInput("typeIErrorRate", strong("Type I Error Rate"), value = 0.05, min = 0, max = 1, width = '400px')
                                                    ),
                                                    column(8,
                                                           align = 'left',
                                                           HTML("This subsection allows users to calculate the required sample size for a conventional two-arm RCT design. The computed sample size and the corresponding RCT design serves as a reference for subsequent optimization of hybrid trial designs."),
                                                           HTML("<br><strong>Parameters to specify:</strong><br>
                                                                 <strong>1. Standard Deviation:</strong> Assumes that the variability of individual outcome measures is the same across both treatment and control groups. This parameter defines the standard deviation of individual patient responses.<br>
                                                                 <strong>2. Randomization Ratio (Treatment:Total):</strong> Specifies the proportion of participants assigned to the treatment arm relative to the control arm.<br>
                                                                 <strong>3. Effect Size:</strong> Represents the detectable difference between the treatment and control groups for a given power level.<br>
                                                                 <strong>4. Statistical Power:</strong> Defines the probability of correctly detecting a true treatment effect. The default value is set to 80%.<br>
                                                                 <strong>5. Type I Error Rate:</strong> Represents the probability of incorrectly rejecting the null hypothesis when no true treatment effect exists. The default value is set to 5%.<br>"),
                                                           HTML("<br>Once all parameters are appropriately specified, click the <strong>CALCULATE SAMPLE SIZE</strong> button. The computed results will be displayed below.")
                                                    )
                                             ),
                                             column(6,
                                                    h5("Formulation of the Cost Function"),
                                                    column(4,
                                                           numericInput(inputId = "weightPower", label = strong("Weight for Statistical Power (\\(w_{1}\\))"), value = 100, min = 0, max = 10000, width = '70%'),
                                                           numericInput(inputId = "weightTrtAllocation", label = strong("Weight for Treatment Allocation (\\(w_{2}\\))"), value = 1, min = 0, max = 10000, width = '70%'),
                                                           numericInput(inputId = "weightSampleSize", label = strong("Penalty for Increased Sample Size (\\(w_{3}\\))"), value = 1, min = 0, max = 10000, width = '70%'),
                                                           numericInput(inputId = "weightPowerLoss", label = strong("Penalty for Power Constraint Violation (\\(w_{4}\\))"), value = 10000, min = 0, max = 10000, width = '70%'),
                                                           numericInput(inputId = "weightTypeIErrorInflation", label = strong("Penalty for Type I Error Constraint Violation (\\(w_{5}\\))"), value = 10000, min = 0, max = 10000, width = '70%'),
                                                           style = "border-left: 0.5px solid #000000; border-color: gray;"
                                                    ),
                                                    column(8, 
                                                           align = 'left',
                                                           withMathJax(),
                                                           HTML("In different research scenarios, investigators may prioritize different objectives, such as maximizing statistical power, minimizing the total sample size, and optimizing treatment allocation. 
                                                                 These three objectives often conflict, requiring trade-offs in study design. For example, increasing the proportion of participants assigned to the treatment arm may necessitate an unequal randomization ratio, 
                                                                 which, in turn, increases the required sample size to maintain the same power level.
                                                                
                                                                 <br>
                                                                 To address this challenge, we propose a cost function that balances these competing objectives:
                                                                 <br>
                                                            
                                                                 <span style='font-size: 80%;'>
                                                                 \\[
                                                                 C(r,m,N) = -w_1(1-\\beta(0)) - w_2 N_t + w_3 N + w_4 \\left( \\max_{\\delta} \\alpha(\\delta) - \\eta \\right) \\mathbb{I}_{\\max_{\\delta} \\alpha(\\delta) > \\eta} 
                                                                 + w_5 \\left( \\zeta - \\min_{\\delta} (1 - \\beta(\\delta)) \\right) \\mathbb{I}_{\\min_{\\delta} (1 - \\beta(\\delta)) < \\zeta}
                                                                 \\]
                                                                 </span>
                                                                 <br>
                                                            
                                                                 Here, the terms in the cost function are defined as follows:  
                                                                 <ul>
                                                                     <li><strong>\\( (1-\\beta(0)) \\)</strong> represents the statistical power when the bias between the RCT control arm and real-world data (RWD) is zero, i.e., when the exchangeability assumption between the RCT control arm and RWD holds.</li>
                                                                     <li><strong>\\( N_t \\)</strong> denotes the number of patients allocated to the treatment arm.</li>
                                                                     <li><strong>\\( N \\)</strong> represents the total sample size.</li>
                                                                     <li><strong>\\( \\left( \\max_{\\delta} \\alpha(\\delta) - \\eta \\right) \\mathbb{I}_{\\max_{\\delta} \\alpha(\\delta) > \\eta} \\)</strong> quantifies the extent to which the Type I error constraint is violated.</li>
                                                                     <li><strong>\\( \\left( \\zeta - \\min_{\\delta} (1 - \\beta(\\delta)) \\right) \\mathbb{I}_{\\min_{\\delta} (1 - \\beta(\\delta)) < \\zeta} \\)</strong> quantifies the extent to which the power constraint is violated.</li>
                                                                 </ul>
                                                                 
                                                                 Once the five weights are specified, the cost function is fully defined. These weights represent:
                                                                 <ul>
                                                                    <li>The reward for power gain when the bias is zero (\\(w_{1}\\))</li>
                                                                    <li>The reward for additional patients allocated to the treatment arm (\\(w_{2}\\))</li>
                                                                    <li>The penalty for an increased sample size (\\(w_{3}\\))</li>
                                                                    <li>The penalty for violating the Type I error constraint (\\(w_{4}\\))</li>
                                                                    <li>The penalty for violating the power constraint (\\(w_{5}\\))</li>
                                                                 </ul>
                                                                  
                                                                 The first three weights represent the relative priorities assigned to maximizing statistical power, increasing treatment allocation, and minimizing sample size.<br>
                                                                 For example, the default values of \\(w_{2}\\), \\(w_{2}\\), \\(w_{3}\\) are 100, 1, and 1, respectively, which represent one would be willing to accept a 1% reduction in statistical power (when bias is zero) in exchange for either allocating one additional patient to the treatment arm or reducing the total sample size by one.<br>
                                                                 The default values of \\(w_{4}\\) and \\(w_{5}\\) are both set to 10,000. If one seeks to strictly control the Type I error and statistical power at a global level, assigning a significantly large value to these weights would be preferred.<br>
                                                                 For reference, once the cost function is specified, the cost of the previously defined RCT design in the left section is calculated and serves as a benchmark for optimizing hybrid trial designs.")
                                                           )
                                                    ),
                                             column(12,
                                                    br(),
                                                    actionButton("actionButton_sampleSizeCalculator", "Calculate Sample Size"),
                                                    add_busy_bar(color = '#848482', centered = TRUE, height = "16px"),
                                                    align = "center",
                                                    style = "margin-bottom: 10px;",
                                                    style = "margin-top: -10px;"
                                                    )
                                             )
                                           ),
                                 fluidRow(br(),
                                          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 900px;"),
                                          uiOutput("sampleSizeText"),
                                          uiOutput("result_sampleSizeCalculator"),
                                          uiOutput("costText"),
                                          uiOutput("result_costFunction"),
                                          hr(style = "border-top: 0.5px solid #000000; border-color: gray; width: 900px;"),
                                          br()),
                                 wellPanel(align = 'center',
                                           h5("Locating the Optimal Hybrid Design Based on the Defined Cost Function"),
                                           br(),
                                           fluidRow(
                                             align = 'center',
                                             textInput(inputId = "randomizationRatioCandidates",
                                                       label = strong("Candidate Randomization Ratios for Optimization"),
                                                       value = "0.5, 2/3, 3/4",
                                                       width = '70%'),
                                             p(strong("Note:"), "Please enter randomization ratios as decimal numbers or fractions, separated by commas."),
                                             br(),
                                             column(3,
                                                    numericInput(inputId = "RWDRatio",
                                                                label = strong("The ratio of available RWD to trial patients"),
                                                                value = 1,
                                                                min = 0,
                                                                max = 10000,
                                                                width = '400px')),
                                             column(3,
                                                    numericInput(inputId = "maxSampleSize",
                                                                 label = strong("Maximum Allowed Sample Size"),
                                                                 value = 400,
                                                                 min = 0,
                                                                 max = 10000,
                                                                 width = '70%')),
                                             column(3,
                                                    sliderInput(inputId = "lowerBoundaryPowerLoss",
                                                                 label = strong("Lower Boundary of Power (\\(\\eta\\))"),
                                                                 value = 0.77,
                                                                 min = 0,
                                                                 max = 1,
                                                                 width = '400px')),
                                             column(3,
                                                    sliderInput(inputId = "upperBoundaryTypeIErrorInflation",
                                                                 label = strong("Upper Boundary of Type I Error (\\(\\zeta\\))"),
                                                                 value = 0.07,
                                                                 min = 0,
                                                                 max = 1,
                                                                 width = '400px')),
                                             column(1),
                                             column(10,
                                                    align = 'left',
                                                    HTML("In this section, we use the previously defined cost function to identify the optimal hybrid design. 
                                                          The parameters to be determined include:
                                                          
                                                          <ul>
                                                              <li><strong>Candidate Randomization Ratios for Optimization:</strong> A set of candidate randomization ratios for hybrid design selection.</li>
                                                              <li><strong>Maximum Allowed Sample Size:</strong> The upper limit on the total sample size.</li>
                                                              <li><strong>Lower Boundary of Power:</strong> The minimum acceptable power.</li>
                                                              <li><strong>Upper Boundary of Type I Error:</strong> The maximum allowable inflation in Type I error.</li>
                                                          </ul>
                                                      
                                                          As the randomization ratio increases, the required sample size also increases. To prevent excessively high randomization ratios that result in extreme sample sizes and to better align with realistic constraints, we impose an upper boundary on the sample size.
                                                      
                                                          <br><br>
                                                          Once all parameters are appropriately specified, click the <strong>FIND THE OPTIMAL HYBRID DESIGN</strong> button. The results will be displayed in a dedicated output tab.
                                                      ")
                                             ),
                                             column(1),
                                             column(12,
                                                    br(),
                                                    actionButton("actionButton_optimalHybridDesign", "Find the optimal hybrid design"),
                                                    add_busy_bar(color = '#848482', centered = TRUE, height = "16px"),
                                                    align = "center",
                                                    style = "margin-bottom: 10px;",
                                                    style = "margin-top: -10px;"
                                             )
                                           ),
                                 ),
                                 
                          )
                        ),
                      )
                      ),
             #------------------------------------------ Optimal Equivalence Boundary Finder -------------------------------------
             tabPanel(title = "Optimal Equivalence Boundary Finder", icon = icon("chart-line", verify_fa = FALSE),
                      fluidPage(
                        withMathJax(),
                        wellPanel(align = 'center',
                                  h5("Determining the Optimal Equivalence Boundary under Type I Error and Power Constrains"),
                                  br(),
                                  fluidRow(column(4,
                                                  selectInput('twoSidedOptimalEQ',
                                                              strong('One/Two-Sided Test'),
                                                              c('One-Sided Test' = 'FALSE',
                                                                'Two-Sided Test' = 'TRUE'),
                                                              selected = 'Two-Sided Test',
                                                              multiple = FALSE,
                                                              width  = '400px'),
                                                  numericInput("standardDeviationOptimalEQ",   strong("Standard Deviation"),    value = 1,    min = 0,  max = 10000, width  = '400px'),
                                                  numericInput("randomizationRatioOptimalEQ",   strong("Randomization Ratio (Treatment:Total)"),    value = 0.5,    min = 0,  max = 1, width  = '400px'),
                                                  numericInput("effectSizeOptimalEQ", strong("Effect size"),    value = 0.4,    min = 0,  max = 10000, width  = '400px'),
                                                  numericInput("sampleSizeOptimalEQ", strong("Total Sample Size"),    value = 200,    min = 0,  max = 1, width  = '400px'),
                                                  numericInput("RWDRatioOptimalEQ",  strong("The ratio of available RWD to trial patients"), value = 1, min = 0, max = 10000, width = '400px'),
                                                  sliderInput("typeIErrorRateOptimalEQ",   strong("Type I error rate"),    value = 0.05,    min = 0,  max = 1, width  = '400px'),
                                                  sliderInput(inputId = "lowerBoundaryPowerLossOptimalEQ",
                                                               label = strong("Lower Boundary of Power (\\(\\eta\\))"),
                                                               value = 0.77,
                                                               min = 0,
                                                               max = 1,
                                                               width  = '400px'),
                                                  sliderInput(inputId = "upperBoundaryTypeIErrorInflationOptimalEQ",
                                                               label = strong("Upper Boundary of Type I Error (\\(\\zeta\\))"),
                                                               value = 0.07,
                                                               min = 0,
                                                               max = 1,
                                                               width  = '400px')
                                  ),
                                  column(8,
                                         align = 'left',
                                         HTML("In real-world applications of hybrid design, a critical step is selecting an appropriate equivalence boundary. A higher equivalence boundary increases the risk of introducing bias, which can lead to power loss and Type I error inflation when bias exists.  
                                               <br><br>
                                               Conversely, a smaller equivalence boundary makes the borrowing decision more challenging, potentially being too conservative and reducing the power gain under the exchangeability assumption. Therefore, in this section, we propose a method to determine the optimal equivalence boundary based on constraints for Type I error and power.  
                                               <br><br>
                                               Before calculating, users need to specify the constraints for Type I error and power, i.e., the <strong>Lower Boundary of Power</strong> and the <strong>Upper Boundary of Type I Error</strong>.  
                                               <br><br>
                                               Once all parameters are appropriately specified, click the <strong>FIND THE OPTIMAL EQUIVALENCE BOUNDARY</strong> button. The results will be displayed in a dedicated output tab, which includes the equivalence boundaries ensuring global control of power and Type I error, the optimal equivalence boundary, and the operating characteristics of the corresponding hybrid design.
                                          ")
                                         
                                  )),
                                  
                                  fluidRow(column(12,
                                                  br(),
                                                  actionButton("actionButtonOptimalEQ", "Find the optimal equivalence boundary"),
                                                  add_busy_bar(color = '#848482', centered = TRUE, height = "16px"),
                                                  align = "center",
                                                  style = "margin-bottom: 10px;",
                                                  style = "margin-top: -10px;"
                                  )))
                      )
                      ),
             tabPanel(
               title = "Reference",
               icon = icon("book", verify_fa = FALSE),
               fluidPage(
                 fluidRow(
                   column(12,
                          align = 'center',
                          wellPanel(
                            align = 'left',
                            HTML("<p><strong>Reference:</strong></p>
                                  <p>Xu, J., Shen, A., van Eijk, R. P. A., Wong, W. K., Nelson, L. M., Tian, L., & Lu, Y. (2025). 
                                  <em>Optimal Two-Step Hybrid Design: A Framework for Equivalence Boundary Selection Under Type I Error and Power Constraints.</em></p>"),
                            HTML("<p>Xu, J., van Eijk, R. P. A., Ellis, A., Pan, T., Nelson, L. M., Roes, K. C. B., van Dijk, M., Sarno, M., van den Berg, L. H., Tian, L., & Lu, Y. (2025). 
                                  <em>On the two-step hybrid design for augmenting randomized trials using real-world data.</em> 
                                  arXiv preprint <a href='https://arxiv.org/abs/2501.12453' target='_blank'>arXiv:2501.12453</a>.</p>")
                            
                            
                          )
                   )
                 )
               )
             )
             ) # navbarPage End
  ) # fluidPage End
) # Shiny UI End
