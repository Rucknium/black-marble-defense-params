


library(shiny)
library(gt)


ui <- fluidPage(
  
  numericInput("b", "b, the daily budget of the adversary to pay for transaction fees.",
    2.5, min = .Machine$double.eps, width = "100%"
  ),
  
  numericInput("m", "m, the adjustment parameter that raises or lowers total node operators’ costs by a linear factor to adjust for uncertainty about the true number of nodes and to add costs that are more difficult to compute like CPU and RAM use.",
    2, min = .Machine$double.eps, width = "100%"
  ),
  
  numericInput("fmin", "fmin, the minimum fee of the search window. The lower bound of the x axis of the plot.",
    10, min = .Machine$double.eps, width = "100%"
  ),
  numericInput("fmax", "fmax, the maximum fee of the search window. The upper bound of the x axis of the plot.",
    400, min = .Machine$double.eps, width = "100%"
  ),
  numericInput("nmin", "nmin, the minimum ring size of the search window. The lower bound of the y axis of the plot.",
    11, min = .Machine$double.eps, width = "100%"
  ),
  numericInput("nmax", "nmax, the maximum ring size of the search window. The upper bound of the y axis of the plot.",
    60, min = .Machine$double.eps, width = "100%"
  ),
  numericInput("min.acceptable.eff.ring.size", "The minimum acceptable effective ring size. The 'restricted' optimum will be above this level.",
    5, min = .Machine$double.eps, width = "100%"
  ),
  
  numericInput("max.cost", "The maximum budget of Alice, in XMR per day. This is the maximum total cost of aggregate daily fees and storage cost to node operators. The purple solid line forms the budget line.",
    36, min = .Machine$double.eps, width = "100%"
  ),
  
  checkboxInput("trace.min.acceptable.eff.ring.size", "Trace different values of the minimum effective ring size. This prints red numbers on the plot in the position of the best fee and ring size for each minimum effective ring size. This simulates sweeping the minimum effective ring size line upward through the plot.", width = "100%"),
  
  checkboxInput("trace.max.cost", "Trace different values of the best fee and ring size on Alice's budget line. The trace line will be dotted purple in the position of the best fee and ring size for each choice of (lower) Alice budget. This simulates sweeping Alice's budget line downward through the plot.", width = "100%"),
  
  actionButton("submit", "Submit", class = "btn-primary"),
  
  actionButton("reset", "Restore default values"),
  
  gt_output(outputId = "table"),
  
  mainPanel(
    plotOutput("contourPlot")
  ),
  
  
)

