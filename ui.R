


library(shiny)


ui <- fluidPage(
  
  numericInput("b", "b",
    2.5, min = .Machine$double.eps,
  ),
  
  numericInput("m", "m",
    2, min = .Machine$double.eps,
  ),
  
  numericInput("fmin", "fmin",
    10, min = .Machine$double.eps,
  ),
  numericInput("fmax", "fmax",
    400, min = .Machine$double.eps,
  ),
  numericInput("nmin", "nmin",
    11, min = .Machine$double.eps,
  ),
  numericInput("nmax", "nmax",
    60, min = .Machine$double.eps,
  ),
  numericInput("min.acceptable.eff.ring.size", "min.acceptable.eff.ring.size",
    5, min = .Machine$double.eps,
  ),
  
  gt_output(outputId = "table"),


    mainPanel(
      plotOutput("distPlot")
    ),
  
)

