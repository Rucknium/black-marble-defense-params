


library(data.table)
library(shiny)
library(gt)

pass.to.shiny <- readRDS("data/best-fee-ring-size-shiny-objects.rds")

for (i in names(pass.to.shiny)) {
  assign(i, pass.to.shiny[[i]])
  pass.to.shiny[[i]] <- NULL
}

rm(pass.to.shiny)




prunable.bpp <- function(x) {
  x <- cut(x, breaks = c(0, 2, 4, 8, 16), labels = c("7", "8", "9", "10"))
  x <- 32 * 2 * as.numeric(as.character(x))
  # For L and R
  # Two outputs: 32 * 2 * 7
  # 3-4 outputs: 32 * 2 * 8
  # 5-8 outputs: 32 * 2 * 9
  # 9-16 outputs: 32 * 2 * 10
  # (Times 2 since there is one 32-byte string each for L and R)
  x + 32 * 6
  # 32 * 6 because there are 32 bytes each for A, A1, B, r1, s1, and d1
}




C_u <- function(f, n) {
  
  ring.size.sim <- n
  
  non.spam.fingerprint.tx.sim.tabulated.weight[, tx_weight_bytes.sim :=
      N * (tx_weight_bytes - mean.size.16.ring.input * number_of_inputs +
          number_of_inputs * mean.size.one.ring.member * ring.size.sim)]
  
  mean.non.spam.kb.per.block <-
    (sum(non.spam.fingerprint.tx.sim.tabulated.weight[, tx_weight_bytes.sim])/num.blocks.before.spam)
  # Coinbase txs are excluded
  
  mean.non.spam.kb.per.block * f
  
}



C_d <- function(n, m) {
  
  tb <- 1e+12
  
  ring.size.sim <- n
  
  non.spam.fingerprint.tx.sim.tabulated.size[, tx_size_bytes.sim :=
      N * (tx_size_bytes - mean.size.16.ring.input * number_of_inputs + number_of_inputs * mean.size.one.ring.member * ring.size.sim)]
  
  mean.non.spam.kb.per.block <- mean.coinbase.tx.size +
    (sum(non.spam.fingerprint.tx.sim.tabulated.size[, tx_size_bytes.sim])/num.blocks.before.spam)
  # Keep coinbase tx since that has to be stored
  
  bytes <- mean.non.spam.kb.per.block
  
  m * bytes * n.nodes * (ssd.1.tb.usd / usd.xmr.rate) / tb
  
}

C_u <- Vectorize(C_u)
C_d <- Vectorize(C_d)
# Have to Vectorize() these because otherwise the arguments will be
# expanded as vectors incorrectly when working on the blockchain dataset


ref.tx.size.cost <- function(f, n) {
  
  stopifnot(length(f) == length(n))
  
  tx.size <- ref.tx.tx_size_bytes - mean.size.16.ring.input * 2 + 2 * mean.size.one.ring.member * n
  
  tx.weight <- ref.tx.tx_weight_bytes - mean.size.16.ring.input * 2 + 2 * mean.size.one.ring.member * n
  
  list(size = tx.size, cost = tx.weight * f * usd.xmr.rate)
  
}

# No need to Vectorize() ref.tx.size.cost()




adversary.owned.outputs <- function(b, f, n) {
  2 * b / (f * (975 + 35 * n))
}

eff.ring.size <- function(b, f, n) {
  1 + (n - 1) * r /
    (r + adversary.owned.outputs(b, f, n) )
}

adversary.share.outputs <- function(b, f, n) {
  1 - r /
    (r + adversary.owned.outputs(b, f, n) )
}


CE <- function(f, n, m, b) {
  (C_u(f, n) + C_d(n, m)) /
    eff.ring.size(b, f, n)
}
# Cost effectiveness



status.quo.no.attack.block.size <- C_u(1, 16)
status.quo.no.attack.block.size.one.year <-
  status.quo.no.attack.block.size * 30 * 24 * 365 / 10^9

non.spam.fingerprint.tx.sim.tabulated.in.out[, prunable.data :=
    N * (number_of_inputs * (32 * 3 + 32 * 16) + prunable.bpp(number_of_outputs) ) ]

prunable.data.per.block <- sum(non.spam.fingerprint.tx.sim.tabulated.in.out$prunable.data)/num.blocks.before.spam

status.quo.no.attack.block.size.one.year.pruned <-
  (status.quo.no.attack.block.size - (7/8) * prunable.data.per.block)  * 30 * 24 * 365 / 10^9





perf.model.short <- lm(mean ~ ring_size * inputs + factor(ceiling(log2(outputs))), data = perf.tests)
perf.model.long <- lm(mean ~ ring_size * inputs + log2(ring_size) * log2(inputs) + factor(ceiling(log2(outputs))), data = perf.tests)

nested.model.test <- anova(perf.model.short, perf.model.long, test = "F")

print(nested.model.test)

stopifnot(nested.model.test[["Pr(>F)"]][2] == 0)
# nested.model.test[["Pr(>F)"]][2] == 0 means p value is < 2.2e-16


alice.best.response <- function(b, f.range, n.range, m = 2, min.acceptable.eff.ring.size = 5, max.cost = 0.05, force.eval.point = NULL, plot = TRUE) {
  
  f.n.range <- expand.grid(f = f.range, n = n.range)
  
  b <- b / (24*30)
  # Convert to budget per block
  
  
  if ( ! is.null(force.eval.point)) {
    
    if (plot == TRUE) {
      stop("Cannot force.eval.point and plot at the same time.")
    }
    
    stopifnot(is.data.frame(force.eval.point))
    stopifnot(all(colnames(force.eval.point) == c("f", "n")))
    
    
    contour.contents <- CE(force.eval.point$f, force.eval.point$n, m, b)
    contour.contents <- contour.contents * 1000 # Convert to millineros
    cost.effectiveness.at.optimum <- contour.contents
    
    unrestricted.min <- force.eval.point
    restricted.min <- force.eval.point
    eff.ring.size.at.optimum <- eff.ring.size(b, force.eval.point$f, force.eval.point$n)
    
    
    
  } else {
    
    
    contour.contents <- CE(f.n.range$f, f.n.range$n, m, b)
    contour.contents <- contour.contents * 1000 # Convert to millineros
    contour.matrix <- matrix(contour.contents, nrow = length(f.range), byrow = FALSE)
    
    eff.ring.size.evaluated <- eff.ring.size(b, f.n.range$f, f.n.range$n)
    
    unrestricted.min <- f.n.range[which.min(contour.contents), ]
    
    restricted.min <- f.n.range[
      eff.ring.size.evaluated >= min.acceptable.eff.ring.size, ][
        which.min(contour.contents[eff.ring.size.evaluated >= min.acceptable.eff.ring.size]), ]
    
    eff.ring.size.at.optimum <- eff.ring.size.evaluated[
      eff.ring.size.evaluated >= min.acceptable.eff.ring.size][
        which.min(contour.contents[eff.ring.size.evaluated >= min.acceptable.eff.ring.size])]
    
    cost.effectiveness.at.optimum <- contour.contents[f.n.range$f == restricted.min$f & f.n.range$n == restricted.min$n]
    
    
  }
  
  if (nrow(restricted.min) == 0) {
    stop("Constraint set is empty. This means no values in f.n.range satisfied the min.acceptable.eff.ring.size.")
  }
  
  
  attack.block.size.at.restricted.min <-
    C_u(restricted.min[, "f"], restricted.min[, "n"]) / restricted.min[, "f"] + # Data contributed by normal users
    b / restricted.min[, "f"] # Data contributed by adversary
  
  no.attack.block.size.at.restricted.min <-
    C_u(restricted.min[, "f"], restricted.min[, "n"]) / restricted.min[, "f"]
  
  no.attack.block.size.at.restricted.min.one.year <-
    no.attack.block.size.at.restricted.min * 30 * 24 * 365 / 10^9
  
  
  non.spam.fingerprint.tx.sim.tabulated.in.out[, prunable.data :=
      N * (number_of_inputs * (32 * 3 + 32 * restricted.min[, "n"]) + prunable.bpp(number_of_outputs) ) ]
  # TODO: explain this computation
  
  prunable.data.per.block <- sum(non.spam.fingerprint.tx.sim.tabulated.in.out$prunable.data)/num.blocks.before.spam
  
  
  no.attack.block.size.at.restricted.min.one.year <- no.attack.block.size.at.restricted.min * 30 * 24 * 365 / 10^9
  no.attack.block.size.at.restricted.min.one.year.pruned <-
    (no.attack.block.size.at.restricted.min - (7/8) * prunable.data.per.block) * 30 * 24 * 365 / 10^9
  # TODO: explain this computation
  
  attack.block.size.at.restricted.min.one.year <- attack.block.size.at.restricted.min * 30 * 24 * 365 / 10^9
  
  
  n.adversary.tx.per.block <- adversary.owned.outputs(b, restricted.min[, "f"], restricted.min[, "n"]) / 2
  
  prunable.data.per.block.adversary <- n.adversary.tx.per.block * (1 * (32 * 3 + 32 * restricted.min[, "n"]) + prunable.bpp(2) )
  # TODO: explain this computation
  
  attack.block.size.at.restricted.min.one.year.pruned <-
    (attack.block.size.at.restricted.min - (7/8) * (prunable.data.per.block + prunable.data.per.block.adversary)) * 30 * 24 * 365 / 10^9
  # TODO: explain this computation
  
  ref.tx.at.optimum <- ref.tx.size.cost(restricted.min[, "f"], restricted.min[, "n"])
  
  predicted.verif.time.ref.tx <- predict(perf.model.long,
    newdata = data.frame(inputs = 2, outputs = 2, ring_size = restricted.min[, "n"]))
  
  
  predicted.verif.time.normal.block <- predict(perf.model.long,
    newdata = non.spam.fingerprint.tx.sim.tabulated.in.out[,
      .(inputs = number_of_inputs, outputs = number_of_outputs, ring_size = restricted.min[, "n"])])
  
  predicted.verif.time.normal.block <- sum(predicted.verif.time.normal.block *
      non.spam.fingerprint.tx.sim.tabulated.in.out$N) / num.blocks.before.spam
  
  
  predicted.verif.time.spam.part.block <- n.adversary.tx.per.block * predict(perf.model.long,
    newdata = data.frame(inputs = 1, outputs = 2, ring_size = restricted.min[, "n"]))
  
  
  # Adversary budget, ring_size, fee_per_byte, effective_ring_size,
  # User's cost for 2in/2out tx,, User's tx size for 2in/2out tx,
  # Block size (no attack), Block size (attack),
  # One year blockchain growth (unpruned, no attack), One year blockchain growth (unpruned, attack),
  # One year blockchain growth (pruned, no attack), One year blockchain growth (pruned, attack)
  
  results.table <- data.table(
    b = b, b_per_day = b * 30 * 24,
    ring_size = restricted.min[, "n"], fee_per_byte = restricted.min[, "f"] * 10^9,
    cost_effectiveness = cost.effectiveness.at.optimum,
    effective_ring_size = eff.ring.size(b, restricted.min[, "f"], restricted.min[, "n"]),
    user_tx_cost_2in_2out = ref.tx.at.optimum$cost, user_tx_size_2in_2out = ref.tx.at.optimum$size,
    block_size_no_attack = no.attack.block.size.at.restricted.min,
    block_size_attack = attack.block.size.at.restricted.min,
    one_year_blockchain_growth_unpruned_no_attack = no.attack.block.size.at.restricted.min.one.year,
    one_year_blockchain_growth_unpruned_attack = attack.block.size.at.restricted.min.one.year,
    one_year_blockchain_growth_pruned_no_attack = no.attack.block.size.at.restricted.min.one.year.pruned,
    one_year_blockchain_growth_pruned_attack = attack.block.size.at.restricted.min.one.year.pruned,
    verif_time_2in_2out = predicted.verif.time.ref.tx * 10^-3, # Times 10^-3 converts to seconds
    verif_time_no_attack = predicted.verif.time.normal.block * 10^-3,
    verif_time_attack = (predicted.verif.time.normal.block + predicted.verif.time.spam.part.block)* 10^-3
  )
  
  
  
  if (! plot) {
    return(results.table)
  }
  
  
  
  par(mar = c(10, 4, 4, 2) + 0.1)
  
  filled.contour(x = f.range, y = n.range, z = contour.matrix,
    main = paste0("Most cost-effective minimum fee and ring size, given adversary budget of ",
      round(b * 24*30, 2), " XMR/day"),
    # TODO list adversary budget in title
    ylab = "Nominal ring size",
    key.title = title(main = "Cost\nEffectiveness\n(lower is better)",
      cex.main = 1, font.main = 1, xpd = NA),
    # non-bold font. xpd = NA means that title isn't clipped.
    ylim = c(min(n.range) - 1, max(n.range) + 1),
    xlim = c(min(f.range) - mean(f.range) * 0.05, max(f.range) + mean(f.range) * 0.05),
    plot.axes = {
      mtext(side = 1, line = 8.5, adj = 0,
        text =
          paste0("Adversary XMR budget per day: ", round(b * 24*30, 2),
            "\nEffective ring size: ", round(eff.ring.size.at.optimum, 2),
            ", Adversary share of outputs: ", round(adversary.share.outputs(b, restricted.min[, "f"], restricted.min[, "n"]), 2),
            "\nAggregate XMR cost to transacting users per block: " , round(C_u(restricted.min[, "f"], restricted.min[, "n"]), 3),
            "\nAggregate XMR cost to node operators per block with normal tx volume: ", round(C_d(restricted.min[, "n"], m), 3),
            "\n2in/2out tx size at optimum: ", round(ref.tx.at.optimum$size, 0),
            " bytes, 2in/2out tx cost at optimum: ", round(ref.tx.at.optimum$cost * 100, 2), " USD cents",
            "\nOne year blockchain growth with normal tx volume unpruned: ",
            round(no.attack.block.size.at.restricted.min.one.year), " GB, pruned: ",
            round(no.attack.block.size.at.restricted.min.one.year.pruned), " GB",
            "                github.com/Rucknium"))
      legend("bottomright",
        legend = c("Optimal fee and ring size, unrestricted", "Optimal fee and ring size, restricted",
          "Minimum acceptable effective ring size"), xpd = NA, inset = c(-0.175, -0.15),
        pch = c(17, 16, 15, NA), lty = c(NA, NA, NA, 1),
        col = c("blue", "green4", "red", "black"), pt.cex = c(2, 4, 2, NA), lwd = 2, cex = 1.1)
      title(xlab = "Minimum fee (nanoneros per byte)", line = 2.25)
      x.axis.ticks <- seq(min(f.range), max(f.range), by = 2e-08)
      axis(1, at = x.axis.ticks, labels = x.axis.ticks * 10^9)
      axis(2)
      contour.contents.2 <- eff.ring.size(b, f.n.range$f, f.n.range$n)
      contour.matrix.2 <- matrix(contour.contents.2, nrow = length(f.range), byrow = FALSE)
      contour(x = f.range, y = n.range, z = contour.matrix.2, method = "edge",
        levels = min.acceptable.eff.ring.size, vfont = c("sans serif", "bold"),
        labcex = 1.75, add = TRUE)
      
      points(restricted.min, pch = 16, col = "green4", cex = 4, xpd = NA)
      points(unrestricted.min, pch = 17, col = "blue", cex = 2, xpd = NA)
     # points(minimizers.at.min.eff.ring.size, pch = 15, col = "red", cex = 2, xpd = NA)
      
     # lines(alice.budget.line)
      
      
    }
  )
  
  return(results.table)
  
}




latex.table <- function(x, title = NULL, output = "", label = "", source.note = NULL, add.row.color = NULL) {
  
  latex.output <- as.data.frame(x) |>
    gt() |>
    cols_label(
      b_per_day = "Adversary XMR budget per day",
      ring_size = "Nominal ring size",
      fee_per_byte = "Min fee (nanoneros per byte)",
      cost_effectiveness = "CE",
      effective_ring_size = "Effective ring size",
      user_tx_size_2in_2out = "Size of 2in/2out (bytes)",
      user_tx_cost_2in_2out = "User's cost to send 2in/2out (USD cents)",
      block_size_no_attack = "Block size (KB)",
      block_size_attack = "Block size (KB)",
      one_year_blockchain_growth_unpruned_no_attack = "Unpruned",
      one_year_blockchain_growth_unpruned_attack = "Unpruned",
      one_year_blockchain_growth_pruned_no_attack = "Pruned",
      one_year_blockchain_growth_pruned_attack = "Pruned",
      verif_time_2in_2out = "Seconds to verify 2in/2out",
      verif_time_no_attack = "Seconds to verify txs in block",
      verif_time_attack = "Seconds to verify txs in block"
    ) |>
    fmt_number(columns = matches("(blockchain)|(fee_per_byte)|(block_size)|(user_tx_size_2in_2out)"), decimals = 0) |>
    fmt_number(columns = matches("(b_per_day)|(effective_ring_size)|(user_tx_cost)|(verif_time_no_attack)|(verif_time_attack)|(cost_effectiveness)"), decimals = 2) |>
    fmt_number(columns = matches("(verif_time_2in_2out)"), decimals = 3) |>
    tab_header(title = title) |>
    tab_spanner(label = "One year blockchain growth (GB)", columns = starts_with("one_year_blockchain_growth")) |>
    tab_spanner(label = "Normal tx volume", columns = matches("no_attack")) |>
    tab_spanner(label = "Normal tx volume + black marble flooding", columns = matches("([^n][^o]_attack)|(effective_ring_size)")) |>
    # "we build spanners from the bottom up" https://gt.rstudio.com/reference/tab_spanner.html
    tab_source_note(source_note = source.note)
  
  latex.output
  
}






server <- function(input, output, session) {
  
  react.table <- reactiveVal(NULL)
  
  
  output$distPlot <- renderPlot({
  
    f.range <- seq(input$fmin * 1e-09, input$fmax * 1e-09, length.out = 40)
    n.range <- seq(input$nmin, input$nmax, by = 1)
    n.range.whittle <- unique(round(seq(1, length(n.range),  length.out = 50)))
    n.range <- n.range[n.range.whittle]
    # If more than 50 different ring sizes, then whittle down the number of elements
    # that have to be computed
    
    alice.results <- alice.best.response(b = input$b, f.range, n.range, m = input$m,
      min.acceptable.eff.ring.size = input$min.acceptable.eff.ring.size,
      max.cost = 0.05, force.eval.point = NULL, plot = TRUE)
    
    alice.results <- alice.results[, .(
      b_per_day, ring_size, fee_per_byte, cost_effectiveness,
      user_tx_size_2in_2out = user_tx_size_2in_2out,
      user_tx_cost_2in_2out = user_tx_cost_2in_2out * 100,
      verif_time_2in_2out,
      block_size_no_attack = block_size_no_attack / 1000,
      verif_time_no_attack,
      one_year_blockchain_growth_unpruned_no_attack,
      one_year_blockchain_growth_pruned_no_attack,
      one_year_blockchain_growth_unpruned_attack,
      one_year_blockchain_growth_pruned_attack,
      effective_ring_size,
      block_size_attack = block_size_attack / 1000,
      verif_time_attack
    )]
    
    isolate(react.table(rbind(alice.results, react.table())))
    
    },
    width = 800, height = 900)
  
  
  output$table <- render_gt(expr = latex.table(react.table()))
  
}


