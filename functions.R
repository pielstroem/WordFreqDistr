
GLDensityPlot = function(sample, word)
{
  x = seq(min(sample),max(sample),0.5)
  g = dnorm(x,
            mean = mean(sample),
            sd = sd(sample)
  )
  b = mean(abs(sample - mean(sample)))
  l = dlaplace(x,
               location = median(sample),
               scale = b
  )
  plot(density(sample),
       main = word,
       xlab = "Word frequency",
       ylim = c(0,max(c(g,l)))
  )
  lines(x,g, col = "red")
  lines(x,l, col = "blue")
}
