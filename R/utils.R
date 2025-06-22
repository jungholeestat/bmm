
# install.packages("remotes")
# remotes::install_github("hericks/KDE")
library("KDE")

# measure_id: case characteristics
# filter_id: defendant characteristics
# obtain kernel density estimates and confidence intervals
# based on Lepski's method for bandwidth selection / undersmoothing
get_results = function(measure_id, filter_id,
                       u = seq(0, 1, by = 0.01),
                       alpha = 0.05,
                       c = 0.9) {
  
  # load successes and trials
  X = data$numerator[data$measure_id==measure_id & data$filter_id==filter_id]
  t = data$denominator[data$measure_id==measure_id & data$filter_id==filter_id]
  Qhat = X/t; Qhat = Qhat[!is.nan(Qhat)]
  
  n = length(Qhat)
  
  # optimal banwidth chosen via lepski's method
  hhat = goldenshluger_lepski(epanechnikov, Qhat)
  estimator = kernel_density_estimator(epanechnikov, Qhat, bandwidth = hhat)
  
  # density estimates
  phat = estimator$fun(u)
  
  # ci based on undersmoothing
  z = qnorm(1 - alpha/2)
  se = lo = hi = numeric(length(u))
  
  h_under = c * hhat # undersmoothing bandwidth, lower c means more aggressive undersmoothing
  for (j in seq_along(u)) {
    kh = epanechnikov$fun(((Qhat - u[j]) / h_under) / h_under)
    se[j] = sqrt(sum((kh - phat[j])^2) / (n * (n - 1)))
    lo[j] = max(phat[j] - z * se[j], 0)
    hi[j] = phat[j] + z * se[j]
  }
  
  res = list(phat = phat, lo = lo, hi = hi)
}

# helper function for plotting
plot_measure = function(res_list, 
                        u=seq(0,1,0.01), 
                        xlab, pos, legend_loc) {
  ylim = range(c(res_list$pri$phat, res_list$pub$phat, res_list$coun$phat,
                 res_list$pri$lo, res_list$pub$lo, res_list$coun$lo,
                 res_list$pri$hi, res_list$pub$hi, res_list$coun$hi))
  
  # plot density estimates
  plot(u, res_list$pri$p, type = 'l', col = 'black', lwd = 2, ylim = ylim,
       xlab = xlab, ylab = 'Density')
  lines(u, res_list$pub$phat, col = 'red',   lwd = 2)
  lines(u, res_list$coun$phat, col = 'blue',  lwd = 2)
  
  # plot confidence intervals
  polygon(c(u, rev(u)), c(res_list$pri$hi, rev(res_list$pri$lo)), col = rgb(0,0,0,0.2), border = NA)
  polygon(c(u, rev(u)), c(res_list$pub$hi, rev(res_list$pub$lo)), col = rgb(1,0,0,0.2), border = NA)
  polygon(c(u, rev(u)), c(res_list$coun$hi, rev(res_list$coun$lo)), col = rgb(0,0,1,0.2), border = NA)
  
  # plot mean values
  m_pri  = weighted.mean(u, res_list$pri$phat)
  m_pub  = weighted.mean(u, res_list$pub$phat)
  m_coun = weighted.mean(u, res_list$coun$phat)
  abline(v = c(m_pri, m_pub, m_coun), lty = 2, col = c('black','red','blue'))
  
  # annotate mean values
  y_max = max(ylim)
  text(m_pri,  y_max*0.95, sprintf('%.2f', m_pri), col = 'black', pos = pos[1])
  text(m_pub,  y_max*0.95, sprintf('%.2f', m_pub), col = 'red',   pos = pos[2])
  text(m_coun, y_max*0.95, sprintf('%.2f', m_coun), col = 'blue', pos = pos[3])
  
  # create legend
  legend(legend_loc, legend = c('Private','Public','Appointed Counsel'),
         col = c('black','red','blue'), lwd = 2, bty = 'n', cex=1.1)
}