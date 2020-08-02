####################################################################

library(circular)

# Function to simulate a relative orientation change data point for each individual (k = kappa)

vm.diff = function(k) {

# First simulate a random "preferred" direction for the individual

rnd1 = rcircularuniform(1)

# Now, simulate the first 'trial' for this individual with the provided kappa

# Since the orientation can be axial, we pick an angle from a symmetric axial distribution

t1 = rmixedvonmises(1, mu1 = rnd1, mu2 = rnd1 + pi, kappa1 = k, kappa2 = k, prop = 0.5)

# Next, randomly pick a change in stimulus direction (90 or -90)

stim = sample(c(-pi/2, pi/2), 1)

# Simulate a second 'trial' with the new stimulus direction

t2 = rmixedvonmises(1, mu1 = rnd1 + stim, mu2 = rnd1 + pi + stim, kappa1 = k, kappa2 = k, prop = 0.5)

# Subtract heading 1 from heading 2 (“orientation error” [OE] or “orientation change”)

OE = t2 - t1

# Subtract the turn of the polarizer (either 90° or -90°; in the manuscript I’ve called this “relative orientation change” [ROC].)

ROC = OE - stim

# Put them through as.circular I get values 0°–355°

ROC = conversion.circular(ROC, modulo = '2pi')

#  Double the angles (Doubled values are of course 0°–710°)
ROC = 2 * ROC

# Go through as.circular again before the v-test, cutting them back to 0°–355°

ROC = conversion.circular(ROC, modulo = '2pi')

# Return the data point for this individual

return(ROC)

}



# This function requires n = c(n,k); n = sample size, and k = kappa for each simulation.

# The function then simulates 1000 experiments with the above values and returns the

# proportion of significant test results.

vm.sim = function(n){

   p.vals = vector()

   for (i in 1:1000){

      # Setup a vector of circular values, will remove later

      data = circular(0, modulo = '2pi')

      # Simulate an ROC for n individuals

      for (c in 1:n[1]){

         data = c.circular(data, vm.diff(n[2]))

      }

     # Run a v-test with mu = 0 as the a priori mean direction

      p.vals = c(p.vals, rayleigh.test(data[-1], mu = circular(0, modulo = '2pi'))[2])

   }

   return(length(p.vals[p.vals < 0.05])/1000)

}



# Setup table of parameters to test (n and k)

n = rep(c(5,10,20,30,50,100), each = 10)

k = rep(c(0, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2, 5), 6)

data = cbind(n, k)



# Run Sims, takes ~ 15 minutes

p05 = apply(data, 1, vm.sim)

p05[1] = 1 - p05[1]  # This just correct the power for k=0, where the true answer should be p>0.05

data = cbind(data, p05)

data = as.data.frame(data)

data$n = as.factor(data$n)



# plot

library(ggplot2)

p = ggplot(data = data, aes(x = k, y = p05, group = n, color = n)) +

   geom_line(size = 1.5) +

   geom_hline(yintercept = 0.80, linetype = "dashed", color = "red", size = 1.5) +

   theme(axis.text.x=element_text(size=12), axis.text.y=element_text(size=12)) +

   theme(axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) +

   xlab("Kappa") + ylab("Power") +

   scale_x_continuous(limits = c(0,5), expand = c(0, 0)) +

   scale_y_continuous(limits = c(0,1), expand = c(0, 0))
