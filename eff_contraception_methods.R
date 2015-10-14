library("ggplot2")
library("reshape")

# source of data is Contraceptive failure in the United States.
# Trussell,J. Contraception,2011,Vol.83(5),p.398
# chance of failure is after 1 year with typical use

# probability of failure in one year
# "Percentage of women experiencing an unintended pregnancy during the first
# year of typical use, United States"

no.method <- 0.85
spermicide <- 0.28
fertility.aware <- 0.24
withdrawal <- 0.22
sponge.parous <- 0.24
sponge.n.parous <- 0.12
condom.female <- 0.21
condom.male <- 0.18
diaphram <- 0.12
pill <- 0.09
nuvaring <- 0.09
depoprovera <- 0.06
iuc.paragard <- 0.008
iuc.mirena <- 0.002
implanon <- 0.0005
female.ster <- 0.005
male.ster <- 0.0015

# prob of getting 1 or greater failures in N years with given prob per method X
FailRate <- function(years, method){
     pbinom(0, years, method, lower.tail=FALSE)
}

# plotted prob of getting 1 or greater failures in N years with given prob per method X
PlotFailRate <- function(years, method){
     rate <<- pbinom(0, 1:years, method, lower.tail=FALSE)
     p <- ggplot(as.data.frame(rate), aes(x = seq(1:length(rate)), y = rate))

     p +
     geom_line(aes(color = rate)) +
     scale_colour_gradient(high = "red")
}

# example input/output
FailRate(4, condom.male)

FailRate(4, condom.male * pill)

PlotFailRate(4, condom.male)

PlotFailRate(4, condom.male * pill)

# create data.frame of all methods in order to plot comparison of long term results
all.methods <- data.frame(condom.female, condom.male, depoprovera, diaphram, female.ster,
                fertility.aware, implanon, iuc.mirena, iuc.paragard, male.ster,
                no.method, nuvaring, pill, spermicide, sponge.n.parous,
                sponge.parous, withdrawal, row.names = "probability")

all.methods <- all.methods[, order(all.methods)]

# create data.frame to include upper tail pbinom of each method over lifetime
# fertility years
lifetime.failure <- as.data.frame(matrix(NA, nrow = 20, ncol = 18))

lifetime.failure[18] <- 1:20

names(lifetime.failure) <- names(all.methods)
names(lifetime.failure)[18] <- "years"

# would prefer to do this without looping, but looped for readability
for (n in 1:17){
  lifetime.failure[, n] <- pbinom(0, 1:20, as.numeric(all.methods[n]), lower.tail=FALSE)
}

# inverse of lifetime.failure is taken as lifetime.success, representing the
# chance of initial pregnancy after N years
lifetime.success <- 1 - lifetime.failure
lifetime.success[18] <- lifetime.failure[18]

# create plot of lifetime.failure by first melting dataframe and then drawing
melted.success <- melt(lifetime.success, id = "years", variable_name = "method")

# draw using cairo for anti-aliasing
png(height=810, width=1440, filename="eff_contraception_methods.png", type="cairo")

p <- ggplot(melted.success, aes(years, value)) +
  geom_line(aes(color = method)) +
  geom_vline(xintercept = 1) +
  theme_bw() +
  labs(title = "Contraceptive Success Without Failure, by Method",
       y = "Probability",
       x = "Years of Continuous Use",
       color = "Method, By Effectiveness")

# print plot to file
p
dev.off()

########## Only 14 lines show up on plot, but 17 methods due to nuvaring/pill,
########## diaphram/sponge.n.parous, fertility.aware/sponge.parous equivalence

# plot.ly integration; export to plot.ly
gg <- ggplotly(p)
