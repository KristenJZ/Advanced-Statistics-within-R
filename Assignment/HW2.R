> setwd("/Users/kristen/Desktop/Life at UNCC/学习/23FALL Advanced Statistics/resources/codebook")
> eclsk <- read.csv("eclsk.csv")
> View(eclsk)

# Anova between eclsk$gen and eclsk$parent.ed.cat
anova.output <- anova(lm(gen~parent.ed.cat, data=eclsk))
anova.output

# Multiple Comparison
pairwise.t.test(eclsk$gen, eclsk$parent.ed.cat, p.adj = "bonf") # post hoc ??/C
pairwise.t.test(eclsk$gen, eclsk$parent.ed.cat, p.adj = "holm") # planned ??/C-1

# Ancova
ancova.output <- anova(lm(gen~income+parent.ed.cat, data=eclsk))
ancova.output

#factorial ANOVA
factorial.anova.output <- anova(lm(read~parent.ed.cat+female+parent.ed.cat:female, data=eclsk))
factorial.anova.output
interaction.plot(x.factor = eclsk$parent.ed.cat, trace.factor = eclsk$female, 
                 response = eclsk$read, fun = mean)
