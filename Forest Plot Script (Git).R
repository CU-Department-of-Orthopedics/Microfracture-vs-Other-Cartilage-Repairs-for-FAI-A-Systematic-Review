## Forest Plots 

library(readxl)
library(metafor)
library(esc)

# FP 1 

dat1 <- ...

dat_ef <- esc_mean_sd(
  grp1m = dat1$Gr2_Mean, 
  grp2m = dat1$Gr1_Mean, 
  grp1sd = dat1$Gr2_SD, 
  grp2sd = dat1$Gr1_SD, 
  grp1n = dat1$n2, 
  grp2n = dat1$n1
)

dat1$ef <- dat_ef$es
dat1$ef_se <- dat_ef$se

ma1 <- rma(ef, ef_se, data = dat1, method = "REML")

summary(ma1)

forest(
  x = ma1,
  slab = paste0(dat1$Study, " (", dat1$Tr2, "-", dat1$Tr1, ")"),
  addfit = FALSE,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "Post-Mean Difference Effect Size"
);par(cex=1, font=2); text(-9.25, 7.5, pos=4, cex=0.75, bquote(
  paste("RE Model for All Studies (Q = ",
        .(formatC(ma1$QE, digits=2, format="f")), 
        ", df = ", .(ma1$k - ma1$p),", p = ", 
        .(formatC(ma1$QEp, digits=2, format="f")),
        "; ", I^2, " = ",
        .(formatC(ma1$I2, digits=1, format="f")), 
        "%)", "; ", tau^2 == 
          .(formatC(ma1$tau2, digits=2, format="f")))))

