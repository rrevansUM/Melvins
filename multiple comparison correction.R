#==========================================================#
# title: "Multiple Comparisons Correction with p.adjust"   #
#==========================================================#

#############################
# table 2. tumor vs normal  #
#############################

Input = (
  "Gene     Raw.p
  BMP2      .452
  BMP4      .182
  BMP7      .253
  BMPR1A    .388
  ID1       .008
  ID2       .016
  ID4_1     .742
  ID4_2     .320
  SMAD6     .546
  SMAD7     .498
  GREM1     .237
  IFNA      .521
  IL_1B     .093
  IL_8      .028
  TNF       .358
  TROY      .648
  LGR5      .498
  TFF2      .036
  TFF3      .00001
  CD44      .013
  C_MYC     .00001
  ")

BMP.tumorVnormal <- read.table(textConnection(Input), header=TRUE)

BMP.tumorVnormal <- BMP.tumorVnormal[order(BMP.tumorVnormal$Raw.p), ]

BMP.tumorVnormal$Bonferroni <- p.adjust(BMP.tumorVnormal$Raw.p, 
                                        method = "bonferroni")
BMP.tumorVnormal$BH <- p.adjust(BMP.tumorVnormal$Raw.p, method = "BH")

BMP.tumorVnormal

# plots

X <- BMP.tumorVnormal$Raw.p
Y <- cbind(BMP.tumorVnormal$Bonferroni, 
           BMP.tumorVnormal$BH)

matplot(X, Y,
        xlab = "Raw p-value", 
        ylab = "Adjusted p-value",
        type = "l", asp = 1, col = 1:6, lty = 1, lwd = 2)

legend('bottomright', 
       legend = c("Bonferroni", "BH"),
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col = 1, lty = 2, lwd  = 1)

######################################
# Cancer staging p-value correction  #
######################################

Input <- (
  "stage    Raw.p
  I        0.043
  II       0.484
  III      1.000
  IV       0.016
  ")

staging.p <- read.table(textConnection(Input), header = TRUE)

staging.p <- staging.p[order(staging.p$Raw.p), ]

staging.p$Bonferroni <- p.adjust(staging.p$Raw.p, method = "bonferroni")
staging.p$BH <- p.adjust(staging.p$Raw.p, method = "BH")

staging.p

X <- staging.p$Raw.p
Y <- cbind(staging.p$Bonferroni, staging.p$BH)

matplot(X, Y, 
        xlab = "Raw p-value",
        ylab = "Adjusted p-value", 
        type = "l", asp = 1, col = 1:6, lty = 1, lwd = 2)

legend('bottomright',
       legend = c("Bonferroni", "BH"),
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col=1, lty=2, lwd=1)

#######################################
# body vs lc wilcoxon rank sum tests  #
#######################################

Input = (
  "Gene     Raw.p
  BMP2      .012
  BMP4      .008
  BMP7      .022
  BMPR1A    .032
  ID1       .186
  ID2       .019
  ID4_1     .920
  ID4_2     .763
  SMAD6     .031
  SMAD7     .059
  GREM1     .311
  IFNA      .006
  IL_1B     .018
  IL_8      .0001
  TNF       .030
  TROY      .027
  LGR5      .011
  TFF2      .840
  TFF3      .856
  CD44      .142
  C_MYC     .006
  ")

bodyVlc <- read.table(textConnection(Input), header = TRUE)

bodyVlc <- bodyVlc[order(bodyVlc$Raw.p), ]

bodyVlc$Bonferroni <- p.adjust(bodyVlc$Raw.p, method = "bonferroni")
bodyVlc$BH <- p.adjust(bodyVlc$Raw.p, method = "BH")

bodyVlc

X <- bodyVlc$Raw.p
Y <- cbind(bodyVlc$Bonferroni, bodyVlc$BH)

matplot(X, Y, 
        xlab = "Raw p-value", 
        ylab = "Adjusted p-value", 
        type = "l", asp = 1, col = 1:6, lty = 1, lwd = 2)

legend('bottomright',
       legend = c("Bonferroni", "BH"), 
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col = 1, lty = 2, lwd = 1)

#####################################################
# body tumor vs normal tissue multiple comparisons  #
#####################################################

Input = (
  "Gene     Raw.p
  BMP2      .252
  BMP4      .404
  BMP7      .211
  BMPR1A    .074
  ID1       .495
  ID2       .860
  ID4_1     .597
  ID4_2     .597
  SMAD6     .175
  SMAD7     .175
  GREM1     .860
  IFNA      .159
  IL_1B     .528
  IL_8      .231
  TNF       .348
  TROY      .074
  LGR5      .058
  TFF2      .597
  TFF3      .004
  CD44      .562
  C_MYC     .348
  ")

bodyVnormal <- read.table(textConnection(Input), header = TRUE)

bodyVnormal <- bodyVnormal[order(bodyVnormal$Raw.p), ]

bodyVnormal$Bonferroni <- p.adjust(bodyVnormal$Raw.p, method = "bonferroni")
bodyVnormal$BH <- p.adjust(bodyVnormal$Raw.p, method = "BH")

bodyVnormal

X <- bodyVnormal$Raw.p
Y <- cbind(bodyVnormal$Bonferroni, bodyVnormal$BH)

matplot(X, Y, 
        xlab = "Raw p-value",
        ylab = "Adjusted p-value",
        type = "l", asp = 1, col= 1:6, lty = 1, lwd = 2)

legend('bottomright', 
       legend = c("Bonferroni", "BH"), 
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col = 1, lty = 2, lwd = 1)

#################################################################
# lesser curvature tumor vs normal tissue multiple comparisons  #
#################################################################

Input = (
  "Gene     Raw.p
  BMP2      .065
  BMP4      .029
  BMP7      .008
  BMPR1A    .018
  ID1       .006
  ID2       .003
  ID4_1     .899
  ID4_2     .404
  SMAD6     .074
  SMAD7     .083
  GREM1     .144
  IFNA      .034
  IL_1B     .009
  IL_8      .0001
  TNF       .058
  TROY      .065
  LGR5      .025
  TFF2      .016
  TFF3      .018
  CD44      .006
  C_MYC     .0001
  ")

lcVnormal <- read.table(textConnection(Input),header = TRUE)

lcVnormal <- lcVnormal[order(lcVnormal$Raw.p),]

lcVnormal$Bonferroni <- p.adjust(lcVnormal$Raw.p, method = "bonferroni")
lcVnormal$BH <- p.adjust(lcVnormal$Raw.p, method = "BH")

lcVnormal

X <- lcVnormal$Raw.p
Y <- cbind(lcVnormal$Bonferroni, lcVnormal$BH)

matplot(X, Y, 
        xlab = "Raw p-value",
        ylab = "Adjusted p-value",
        type = "l", asp = 1, col = 1:6, lty = 1, lwd = 2)

legend('bottomright',
       legend = c("Bonferroni", "BH"), 
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col = 1, lty = 2, lwd = 1)

# mixed model multiple comparisons

Input = (
  "Gene     Raw.p
  BMP2      .008
  BMP4      .002
  BMP7      .007
  BMPR1A    .008
  ID1       .078
  ID2       .006
  ID4_1     .686
  ID4_2     .583
  SMAD6     .008
  SMAD7     .012
  GREM1     .338
  IFNA      .006
  IL_1B     .025
  IL_8      .001
  TNF       .019
  TROY      .012
  LGR5      .006
  TFF2      .314
  TFF3      .832
  CD44      .049
  C_MYC     .006
  ")

mixed <- read.table(textConnection(Input), header = TRUE)

mixed <- mixed[order(mixed$Raw.p),]

mixed$Bonferroni<- p.adjust(mixed$Raw.p, method = "bonferroni")
mixed$BH <- p.adjust(mixed$Raw.p, method = "BH")

mixed

X <- mixed$Raw.p
Y <- cbind(mixed$Bonferroni, mixed$BH)

matplot(X, Y, 
        xlab = "Raw p-value",
        ylab = "Adjusted p-value", 
        type = "l", asp = 1, col = 1:6, lty = 1, lwd = 2)

legend('bottomright', 
       legend = c("Bonferroni", "BH"), 
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col = 1, lty = 2, lwd = 1)

###################################
# log(gene_tumor) ~ site p-values #
###################################

Input = (
  "Gene     Raw.p
  BMP2      .0011
  BMP4      .0004
  BMP7      .0023
  BMPR1A    .0013
  ID1       .0484
  ID2       .0036
  ID4_1     .9629
  ID4_2     .9720
  SMAD6     .0036
  SMAD7     .0070
  GREM1     .2287
  IFNG      .0004
  IL_1B     .0049
  IL_8      .00001
  TNF       .0032
  TROY      .0024
  LGR5      .0007
  TFF2      .8477
  TFF3      .9600
  CD44      .0418
  C_MYC     .0009
  ")

logTmodel <- read.table(textConnection(Input), header = TRUE)

logTmodel <- logTmodel[order(logTmodel$Raw.p), ]

logTmodel$Bonferroni <- p.adjust(logTmodel$Raw.p, method = "bonferroni")
logTmodel$BH <- p.adjust(logTmodel$Raw.p, method = "BH")

logTmodel

X <- logTmodel$Raw.p
Y <- cbind(logTmodel$Bonferroni, logTmodel$BH)

matplot(X, Y, 
        xlab="Raw p-value", 
        ylab="Adjusted p-value",
        type="l", asp=1, col=1:6, lty=1, lwd=2)

legend('bottomright', 
       legend = c("Bonferroni", "BH"), 
       col = 1:6, cex = 1, pch = 16)

abline(0, 1, col=1, lty=2, lwd=1)
```

