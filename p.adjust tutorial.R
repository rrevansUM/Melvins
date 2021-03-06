### -----------------------------
### Multiple comparisons example
### -----------------------------

install.packages("FSA")
library(FSA)
library(sciplot)
library(plotrix)

Input = (
  
  "Food              Raw.p
  Blue_fish         .34
  Bread             .594
  Butter            .212
  Carbohydrates     .384
  Cereals_and_pasta .074
  Dairy_products    .94
  Eggs              .275
  Fats              .696
  Fruit             .269
  Legumes           .341
  Nuts              .06
  Olive_oil         .008
  Potatoes          .569
  Processed_meat    .986
  Proteins           .042
  Red_meat           .251
  Semi-skimmed_milk  .942
  Skimmed_milk       .222
  Sweets             .762
  Total_calories     .001
  Total_meat         .975
  Vegetables         .216
  White_fish         .205
  White_meat         .041
  Whole_milk         .039
  ")

Data = read.table(textConnection(Input),header=TRUE)

Data = Data[order(Data$Raw.p),]

Data$Bonferroni = p.adjust(Data$Raw.p, method="bonferroni")
Data$BH = p.adjust(Data$Raw.p, method="BH")
Data$Holm = p.adjust(Data$Raw.p, method="holm")
Data$Hochberg = p.adjust(Data$Raw.p, method="hochberg")
Data$Hommel = p.adjust(Data$Raw.p, method="hommel")
Data$BY = p.adjust(Data$Raw.p, method="BY")

View(Data)

# Plots

X = Data$Raw.p
Y = cbind(Data$Bonferroni,
  Data$BH, Data$Holm,
  Data$Hochberg, 
  Data$Hommel, Data$BY)

matplot(X, Y,
 xlab="Raw p-value",
  ylab="Adjusted p-value",
   type="l", asp=1, col=1:6, lty=1, lwd=2)

legend('bottomright', 
  legend = c("Bonferroni", "BH", "Holm", "Hochberg", "Hommel", "BY"), 
  col = 1:6, cex = 1, pch = 16)

abline(0, 1, col=1, lty=2, lwd=1)
