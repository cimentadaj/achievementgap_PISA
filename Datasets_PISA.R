if (!require(devtools)) {
    install.packages("devtools")
    require(devtools)
}
# install_github("pbiecek/PISA2012lite")
# load it

library("PISA2012lite")
library(Hmisc)
cntrylist <- list(student=student2012, parent=parent2012, school=school2012, cognitive=scoredItem2012)

means <- by(student2012[, c("PV1MATH", "W_FSTUWT")], student2012$CNT,
            function(x) wtd.quantile(x[,1], x[,2], probs = c(0.25,0.75)))

meansx <- by(student2012[, c("PV1MATH", "W_FSTUWT")], student2012$CNT,
            function(x) wtd.mean(x[,1], x[,2]))

library(car)
library(arm)
library(purrr)
library(dplyr)
library(tidyr)

student2012$ST17Q01 <- as.numeric(student2012$ST17Q01)
student2012$ST17Q01 <- recode(student2012$ST17Q01,"7:8 = NA; 1:2 = 1")
student2012$ST17Q01 <- as.factor(student2012$ST17Q01)


r.sq2 <- by(student2012[, c("PV1MATH", "ST17Q01")], student2012$CNT,
   function(x) summary(lm(PV1MATH ~ ST17Q01, x))$adj.r.squared)


means2 <- as.data.frame(do.call(rbind, means))
means2$cntr <- row.names(means2); means2 <- means2[,c("cntr","25%","75%")]
row.names(means2) <- 1:nrow(means2)

means2 <- cbind(means2, mean=as.numeric(meansx), r.sq=as.numeric(r.sq2))
means2$Diff <- means2[,"75%"] - means2[,"25%"]

means2 <- means2[order(means2$Diff), ]


means3 <- means2$Diff; names(means3) <- row.names(means2)
dotchart(means3, pch = 19)

means4 <- means2$r.sq; names(means4) <- means2$cntr;
dotchart(sort(means4), pch=19)



ggplot(means2, aes(mean, Diff, color=r.sq)) + geom_smooth(colour="blue",method="lm", se=F) +
    geom_point(size=4, alpha=0.5) +
    geom_smooth(colour="red",se=F) +
    scale_color_continuous(low="red",high = "green") +
    annotate("text", x=375, y=175, label=paste("R of",
    round(cor(means2$mean, means2$Diff),2)), size=5.5)

country_model <- function(df) {
    lm(PV1MATH ~ ST17Q01, df)
}

data <- student2012 %>%
    group_by(CNT) %>%
    nest() %>%
    mutate(mod = data %>% map(country_model))

data2 <- data %>%
    mutate(
        tidy = map(mod, broom::tidy),
        glance = map(mod, broom::glance),
        rsq = glance %>% map_dbl("r.squared"),
        augment = map(mod, broom::augment)
    )