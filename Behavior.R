library(readxl)
library(dplyr)
library("Hmisc")
library(corrplot)


#Read in the data
path_to_data <- '/Users/jonask/fMRI/TWCF2/Behavioral_Data_Master.xlsx'
twcf <- read_excel(path_to_data)

#Exclusions
twcf <- filter(twcf,twcf$Complete==1)
intel <- select(twcf,`Verbal Comprehension`,`Perceptual Reasoning`,`Working Memory`,`Processing Speed`,`FSIQ`,`Perceiving`,`Facilitating`,`Understanding`,`Maging`,`Emotiol Intelligence`)

#Correlations
res2 <- rcorr(as.matrix(intel))
corrpvalues <- p.adjust(res2$P,"bonferroni")
dim(corrpvalues)<-c(10,10)
res2$P <- corrpvalues

corrplot(res2$r, type="upper", order="hclust", p.mat = res2$p, sig.level = 0.01, insig = "blank",tl.cex=0.7)

#PERCEIVING
#FACILITATING
#UNDERSTANDING
#MANAGING
#Predicting IQ with the four MSCEIT subscales
model <- lm(FSIQ ~ `Perceiving` + `Maging` + `Understanding` + `Facilitating`, data = twcf)
summary(model)

#Predicting EQ with the fourt WAIS subscales
model <-lm(`Emotiol Intelligence` ~ `Verbal Comprehension` + `Working Memory` + `Perceptual Reasoning` + `Processing Speed`, data=twcf)

model <- lm(`FSIQ` ~ `Empathic Concern` +	`Persol Distress` +	`Perspective Taking` + 	`Fantasizing`, data = twcf)
