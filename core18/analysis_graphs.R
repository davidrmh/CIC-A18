library(ggplot2)
svm <- c(-0.013,0,0,-0.058,0,0.02,0.113,-0.053,0.046,0.003,0.028,-0.008,-0.029,0.016,-0.033,0)
mlp <- c(-0.067,-0.006,0,-0.066,0,0,0.054,-0.053,-0.025,-0.027,0.004,-0.045,-0.029,0.054,0.032,0)
c4 <- c(-0.067,-0.006,0,-0.07,-0.039,0.025,0.091,-0.065,-0.024,-0.027,0.01,-0.047,-0.014,0.045,-0.06,0.069)
n <- length(svm)
todos <- c(svm,mlp,c4)
etiquetas <- c(rep("svm",n),rep("mlp",n),rep("c4.5",n))

datos <- data.frame(index = 1:n, excess.return = todos, model = etiquetas)

g <- ggplot(data = datos, mapping = aes(x = model, y = excess.return))

g <- g + geom_boxplot(fill='#A4A4A4', outlier.size = 0) 

g + geom_jitter(shape=16, position=position_jitter(0.2), size= 3.5) + coord_flip()


