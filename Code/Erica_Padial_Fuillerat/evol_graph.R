library(ggplot2)
library(ggrepel)
modelos <- c('kNN_Eu', 'kNN_Mi',
             'kNN_Ma','kNN.k_cv_Eu', 
             'kNN.k_cv_Mi', 'kNN.k_cv_Ma', 
             'kNN.k_cv_ens_Eu',
             'kNN.k_cv_multic_Eu',
             'kNN.k_cv_ova_Eu')
AUC_ROC <- c(0.7692, 0.7550, 0.7444, 0.7884, 0.7880, 0.7764, 0.7386, 0.7889, 0.7889)
df <- data.frame(modelos,AUC_ROC)

png("D:/Escritorio/plot.png", width=1500, height=800)
ggplot(df) +
  aes(x = modelos, y = AUC_ROC, group = 1) + 
  scale_x_discrete(limits=c('kNN_Eu','kNN_Mi','kNN_Ma','kNN.k_cv_Eu', 
                            'kNN.k_cv_Mi', 'kNN.k_cv_Ma', 'kNN.k_cv_ens_Eu',
                            'kNN.k_cv_multic_Eu','kNN.k_cv_ova_Eu')) +
  theme(axis.title.x = element_text(face="bold", size=25),
        axis.title.y  = element_text(face="bold", size=25),
        axis.text.x = element_text(face="bold",size=15),
        axis.text.y = element_text(size=15)) +
  geom_point(size=6, colour="#009999") +
  geom_line(color = '#00FFFF',
            lwd = 2,  
            linetype = 1) + ylim(0.65, 0.85)+ ggtitle("Evolución de la tasa de acierto con kNN") +
  geom_label_repel(aes(label = AUC_ROC),size=5)+
  theme(plot.title = element_text(face="bold", size=25,hjust = 0.5))
dev.off()
  

