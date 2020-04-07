install.packages("readstata13") ##########Pacote para ler dados em formato .dta
install.packages("MatchIt")
install.packages("dplyr")
install.packages("ggplot2")
library(readstata13)            ##########Ativando o pacotes
library(MatchIt)
library(dplyr)
library(ggplot2)

############Dados
local="/home/alexandre/Documentos/PMS/BASE_PNAD2014.dta" ########Local da base de dados
dados=read.dta13(local) ########## Criando base de dados

summary(dados)          ########## Resumo das variaveis
var_cov<-c('GENERO','IDADE','RACA','EDUC','FIN_CRED','ASSIS_TEC','CONS_PROPRIO','D_REGIAO2','Ln_AREA')
dados=dados[,1:11]
dados=na.omit(dados)
############PSM
###########Diferenças em media


##############Estimando um modelo logit
dadesti <- glm(COMP_GOV~GENERO+IDADE+RACA+EDUC+FIN_CRED+ASSIS_TEC+CONS_PROPRIO+D_REGIAO2+Ln_AREA,family=binomial(),data= dados)
summary(esti)

prs_df <- data.frame(pr_score=predict(esti,type='response'),COMP_GOV=dados$COMP_GOV)
png("a.png")
labs <- paste("Participa ou Não da Politica do Governo:", c("Participa", "Não Participa"))
prs_df %>%
  mutate(COMP_GOV = ifelse(COMP_GOV == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~COMP_GOV) +
  xlab("Probabilidade de participar da população") +
    theme_bw()

dev.off()

dados_match<-dados%>%select(POBRE,COMP_GOV,one_of(var_cov)) %>%
    na.omit()

mod_match <- matchit( COMP_GOV~GENERO+IDADE+RACA+EDUC+FIN_CRED+ASSIS_TEC+CONS_PROPRIO+D_REGIAO2+Ln_AREA, method = "nearest", data = dados_match,distance='logit')
dta_m <- match.data(mod_match)
dim(dta_m)


fn_bal <- function(dta, variable) {
    dta$variable <- dta[, variable]
    dta$COMP_GOV <- as.factor(dta$COMP_GOV)
    support <- c(min(dta$variable), max(dta$variable))
    ggplot(dta, aes(x = distance, y = variable, color = COMP_GOV)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}


library(gridExtra)
png("b.png")
grid.arrange(
   fn_bal(dta_m, "IDADE"),
   fn_bal(dta_m, "EDUC"),
   fn_bal(dta_m, "RACA"),
   fn_bal(dta_m, "FIN_CRED"),
   fn_bal(dta_m, "ASSIS_TEC"),
   fn_bal(dta_m, "POBRE"),
   nrow = 3,widths = c(1,1)
)
dev.off()

with(dta_m, t.test(POBRE ~ COMP_GOV))


lm_treat1 <- lm(POBRE ~ COMP_GOV, data = dta_m)
lm_treat2 <- lm(POBRE ~ COMP_GOV+GENERO+IDADE+RACA+EDUC+FIN_CRED+ASSIS_TEC+CONS_PROPRIO+D_REGIAO2+Ln_AREA,,data=dta_m)
