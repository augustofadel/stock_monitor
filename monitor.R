library(quantmod)
library(lubridate)
library(stringr)
library(gmailr)
library(dplyr)

library(RCurl)
library(httr)
set_config( config( ssl_verifypeer = 0L ) )

hora_inicio <- "09:45:00"
hora_fim <- "18:30:00"
intervalo <- 60 * 5
# dir.sav <- "/home/augusto/Área de Trabalho/filesync/ts/advisor"
dir.sav <- "D:/Users/augusto.fadel/Documents/!filesync/ts/advisor"

cod = c(
  "CSNA3.SA", 
  "USIM5.SA",
  "GOAU4.SA",
  "JBSS3.SA",
  
  "GGBR4.SA", 
  "PETR4.SA",
  "ELET3.SA",
  "BRFS3.SA",
  "EMBR3.SA"
)

set_up = 3 #set point de alta
set_down = 4 #set point de baixa
mail_from = "stockadvisor.alfa@gmail.com"
mail_to_1 = "augustofadel@gmail.com"
mail_to_2 = "dllima@petrobras.com.br"

dat_acum <- NULL
lim_sup <- 0
lim_inf <- 0

espera <- hora_inicio %>% hms() - format(Sys.time(), "%X") %>% hms()
if (espera %>% as.numeric() > 0) {
  cat("\n", format(Sys.time(), "%X"), ": aguardando abertura... ")
  Sys.sleep(espera %>% as.numeric())
  cat("concluido.\n")
}

while (format(Sys.time(), "%X") %>% hms() < hora_fim %>% hms()) {
  t <- system.time({
    dat <- getQuote(cod)
    dat_acum <- rbind(dat_acum, cbind(Symbol = rownames(dat) %>% str_sub(1, 5), dat))
    rownames(dat_acum) <- NULL
    txt <- NULL
    
    if (any((tail(dat_acum, length(cod))$`% Change` > set_up) != !(dat$`% Change` < set_up))) {
      lim_sup <- 0
    }
    
    if (any((tail(dat_acum, length(cod))$`% Change` < set_down * -1) != !(dat$`% Change` > set_down * -1))) {
      lim_inf <- 0
    }
    
    if (lim_sup == 0 & any(dat$`% Change` > set_up)) {
      txt <- paste(txt, paste(rownames(dat)[dat$`% Change` > set_up], collapse = ", "), " (", paste0(dat$Last[dat$`% Change` > set_up], collapse = ", "), "; ", paste0(round(dat$`% Change`[dat$`% Change` > set_up], 2), "%", collapse = ", "), "; ", paste0(str_sub(dat$`Trade Time`, 12, 21)[dat$`% Change` > set_up], collapse = ", "), ").\n", sep = "")
      lim_sup <- 1
    }
    if (lim_inf == 0 & any(dat$`% Change` < set_down * -1)) {
      txt <- paste(txt, paste(rownames(dat)[dat$`% Change` < set_down * -1], collapse = ", "), " (", paste0(dat$Last[dat$`% Change` < set_down * -1], collapse = ", "), "; ", paste0(round(dat$`% Change`[dat$`% Change` < set_down * -1], 2), "%", collapse = ", "), "; ", paste0(str_sub(dat$`Trade Time`, 12, 21)[dat$`% Change` < set_down * -1], collapse = ", "), ").\n", sep = "")
      lim_inf <- 1
    }
    # if (any(abs(dat$Last - dat$Low) == 0)) {
    #   txt <- paste(txt, "\nOs seguintes papeis estao no minimo do dia: ", paste(rownames(dat)[abs(dat$Last - dat$Low) == 0], collapse = ", "), " (", paste(dat$Low[abs(dat$Last - dat$Low) == 0], collapse = " e "), ").\n", sep = "")
    #   send_msg <- 1
    # }
    # if (any(abs(dat$Last - dat$High) == 0)) {
    #   txt <- paste(txt, "\nOs seguintes papeis estao no maximo do dia: ", paste(rownames(dat)[abs(dat$Last - dat$High) == 0], collapse = ", "), " (", paste(dat$High[abs(dat$Last - dat$High) == 0], collapse = " e "), ").\n", sep = "")
    #   send_msg <- 1
    # }
    
    if (!is.null(txt)) {
      txt <- paste0(txt, "\n\nObs.: Informacoes com atraso de 15 minutos.")
      
      msg <-
        mime() %>% 
        to(mail_to_1) %>% 
        from(mail_from) %>% 
        subject("ALERTA") %>% 
        text_body(txt) %>% 
        as.character()
      
      send_message(msg)
    }
    
    saveRDS(dat_acum, file.path(dir.sav, paste0(format(Sys.time(), "%y%m%d"), ".rds")))
  })
  cat("\n", format(Sys.time(), "%X"), ": aguardando... ")
  Sys.sleep(max(0, intervalo - t[[3]]))
  cat("concluido.\n")
}

### Daniel:
txt <- NULL
dat_acum <- dat_acum %>% filter(dat_acum$`Trade Time` %>% str_sub(1,10) == paste0(format(Sys.time(), "%Y-%m-%d")))
# min_idx <- tapply(dat_acum$Low, dat_acum$Symbol, which.min)
# max_idx <- tapply(dat_acum$High, dat_acum$Symbol, which.max)
for (i in cod) {
  aux <- dat_acum %>% filter(Symbol == i)
  min_i <- aux[which.min(aux$Low),]
  max_i <- aux[which.max(aux$High),]
  txt_tmp <- c(
    paste0(i, ': ', aux$Open %>% tail(1)), 
    paste0(min_i$Low, ' (', min_i$`Trade Time` %>% format("%X"), ')'),
    paste0(max_i$High, ' (', max_i$`Trade Time` %>% format("%X"), ')'),
    paste0(aux$Last %>% tail(1), ' (', aux$`Trade Time` %>% tail(1) %>% format("%X"), ')\n')
  )
  if (max_i$`Trade Time` %>% format("%X") %>% hms() < min_i$`Trade Time` %>% format("%X") %>% hms()) {
    txt_tmp <- txt_tmp[c(1, 3, 2, 4)]
  }
  txt_tmp <- txt_tmp %>% paste(collapse = ', ')
  txt <- paste(txt, txt_tmp, sep = '\n')
}

txt <- paste0(txt, '\n\nObs.: O horarios de maximo e minimo sao aproximados (atraso de ate 5 minutos).')

msg <-
  mime() %>% 
  to(mail_to_1) %>% 
  from(mail_from) %>% 
  subject(paste("Informe diario:", format(Sys.time(), "%d/%m/%Y"))) %>% 
  text_body(txt) %>% 
  as.character()

send_message(msg)

# msg <-
#   mime() %>% 
#   to(mail_to_2) %>% 
#   from(mail_from) %>% 
#   subject(paste("Informe diario:", format(Sys.time(), "%d/%m/%Y"))) %>% 
#   text_body(txt) %>% 
#   as.character()
# 
# send_message(msg)



# dat_acum$Last %>% tapply(dat_acum %>% rownames() %>% str_sub(1, 8), max)
# dat_acum$Symbol <- dat_acum %>% rownames() %>% str_sub(1, 8)
# rownames(dat_acum) <- NULL