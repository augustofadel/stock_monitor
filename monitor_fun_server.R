
# MAIN FUNCTION -----------------------------------------------------------

monitor <- function(
   t_start = "09:45:00",                                            #horario inicio monitoramento
   t_stop = "18:30:00",                                             #horario fim monitoramento
   t_hold = 60 * 1,                                                 #intervalo entre consultas (em segundos)
   # data_path = "~/!filesync/stock_monitor_data",                    #caminho arquivos (dados e log)
   data_path = "/home/augustofadel/stock_monitor_data",
   data_file = paste0(today() %>% format("%y%m%d"), ".rds"),        #nome arquivo dados
   data_log = paste0("log_", today() %>% format("%y%m%d"), ".txt"), #nome arquivo log
   cod = c(                                                         #codigos dos ativos (symbols)
      "CSNA3.SA", 
      "USIM5.SA",
      "GOAU4.SA",
      "JBSS3.SA",
      
      "GGBR4.SA", 
      "PETR4.SA",
      "ELET3.SA",
      "BRFS3.SA",
      "EMBR3.SA"
   ),
   set_up = 1,                                                      #set point de alta
   set_down = 1,                                                    #set point de baixa
   set_delta = 1,                                                   #set alerta 'continuo'
   mail_from = "stockadvisor.alfa@gmail.com",                       #email envio
   mail_to = c("augustofadel@gmail.com")                            #lista destinatarios
) {
   
   t_start_hold <- t_start %>% hms() - Sys.time() %>% format("%T") %>% hms()
   if (t_start_hold %>% as.numeric() > 0) {
      cat("\n", format(Sys.time(), "%T"), "> aguardando abertura... ")
      Sys.sleep(t_start_hold %>% as.numeric())
      cat("concluido.\n")
   } #end if
   
   dat <- data_load(data_path, data_file, t_start)

   set_points <- data.frame(
      up = rep(set_up, length(cod)),
      down = rep(set_down, length(cod)),
      row.names = cod
   )
   set_points <- set_points[order(row.names(set_points)),]
   
   t_start_collect <- Sys.time() %>% format("%T")
   
   while (Sys.time() %>% format("%T") %>% hms() < t_stop %>% hms()) {
      t <- system.time({
         dat <- try(
            data_bind(cod, dat, data_path, data_log),
            silent = T
            # outFile = file.path(data_path, data_log)
         )
         if (class(dat) == "try-error") {
            break
         } #end if
         
         news <- alert(dat, set_points, set_delta)
         txt <- news$txt
         set_points <- news$set_points
         if (!is.null(txt)) {
            txt <- paste0(txt, "\n\nObs.: Informacoes com atraso de 15 minutos.")
            # for (i in mail_to[1]) {
            #    msg <-
            #       mime() %>% 
            #       to(i) %>% 
            #       from(mail_from) %>% 
            #       subject("ALERTA") %>% 
            #       text_body(txt) %>% 
            #       as.character()
            #    try(
            #       send_message(msg),
            #       silent = T
            #       # outFile = file.path(data_path, data_log)
            #    )
            # } #end for
            send.mail(
              from = mail_from,
              to = mail_to[1],
              subject = "ALERTA",
              body = txt,
              smtp = list(
                host.name = "smtp.gmail.com",
                port = 465,
                user.name = mail_from,
                passwd = "b5gz2252",
                ssl = T
              ),
              authenticate = TRUE,
              send = TRUE)
         } #end if
         
         dat <- data_clean(dat, data_path, data_file)
         saveRDS(dat, file.path(data_path, data_file))
      })#end system.time
      
      if (t_hold > t[[3]]) {
         cat("\n", Sys.time() %>% format("%T"), "> aguardando... ")
         Sys.sleep(t_hold - t[[3]])
         cat("concluido.\n")
      } #end if
   } #end while
   
   if (format(Sys.time(), "%T") %>% hms() >= t_stop %>% hms()) {
      txt <- daily_summary(dat, cod)
      txt <- paste0(txt, '\n\nObs.: O horarios de maximo e minimo sao aproximados (atraso de ate 1 minuto).')
      # for (i in mail_to) {
      #    msg <-
      #       mime() %>% 
      #       to(i) %>% 
      #       from(mail_from) %>% 
      #       subject(paste("Informe diario:", format(Sys.time(), "%d/%m/%Y"))) %>% 
      #       text_body(txt) %>% 
      #       as.character()
      #    try(
      #       send_message(msg),
      #       silent = T
      #       # outFile = file.path(data_path, data_log)
      #    )
      # } #end for
      send.mail(
        from = mail_from,
        to = mail_to,
        subject = paste("Informe diario:", format(Sys.time(), "%d/%m/%Y")),
        body = txt,
        smtp = list(
          host.name = "smtp.gmail.com",
          port = 465,
          user.name = mail_from,
          passwd = "b5gz2252",
          ssl = T
        ),
        authenticate = TRUE,
        send = TRUE)
   } else {
      #monitor(mail_to = c("augustofadel@gmail.com", "dllima@petrobras.com.br"))
   } #end if-else
   cat("\n", Sys.time() %>% format("%T"), " > Encerrada coleta iniciada ", t_start_collect, ".", sep = "")
   
} #end function


# AUX FUNCTIONS -----------------------------------------------------------

data_load <- function(
  data_path = data_path,
  data_file = data_file,
  t_start = t_start
) {
   if (!file.exists(file.path(data_path, data_file))) {
      dat <- NULL
   } else {
      if (t_start %>% hms() > format(Sys.time(), "%T") %>% hms()) {
         stop("Arquivo com dados de ", format(Sys.time(), "%d/%m/%Y"), 
              " encontrado (", format(Sys.time(), "%T"), ").")
      } #end if
      dat <- readRDS(file.path(data_path, data_file))
   } #end if-else
   return(dat)
} #end function

data_bind <- function(
   cod = cod,
   dat = dat,
   data_path = data_path,
   data_log = data_log,
   loop_lim = 60
) {
   tmp <- NULL
   i <- 0
   while(class(tmp) != "data.frame" & i <= loop_lim) {
      i <- i + 1
      tmp <- try(
         getQuote(cod),
         silent = T
         # outFile = file.path(data_path, data_log)
      )
      if (class(tmp) == "try-error") {
         Sys.sleep(10)
      } #end if
   } #end while
   tmp <- cbind(Symbol = rownames(tmp) %>% str_sub(1, 5), tmp)
   rownames(tmp) <- NULL
   dat <- rbind(dat, tmp)
   return(dat)
} #end function

data_clean <- function(
   dat = dat,
   data_path = data_path,
   data_file = data_file
) {
   dat <- dat[!duplicated(dat),]
   t_last_quote <- 
      dat$`Trade Time` %>% 
      format("%d%m%Y") %>% 
      dmy() %>% 
      tapply(dat$Symbol, max) %>% 
      as_date() %>% 
      format("%d%m%Y") %>% 
      dmy()
   if (all(t_last_quote == today())) {
      dat <- 
         dat %>% 
         filter(dat$`Trade Time` %>% format("%d%m%Y") %>% dmy() == today())
   } #end if
   return(dat)
} #end function

alert <- function(
   dat = dat,
   set_points = set_points,
   set_delta = set_delta
) {
   txt <- NULL
   n <- nrow(set_points)
   last_quote <-
      dat %>% 
      tail(n)
   ord <- order(last_quote$Symbol)
   last_quote <- last_quote[ord,]
   up <- last_quote$`% Change` >= set_points$up & last_quote$`Trade Time` %>% format("%d%m%Y") %>% dmy() == today()
   set_points$up[up] <- last_quote$`% Change`[up] %>% floor() + set_delta
   down <- last_quote$`% Change` <= set_points$down * -1  & last_quote$`Trade Time` %>% format("%d%m%Y") %>% dmy() == today()
   set_points$down[down] <- last_quote$`% Change`[down] %>% abs() %>% floor() + set_delta
   if (any(up)) {
      txt <- 
         paste(
            txt, 
            paste0(
               last_quote$Symbol[up], 
               " (", 
               last_quote$Last[up],
               ", ",
               last_quote$`% Change`[up] %>% round(2), 
               "%, ", 
               last_quote$`Trade Time`[up] %>% format("%T"), 
               ")"
            ) %>% 
               paste(collapse = "\n"), 
            sep = "\n"
         )
   } #end if
   if (any(down)) {
      txt <- 
         paste(
            txt, 
            paste0(
               last_quote$Symbol[down], 
               " (", 
               last_quote$Last[down], 
               ", ",
               last_quote$`% Change`[down] %>% round(2), 
               "%, ", 
               last_quote$`Trade Time`[down] %>% format("%T"), 
               ")"
            ) %>% 
               paste(collapse = "\n"), 
            sep = "\n"
         )
   } #end if
   return(list(txt = txt, set_points = set_points))
} #end function

daily_summary <- function(
   dat = dat,
   cod = cod
) {
   txt <- NULL
   for (i in cod %>% str_sub(1, 5)) {
      aux <- dat %>% filter(Symbol == i)
      min_i <- aux[which.min(aux$Low),]
      max_i <- aux[which.max(aux$High),]
      # txt_tmp <- c(
      #    paste0(i, ': ', aux$Open %>% tail(1)), 
      #    paste0('MIN (', min_i$Low, "@", min_i$`Trade Time` %>% format("%T"), ')'),
      #    paste0('MAX (', max_i$High, "@", max_i$`Trade Time` %>% format("%T"), ')'),
      #    aux$Last %>% tail(1)
      # )
      txt_tmp <- c(
        format(Sys.time(), "%d/%m/%Y"),
        i,
        aux$Open %>% tail(1),
        min_i$Low,
        min_i$`Trade Time` %>% format("%T"),
        max_i$High, 
        max_i$`Trade Time` %>% format("%T"),
        aux$Last %>% tail(1)
      )
      if (max_i$`Trade Time` %>% format("%T") %>% hms() < min_i$`Trade Time` %>% format("%T") %>% hms()) {
         txt_tmp <- txt_tmp[c(1, 2, 3, 6, 7, 4, 5, 8)]
      }
      txt_tmp <- txt_tmp %>% paste(collapse = ',')
      txt <- paste(txt, txt_tmp, sep = '\n')
   }
   return(txt)
} #end function

# PACKAGES ----------------------------------------------------------------

packages.list <- c('dplyr', 'quantmod', 'lubridate', 'stringr', 'mailR', 'httr')
new.packages <- packages.list[!(packages.list %in% installed.packages()[,'Package'])]
if(length(new.packages)) {
   install.packages(new.packages, repos = 'https://cran.fiocruz.br/')
}
lapply(packages.list, require, character.only = TRUE)

# httr::set_config(config(ssl_verifypeer = 0L))

# CALL --------------------------------------------------------------------

# Sys.setenv(TZ = "America/Sao_Paulo")
monitor(mail_to = c("augustofadel@gmail.com", "dllima@petrobras.com.br", "dllsxl@gmail.com"))
# monitor(mail_to = c())