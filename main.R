library(tercen)
library(dplyr)

do.loess <- function(df, n_preds, span, mode) {
  if (mode == "smooth"){
    result = loess.smooth(df, n_preds, span)
  } else {
    result = loess.regress(df, span)
  }
  return(result)
}

loess.smooth = function(df, n_preds, span){
  m <- loess(.y ~ .x, data = df, span = span)
  x_pred <- seq(min(df$.x), max(df$.x), length.out = n_preds)
  y_pred <- predict(m, newdata = data.frame(.x = x_pred))
  df_out <- data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    x_pred = x_pred,
    y_pred = y_pred
  )
}

loess.regress = function(df, span){
  m = loess(.y ~ .x, data = df, span = span)
  df_out = data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1],
    xData = df$.x,
    yData = df$.y,
    yFit = predict(m),
    yRes = resid(m)
  )
  if (!is.null(df$labels))
    df_out = data.frame(df_out, labels = df$labels)
  
  return(df_out)
  
}

ctx = tercenCtx()

n_preds <- 1000
if(!is.null(ctx$op.value('n_preds'))) n_preds <- as.numeric(ctx$op.value('n_preds'))
span <- 0.75
if(!is.null(ctx$op.value('span'))) span <- as.numeric(ctx$op.value('span'))
operatorMode = "smooth"
if(!is.null(ctx$op.value('mode'))) mode <- as.character(ctx$op.value('mode'))

df = ctx %>% 
  select(.ci, .ri, .x, .y)

if(length(ctx$labels) ==1){
  df = df %>% 
    bind_cols(ctx$select(ctx$labels)) %>%
    select(.ci, .ri, .x, .y, labels = all_of(ctx$labels[[1]]))
}

result = df %>%
  group_by(.ci, .ri) %>%
  do(do.loess(., n_preds, span, operatorMode))

if(!is.null(result$labels)){
  mapname = paste0(ctx$labels[[1]],".labels")
  result = result %>% rename(!!mapname:=labels)
}
  
result %>%
  ctx$addNamespace() %>%
  ctx$save()
