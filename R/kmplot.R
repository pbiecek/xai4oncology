kmplot <- function(time, status, group,  ...) {
  df <- data.frame(time, status, group)
  sfit <- survfit(Surv(time, status)~group, data = df)

  legend.labs = levels(factor(group)) # paste0("value=", levels(factor(group)))

  tmpplot <- survminer::ggsurvplot(sfit,
                                   pval = TRUE, risk.table = TRUE, data = df,
                                   ...
  )

  tmpplot[[1]] <- tmpplot[[1]] +
    scale_y_continuous(expand = c(0,0),
                       labels = percent_format()) +
    xlab("") +
    theme(legend.position = "none")

  tmpplot[[2]] <- tmpplot[[2]] +
    ylab("")

  tmpplot
}


