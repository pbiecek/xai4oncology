kmplot <- function(time, status, group, title = "OS", xlab = "", break.x.by = 1) {
  df <- data.frame(time, status, group)
  sfit <- survfit(Surv(time, status)~group, data = df)

  legend.labs = paste0("value=", levels(factor(group)))

  tmpplot <- survminer::ggsurvplot(sfit,
                                   pval = TRUE, risk.table = TRUE, data = df,
                                   title = zmToPlot,
                                   xlab = xlab, break.x.by = break.x.by,
                                   legend.title = title,
                                   legend.labs = legend.labs
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


