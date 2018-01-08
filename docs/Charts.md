# Графики и диаграммы в YaLedger

В основную функциональность YaLedger не входит построение графиков и диаграмм, т.к. невозможно в программе для домашней бухгалтерии сделать поддержку диаграмм лучшую, чем в программах, предназначенных для построения диаграмм.
Вместо этого, большинство [отчётов][Reports] YaLedger поддерживают вывод в CSV (опция `-C`). Полученные данные можно передавать другим программам для дальнейшей обработки.

Пример возможного результата:

![yaledger charts example][yaledger-charts-edited]

Скрипт для получения подобных диаграмм:

~~~
    #!/bin/bash

    START=12/09/15

    set -e

    cd ~/stats
    yaledger -d error -S $START reg -C --no-currencies карточка > card.csv
    #yaledger -d error -S $START --monthly stats -zC --no-currencies карточка > cardstats.csv
    yaledger -d error -S $START reg -C --no-currencies карман > cash.csv
    #yaledger -d error -S $START --monthly stats -zC --no-currencies карман > cashstats.csv
    yaledger -d error -S $START --monthly saldo -zagC --no-currencies Root/доходы > salary.csv
    yaledger -d error -S $START --monthly saldo -zagC --no-currencies Root/расходы > expences.csv

    sed -i -e "1d" -e "s!BALANCE B/D!BALANCE!" card.csv
    sed -i -e "1d" -e "s!BALANCE B/D!BALANCE!" cash.csv

    R --vanilla < yaledger-charts.R
    eog yaledger-charts.png &
~~~

где `yaledger-charts.R`:

~~~
    library(lubridate)

    source("bollinger.R")
    source("aggregates.R")
    source("stackplot.R")

    meandate <- function(string) {
      if (string == "ACCOUNT") {
          string
      } else {
          from <- as.Date(string, format="From.%Y.%m.%d")
          from
          tmp <- sub("From.[^.]+.till.", "\\1", string)
          to <- as.Date(tmp, format="%Y.%m.%d")
          strftime(mean(c(from,to), na.rm=T), format="%B.%Y")
      }
    }

    describe <- function(x) {
      list(MIN=min(x), MAX=max(x),
           OPEN=head(x, n=1), CLOSE=tail(x, n=1),
           Q1 = quantile(x, 0.25),
           MEDIAN = quantile(x, 0.5),
           Q3 = quantile(x, 0.75))
    }

    salary <- read.table("salary.csv", header=T, sep=";")
    expences <- read.table("expences.csv", header=T, sep=";")

    names(salary) <- Vectorize(meandate)(names(salary))
    names(expences) <- Vectorize(meandate)(names(expences))

    card <- read.table("card.csv", header=T, sep=";")
    cash <- read.table("cash.csv", header=T, sep=";")

    card$DATE <- as.Date(card$DATE, format="%Y/%m/%d")
    card$MONTH <- update(card$DATE, day=15)

    cash$DATE <- as.Date(cash$DATE, format="%Y/%m/%d")
    cash$MONTH <- update(cash$DATE, day=15)

    card.stats <- aggregates(BALANCE~MONTH, data=card, describe)
    cash.stats <- aggregates(BALANCE~MONTH, data=cash, describe)

    ns <- c("MIN", "MAX", "OPEN", "CLOSE", "Q1", "MEDIAN", "Q3")
    card.stats[,ns] <- sapply(card.stats[,ns], as.numeric)
    cash.stats[,ns] <- sapply(cash.stats[,ns], as.numeric)

    #card.stats <- read.table("cardstats.csv", header=T, sep=";")
    #card.stats$DATE <- as.Date(card.stats$FROM, format="%Y/%m/%d")

    #cash.stats <- read.table("cashstats.csv", header=T, sep=";")
    #cash.stats$DATE <- as.Date(cash.stats$FROM, format="%Y/%m/%d")

    png(filename="yaledger-charts.png", width=1900, height=1000, units="px", type="cairo")
    par(xpd=T, mar=par()$mar+c(0,0,0,4), mfrow=c(2,2))

    barplot(as.matrix(subset(salary, select=-c(ACCOUNT))), legend=salary$ACCOUNT, col=heat.colors(5), main="Доходы")
    #barplot(as.matrix(subset(expences, select=-c(ACCOUNT))), legend=expences$ACCOUNT, col=rainbow(20), args.legend=list(bg="transparent"), main="Расходы")
    expences.data <- subset(expences, select=-c(ACCOUNT))
    expences.data$MEAN <- apply(expences.data, 1, mean)
    ixs <- order(-expences.data$MEAN)
    expences.sorted <- subset(expences.data[ixs,], select=-c(MEAN))
    colors <- rainbow(26, s=0.5)[ixs]
    stackplot.rows(expences.sorted, colors=colors, catnames=expences$ACCOUNT[ixs], bg="transparent")
    title("Расходы")

    BollingerBars(card, card.stats, title="Карточка")
    BollingerBars(cash, cash.stats, title="Карман")

    #boxplot(BALANCE~MONTH, data=card, varwidth=T, col=heat.colors(5), outline=F, main="Карточка")
    #boxplot(BALANCE~MONTH, data=cash, varwidth=T, col=rainbow(20), outline=F, main="Карман")

    dev.off()
    warnings()
~~~

`bollinger.R`:

~~~
    band <- function(xs, mins, maxs, ...) {
      polygon(x=c(xs, rev(xs)), y=c(maxs, rev(mins)), ...)
    }

    boxes <- function(xs, mins, maxs, ...) {
      rect(xs-1, mins, xs, maxs, ...)
    }

    ## plot Bollinger Bars for the given OPEN/MIN/MAX/CLOSE data frame
    BollingerBars <- function(data, stats, title=NA) {

      plot.new()
      plot.window(range(c(data$DATE, stats$MONTH), na.rm=T), range(data$BALANCE, na.rm=T))
      grid()
      #par(lend="square")
      band(stats$MONTH, stats$MIN, stats$MAX, col='#eeeeee', border=NA)
      band(stats$MONTH, stats$Q1, stats$Q3, col='#cccccc', border=NA)
      lines(MEDIAN~MONTH, data=stats, col='#777777')
      lines(BALANCE~DATE, data=data, col='blue')

      ## part one: blue bars for the days intraday extension from the higher
      ##      of OPEN and CLOSE to the MAX
      segments(stats$MONTH, apply(stats[,c("OPEN","CLOSE")], 1, max), stats$MONTH, stats$MAX,
               lwd=1)

      ## part two: for winning days where close is higher than open, plot
      ##      green bars showing advance from the open to the close
      ind <- which(stats$CLOSE > stats$OPEN)
      boxes(stats[ind, "MONTH"], stats[ind,"OPEN"], stats[ind,"CLOSE"],
            col="white", border="black")

      ## part three: for losing days where close is lower than open, plot
      ##      red bars showing retreat from the open to the close
      ind <- which(stats$CLOSE < stats$OPEN)
      boxes(stats[ind, "MONTH"], stats[ind,"OPEN"], stats[ind,"CLOSE"],
            col="black")

      # part four: blue bars for the days intraday retreat from the lower
      ##      of OPEN and CLOSE to the MIN
      segments(stats$MONTH, stats$MIN, stats$MONTH, apply(stats[,c("OPEN","CLOSE")], 1, min),
               lwd=1)
      axis(1, labels=strftime(data$DATE, format="%d %b"), at=data$DATE)
      axis(2)
      #grid(nx=NA, ny=NULL, lty="solid")
      box()
      title(title)

    }
~~~

`aggregates.R`:

~~~
    aggregates <- function(formula, data=NULL, FUNS){
        if(class(FUNS)=="list"){
            f <- function(x) sapply(FUNS, function(fun) fun(x))
        }else{f <- FUNS}
        temp <- aggregate(formula, data, f)
        out <- data.frame(temp[,-ncol(temp)], temp[,ncol(temp)])
        colnames(out)[1] <- colnames(temp)[1]
    return(out)
    }
~~~

`stackplot.R`:

~~~
    stackplot.cols <- function(data, ylim=NA, main=NA, colors=NA, xlab=NA, ylab=NA) {
      # stacked line plot
      if (is.na(ylim)) {
        ylim=c(0, max(rowSums(data, na.rm=T)))
      }
      if (is.na(colors)) {
        colors = c("green","red","lightgray","blue","orange","purple", "yellow")
      }
      xval = as.numeric(row.names(data))
      summary = rep(0, nrow(data))
      recent = summary

      # Create empty plot
      plot(c(-100), c(-100), xlim=c(min(xval, na.rm=T), max(xval, na.rm=T)), ylim=ylim, main=main, xlab=xlab, ylab=ylab)

      # One polygon per column
      cols = names(data)
      for (c in 1:length(cols)) {
        current = data[[cols[[c]]]]
        summary = summary + current
        polygon(
          x=c(xval, rev(xval)),
          y=c(summary, rev(recent)),
          col=colors[[c]]
        )
        recent = summary
      }
    }

    stackplot.rows <- function(data, catnames=NA, main=NA, ylim=NA, colors=NA, ...) {
      if (is.na(ylim)) {
        ylim = c(0, max(colSums(data, na.rm=T)))
      }
      if (is.na(colors)) {
        colors = c("green","red","lightgray","blue","orange","purple", "yellow")
      }
      xlabels = colnames(data)
      xval = 1: ncol(data)
      summary = rep(0, ncol(data))
      recent = summary

      plot.new()
      plot.window(xlim=c(min(xval, na.rm=T), max(xval, na.rm=T)), ylim=ylim, main=main)

      # One polygon per row
      rows = 1: nrow(data)
      for (j in rows) {
        current = data[j,]
        summary = summary + current
        polygon(
          x=c(xval, rev(xval)),
          y=c(summary, rev(recent)),
          col=colors[[j]]
        )
        recent = summary
      }
      box()
      axis(1, labels=xlabels, at=xval)
      axis(2)
      if (!all(is.na(catnames))) {
        legend("topright", legend=catnames, fill=colors, ...)
      }
    }
~~~

[Reports]: Reports.md

[yaledger-charts-edited]: yaledger-charts-edited.png
