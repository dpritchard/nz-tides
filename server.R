library(shiny)
library(dplyr)

read_dat <- function(port_url, years){
    dat <- list()
    for(a in seq_along(years)){
        url_text <- paste0(port_url, years[a], '.csv')
        dat[[a]] <- read.csv(url(url_text), header = F, stringsAsFactors = F, skip=3)
    }
    dat <- bind_rows(dat)
    names(dat) <- c("day", "dow", "month", "year", rep(c("time", "level"), 4))
    tide_dat <- bind_rows(dat[, c(5,6)], dat[, c(7,8)], 
                          dat[, c(9,10)], dat[, c(11,12)])
    Sdt <- paste0(dat[,'year'],"-", sprintf("%02d", dat[,'month']), "-", sprintf("%02d", dat[,'day']))
    tide_dat$Sdt <- rep(Sdt, 4)
    names(tide_dat) <- c("time", "level", "Sdt")
    tide_dat$Rdt <- as.POSIXct(strptime(paste(tide_dat$Sdt, tide_dat$time), 
                                        format = "%Y-%m-%d %H:%M"))
    return(tide_dat)
}

do_tides <- function(trgt, d){
    ndx <- which(d$Rdt == trgt)
    if(length(ndx) != 0){
        out <- data.frame(Rdt = d$Rdt[ndx], level = d$level[ndx])
    } else {
        t <- as.numeric(trgt)/60
        difs <- abs(d$Rdt-trgt)
        ndx <- order(difs)[1:2]
        d <- d[ndx, ]
        d <- d[order(d$Rdt), ]
        t1 <- as.numeric(d$Rdt[1])/60
        t2 <- as.numeric(d$Rdt[2])/60
        h1 <- d$level[1]
        h2 <- d$level[2]
        
        td <- (t - t1)/(t2 - t1)
        A <- pi*(td+1)
        hd <- h2 - h1
        h <- h1 + hd*((cos(A) + 1)/2)
        out <- data.frame(Rdt = trgt, level = h)
    }
    return(out)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    output$plot <- renderPlot({
        input$updateButton
        isolate({
        years <- unique(format(input$start_date, "%Y"), format(input$endend_date, "%Y"))
        dat <- read_dat(input$port, years)
        stdate <- as.POSIXct(input$start_date)
        stdate <- as.POSIXct(paste(format(stdate, "%Y-%m-%d"), "00:00"), format = "%Y-%m-%d %H:%M")
        endate <- as.POSIXct(input$end_date)
        endate <- as.POSIXct(paste(format(endate, "%Y-%m-%d"), "23:00"), format = "%Y-%m-%d %H:%M")
        trgts <- seq(from = as.POSIXct(stdate), 
                     to = as.POSIXct(endate), 60*input$pred_interval)
        trgts <- as.POSIXct(trgts)
        aa <- lapply(trgts, do_tides, d=dat)
        aa <- bind_rows(aa)
        print(tail(aa))
        plot(aa$Rdt, aa$level, type="l", xlab = "", ylab = "Level (m)", 
             ylim = c(min(dat$level, na.rm = T), max(dat$level, na.rm = T)))
        points(dat$Rdt, dat$level, col=2, pch=19)
        })
    })
    
    observeEvent(input$start_date, {
        updateDateInput(session, inputId = "end_date", 
                        max = input$start_date+10,
                        min = input$start_date+1,
                        value = input$start_date+3)
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            years <- unique(format(input$start_date, "%Y"), format(input$endend_date, "%Y"))
            dat <- read_dat(input$port, years)
            stdate <- as.POSIXct(input$start_date)
            stdate <- as.POSIXct(paste(format(stdate, "%Y-%m-%d"), "00:00"), format = "%Y-%m-%d %H:%M")
            endate <- as.POSIXct(input$end_date)
            endate <- as.POSIXct(paste(format(endate, "%Y-%m-%d"), "23:00"), format = "%Y-%m-%d %H:%M")
            trgts <- seq(from = as.POSIXct(stdate), 
                         to = as.POSIXct(endate), 60*input$pred_interval)
            trgts <- as.POSIXct(trgts)
            aa <- lapply(trgts, do_tides, d=dat)
            aa <- bind_rows(aa)
            
            # Set up parameters to pass to Rmd document
            params <- list(dat = dat, aa = aa, port_url = input$port, years = years)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
})
