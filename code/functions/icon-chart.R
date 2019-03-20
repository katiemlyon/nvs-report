iconChart <- function(values, bar_width=round(.10 * max(values)), icon, categories=NULL) {
    
    max_val <- max(values)
    padding <- .15 * bar_width
    
    # Setup blank plot.
    xlim <- c(0, (bar_width+padding)*length(values))
    ylim <- c(0, ceiling(max_val / bar_width)+1)
    plot(0, 0, type="n", xlab="", ylab="", bty="n", xlim=xlim, ylim=ylim, asp=1, axes=FALSE)
    
    # Get coordinates for each element.
    for (i in 1:length(values)) {
        xoffset <- (i-1)*(bar_width+padding)
        num_rows <- ceiling(values[i] / bar_width)
        xleft <- rep(xoffset:(xoffset+bar_width-1), num_rows)[1:values[i]]
        ybottom <- rep(1:num_rows, each=bar_width)[1:values[i]]
        xright <- xleft + 1
        ytop <- ybottom + 1
        
        # Plot grid for current value.
        if (i <= length(icon)) {
            rasterImage(icon[[i]], xleft, ybottom, xright, ytop)
        } else {
            rasterImage(icon[[1]], xleft, ybottom, xright, ytop)
        }
        
        
        if (i <= length(categories)) {
            mtext(categories[i], 1, line = -2, at = (xoffset+bar_width/2))
        }
    }  
}