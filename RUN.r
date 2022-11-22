require(jpeg)
require(png)
SUPPORTED_EXTENSIONS <- c("jpeg", "jpg", "png")
POINTS <- data.frame(x = NULL, y = NULL, fname = NULL)
DISPLAY_POINTS <- data.frame(x = NULL, y = NULL, fname = NULL)

ZOOM_FACTOR <- 1
ZOOM_CENTER <- NULL

locator2 <- function() {
  x <- NULL
  y <- NULL
  on.exit(return(list(x = x, y = y)))

  message('select points, pressure ESC to end')
  
  while(TRUE) {
    pt <- locator(1)
    
    if(is.null(pt)) break
    
    x <- c(x, pt$x)
    y <- c(y, pt$y)
    
    points(x, y, pch = 16)
    points(x, y, pch = 16, cex = .75, col = "grey75")
  }
}

drawimg <- function(img, fname) {
  res <- dim(img)[2:1]
  xlims <- c(0, res[1])
  ylims <- c(0, res[2])
  
  if(ZOOM_FACTOR != 1) {
    zx <- ZOOM_CENTER$x
    zy <- ZOOM_CENTER$y

    zxf <- res[1] / ZOOM_FACTOR
    zyf <- res[2] / ZOOM_FACTOR
    xlims <- c(zx - zxf/2, zx + zxf/2)
    ylims <- c(zy - zyf/2, zy + zyf/2)
  }
  
  plot(1, 1, xlim = xlims, ylim = ylims, type = 'n', asp = 1, axes = FALSE, xlab = "", ylab = "", main = fname)
  
  # put a hard stop at the dims of the image
  # this is necc to reverse the y axis
  # but just did on the x too for symmetry and an easy read of the dims
  
  prettylabs <- pretty(0:res[2])
  prettylabs <- c(prettylabs[-length(prettylabs)], res[2])
  prettxlabs <- pretty(0:res[1])
  prettxlabs <- c(prettxlabs[-length(prettxlabs)], res[1])
  
  axis(2, at = prettylabs, lab = NA)
  axis(2, at = prettylabs, lab = rev(prettylabs), tcl = 0.3)
  axis(1, at = prettxlabs, lab = NA)
  axis(1, at = prettxlabs, lab = prettxlabs, tcl = 0.3)
  box()
  
  # plot the image
  rasterImage(img, 1, 1, res[1], res[2])
  
  # some gentle grids
  abline(v = prettxlabs, lty = 3, col = "grey75")
  abline(h = prettylabs, lty = 3, col = "grey75")
  
  # zoom factor
  mtext(paste0('zoom = ', ZOOM_FACTOR), side = 1, line = 3.1)
  
  # return
  res
}

addpts <- function(fname, res) {
  results <- locator2()
  DISPLAY_POINTS <<- rbind(DISPLAY_POINTS, data.frame(x = results$x, y = results$y, fname = fname, stringsAsFactors = FALSE))
  results$y <- res[2] - results$y
  POINTS <<- rbind(POINTS, data.frame(x = results$x, y = results$y, fname = fname, stringsAsFactors = FALSE))
}

clearpts <- function(img, fname) {
  message(paste0('clearing points in ', fname, '...'))
  POINTS <<- POINTS[POINTS$fname != fname, ]
  DISPLAY_POINTS <<- DISPLAY_POINTS[DISPLAY_POINTS$fname != fname, ]
  refresh(img, fname)
}

listpts <- function() {
  if(nrow(POINTS) == 0) {
    message('currently no saved points...')
  } else {
    print(POINTS)
  }
}

refresh <- function(img, fname) {
  drawimg(img, fname)
  desepts <- DISPLAY_POINTS[POINTS$fname == fname, ]
  points(desepts$x, desepts$y, pch = 16)
  points(desepts$x, desepts$y, pch = 16, cex = .75, col = 'grey75')
}

savepts <- function() {
  outfname <- readline("output filename (csv): ")
  outpath <- file.path(getwd(), outfname)
  
  message(paste0('writing to: ', outpath))
tryCatch(
  write.table(POINTS, file = outpath, sep = ',', row.names = FALSE, col.names = TRUE),
error = function(e) { message(e) })
}

open_img <- function() {
  # get an image file
  f <- file.choose()
  
  # parse filename
  spl <- strsplit(f, "\\.")[[1]]
  ext <- tolower(spl[length(spl)])
  if(!(ext %in% SUPPORTED_EXTENSIONS)) {
    message('open failed: must have a jp(e)g or png extensions (and actually be that file type)')
    return(-1)
  }
  
  if(ext == 'jpg' | ext == 'jpeg') img <- jpeg::readJPEG(f)
  if(ext == 'png') img <- png::readPNG(f)
  
  res <- drawimg(img, basename(f))
  
  # send to the image menu
  imgmenu(img, basename(f), res)
  # f <- file.choose()
}

# mainmenu
mainmenu <- function() {
  ans = 0
  while(ans != 'q') {
    ans <- readline("(o)pen (q)uit ?> ")
    ans <- tolower(ans)
    
    switch(ans,
      'o' = open_img(),
      'q' = message('goodbye'),
      message('come again?')
    )
  }
}

zoom <- function(img, fname) {
  message('pick zoom center')
  ZOOM_CENTER <<- locator(1)
  ans <- readline("zoom factor: ")
  ans <- as.numeric(ans)
  if(is.na(ans)) {
    message('must a number > 0')
    ZOOM_FACTOR <<- 1
  } else {
    ZOOM_FACTOR <<- ans
    
    if(ans <= 0) ZOOM_FACTOR <<- 1
    if(ans >= 10) ZOOM_FACTOR <<- 1
  }
  
  refresh(img, fname)
}


imgmenu <- function(img, fname, res) {
  ans = 0
  while(ans != 'e') {
    ans <- readline("(z)oom (a)dd (c)lear (l)ist (s)ave (e)nd ?> ")
    ans <- tolower(ans)
    
    switch(ans,
     'z' = zoom(img, fname),
     'a' = addpts(fname, res),
     'c' = clearpts(img, fname),
     'l' = listpts(),
     's' = savepts(),
     'e' = message('exiting to main menu'),
     message('what\'s that?')
    )
  }
}

### RUN
if(!interactive()) stop("need to be interactive yo")
mainmenu()
