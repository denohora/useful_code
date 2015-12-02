# https://github.com/ateucher/useful_code/blob/master/R/multiplot.r

#'Plot multiple plots in a single pane
#'
#'ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @import grid ggplot2
#' @export
#' 
#' @param ... Two or more ggplot2 objects
#' @param  plotlist (optional) a list of ggplot2 objects
#' @param  cols Number of columns in layout
#' @param  layout A matrix specifying the layout. If present, 'cols' is ignored. See Details
#' @param  titles Optional vector of titles (character strings)
#' @param  widths a vector of relative column widths eg. c(3,2)
#' @param  heights a vector of relative column heights eg. c(3,2)
#' @param  titlefont The font of the title
#' @param  titleface The font face (1 = normal, 2 = bold, 3 = italic, 4 = bold italic)
#' @param  titlesize The size of the title font
#' 
#' @details If plotting three plots and the layout is something like
#'   matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper
#'   left, 2 will go in the upper right, and 3 will go all the way across the
#'   bottom.  To save, you must use the desired device (eg \code{png()}), or
#'   save from the RStudio Viewer.
#' 
#' Borrowed and modified from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' 
#' @return NULL (invisibly)
#' @examples \dontrun{
#' library("ggplot2")
#' plot1 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
#'    geom_bar(stat = "identity")
#' plot2 <- ggplot(mtcars, aes(x = mpg, y = disp)) + 
#'    geom_smooth()
#' multiplot(plot1, plot2, cols = 2, widths = c(3,2), titles = "My two unrelated plots")
#' multiplot(plot1, plot2, cols = 1, heights = c(10,2), titles = "My two unrelated plots")
#' myplots <- list(plot1, plot2, plot1)
#' multiplot(plotlist = myplots, layout =matrix(c(1,2,3,3), nrow=2), 
#'      heights = c(1,3), widths = c(3,4), titles = "My three unrelated plots")
#' ## Adjusting fonts
#' library(extrafont)
#' loadfonts()
#' 
#' 
#' multiplot(plotlist = myplots, layout =matrix(c(1,2,3,3), nrow=2),
#'           heights = c(1,3), widths = c(3,4), titles = "My three unrelated plots", 
#'           titlefont = "Wingdings", titleface = 4, titlesize = 20)
#'}
#'
#'# DOH 
#'# add mutliple titles (to label panels)
# - titles holds an array of label names, e.g. titles = c("A", "B")
# - title.layout is an array that positions the labels, e.g., title.layout = c(1,2,2)
# - title.layout has as many slots as cols in layout and the number refers to the index of labels
# - using labels = c("A", "B") and labels.layout = c(1,2,2) 
#   centres A above left 1 col panel and B above righthand 2 col panel
# # also reduced rowheight for titles

multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL, widths=NULL, heights=NULL, 
                      titles=NULL, title.layout=NULL, title.just = "centre",
                      titlefont = "", titleface = 1, titlesize = 16) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (!is.null(titles)) { # Add a narrow row at the top for the titles
    layout <- rbind(rep(0,ncol(layout)),layout)
    if (is.null(heights)) {
      plotrows <- nrow(layout)-1
      rowheights <- c(0.05, rep(1,plotrows)/plotrows)
    } else {
      rowheights <- c(0.05, heights/sum(heights))
    }
  } else {
    if (is.null(heights)) {
      rowheights <- rep(1,nrow(layout))  
    } else {
      rowheights <- heights
    }
  }
  
  if (is.null(widths)) {
    colwidths <- rep(1, cols)
  } else {
    colwidths <- widths
  }
  
  if (numPlots==1) {
    
    return(plots[[1]] + labs(title=titles))
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), 
                                               widths=colwidths, 
                                               heights=rowheights)))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if (!is.null(titles)) {
      
      if (!is.null(title.layout)) {
        for(title.ix in 1:length(titles))  
            grid.text(titles[title.ix], just = title.just,
                      vp = viewport(layout.pos.row = 1,
                                    layout.pos.col = which(title.layout==title.ix)), 
                                    gp = gpar(fontfamily = titlefont, fontface = titleface, 
                                    fontsize = titlesize))
        
      } else {
          grid.text(titles,  just = title.just, 
                    vp = viewport(layout.pos.row = 1, 
                                  layout.pos.col = 1:ncol(layout)), 
                                  gp = gpar(fontfamily = titlefont, fontface = titleface, 
                                  fontsize = titlesize))
      }
      
      
    }
    
  }
  return(invisible(NULL))
}
