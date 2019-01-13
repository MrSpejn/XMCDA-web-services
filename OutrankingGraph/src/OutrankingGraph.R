library(ggraph)
library(igraph)
library(RCurl)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

OutrankingGraph <- function(inputs) {
  if (inputs$seed != 0) {
    set.seed(inputs$seed)
  }
  alternatives <- inputs$alternatives
  highlighted_alternatives <- inputs$highlighted_alternatives
  font_size <- inputs$font_size
  is_directed <- inputs$is_directed
  adjecency_matrix <- inputs$adjecency_matrix

  from <- c()
  to <- c()
  
  for (row in rownames(adjecency_matrix)) {
    for (col in colnames(adjecency_matrix)) {
      if (adjecency_matrix[row, col]) {
        from <- c(from, row)
        to <- c(to, col)
      }
    }
  }
  
  if (length(from) == 0) {
    stop("No edges in adjecency matrix")
  }

  edges <- data.frame('from'=from, 'to'=to)
  vertices <- data.frame(
      'name'=sapply(alternatives, function (alt) alt$id),
      'display_name'=sapply(alternatives, function (alt) ifelse(is.null(alt$name), alt$id, alt$name)),
      'size'=rep(1, length(alternatives)))

  graph <- graph_from_data_frame(
      edges,
      directed = inputs$is_directed,
      vertices = vertices
  )

  png(result <- tempfile(fileext = ".png"), height = 800, width=800);   
  
  arrow_for_edge = NULL
  if (inputs$is_directed) {
    arrow_for_edge <- arrow(angle = 30, length = unit(0.15, 'inches'), ends = 'last', type = 'closed')
  }

  g1 <- ggraph(graph, layout = 'kk') +
      geom_point(
          aes(x=x, y=y),
          size=21
      ) +
      geom_point(
          aes(x=x, y=y, colour=ifelse(name %in% highlighted_alternatives, 'selected', 'normal')),
          size=20,
      ) +
      scale_colour_manual(values=c("white", "gray30")) +
      geom_node_label(aes(label=display_name), size=inputs$font_size, fill='white', label.size=0) +
      geom_node_circle(aes(x0=x, y0=y, r=0.15), size=0) +
      geom_edge_link(
          colour = 'gray30',
          arrow = arrow_for_edge,
          start_cap = circle(1),
          end_cap = circle(1)) + 
      theme_void() + theme(
          legend.position="none",
          plot.title=element_text(size=30, hjust=0.5),
      ) +
      ggtitle("Outranking graph")

  multiplot(g1)
  dev.off()
  encoded <- base64Encode(readBin(result, "raw", file.info(result)[1, "size"]), "txt")

  return(list(plot=encoded, alternatives=alternatives))
}
