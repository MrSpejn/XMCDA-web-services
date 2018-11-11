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

  from <- c()
  to <- c()
  for (pair in inputs$alternative_comparisions) {
    from <- c(from, pair[[1]])
    to <- c(to, pair[[2]])
  }

  edges <- data.frame('from'=from, 'to'=to)
  vertices <- data.frame(
      'name'=sapply(alternatives, function (alt) alt$id),
      'display_name'=sapply(alternatives, function (alt) ifelse(is.null(alt$name), alt$id, alt$name)),
      'size'=rep(1, length(alternatives)))

  graph <- graph_from_data_frame(
      edges,
      directed = TRUE,
      vertices = vertices
  )

  png(result <- tempfile(fileext = ".png"), height = 800, width=800);   
    
  arrow_g <- arrow(angle = 30, length = unit(0.15, 'inches'), ends = 'last', type = 'closed')

  g1 <- ggraph(graph, layout = 'kk') +
      geom_edge_link(
          colour = 'palegreen4',
          arrow = arrow_g,
          end_cap = circle(0.5, 'inches')) + 
      geom_node_circle(
          aes(r=0.15, fill=ifelse(name %in% highlighted_alternatives, 'selected', 'normal')),
          size=0
      ) + 
      scale_fill_manual(values=c("skyblue2", "skyblue4")) +
      geom_node_label(aes(label=display_name), size=font_size, fill='azure2', label.size=0) +
      theme_void() + theme(legend.position="none", plot.title=element_text(size=30, hjust=0.5)) +
      ggtitle("Outranking graph")

  multiplot(g1)
  dev.off()
  encoded <- base64Encode(readBin(result, "raw", file.info(result)[1, "size"]), "txt")

  return(list(plot=encoded, alternatives=alternatives))
}
