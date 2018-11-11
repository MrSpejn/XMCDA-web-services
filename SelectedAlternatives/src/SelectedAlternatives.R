library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)
library(ggplot2)
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

nameIfExists <- function(alternative) {
  if (!is.null(alternative$name)) {
    return(alternative$name)
  }
  return(alternative$id)
}

SelectedAlternatives <- function(inputs) {
  if (inputs$seed != 0) {
    set.seed(inputs$seed)
  }

  selected = unlist(Map(nameIfExists, inputs$selected_alternatives))
  rejected = unlist(Map(nameIfExists, inputs$rejected_alternatives))

  selected_edge_df <- data.frame("from"=rep("Active", length(selected)), "to"=selected)
  rejected_edge_df <- data.frame("from"=rep("Rejected", length(rejected)), "to"=rejected)

  selected_vertices_df <- data.frame(
    "name"=c(selected, "Active"),
    "size"=rep(1, length(selected) + 1)
  )
  rejected_vertices_df <- data.frame(
    "name"=c(rejected, "Rejected"),
    "size"=rep(1, length(rejected) + 1)
  )

  selected_graph <- graph_from_data_frame(selected_edge_df, vertices=selected_vertices_df)
  rejected_graph <- graph_from_data_frame(rejected_edge_df, vertices=rejected_vertices_df)

  png(tf1 <- tempfile(fileext = ".png"), height = 800, width=1200)

  g1 <- ggraph(selected_graph, layout = 'circlepack', weight="size") +
    geom_node_circle(aes(
      size=ifelse(depth > 0, "leaf", "root")
    ), color="gray67") +
    scale_size_manual(values=c(0, 1)) +
    geom_node_label( aes(label=name, filter=leaf), label.size=0, size=inputs$font_size, fill=NA) +
    theme_void() + theme(legend.position="none", plot.title=element_text(size=30, hjust=0.5)) +
    ggtitle("Selected Alternatives")

  g2 <- ggraph(rejected_graph, layout = 'circlepack', weight="size") + 
    geom_node_circle(aes(
      size=ifelse(depth > 0, "leaf", "root")
    ), color="gray67") +
    scale_size_manual(values=c(0, 1)) +
    geom_node_label(aes(label=name, filter=leaf), label.size=0, size=inputs$font_size, fill=NA) +
    theme_void() + theme(legend.position="none", plot.title=element_text(size=30, hjust=0.5)) +
    ggtitle("Rejected Alternatives")

  multiplot(g1, g2, cols=2)

  dev.off()
  encoded <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")

  alternatives <- append(inputs$selected_alternatives, inputs$rejected_alternatives)

  return(list(
    alternatives=alternatives,
    plot=encoded
  ))
}
