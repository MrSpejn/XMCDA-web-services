library(gridExtra)
library(grid)
library(RCurl)
library(rlist)

getAllWithRank <- function(ranking, alternatives, rank) {
    result = c();
    ranking_ids = names(ranking)
    for (row_index in 1:length(ranking)) {
        row = ranking[row_index]
        if (as.integer(row) == rank) {
            alternative = list.find(alternatives, id==ranking_ids[row_index])
            
            if (!is.null(alternative[[1]]$name) {
                display = alternative[[1]]$name
            } else {
                display = alternative[[1]]$id
            }
            result = c(result, display);
        }
    }
    return(paste(result, collapse = '\n')[1]);
}

RankingComparision <- function(inputs) {
  first_ranking = inputs$first_ranking
  second_ranking = inputs$second_ranking
  alternatives = inputs$alternatives

  max_rank <- max(c(first_ranking, second_ranking))
  ranks <- 1:max_rank

  frame <- data.frame(
      "Ranking1"=unlist(Map({function (rank) getAllWithRank(first_ranking, alternatives, rank)}, ranks)),
      "Ranking2"=unlist(Map({function (rank) getAllWithRank(second_ranking, alternatives, rank)}, ranks))
  )

  png(tf1 <- tempfile(fileext = ".png"), height = 800, width=800);    
  thm <- ttheme_default(
      colhead = list(padding=unit.c(unit(30, "mm"), unit(10, "mm"))),
      rowhead = list(padding=unit.c(unit(30, "mm"), unit(10, "mm")))
  )
  grid.newpage()
  grid.draw(tableGrob(frame, theme=thm))
  dev.off()
  encoded <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")

  return(list(alternatives=inputs$alternatives, plot=encoded))
}
