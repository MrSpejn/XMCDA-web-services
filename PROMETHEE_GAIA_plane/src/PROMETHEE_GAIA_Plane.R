library(ggplot2)
library(RCurl)
library(rlist)

createPlotWithAlternatives <- function(points, bounds) {
    ggplot(points, aes(x=PC1, y=PC2)) + 
    geom_hline(yintercept=0, color="grey68") +
    geom_vline(xintercept=0, color="grey68") +
    geom_point(size=2, shape=15) +
    geom_text(
        data=points,
        aes(label=row.names(points), x=PC1, y=PC2),
        nudge_x = 0.05,
        nudge_y=0.05
    ) +
    coord_cartesian(ylim=c(bounds[4], bounds[2]), xlim=c(bounds[3], bounds[1])) +
    theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(margin=margin(t = 0, r = -20, b = 0, l = 0, unit = "pt")),
        axis.text.x=element_text(margin=margin(t = -20, r = 0, b = 0, l = 0, unit = "pt")
),
    )
}

addCriteria <- function (plot, points) {
    plot + geom_segment(
            data=points,
            inherit.aes=FALSE,
            aes(xend=PC1, yend=PC2),
            x=0,
            y=0,
            color="darkslategray4",
            arrow=arrow(length=unit(0.30,"cm"), type = "closed")
        ) +
        geom_text(
            data=points,
            aes(label=row.names(points), x=PC1/2, y=PC2/2),
            colour="darkslategray",
            nudge_x = 0.03,
            nudge_y=0.03
        )
}

addDecisionStick <- function (plot, stick) {
    plot + geom_segment(
        data=stick,
        inherit.aes=FALSE,
        aes(xend=PC1, yend=PC2),
        x=0,
        y=0,
        color="chocolate1",
        arrow=arrow(length=unit(0.30,"cm"), type = "closed")
    ) +
    geom_segment(
        data=stick,
        inherit.aes=FALSE,
        aes(
            x=PC1*-100,
            y=PC2*-100,
            xend=PC1*100,
            yend=PC2*100,
        ),
        color="chocolate1",
        linetype="dashed",
        arrow=arrow(length=unit(0.30,"cm"), type = "closed")
    ) +
    geom_text(
        data=stick,
        label="D",
        aes(x=PC1/2, y=PC2/2),
        colour="chocolate3",
        nudge_x = 0.05,
        nudge_y=0.05
    )
}


GAIAPlain <- function(flows, criteria_weights) {
    pca <- prcomp(flows)
    
    transform_matrix <- pca[['rotation']][,1:2]
    points <- flows %*% transform_matrix
    points <- as.data.frame(points)

    number_of_criteria = dim(flows)[2]
    criteria_points <- (diag(number_of_criteria) %*% transform_matrix)
    criteria_points <- as.data.frame(criteria_points, row.names=colnames(flows))

    if (missing(criteria_weights) || length(criteria_weights) != number_of_criteria) {
        weights <- rep(1, number_of_criteria) / number_of_criteria 
    } else {
        weights <- criteria_weights / sum(criteria_weights)
    }

    weights <- matrix(weights, ncol=number_of_criteria, nrow=1)
    decision_stick <- as.matrix((weights %*% transform_matrix))
    decision_stick <- as.data.frame(decision_stick, col.names=c("PC1", "PC2"))

    x_upper_bound <- max(points$PC1, decision_stick$PC1, criteria_points$PC1)*1.05
    y_upper_bound <- max(points$PC2, decision_stick$PC2, criteria_points$PC2)*1.05
    x_lower_bound <- min(points$PC1, decision_stick$PC1, criteria_points$PC1)*1.05
    y_lower_bound <- min(points$PC2, decision_stick$PC2, criteria_points$PC2)*1.05

    addDecisionStick(
        addCriteria(
            createPlotWithAlternatives(
                points,
                bounds=c(x_upper_bound, y_upper_bound, x_lower_bound, y_lower_bound)
            ),
            criteria_points
        ),
        decision_stick
    )
}


PROMETHEE_GAIA_Plane <- function(inputs)
{
  profiles = inputs$profiles
  criteriaNames =  Map({
    function (crit) ifelse(is.null(crit$name), crit$id, crit$name)
  }, inputs$criteria)

  alternativesNames = Map({
    function (alt) ifelse(is.null(alt$name), alt$id, alt$name)
  }, inputs$alternatives)

  rownames(profiles) = alternativesNames
  colnames(profiles) = criteriaNames

  png(tf1 <- tempfile(fileext = ".png"), height = 800, width=800)

  gaiaPlane <- GAIAPlain(profiles, inputs$weights)

  multiplot(gaiaPlane)
  dev.off()
  encoded <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")

  return(list(
    alternatives=inputs$alternatives,
    plot=encoded
  ))
}
