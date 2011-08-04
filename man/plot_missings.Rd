\name{plot_missings}
\alias{plot_missings}


\title{Plot missing values}
\description{
This function plots missing values in the round robin matrix for visual inspection.
}
\usage{
plot_missings(formule, data, show.ids=TRUE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
\item{formule}{A formula specifying the variable names. Example usage: \code{DV ~ perceiver.id*target.id | group.id} (group.id only necessary in multi group data sets)}
\item{data}{The data frame}   
\item{show.ids}{Should the id's of the persons be printed on the axes?}
}