% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfs.R
\name{.bs_out}
\alias{.bs_out}
\alias{lfs_bs_mean}
\alias{lfs_bs_total}
\title{quality measures for bootstrap estimates}
\usage{
.bs_out(est_fw, est_bs)

lfs_bs_mean(x, bootstrap_weights, final_weights)

lfs_bs_total(bootstrap_weights, final_weights)
}
\arguments{
\item{x}{:number vector of survey data.}

\item{bootstrap_weights}{:array of bootstrap replicate weights.}

\item{final_weights}{:vector of final weights.}
}
\value{
a data.frame with columns est, var, and se.

a data.frame with columns est, var, and se.
}
\description{
\if{html}{\out{<div class="sourceCode">}}\preformatted{designed for use in a grouped dplyr::summarize() context
where bootstrap weights are stored as an array column
}\if{html}{\out{</div>}}

\if{html}{\out{<div class="sourceCode">}}\preformatted{designed for use in a grouped dplyr::summarize() context
where bootstrap weights are stored as an array column
}\if{html}{\out{</div>}}
}
