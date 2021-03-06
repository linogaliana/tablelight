% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light_table_header.R
\name{light_table_header}
\alias{light_table_header}
\title{Header for the regression table}
\usage{
light_table_header(
  ncols_models,
  type = c("latex", "html"),
  title = "Title",
  label = "label",
  dep.var.labels = "Label dep.var.labels",
  dep.var.separate = NULL,
  column.labels = "blab",
  adjustbox_width = c(NULL, 1.1)
)
}
\arguments{
\item{ncols_models}{Number of columns}

\item{type}{Reporting output used. Accepted values are
\emph{latex} and \emph{html}}

\item{title}{Table caption}

\item{label}{Table label}

\item{dep.var.labels}{Label for dependent variables}

\item{dep.var.separate}{Numeric vector that specifies how
\code{dep.var.labels} should be laid out across regression
table columns. A value of c(2, 1, 3), for instance, will
apply the first label to the two first columns, the second
label to the third column, and the third label will apply
to the following three columns (i.e., columns
number four, five and six).}

\item{column.labels}{Label for columns}

\item{adjustbox_width}{If the table needs to be adjusted to page width,
what should be the parameter ?}
}
\description{
Header for the regression table
}
