#' epiviz interactive device
#' 
#' This class wraps a GenomicRanges object to be plotted in an interactive epiviz device
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{gr}:}{The the \link{GRanges-class} object bound to this track}
#'  \item{\code{tree}:}{A \link{GIntervalTree-class} object used for querying}
#'  \item{\code{id}:}{The id for this device assigned by the epiviz manager object}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{getData}:}{Subset data from the GRanges object within given region}
#' }
#' 
#' @param ... arguments passed to constructor
#' 
#' @aliases EpivizDevice 
#' @name EpivizDevice-class
#' @rdname EpivizDevice-class
#' 
#' @exportClass EpivizDevice
EpivizData <- setRefClass("EpivizData",
  contains="VIRTUAL",
  fields=list(
    object="ANY",
    mgr="EpivizDeviceMgr",
    id="character",
    name="character",
    columns="ANY",
    ylim="ANY",
    curQuery="ANY",
    curHits="ANY"
  ),
  methods=list(
    initialize=function(object=GIntervalTree(GRanges()), columns=NULL, ...) {
      object <<- object
      columns <<- columns

      if (!.self$.checkColumns(columns))
        stop("Invalid 'columns' argument")

      if (is.null(.self$columns))
        columns <<- .self$.getColumns()

      ylim <<- .self$.getLimits()

      curQuery <<- NULL
      curHits <<- NULL
      callSuper(...)
    },
    .checkColumns=function(columns) {
      is.null(columns)
    },
    .getColumns=function() {
      NULL
    },
    .getLimits=function() {
      NULL
    },
    update=function(newObject) {
      if(class(newObject) != class(object)) {
        stop("class of 'newObject' is not equal to class of current 'object'")
      }

      if (!is.null(columns)) {
        if (!.checkColumns(object, columns))
          stop("columns not found in 'newObject'")

        ylim <<- .getLimits(object, columns)
      }
      object <<- newObject
      invisible()
    },
    getId=function() {
      return(id)
    },
    setId=function(id) {
      id <<- id
      invisible()
    },
    getName=function() {return(name)},
    setName=function(name) {
      name <<- name
      invisible()
    },
    setLimits=function(ylim) {
      if (!.checkLimits(ylim))
          stop("'invalid' limits argument")
      ylim <<- ylim
    }, 
    getMeasurements=function() {
      stop("'getMeasurements' called on virtual class object")
    },
    setMgr=function(mgr) {
      mgr <<- mgr
      invisible()
    },
    show=function() {
      cat(class(.self), "object", id, "\n")
      methods::show(object)
      cat("\n\tcolumns:", paste(columns,collapse=","),"\n")
      cat("\tlimits:\n")
      print(ylim)
    },
    plot=function() {
      stop("'plot' method called on virtual class object")
    }
  )
)

#####
# validity
.valid.EpivizData.columns <- function(x) {
  if(!x$.checkColumns(x$columns))
    return("invalid 'columns' slot")
  NULL
}

.valid.EpivizData <- function(x) {
  c(.valid.EpivizData.columns(x))
}

IRanges::setValidity2("EpivizData", .valid.EpivizData)

#######
# get data
EpivizData$methods(
  packageData=function(msId) {
    stop("'packageData' called on object of virtual class")
  },
  getData=function(query, msId=NULL) {
    if (!is(query, "GRanges"))
      stop("'query' must be a GRanges object")
    if (length(query) != 1) {
      stop("'query' must be of length 1")
    }

    if (is.null(curQuery) || !identical(unname(query), unname(curQuery))) {
      curQuery <<- query
      olaps <- GenomicRanges::findOverlaps(query, object, select="all")
      curHits <<- subjectHits(olaps)
      if (IRanges:::isNotSorted(start(object)[curHits])) {
        ord <- order(start(object)[curHits])
        curHits <<- curHits[ord]
      }
    }
    packageData(msId=msId)
  }
)

EpivizDataPack <- setRefClass("EpivizDataPack",
  fields=list(
    length="integer"),
  methods=list(
    initialize=function(length=0L, ...) {
      length <<- length
      callSuper(...)
    },
    set=function(obj, msId, index) {
      step("calling 'set' on virtual class")
    },
    getData=function() {
      stop("calling 'getData' on virtual class")
    }
  )
)


