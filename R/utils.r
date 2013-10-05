IndexedArray <- setRefClass("IndexedArray",
                            fields=list(nextId="integer", items="list"),
                            methods=list(
                              initialize=function(...) {
                                nextId <<- 1L
                                items <<- vector("list")
                                callSuper(...)
                              },
                              finalize=function() {
                                empty()
                                invisible()
                              },
                              append=function(item) {
                                id=nextId
                                nextId <<- nextId+1L
                                items[[id]] <<- item
                                return(id)
                              },
                              get=function(id) {
                                if (is.null(items[[id]]))
                                  return(NULL)
                                out=items[[id]]
                                items[[id]] <<- NULL
                                return(out)
                              },
                              empty=function() {
                                items <<- vector("list")
                                nextId <<- 1L
                                invisible()
                              })
)

Queue <- setRefClass("Queue",
                     fields=list(items="list"),
                     methods=list(
                       initialize=function(...) {
                         items <<- vector("list")
                         callSuper(...)
                       },
                       finalize=function() {
                         empty()
                         invisible()
                       },
                       push=function(item) {
                         n=length(items)
                         items[[n+1]] <<- item
                         invisible(NULL)
                       },
                       pop=function() {
                         n=length(items)
                         if (n<1)
                           return(NULL)
                         out=items[[1]]
                         items[[1]] <<- NULL
                         return(out)
                       },
                       empty=function() {
                         items <<- vector("list")
                         invisible()
                       })
)

epivizrMsg <- function(...) message("[epivizr]", ...)