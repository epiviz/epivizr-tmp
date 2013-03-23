IndexedArray <- setRefClass("IndexedArray",
                            fields=list(nextId="integer", items="list"),
                            methods=list(
                              initialize=function() {
                                nextId <<- 1L
                                items <<- vector("list")
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
                              })
)

Queue <- setRefClass("Queue",
                     fields=list(items="list"),
                     methods=list(
                       initialize=function() {
                         items <<- vector("list")
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
                       })
)
