plotarrays<-function(coord, axis1 = 1, axis2 = 2, arraylabels = NULL, classvec=NULL,  graph = c("groups", "simple", "labels", "groups2", "coinertia","coinertia2"), labelsize=1, star=1, ellipse=1, arraycol=NULL, ...){



    graph=match.arg(graph)

    if (!inherits(coord, "cia")) {
        if (!inherits(coord, "data.frame")) {
          if (inherits(coord, "bga")){
              if (is.null(classvec)) classvec= coord$fac
              coord = coord$bet$ls
              }
              
           if (inherits(coord, "between"))  coord = coord$ls
           if (inherits(coord, "ord")) {
              if (is.null(classvec)) {if (!is.null(coord$fac)) {
                        classvec= coord$fac
               }
              coord = coord$ord$co
              }
           if  (inherits(coord, "dudi"))  coord = coord$co
           }
        }

      }
       if (!is.null(classvec)) {
           classvec = checkfac(classvec)
           if (is.null(arraycol)) arraycol=getcol(1:length(levels(classvec)))
        }

    #print(classvec)
        colpts=NULL
        if (is.null(arraylabels)) arraylabels= row.names(coord)

        switch(graph,
                 "simple" = {labelsize=0; if (!is.null(arraycol)) colpts = as.character(factor(classvec, labels=arraycol))},
                 "labels" = {if (!is.null(arraycol)) colpts = as.character(factor(classvec, labels=arraycol))}   ,
                 "groups" = {if (is.null(classvec)) print("Need to specify groups")} ,
		 "groups2" = {star=0; ellipse=0; if (is.null(classvec)) print("Need to specify groups")} ,
                 "coinertia"= {if (all(!inherits(coord, "cia"), !inherits(coord, "coinertia")))
                                  print("Type of class cia or coinertia expected") },
                 "coinertia2"= {labelsize=0; if (!is.null(arraycol)) colpts = as.character(factor(classvec, labels=arraycol)); if (all(!inherits(coord, "cia") , !inherits(coord, "coinertia")))
                                  print("Type of class cia or coinertia expected") },
                  stop(paste("The type of plot",graph, "was not recognised. Permitted are: simple, labels, groups, groups2, coinertia", "The default is groups",
                  sep = " ")))

  
     if (!inherits(coord, "cia")) {
     if (!is.null(classvec)&&is.null(colpts)) {
          s.groups(coord, classvec=classvec, col=arraycol,  xax = axis1, yax = axis2,  clabel= labelsize, cstar=star, cellipse= ellipse,... )
        }

     if (!is.null(colpts)){
          s.var(coord, colpoints = colpts, label=arraylabels,  xax = axis1, yax = axis2, clabel=labelsize,...)
          }
          
     if (is.null(classvec)) {
        s.var(coord,   xax = axis1, yax = axis2, label=arraylabels,  clabel=labelsize,...)
      }
   }
   

   if (inherits(coord, "cia")) {
      if (is.null(arraylabels)) arraylabels= row.names(coord$coinertia$mX)
      if (!is.null(arraycol)) colpts = as.character(factor(classvec, labels=arraycol))
      s.match.col(coord$coinertia$mX, coord$coinertia$mY, xax = axis1, yax = axis2, classvec=classvec, label=arraylabels,  clabel=labelsize, col=colpts, ...)
      }
    if (inherits(coord, "coinertia")) { 
         if (is.null(arraylabels)) arraylabels= row.names(coord$mX)
         if (!is.null(arraycol)) colpts = as.character(factor(classvec, labels=arraycol))
         s.match.col(coord$mX, coord$mY, xax = axis1, yax = axis2, classvec=classvec, label=arraylabels, clabel=labelsize,col=colpts,...)
         }
    
    }
