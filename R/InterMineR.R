# require(RCurl)
# require(XML)
# require(RJSONIO)
# require(sqldf)
# require(igraph)
# require(Biostrings)

listMines <- function(){
    mines <- c('http://www.flymine.org/flymine',
    'http://www.mousemine.org/mousemine',
    'http://ratmine.mcw.edu/ratmine',
    'http://www.wormbase.org/tools/wormmine',
    'http://yeastmine.yeastgenome.org/yeastmine',
    'http://zebrafishmine.org',
    'http://targetmine.mizuguchilab.org/targetmine',
    'http://mitominer.mrc-mbu.cam.ac.uk/release-3.1',
    'http://www.humanmine.org/humanmine',
    'http://www.cbrc.kaust.edu.sa/indigo',
    'https://apps.araport.org/thalemine',
    'http://medicmine.jcvi.org/medicmine',
    'http://phytozome.jgi.doe.gov/phytomine')

    names(mines) <- c('FlyMine',
    'MouseMine',
    'RatMine',
    'WormMine',
    'YeastMine',
    'ZebraFishMine',
    'TargetMine',
    'MitoMiner',
    'HumanMine',
    'indigoMine',
    'thalemine',
    'medicmine',
    'PhytoMine')
    mines
}


##0 - Initilization
# initialize the base and token for future reuse
initInterMine <- function(mine = listMines()["HumanMine"], token=""){
    im <- list(mine = mine, token = token)
    im
}


##1 - Version
#the implementation of the web service:
#<servlet-name>ws-version</servlet-name>
#<url-pattern>/service/version/*</url-pattern>
getVersion <- function(im, timeout=3){
    r <- GET(paste(im$mine, "/service/version", sep = ""))
    stop_for_status(r)
    v <- content(r)
    v$version
}

getRelease <- function(im, timeout=3){
    r <- GET(paste(im$mine, "/service/version/release", sep = ""))
    stop_for_status(r)
    v <- content(r)
    v$version
}


##2 - Model
#the implementation of the web service:
#<servlet-name>ws-model</servlet-name>
#<url-pattern>/service/model/*</url-pattern>
getModel <- function(im, timeout=3){
    r <- GET(paste(im$mine, "/service/model", sep=""))
    stop_for_status(r)
    model.string <- content(r, "text")
    model <- fromJSON(model.string)$model$classes
    res <- listModelSummary(model)
    res
}

listModelSummary <- function(model){
    class.name <- names(model)
    class.parent <- lapply(class.name, function(x) {
        y <- model[[x]][["extends"]]
        if(is.list(y)){
            y <- NA
        }
        y
    })

    class.name <- rep(class.name, sapply(class.parent, length))
    class.parent <- unlist(class.parent)
    igr <- graph.data.frame(data.frame(
        parent=class.parent[which(!is.na(class.parent))],
        name=class.name[which(!is.na(class.parent))]),
        vertices=data.frame(unique(c(class.name,
            class.parent[which(!is.na(class.parent))]))))

    igr.sp <- shortest.paths(igr, mode="in")
    att <- lapply(class.name, function(x) data.frame(do.call(rbind,
    model[[x]][["attributes"]]), stringsAsFactors=FALSE))
        names(att) <- class.name

        att.ext <- rep(list(NULL), length(class.name))
        att.ext <- lapply(class.name, function(x){
            ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]

            y <- unique(do.call(rbind, att[ext]))
            y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
            colnames(y) <- c("type", "child_name", "child_type")
            y <- y[order(y$child_name),, drop=FALSE]
            rownames(y) <- NULL
            y
        })
    att.ext <- do.call(rbind, att.ext)
    att.ext$child_type <- ""
    rownames(att.ext) <- NULL
                  
    # Error occuring when using HumanMine:
    # The fourth column of the att.ext variable is redundant and will prevent the
    # rbind(att.ext, ref.ext, col.ext) below!!!
  
     # columns 2 and 4 contain identical information
    all(tolower(att.ext[,2]) %in% gsub(" ", "", tolower(att.ext[,4])))
  
    # Therefore, we keep only the first 3 columns from the att.ext variable:
    att.ext = att.ext[,1:3]

    ref <- lapply(class.name, function(x) {
        y <- model[[x]][["references"]]
        if(length(y)==0){
            z <- data.frame(matrix(character(0), 0, 2, dimnames=list(NULL,
                c("name", "referencedType"))))
        } else {
            z1 <- names(y)
            z2 <- sapply(y, function(ye)
            ye[["referencedType"]])
            z <- data.frame(name=z1, referencedType=z2)
        }
        z
    })
    names(ref) <- class.name

    ref.ext <- rep(list(NULL), length(class.name))

    ref.ext <- lapply(class.name, function(x) {
        ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
        y <- unique(do.call(rbind, ref[ext]))
        y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
        colnames(y) <- c("type", "child_name", "child_type")
        y <- y[order(y$child_name),, drop=FALSE]
        rownames(y) <- NULL
        y
    })

    ref.ext <- do.call(rbind, ref.ext)
    rownames(att.ext) <- NULL

    col <- lapply(class.name, function(x) {
        y <- model[[x]][["collections"]]
        if(length(y)==0){
            z <- data.frame(matrix(character(0), 0, 2,dimnames=list(NULL,
                c("name", "referencedType"))))
        } else {
            z1 <- names(y)
            z2 <- sapply(y, function(ye) ye[["referencedType"]])
            z <- data.frame(name=z1, referencedType=z2)
        }
        z
    })
    names(col) <- class.name

    col.ext <- rep(list(NULL), length(class.name))

    col.ext <- lapply(class.name, function(x) {
        ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
        y <- unique(do.call(rbind, col[ext]))
        y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
        colnames(y) <- c("type", "child_name", "child_type")
        y <- y[order(y$child_name),, drop=FALSE]
        rownames(y) <- NULL
        y
    })
    col.ext <- do.call(rbind, col.ext)
    rownames(col.ext) <- NULL
    res <- rbind(att.ext, ref.ext, col.ext)
    rownames(res) <- NULL
    res <- sqldf("select * from res order by type, child_type")
    res
}
