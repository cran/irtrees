##' Convert a tree description in mermaid format into a mapping matrix
##' that can be used with the remaining functions in the package.
##' 
##' @title Convert a tree to a mapping matrix
##' @param td tree description in mermaid format
##' @return the mapping matrix
graph2mx = function(td){
td = gsub('>','',td)
tdv = strsplit(td,'\n')
tdl = strsplit(tdv[[1]][-1],'--')
from = trimws(sapply(tdl,function(x)x[1]))
to = trimws(sapply(tdl,function(x)x[3]))
value = trimws(sapply(tdl,function(x)x[2]))
from_labs = sapply(strsplit(from,'[[:punct:]]{2}'),function(x)x[2])
from = sapply(strsplit(from,'[[:punct:]]{2}'),function(x)x[1])
funny = which(from!=from_labs)
if(length(funny)) {
        funny = data.frame(from, from_labs)[funny,]
        funny = funny[!duplicated(funny),]
} else funny = NULL
to_lat = sapply(strsplit(to,'[[:punct:]]{2}'),function(x)x[1])
to_obs = sapply(strsplit(to,'[[:punct:]]{1}'),function(x)x[1])
lat_tos = grep('[[:punct:]]{2}', to)
obs_tos = setdiff(1:length(value),lat_tos)
to = rep('',length(value))
to[lat_tos] = to_lat[lat_tos]
to[obs_tos] = to_obs[obs_tos]
latents = sort(unique(from))
sorted = c(latents,sort(to_obs[obs_tos]))
nn = length(sorted)
nl = length(latents)
ndf = create_node_df(n=nn,label=sorted)
edf = create_edge_df(from=match(from,sorted), to=match(to,sorted), value=as.numeric(value), rel='rel')
grf = create_graph(ndf,edf,directed=TRUE)
ffrom = tto = leaf = integer(0)
for  (i in 1:nl) {
        for (j in (nl+1):nn) {
                pth = get_paths(grf, from=i,to=j)[[1]]
                ffrom = c(ffrom, pth[-length(pth)])
                tto = c(tto, pth[-1])
                leaf = c(leaf,rep(j,length(pth)-1))
        }
}
frm = data.frame(from=ffrom, to=tto, leaf=leaf)
frm = merge(frm, edf, by=c('from','to'), all.x=T, all.y=F)
mx = suppressWarnings(as.matrix(reshape2::dcast(frm,leaf~from, fun.aggregate=max)))
mx[!is.finite(mx)] = NA
mx = mx[,-1]
if (!is.null(funny)) {
        for (i in 1:nrow(funny)) {
                pai = unclass(funny[i,])
                pan = match(pai,latents)
                kill = max(pan)
                keep = min(pan)
                new_col = pmax(mx[,keep], mx[,kill], na.rm=TRUE)
                mx[,keep] = new_col
                mx = mx[, -kill]
                latents = latents[-kill]
        }
}
mx
}