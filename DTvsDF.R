library(data.table)
library(ggplot2)

# table builders for testing -----------------
buildtable <- function(numcols,numrows,numlevels){
    colnames = vapply(1:numcols,function(n){paste0("X",n)},"")
    
    values =  lapply(colnames, function(name){sample(1:numlevels,numrows,replace=T)})
    names(values) = colnames
    
    data.frame(values)
}

# this basically makes sure that we don't compute so many samples of the function for large dataframes/datatables
# for really big ones, 1 sample is sufficient to get the idea
numsamples=function(size,maxsamples){floor(1+(maxsamples)/size)}

# function to compare a df operation to a dt operation over a list of data sizes
df_dt_benchmark <- function(sizes,df.op,dt.op,
                        numcols=10,numkeys=3,numlevels=20,
                        log_scale_x = T,
                        log_scale_y = F,
                        maxsamples = 100000){
    
    # allocate a dataframe to hold the results
    results <-  data.frame(size=sizes,logsize=log(sizes),dataframe=rep(NA,length(sizes)),datatable=rep(NA,length(sizes)))
    
    # loop over the specified data sizes
    for(i in 1:length(sizes)){
        size <-  results$size[i]
        
        # make a dataframe with the specified number of rows, columns, and levels
        df <-  buildtable(numcols = numcols, numrows = size, numlevels = numlevels)
        # make a datatable from this
        dt <-  data.table(df, key = names(df)[1:numkeys])
        
        # # we make samples a decreasing function of size to prevent catching our computer on fire with big dataframes
        samples <- numsamples(size,maxsamples)
        print(paste("benchmarking ",size,"rows with",samples,"samples"))
        
        # how fast does the dataframe do its thing?
        dftime = system.time({
            for(j in c(1:samples)){
                temp = df.op(df)
            }
        }, gcFirst = T)[[3]]
        dftime = dftime/samples
        
        # # how fast does the datatable do its thing?
        dttime = system.time({
            for(j in c(1:samples)){
                temp = dt.op(dt)
            }
        }, gcFirst = T)[[3]]
        dttime = dttime/samples
        
        results[i,'dataframe'] = as.numeric(dftime)
        results[i,'datatable'] = as.numeric(dttime)
    }
    # collect the garbage; we potentially just allocated some pretty big data to temp variables
    gc()
    
    graph = plotbenchmark(results,log_scale_x,log_scale_y)
    graph
    return(list(graph=graph,results=results))
}

# function to plot the results of the above function
plotbenchmark <- function(resultsdf,logx=F,logy=F){
    graph = ggplot(data = resultsdf) + geom_line(aes(x=size,y=dataframe,color="data.frame")) + geom_line(aes(x=size,y=datatable,color="data.table"))
                            
    if(logx){
        if(logy){
            graph + scale_x_log10() + scale_y_log10() + 
                labs(x="log(number of rows)", y="log(average execution time)",color="Table Type")
        } else {
            graph + scale_x_log10() +
                labs(x="log(number of rows)", y="average execution time",color="Table Type")
        }
    } else {
            graph + labs(x="number of rows", y="average execution time",color="Table Type")
    }
}

