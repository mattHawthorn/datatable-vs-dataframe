library(data.table)
library(ggplot2)

# put the directory to the code here if R starts in some fixed working directory for you
setwd('/home/matt/Documents/Presentations/datatable/')

# then this will load the relevant functions
source(file = 'DTvsDF.R')

##############################################################################
# If you have any questions about the code that follows, you can contact me at
# mah6wt@virginia.edu

# The bulk of this code is to demonstrate data.table's speed on certain (in this case very simple) operations.
# If you're a dplyr user, you're already better off than if you were using plain data frames, but certain 
# operations at certain data sizes can still be quite a lot faster with data.table. See, e.g.:
# https://github.com/Rdatatable/data.table/wiki/Benchmarks-%3A-Grouping

# there is a very good faq for the uninitiated, that answers all kinds of questions I don't address here:
vignette("datatable-faq")



##############################################################################
# Now a few basics
# a data.table is really a data frame with perks ------------------------------------------

# here's a toy dataframe
toydf= data.frame(list(a = 1:3, b=4:6))
toydf

# here's it's class according to the R object system
class(toydf)

# and here's a quick way to create a data.table
toydt = data.table(toydf)

# note: you can also read data directly from the disk in tabular text form with data.table::fread()
help('fread')
# which claims to be pretty smart about auto-dectecting separators and datatypes, but I can't personally
# vouch for it.
# This could be helpful if your data is really huge and you don't want to risk having 2 copies in memory
# (which would need to happen to create the dt from a preloaded df)

# Moving on...
# we see that the class of a data.table is both data.table and data.frame; it inherits from data.frame
class(toydt)
# It has the same attributes:
dim(toydf)
dim(toydt)
names(toydf)
names(toydt)
# and a few more, most importantly this one, which we'll get back to:
key(toydt)
# we haven't set this yet (so NULL), but we will.

# So anything you can do to a data.frame you can do with a data.table.
# the standard $ column access returns the same results, for example:
toydf$a[1:2]
toydt$a[1:2]
# for the most part, the syntax of df's carries over to dt's, but dt's have some added syntactic perks.
# in addition, there are a few edge cases where dataframe syntax behaves *differently* on a data.table.
# For instance:
toydf[1]
# returns the first column, while
toydt[1]
# returns the first row.
# soooo.... if you want to make the switch for a project that's proving just too big for a dataframe,
# but you've already written a lot of code, you might have some refactoring to do.

# but just for reference, here's another:
toydf[1,'b']
toydt[1,'b']
# that is a curious example indeed.
# double brackets fixes the problem:
toydf[[1,'b']]
toydt[[1,'b']]
# and in this case as well:
toydf[[1]]
toydt[[1]]
# but these are the slice/index operations that return bare, unnamed vectors, so addmittedly they're 
# not commonly used.  Long and short of it: if you want to make the switch mid-development, you probably
# have a little work ahead.  But if you start a project in the data.table mindset and use its idioms 
# throughout your code (for the big data parts anyway), then you're golden.


# Now, this 'key' thing. This command:
setkey(toydt,'a')
# tells data.table to use the 'a' column as a key for accessing rows in toydt
# then we have this new notation at our disposal:
toydt[.(2)]
# which is quite handy compared to 
toydf[toydf$a==2,]
# and I can have several, hierarchical keys as well, like this:
setkeyv(toydt,c('a','b'))
# which gives me both:
toydt[.(2),]
# AND
toydt[.(3,6)]
# which is even more handy than
toydf[toydf$a==3&toydf$b==6,]

# additionally, I can set the keys at the time of instantiation by passing a 'key' arg:
toydt = data.table(toydf,key = c('a','b'))



##################################################################################################
# But syntactic sweetness isn't the only gift here!
# With big enough data, these keys can really pay off.
# Under the hood, they allow a binary search on the key values, which means the number of operations
# required to find a row scales like
#                log(N)
# vs the standard data.frame linear scan of all the values, which scales simply like
#                N
# where N is the number of rows.
# So data.table access of values **in keyed columns** is literally exponentially faster than for dataframes.
# This really starts to pay off as your data size grows.

# Now, in order to take advantage of that speed, data.tables must be sorted by 
# the key column values, which takes a little time up front .
# As a technicality, this operation's time cost grows like N*log(N), so a little more than the 
# needed to simply copy values into a dataframe, which is more like N.  But if you do a lot of slicing,
# it's totally worth the little bit of upfront cost!

# That's what we'll explore here.



# -------------------------------------------------------------------
# Now some benchmarks.
# We'll vary the number of rows exponentially from 10^3 to 10^7 (10M), trying some basic operations on a data.frame and a data.table

# CAUTION: on my machine, a 10-column, 10^8 row data.frame takes up about 6.4Gb of system memory, even though the object size in R is reported as only 4Gb.
# be sure you have the memory free for your benchmarks.
# Also, try not to have any heavy processes running at the same time, if you want a fair benchmark; time constraints prevent us  from taking large samples
# for the bigger benchmarks, so a fairer comparison will result if R isn't competing with other processes.

# to check, build a table of a given size (this one should take you about 400Mb):
df = buildtable(10,10^7,10)
object.size(df)
# garbage cleanup is your friend when your data is big:
gc()
# R isn't very good about doing this automatically for you


# these are the row counts that we'll benchmark
sizes = 10^(3:7)

# And first we'll test this function on both a data.table and a data.frame.
# It simply takes a data.table/frame and performs a simple conditional slice operation
# (note: in what follows, always use 'X1','X2','X3'... for absolute column references):
sliceDF = function(df){ df[df$X1==2&df$X2==2&df$X3==2,] }

# Now, the benchmark function call 
results = df_dt_benchmark(sizes = sizes,
                          df.op = sliceDF,
                          dt.op = sliceDF,
                          numcols = 10,
                          numkeys = 3,
                          numlevels = 10,
                          log_scale_x = T,
                          log_scale_y = F
                        )
# generates a random table with N rows and numcol columns for every N in the vector 'sizes',
# and then populates it randomly and uniformly with ints from the range 1:'numlevels',
# and finally turns this into a data.table with 'numkeys' keys, keeping the original data.frame version
# for comparison.

# Try not to worry if it seems hung up.  That one takes ~1m on my laptop.  It should finish with no problems.


# Now,
# 'df.op' can be any operation we want to test on the data.frame (so a function that simply takes a data.frame,
# and does something to it: slice, reassignmnent, etc.  Generally one line of code)
# 'dt.op' should be semantically the same thing for a data.table. In this example, we used the same function for both.

# Again, for your benchmarks, use 'X1','X2','X3'... when referencing columns by name 
# (They appear in the generated data frame in the order that they're numbered).

# just define 
dfoperation = function(df){ }# <--do something to dataframe df in here 
dtoperation = function(dt){ }# <--do something to datatable dt in here 
# and pass them to df.op in the above benchmarking function as
# df.op and dt.op


# Now, the output of df_dt_benchmark is a length-2 list.
# the first entry is a ggplot object, so:
results[1]
# plots it.
# change scale_log_x and scale_log_y to suit your needs in the call to df_dt_plot.
# The default is a log scale for x, since generally we're interested in how the behavior scales
# as the data size changes by orders of magnitude.

# So notice in this plot we have two basically exponential curves, growing at the same rate as the number of rows (which is also exponential)
# These slices are simply order-N operations, because using the syntax df[df$X1==2&df$X2==2&df$X3==2,] tells both a data.table and a data frame
# to simply scan every row and hold on to the rows that match the query.
# data.table hasn't stood out yet, except for some minor optimizations in the underlying C code that shave off ~25% of the time across the board.


#######################################################################
# Now it's time to see data.table work the magic that it was built for!


# Here's the same query as performed by the function sliceDF above, but translated to data.table syntax, with 3 keys:
sliceDT = function(dt){ dt[.(2,2,2),] }
# For reference, here's the data.frame version again:
sliceDF = function(df){ df[df$X1==2&df$X2==2&df$X3==2,] }

# Now, let's run the benchmark again with dt.op replaced with the data.table version:
results = df_dt_benchmark(sizes = sizes,
                          df.op = sliceDF,
                          dt.op = sliceDT,
                          numcols = 10,
                          numkeys = 3,
                          numlevels = 10,
                          log_scale_x = T,
                          log_scale_y = F
)
gc()

# plot the results:
results[1]

# The slice operation time on a data.table looks basically constant all the way up to 10^8 rows!
# The difference is literally several orders of magnitude for the larger tables.

# It's hard to spot the break-even point here, since the difference is so stark.
# Let's try a less astronomical range.
sizes = 2^c(9:16)

# same benchmark:
results = df_dt_benchmark(sizes = sizes,
                          df.op = sliceDF,
                          dt.op = sliceDT,
                          numcols = 10,
                          numkeys = 3,
                          numlevels = 10,
                          log_scale_x = T,
                          log_scale_y = F
)
gc()

# plot the results
results[1]

# and the second entry
results[2]
# Aha! The break-even point for this kind of operation is somewhere just south of 10,000 rows.
# And somewhere around 30,000 rows it really starts to be Worth It.
# The cost is higher for data.tables than data.frames below the 10,000 mark because the overhead in
# the delicate and complex operations going on to take advantage of the sorted keys is bigger than the gains.

# How does this change with the number of keys? Run another benchmark!
sliceDF = function(df){ df[df$X1==3&df$X2==4,] }
sliceDT = function(df){ df[.(3,4)] }

results = df_dt_benchmark(sizes = sizes,
                          df.op = sliceDF,
                          dt.op = sliceDT,
                          numcols = 10,
                          numkeys = 2,
                          numlevels = 10,
                          log_scale_x = T,
                          log_scale_y = F
)
gc()

results[1]
# Now the break-even point is maybe a little higher, 12,000 rows or so.
# We're losing a little bit of the power of the keys by dropping one.
# Still definitely worth it up there at 30,000-65,000 though, if we're doing a lot of these operations.

# If you want to do more careful analysis on how the execution time is varying with the table size,
results[2]
# has a little dataframe with the results of the benchmark (not a data.table, it's too small!)
# The 'size' column is just the number of rows in the test data, and the columns give the average execution
# times for 1 call each of the data.table and data.frame operations, in seconds.


# Here's another example:
# A group by operation: sum the 4th column, grouped by the 1st (which is keyed for a data.table here):
groupbyDF = function(df){ aggregate(df$X2, by = list(df$X1), FUN = sum)}
groupbyDT = function(dt){ dt[, c('X11'):=list(sum(X2)), by=list(X1)] }
# now, the data.frame is actually advantaged here, because we're not *assigning* the aggregation to a column.
# It doesn't in general have the right shape. dplyr I'm sure makes this sort of thing trivial, but so does
# data.table, with the syntax above.  The := is an assign-by-reference, which if we weren't allocating a new
# column would be even faster than it is here.  Let's compare


# Try them out:
df = buildtable(10 ,10^6,10)
dt = data.table(df,key = c('X1'))

groupbyDF(df)
groupbyDT(dt);  dt
# same values, but the dt version unpacks them into the X11 column
# Note the wait time difference.

# Let's benchmark it
sizes = 2^c(9:18)

results = df_dt_benchmark(sizes = sizes,
                          df.op = groupbyDF,
                          dt.op = groupbyDT,
                          numcols = 11,
                          numkeys = 1,
                          numlevels = 10,
                          log_scale_x = T,
                          log_scale_y = F
)
gc()
results[1]

# A very stark contrast again, and we get column assignment in the data.table version!



# -------------------------------------------------------------------------

# Try it yourself-
# take some operation you do all the time on data.frames, then look up the handy data.table syntax here:
#############################################################################
# https://cran.r-project.org/web/packages/data.table/.../datatable-intro.pdf

# or for more advanced usage, here:
#############################################################################
# https://cran.r-project.org/web/packages/data.table/data.table.pdf
# pp3-11 have most of the core syntax and several useful examples.

# Again, if all that is intimidating, start with:
vignette("datatable-faq")

# These resources also have much, much more to say about the advantages of data.tables than I said here.



# When you have an operation in mind, pop that syntax into a little function as we did above, and pass it, along with a semantically
# identical operation for a data.frame, to 
# df_dt_benchmark()
# using the 'df.op' and 'dt.op' arguments for your example functions.

# change the levels, sizes, and numbers of columns and keys to whatever you like, to get an idea
# of how this would perform on your particular data, before you start development.

# if you want to simulate keying on a continuous variable, just set 'numlevels' very high.

# Let me know if you have any astounding successes!

# Matt Hawthorn
# mah6wt@virginia.edu