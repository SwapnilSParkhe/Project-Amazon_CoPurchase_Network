###############################################
#AMAZON Co-purchase network
##############################################

library(igraph)
library(dplyr)
library(data.table)

#----Setting directory
setwd("/Users/swapnilparkhe/Desktop/MSBA-UIC/IDS 576 - Social Media/Assignments/Project")
dir()

#---Importing data from work directory
Amazon_Mar_02<-read.table("Amazon0302.txt", sep="\t")
Amazon_Mar_12<-read.table("Amazon0312.txt", sep="\t")
Amazon_May_05<-read.table("Amazon0505.txt", sep="\t")
Amazon_Jun_01<-read.table("Amazon0601.txt", sep="\t")
Amazon_meta  <-read.csv("Amazon-meta.csv", sep=",", header=TRUE, fill=TRUE)

#---Analytical dataset creation or Data manipulations (Joining)
#Quality check->Checking which IDs have more than row
qc<-sqldf("select *
          from Amazon_meta
          group by id
          having count(*)>1")

#Joining data (left being our timestamp data, and right being metadata)
#Note-1: g1.data corresponds to joined data for first time stamp data, and similary for other timestamp data
#Note-2: Using data.table option for faster joins
g1.data<-left_join(data.table(Amazon_Mar_02,key="V1"),
                   data.table(Amazon_meta,key="id"),
                   by=c("V1"="id"))%>%
         left_join(.,data.table(Amazon_meta,key="id"),
                   by=c("V2"="id"))%>%
         as.data.frame()

g2.data<-left_join(data.table(Amazon_Mar_12,key="V1"),
                   data.table(Amazon_meta,key="id"),
                   by=c("V1"="id"))%>%
         left_join(.,data.table(Amazon_meta,key="id"),
                   by=c("V2"="id"))%>%
         as.data.frame()

g3.data<-left_join(data.table(Amazon_May_05,key="V1"),
                   data.table(Amazon_meta,key="id"),
                   by=c("V1"="id"))%>%
         left_join(.,data.table(Amazon_meta,key="id"),
                   by=c("V2"="id"))%>%
         as.data.frame()

g4.data<-left_join(data.table(Amazon_Jun_01,key="V1"),
                   data.table(Amazon_meta,key="id"),
                   by=c("V1"="id"))%>%
         left_join(.,data.table(Amazon_meta,key="id"),
                   by=c("V2"="id"))%>%
         as.data.frame()

#---Converting into graph data frame
g1<-graph.data.frame(g1.data, directed = TRUE, vertices= NULL)
g2<-graph.data.frame(g2.data, directed = TRUE, vertices= NULL)
g3<-graph.data.frame(g3.data, directed = TRUE, vertices= NULL)
g4<-graph.data.frame(g4.data, directed = TRUE, vertices= NULL)


#----------------------------------------
#For mygraph
#---------------------------------------

network_EDA<-function(mygraph){
  
  #*********EDA-NUMERIC***********
  #---Basics
  vcount_mygraph<-vcount(mygraph)
  ecount_mygraph<-ecount(mygraph)
  simple_mygraph<-is.simple(mygraph)
  
  #---Density (same using both)
  density_mygraph<-edge_density(mygraph,loops=F)
  density_mygraph_v1<-ecount(mygraph)/(vcount(mygraph)*(vcount(mygraph)-1))
  
  #Connectivity
  conctvty_strng_mygraph<-is_connected(mygraph, mode="strong")
  conctvty_weak_mygraph<-is_connected(mygraph, mode="weak")
  
  #---Reciprocity
  reciprocity_mygraph<-reciprocity(mygraph)
  
  #---Transitivity & Triad Census
  trans_glob_mygraph<-transitivity(mygraph,type="global")
  trans_locl_mygraph<-mean(transitivity(mygraph, type="local"),na.rm=T)
  trans_avg_mygraph<-transitivity(mygraph,type="average",isolates = c("zero"))
  triad_census_mygraph<-triad_census(mygraph)
  
  #---Diameter & Nodes along the diameter
  #dia_mygraph<-diameter(mygraph,directed=T,weights=NA)
  #dia_nodes_mygraph<-as.vector(get_diameter(mygraph, directed=T))
  
  #---Centrality measures
  #betwn_cen_mygraph<-betweenness(mygraph, directed=T, weights=NA)
  #close_cen_mygraph<-closeness(mygraph, mode="all", weights=NA) 
  #eigen_cen_mygraph<-eigen_centrality(mygraph, directed=T, weights=NA)
  
  #---Hubs and Authorities
  hub_score_mygraph<-hub_score(mygraph, weights=NA)$vector
  ath_score_mygraph<-authority_score(mygraph, weights=NA)$vector
  
  #---Distances and Paths
  #mean_dist_mygraph<-mean_distance(mygraph, directed=T)
  #count_dist_mygraph<-distances(mygraph)
  
  #---Cliques, Subgroups and Communities
  #First making it undirected
  UDNW_mygraph<-as.undirected(mygraph, mode= "collapse")
  
  #Cliques
  #cliques(mygraph) # list of cliques       
  #sapply(cliques(mygraph), length) # clique sizes
  largest_cliques(mygraph)
  
  #Communities
  clus_fast_mygraph<-fastgreedy.community(as.undirected(mygraph))
  
  #---Assortativity and Homophily
  deg_asortvty_mygraph<-assortativity_degree(mygraph, directed=T)
  
  #---Giant component
  #giant_comp_mygraph<-giant.component.extract(mygraph, directed = TRUE, bipartite.proj = FALSE, num.proj = 1)
  
  
  #********EDA-PLOTS**********
  #---Node Degrees - SImple graph
  par(mfrow=c(3, 1))
  tot_degree_mygraph<-degree(mygraph)
  plot(table(tot_degree_mygraph),type="p",main="Scatter plot of node all-degree",col="red")
  
  in_degree_mygraph<-degree(mygraph,mode="in")
  plot(table(in_degree_mygraph),type="p", main="Scatter plot of node in-degree",col="green")
  
  out_degree_mygraph<-degree(mygraph,mode="out")
  plot(table(out_degree_mygraph),type="p",main="Scatter plot of node out-degree",col="blue")
  
  #---Degree Distributions - Cummumlative graph
  par(mfrow=c(3, 1))
  tot_deg_dist_mygraph<-degree_distribution(mygraph, cumulative=T, mode="all")
  plot(x=0:max(tot_degree_mygraph), y=1-tot_deg_dist_mygraph, 
       pch=19, cex=1, col="red", xlab="Degree", ylab="Cumulative Frequency",
       main="Scatter plot of node all-degree")
  
  in_deg_dist_mygraph<-degree_distribution(mygraph, cumulative=T, mode="in")
  plot(x=0:max(in_degree_mygraph), y=1-in_deg_dist_mygraph, 
       pch=19, cex=1, col="green", xlab="Degree", ylab="Cumulative Frequency",
       main="Scatter plot of node in-degree")
  
  out_deg_dist_mygraph<-degree_distribution(mygraph, cumulative=T, mode="out")
  plot(x=0:max(out_degree_mygraph), y=1-out_deg_dist_mygraph, 
       pch=19, cex=1, col="blue", xlab="Degree", ylab="Cumulative Frequency",
       main="Scatter plot of node out-degree")
  
  #---Power law graph code
  #Tot degree - Using objects produced above
  par(mfrow=c(3, 1))
  d <- 1:max(tot_degree_mygraph)-1
  ind <- (tot_deg_dist_mygraph != 0)
  plot(d[ind], tot_deg_dist_mygraph[ind], log="xy", col="red",
       xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
       main="Log-Log Degree Distribution")
  
  #in degree - Using objects produced above
  d <- 1:max(in_degree_mygraph)-1
  ind <- (in_deg_dist_mygraph != 0)
  plot(d[ind], in_deg_dist_mygraph[ind], log="xy", col="blue",
       xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
       main="Log-Log Degree Distribution")
  
  #out degree - Using objects produced above
  d <- 1:max(out_degree_mygraph)-1
  ind <- (out_deg_dist_mygraph != 0)
  plot(d[ind], out_deg_dist_mygraph[ind], log="xy", col="blue",
       xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
       main="Log-Log Degree Distribution")
  
}

network_EDA(g1)
network_EDA(g2)
network_EDA(g3)
network_EDA(g4)

