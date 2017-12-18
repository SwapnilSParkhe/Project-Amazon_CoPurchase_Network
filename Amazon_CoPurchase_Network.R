
###############################################
#AMAZON Co-purchase network data
##############################################

#-------------------------------------------------
#Calibration, Data Imports, Data Manipulations
#-------------------------------------------------
library(igraph)
library(dplyr)
library(data.table)
library(sqldf)

#----Setting directory
setwd("/Users/swapnilparkhe/Desktop/MSBA-UIC/IDS 576 - Social Media/Assignments/Project")
dir()

#---Importing data from directory
Amazon_Mar_02<-read.table("Amazon0302.txt", sep="\t")
Amazon_Mar_12<-read.table("Amazon0312.txt", sep="\t")
Amazon_May_05<-read.table("Amazon0505.txt", sep="\t")
Amazon_Jun_01<-read.table("Amazon0601.txt", sep="\t")
Amazon_meta  <-read.csv("Amazon-meta.csv", sep=",", header=TRUE, fill=TRUE)
load("SocialMediaProject.RData") 

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

g1<-graph.data.frame(g1.data, directed = TRUE, vertices= Amazon_meta)
g2<-graph.data.frame(g2.data, directed = TRUE, vertices= Amazon_meta)
g3<-graph.data.frame(g3.data, directed = TRUE, vertices= Amazon_meta)
g4<-graph.data.frame(g4.data, directed = TRUE, vertices= Amazon_meta)


#-------------------------------------------------
#Exploratory Data Analysis - for all 4 timestamps
#-------------------------------------------------

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
  dia_mygraph<-diameter(mygraph,directed=T,weights=NA)
  dia_nodes_mygraph<-as.vector(get_diameter(mygraph, directed=T))
  
  #---Centrality measures
  betwn_cen_mygraph<-betweenness(mygraph, directed=T, weights=NA)
  close_cen_mygraph<-closeness(mygraph, mode="all", weights=NA) 
  eigen_cen_mygraph<-eigen_centrality(mygraph, directed=T, weights=NA)
  
  #---Hubs and Authorities
  hub_score_mygraph<-hub_score(mygraph, weights=NA)$vector
  ath_score_mygraph<-authority_score(mygraph, weights=NA)$vector
  
  #---Distances and Paths
  mean_dist_mygraph<-mean_distance(mygraph, directed=T)
  count_dist_mygraph<-distances(mygraph)
  
  #---Cliques, Subgroups and Communities
  #First making it undirected
  UDNW_mygraph<-as.undirected(mygraph, mode= "collapse")
  
  #Cliques
  cliques(mygraph) # list of cliques       
  sapply(cliques(mygraph), length) # clique sizes
  largest_cliques(mygraph)
  
  #Communities
  clus_fast_mygraph<-fastgreedy.community(as.undirected(mygraph))
  
  #---Assortativity and Homophily
  deg_asortvty_mygraph<-assortativity_degree(mygraph, directed=T)
  
  #---Giant component
  giant_comp_mygraph<-giant.component.extract(mygraph, directed = TRUE, bipartite.proj = FALSE, num.proj = 1)
  
  
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


#----------------------------------------------------------
#Components & Community Detection: GRAPH G1- TIME STAMP 1
#----------------------------------------------------------
get.vertex.attribute(g1)
tempg1 <- g1

#Finding the components of the graph
#NOTE: Considering mode as both weak and strong, gives one big giant component.
#So we consider only strongly connected components
count_components(tempg1, mode = c("strong"))
components <- components(tempg1,mode = c("strong"))
components$csize[order(-components$csize)]
which(components$csize==130) #5461


#Second largest component : 130 nodes
g1Comp <- induced.subgraph(tempg1, which(components$membership==which(components$csize==130)))
plot.igraph(g1Comp,vertex.size=3, vertex.label = group, edge.arrow.size = 0.4,
            edge.arrow.width = 0.1, vertex.label.cex = 0.5)
g1Comp <- graph.data.frame(g1Comp, directed = TRUE)
get.vertex.attribute(g1)

#giant component : maximum nodes; 241761 nodes
giant_comp <- induced.subgraph(tempg1,
        which(components$membership == which(components$csize == components$csize[which.max(components$csize)])))
giant_comp.g1 <- giant_comp

hub_score_mygraph<-hub_score(g1Comp, weights=NA)$vector
ath_score_mygraph<-authority_score(g1Comp, weights=NA)$vector

plot.igraph(giant_comp,vertex.size=3, edge.arrow.size = 0.4,
            edge.arrow.width = 0.1, vertex.label.cex = 0.5)

#Community detection within the component for SECOND LARGEST COMPONENT
group.g1c <- get.vertex.attribute(g1Comp,"group")


hub_score_mygraph<-hub_score(g2Comp, weights=NA)$vector
ath_score_mygraph<-authority_score(g2Comp, weights=NA)$vector


walktrap.g1 <- walktrap.community(g1Comp, weights = NULL)
c.m.w <- membership(walktrap.g1)
plot(walktrap.g1,g1Comp, vertex.label= NA, vertex.size=hub_score_mygraph*7,edge.arrow.size=0.2, edge.arrow.width = 0.3,
     edge.lty = 5, asp = 0.8, layout= layout.kamada.kawai,
     mark.shape = 0, mark.border = NA)
fast_table <- table(c.m.w, group.g1c, useNA = c("no"))

#Community detection within the component for LARGEST component
group.giantg1 <- get.vertex.attribute(giant_comp,"group")
title.giantg1 <- get.vertex.attribute(giant_comp,"title")


##NOTE : THESE COMMUNITIES HAVE BEEN USED AHEAD and in the entire project 
giant_comp_und1 <- as.undirected(giant_comp, mode = "collapse")
louvain_giantg1 <- cluster_louvain(giant_comp_und1, weights = NULL)
c.m.l <- membership(louvain_giantg1)  
fast_table.g <- table(c.m.l, group.giantg1, useNA = c("no"))
# fast_table.gt <- table(c.m.l, title.giantg1, useNA = c("no"))
max(c.m.l) #126 communities
c.m.l.g1 <- c.m.l

#Calculating the CLUSTERING COEFFICIENTS for each of the communities
sub_comm<-NULL
for (i in seq(1,126,1))
{
  
  gr<-induced.subgraph(giant_comp,louvain_giantg1$membership == i)
  
  #individual vs overall clustering in subgraphs
  overall_clustering <- transitivity(gr)
  
  # Individual clustering
  clustering_i <- transitivity(gr, "local")
  # Average clustering
  avg_clustering <-mean(clustering_i, na.rm = TRUE)
  cc<-data.frame(i, overall_clustering, avg_clustering)
  sub_comm<-rbind(sub_comm,cc)
}

ordered_comm <- sub_comm[order(-sub_comm$overall_clustering),]
ordered_comm[1:2,]
#117th and 126th community have the hghest global clustering coefficients of 0.659 and 0.655

#Plot an individual community with the highesr clustering coefficient

#PLOTTING THE STRINGST COMMUNITY
graph1 <- induced.subgraph(giant_comp,louvain_giantg1$membership == 117)
plot.igraph(graph1,vertex.size=5, edge.arrow.size = 0.2,edge.width = 1.5,edge.arrow.color = "black",
            edge.arrow.width = 1.7, vertex.label.cex = 0.7, edge.color = "light blue"
          , vertex.color = "dark blue",vertex.label=V(graph1)$title,layout=layout.kamada.kawai )


#PLOTTING THE SECONG STRONGEST COMMUNITY
graph2 <- induced.subgraph(giant_comp,louvain_giantg1$membership == 126)
plot.igraph(graph2,vertex.size=2.5, edge.arrow.size = 0.4,asp=0.7,
            edge.arrow.width = 0.3, vertex.label.cex = 0.7, edge.color = "light blue"
            , vertex.color = "dark blue", vertex.label=V(graph2)$title,layout=layout.kamada.kawai)

plot.igraph(graph2,vertex.size=3, edge.arrow.size = 0.4,
            edge.arrow.width = 0.1, vertex.label.cex = 0.8, edge.color = "dark blue"
            , vertex.color = "light blue", vertex.label=V(graph1)$group, layout=layout.kamada.kawai,
            vertex.label.color="purple", vertex.label.degree=0)

fast_table.g[117,]
fast_table.g[126,]


#----------------------------------------------------------
#Components & Community Detection: GRAPH G2- TIME STAMP 2
#----------------------------------------------------------
get.vertex.attribute(g2)
tempg2 <- g2

#Finding the components of the graph
#NOTE: Considering mode as both weak and strong, gives one big giant component.
#So we consider only strongly connected components
count_components(tempg2, mode = c("strong"))
components <- components(tempg2,mode = c("strong"))
components$csize[order(-components$csize)]
which(components$csize==130) #5461


#Second largest component : 130 nodes
g2Comp <- induced.subgraph(tempg2, which(components$membership==which(components$csize==130)))
plot.igraph(g2Comp,vertex.size=2.5, edge.arrow.size = 0.4,asp=0.7,
            edge.arrow.width = 0.3, vertex.label.cex = 0.7, edge.color = "light blue"
            , vertex.color = "dark blue",vertex.label=V(tempg2)$title)
g2Comp <- graph.data.frame(g2Comp, directed = TRUE)
get.vertex.attribute(g2)

#giant component : maximum nodes; 241761 nodes
giant_comp.g2 <- induced.subgraph(tempg2,
                               which(components$membership == which(components$csize == components$csize[which.max(components$csize)])))

hub_score_mygraph<-hub_score(g2Comp, weights=NA)$vector
ath_score_mygraph<-authority_score(g2Comp, weights=NA)$vector

plot.igraph(giant_comp,vertex.size=3, edge.arrow.size = 0.4,
            edge.arrow.width = 0.1, vertex.label.cex = 0.5)

#Community detection within the component for SECOND LARGEST COMPONENT
group.g2c <- get.vertex.attribute(g2Comp,"group")


walktrap.g2 <- walktrap.community(g2Comp, weights = NULL)
c.m.w <- membership(walktrap.g2)
plot(walktrap.g2,g2Comp, vertex.label= NA, vertex.size=hub_score_mygraph*7,edge.arrow.size=0.2, edge.arrow.width = 0.3,
     edge.lty = 5, asp = 0.8, layout= layout.kamada.kawai,
     mark.shape = 0, mark.border = NA)
fast_table <- table(c.m.w, group.g2c, useNA = c("no"))

#Community detection within the component for LARGEST component
group.giantg2 <- get.vertex.attribute(giant_comp.g2,"group")
title.giantg2 <- get.vertex.attribute(giant_comp.g2,"title")


##NOTE : THESE COMMUNITIES HAVE BEEN USED AHEAD and in the entire project 
giant_comp_und.g2 <- as.undirected(giant_comp.g2, mode = "collapse")
louvain_giantg2 <- cluster_louvain(giant_comp_und.g2, weights = NULL)
c.m.l.g2 <- membership(louvain_giantg2)  
fast_table.g <- table(c.m.l.g2, group.giantg2, useNA = c("no"))
# fast_table.gt <- table(c.m.l, title.giantg2, useNA = c("no"))
max(c.m.l.g2) #120 communities

c.m.l.g2[247341]

#Calculating the CLUSTERING COEFFICIENTS for each of the communities
sub_comm<-NULL
for (i in seq(1,120,1))
{
  
  gr<-induced.subgraph(giant_comp,louvain_giantg2$membership == i)
  
  #individual vs overall clustering in subgraphs
  overall_clustering <- transitivity(gr)
  
  # Individual clustering
  clustering_i <- transitivity(gr, "local")
  # Average clustering
  avg_clustering <-mean(clustering_i, na.rm = TRUE)
  cc<-data.frame(i, overall_clustering, avg_clustering)
  sub_comm<-rbind(sub_comm,cc)
}

ordered_comm <- sub_comm[order(-sub_comm$overall_clustering),]
ordered_comm[1:2,]
#117th and 126th community have the highest global clustering coefficients of 0.921 and 0.889

#Plot an individual community with the highesr clustering coefficient

#PLOTTING THE STRINGST COMMUNITY
graph12 <- induced.subgraph(giant_comp,louvain_giantg2$membership == 63)
plot.igraph(graph12,vertex.size=5, edge.arrow.size = 0.2,edge.width = 1.5,edge.arrow.color = "black",
            edge.arrow.width = 1.7, vertex.label.cex = 0.7, edge.color = "light blue"
            , vertex.color = "dark blue",vertex.label=V(graph12)$title,layout=layout.kamada.kawai )


#PLOTTING THE SECONG STRONGEST COMMUNITY
graph22 <- induced.subgraph(giant_comp,louvain_giantg2$membership == 27)
plot.igraph(graph2,vertex.size=2.5, edge.arrow.size = 0.4,asp=0.7,
            edge.arrow.width = 0.3, vertex.label.cex = 0.7, edge.color = "light blue"
            , vertex.color = "dark blue", vertex.label=V(graph22)$title,layout=layout.kamada.kawai)

plot.igraph(graph2,vertex.size=3, edge.arrow.size = 0.4,
            edge.arrow.width = 0.1, vertex.label.cex = 0.8, edge.color = "dark blue"
            , vertex.color = "light blue", vertex.label=V(graph1)$group, layout=layout.kamada.kawai,
            vertex.label.color="purple", vertex.label.degree=0)

fast_table.g[117,]
fast_table.g[126,]


graph12 <- induced.subgraph(giant_comp.g2,louvain_giantg2$membership == 61)


#----------------------------------------------------------
#Components & Community Detection: GRAPH G3- TIME STAMP 3
#----------------------------------------------------------
get.vertex.attribute(g3)
tempg3 <- g3

#Finding the components of the graph
#NOTE: Considering mode as both weak and strong, gives one big giant component.
#So we consider only strongly connected components
count_components(tempg3, mode = c("strong"))
components <- components(tempg3,mode = c("strong"))
components$csize[order(-components$csize)]

#giant component : maximum nodes; 
giant_comp.g3 <- induced.subgraph(tempg3,
                                  which(components$membership == which(components$csize == components$csize[which.max(components$csize)])))


#Community detection within the component for LARGEST component
group.giantg3 <- get.vertex.attribute(giant_comp.g3,"group")
title.giantg3 <- get.vertex.attribute(giant_comp.g3,"title")


giant_comp_und.g3 <- as.undirected(giant_comp.g3, mode = "collapse")
louvain_giantg3 <- cluster_louvain(giant_comp_und.g3, weights = NULL)
c.m.l.g3 <- membership(louvain_giantg3)  
fast_table.g <- table(c.m.l.g3, group.giantg3, useNA = c("no"))
# fast_table.gt <- table(c.m.l, title.giantg2, useNA = c("no"))
max(c.m.l.g3) #170 communities


#----------------------------------------------------------
#Components & Community Detection: GRAPH G4- TIME STAMP 4
#----------------------------------------------------------
get.vertex.attribute(g4)
tempg4 <- g4

#Finding the components of the graph
#NOTE: Considering mode as both weak and strong, gives one big giant component.
#So we consider only strongly connected components
count_components(tempg4, mode = c("strong"))
components <- components(tempg4,mode = c("strong"))
components$csize[order(-components$csize)]

#giant component : maximum nodes; 
giant_comp.g4 <- induced.subgraph(tempg4,
                                  which(components$membership == which(components$csize == components$csize[which.max(components$csize)])))


#Community detection within the component for LARGEST component
group.giantg4 <- get.vertex.attribute(giant_comp.g4,"group")
title.giantg4 <- get.vertex.attribute(giant_comp.g4,"title")


giant_comp_und.g4 <- as.undirected(giant_comp.g4, mode = "collapse")
louvain_giantg4 <- cluster_louvain(giant_comp_und.g4, weights = NULL)
c.m.l.g4 <- membership(louvain_giantg4)  
fast_table.g <- table(c.m.l.g4, group.giantg4, useNA = c("no"))
# fast_table.gt <- table(c.m.l, title.giantg2, useNA = c("no"))
max(c.m.l.g4) #182 communities

#**********SUMMARY************************************************************
#giant_comp_und1 = Undirected graph1
#giant_comp.g1 = Giant Component of g1
#c.m.l.g1 = membership g1
#max(c.m.l.g1) #126 communities

#giant_comp_und2 = Undirected graph2
#giant_comp.g2 = Giant Component of g2
#c.m.l.g2 = membership g2
#max(c.m.l.g2) #120 communities

#giant_comp_und3 = Undirected graph3
#giant_comp.g3 = Giant Component of g3
#c.m.l.g3 = membership g3
#max(c.m.l.g3) #170 communities

#giant_comp_und4 = Undirected graph4
#giant_comp.g4 = Giant Component of g4
#c.m.l.g4 = membership g4
#max(c.m.l.g4) #182 communities
#****************SUMMARY OVER************************************************************


#------------GRAPH G1-TIME STAMP1-Communities---------------------------------------
community <- rep(NA,max(c.m.l.g1))
table.g1 <- as.data.frame(community)
table.g1["VCount"] <- NA
table.g1["CommunitiesFormed"] <- NA
membership.table.g1 <- as.data.frame( list(community=NA,Vertex=NA, SubCommunity=NA))
#degree(giant_comp.g1)

for (i in seq(1,max(c.m.l.g1),1))
{
  
  gr<-induced.subgraph(giant_comp.g1,louvain_giantg1$membership == i)
  
  gr.und <- as.undirected(gr, mode = "collapse")
  gr.louvain <- cluster_louvain(gr.und, weights = NULL)
  c.m.gr <- membership(gr.louvain)  
  memb <-array(gr.louvain$membership)

  memb.table <- as.data.frame( list(community=i,Vertex=V(gr)$name, SubCommunity=memb))
  membership.table.g1 <- rbind(membership.table.g1,memb.table)
  
  table.g1$community[i] <- i
  table.g1$VCount[i]<- vcount(gr)
  table.g1$CommunitiesFormed[i] <- max(c.m.gr)
  
}  
  
#------------------GRAPH G2-TIME STAMP2-Communities---------------------------------------
community <- rep(NA,max(c.m.l.g2))
table.g2 <- as.data.frame(community)
table.g2["VCount"] <- NA
table.g2["CommunitiesFormed"] <- NA
membership.table.g2 <- as.data.frame( list(community=NA,Vertex=NA, SubCommunity=NA))
#degree(giant_comp.g1)

for (i in seq(1,max(c.m.l.g2),1))
{
  
  gr<-induced.subgraph(giant_comp.g2,louvain_giantg2$membership == i)
  
  gr.und <- as.undirected(gr, mode = "collapse")
  gr.louvain <- cluster_louvain(gr.und, weights = NULL)
  c.m.gr <- membership(gr.louvain)  
  memb <-array(gr.louvain$membership)
  
  memb.table <- as.data.frame( list(community=i,Vertex=V(gr)$name, SubCommunity=memb))
  membership.table.g2 <- rbind(membership.table.g2,memb.table)
  
  table.g2$community[i] <- i
  table.g2$VCount[i]<- vcount(gr)
  table.g2$CommunitiesFormed[i] <- max(c.m.gr)
}  

#-------------------GRAPH G3-TIME STAMP3-Communities---------------------------------------
community <- rep(NA,max(c.m.l.g3))
table.g3 <- as.data.frame(community)
table.g3["VCount"] <- NA
table.g3["CommunitiesFormed"] <- NA
membership.table.g3 <- as.data.frame( list(community=NA,Vertex=NA, SubCommunity=NA))
#degree(giant_comp.g1)

for (i in seq(1,max(c.m.l.g3),1))
{
  
  gr<-induced.subgraph(giant_comp.g3,louvain_giantg3$membership == i)
  
  gr.und <- as.undirected(gr, mode = "collapse")
  gr.louvain <- cluster_louvain(gr.und, weights = NULL)
  c.m.gr <- membership(gr.louvain)  
  memb <-array(gr.louvain$membership)
  
  memb.table <- as.data.frame( list(community=i,Vertex=V(gr)$name, SubCommunity=memb))
  membership.table.g3 <- rbind(membership.table.g3,memb.table)
  
  table.g3$community[i] <- i
  table.g3$VCount[i]<- vcount(gr)
  table.g3$CommunitiesFormed[i] <- max(c.m.gr)
}  

#---------------------GRAPH G4-TIME STAMP4-Communities---------------------------------------
community <- rep(NA,max(c.m.l.g4))
table.g4 <- as.data.frame(community)
table.g4["VCount"] <- NA
table.g4["CommunitiesFormed"] <- NA
membership.table.g4 <- as.data.frame( list(community=NA,Vertex=NA, SubCommunity=NA))
#degree(giant_comp.g1)

for (i in seq(1,max(c.m.l.g4),1))
{
  
  gr<-induced.subgraph(giant_comp.g4,louvain_giantg4$membership == i)
  
  gr.und <- as.undirected(gr, mode = "collapse")
  gr.louvain <- cluster_louvain(gr.und, weights = NULL)
  c.m.gr <- membership(gr.louvain)  
  memb <-array(gr.louvain$membership)
  
  memb.table <- as.data.frame( list(community=i,Vertex=V(gr)$name, SubCommunity=memb))
  membership.table.g4 <- rbind(membership.table.g4,memb.table)
  
  table.g4$community[i] <- i
  table.g4$VCount[i]<- vcount(gr)
  table.g4$CommunitiesFormed[i] <- max(c.m.gr)
}  


#---------------------------------------------------------------------
#FINDING CONSISTENT PRODUCT PAIRS WITHIN SAME COMMUNITY (ACROSS TIME)
#---------------------------------------------------------------------
  
#---Data Creation for all years at vertex, community and subcommunity level
membership.table.g1 <- subset(membership.table.g1,!is.na(membership.table.g1$Vertex))
sum(is.na(membership.table.g1))
membership.table.g2 <- subset(membership.table.g2,!is.na(membership.table.g2$Vertex))
sum(is.na(membership.table.g2))
membership.table.g3 <- subset(membership.table.g3,!is.na(membership.table.g3$Vertex))
sum(is.na(membership.table.g3))
membership.table.g4 <- subset(membership.table.g4,!is.na(membership.table.g4$Vertex))
sum(is.na(membership.table.g4))


g1g2.comms<-full_join(data.table(membership.table.g1,key="Vertex"),
                   data.table(membership.table.g2,key="Vertex"),
                   by=c("Vertex"="Vertex"))
g1g2.comms[is.na(g1g2.comms)] <- 0 
  
g1g2g3.comms<-full_join(data.table(g1g2.comms,key="Vertex"),
                      data.table(membership.table.g3,key="Vertex"),
                      by=c("Vertex"="Vertex"))
  
g1g2g3.comms[is.na(g1g2g3.comms)] <- 0 

g1g2g3g4.comms<-full_join(data.table(g1g2g3.comms,key="Vertex"),
                        data.table(membership.table.g4,key="Vertex"),
                        by=c("Vertex"="Vertex"))

g1g2g3g4.comms[is.na(g1g2g3g4.comms)] <- 0 


colnames(g1g2g3g4.comms) <- c("g1Community", "Vertex", "g1.SubCommunity",
                              "g2Community","g2.SubCommunity" ,
                              "g3Community","g3.SubCommunity", 
                              "g4Community", "g4.SubCommunity")

AllComms <- g1g2g3g4.comms


#-----Picking out vertex(s) with high degrees, meaning the one that goes with a lot of other products (consistently over 4 timestamps)
#Ans: Vertex-6 pops up first (in top 50 filter)
#E.g.::NodeID-6 corresponds to "How the Other Half Lives Studies Among the Tenements of New York"
top_deg_V_g1<-as.data.frame(degree(g1))%>%
  mutate(.,NodeID=rownames(as.data.frame(degree(g1))))%>%
  arrange(desc(as.data.frame(degree(g1))[,1]))%>%
  head(100)

top_deg_V_g2<-as.data.frame(degree(g2))%>%
  mutate(.,NodeID=rownames(as.data.frame(degree(g2))))%>%
  arrange(desc(as.data.frame(degree(g2))[,1]))%>%
  head(100)

top_deg_V_g3<-as.data.frame(degree(g3))%>%
  mutate(.,NodeID=rownames(as.data.frame(degree(g3))))%>%
  arrange(desc(as.data.frame(degree(g3))[,1]))%>%
  head(100)

top_deg_V_g4<-as.data.frame(degree(g4))%>%
  mutate(.,NodeID=rownames(as.data.frame(degree(g4))))%>%
  arrange(desc(as.data.frame(degree(g4))[,1]))%>%
  head(100)

V_common<-inner_join(data.table(top_deg_V_g4,key="NodeID"),
                    data.table(top_deg_V_g3,key="NodeID"),
                    by=c("NodeID"="NodeID"))%>%
          inner_join(.,data.table(top_deg_V_g2,key="NodeID"),
                    by=c("NodeID"="NodeID"))%>%
          inner_join(.,data.table(top_deg_V_g1,key="NodeID"),
                    by=c("NodeID"="NodeID"))%>%
          as.data.frame()


#----Checking consistency of pairs for NodeID-6 across 4 timestamps
#---Pairs in diffrent time stamps
#Note: Here x corresponds to NodeID, cutoff/sum_out correponds to weight of #times pairs repeat occurrence (ranges from 1 to 10) 
#More cutoff corresponds to more confidence in consistent pair possibility
#Cutoff=1 means copurchase only in timestamp1, Cutoff=8 means occurrence in 1st, 3rd and 4th timestamp

find_reg_pairs_of_with_cutoff<-function(x,cutoff){
  #####pairs in g1
  g1_comm<-AllComms[AllComms$Vertex==x,'g1Community']
  g1_subcomm<-AllComms[AllComms$Vertex==x,'g1.SubCommunity']
  g1_pair<-AllComms%>%
    filter(g1Community==g1_comm & g1.SubCommunity==g1_subcomm)%>%
    mutate(TS="g1",Ref_vertex=x,Vertex,flag=1)%>%
    select(TS,Ref_vertex,Vertex,flag)
  
  #####pairs in g2
  g2_comm<-AllComms[AllComms$Vertex==x,'g2Community']
  g2_subcomm<-AllComms[AllComms$Vertex==x,'g2.SubCommunity']
  g2_pair<-AllComms%>%
    filter(g2Community==g2_comm & g2.SubCommunity==g2_subcomm)%>%
    mutate(TS="g2",Ref_vertex=x,Vertex,flag=2)%>%
    select(TS,Ref_vertex,Vertex,flag)
  
  #####pairs in g3
  g3_comm<-AllComms[AllComms$Vertex==x,'g3Community']
  g3_subcomm<-AllComms[AllComms$Vertex==x,'g3.SubCommunity']
  g3_pair<-AllComms%>%
    filter(g3Community==g3_comm & g3.SubCommunity==g3_subcomm)%>%
    mutate(TS="g3",Ref_vertex=x,Vertex,flag=3)%>%
    select(TS,Ref_vertex,Vertex,flag)
  
  #####pairs in g4
  g4_comm<-AllComms[AllComms$Vertex==x,'g4Community']
  g4_subcomm<-AllComms[AllComms$Vertex==x,'g4.SubCommunity']
  g4_pair<-AllComms%>%
    filter(g4Community==g4_comm & g4.SubCommunity==g4_subcomm)%>%
    mutate(TS="g4",Ref_vertex=x,Vertex,flag=4)%>%
    select(TS,Ref_vertex,Vertex,flag)
  
  ####Combning all
  all_pairs<-rbind(g1_pair,g2_pair,g3_pair,g4_pair)
          
  out<-all_pairs%>%
    group_by(Ref_vertex,Vertex)%>%
    summarise(wt_copurchase_cnt=sum(flag))%>%
    filter(wt_copurchase_cnt>=cutoff)%>%
    arrange(desc(wt_copurchase_cnt))%>%
    as.data.frame()
  
  return(out)
}

find_reg_pairs_of_with_cutoff(6,7)

#------Scaling up for all pairs or reference prodcts 
out<-list()
for(i in 1:nrow(AllComms)){
  out[[i]]<-find_reg_pairs_of_with_cutoff(AllComms[i,"Vertex"],7)
}

#-----Hub and Authority Score for  pairs
find_hub_auth<-function(x){
  
  hub_score_x<-hub_score(x, weights=NA)$vector
  return(hub_score_x)
  
  ath_score_x<-authority_score(x, weights=NA)$vector
  return(ath_score_x)
}

find_hub_auth(g1)
find_hub_auth(g2)
find_hub_auth(g3)
find_hub_auth(g4)



