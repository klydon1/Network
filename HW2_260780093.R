# Assignment 2

edges = read.csv("~/Documents/4th Year/2nd Semester/MGSC 434/Homework/HW2/edges2.csv")
nodes = read.csv("~/Documents/4th Year/2nd Semester/MGSC 434/Homework/HW2/nodes2.csv")

library(igraph)
library(visNetwork)

### STEP 1

#initialize network
net = graph_from_data_frame(d=edges, vertices=nodes, directed=T)
net
?graph_from_data_frame
#visualize
plot(net, edge.arrow.size=0.1, vertex.label=NA)

#better visual
plot(net,
     vertex.size = 10, #choose the node size
     vertex.label.cex= 0.5, #choose the label size
     vertex.label.dist=2,
     edge.arrow.size=0.1, 
     vertex.label=NA)

# create dataframe

nodes_data = data.frame("name"=nodes$email, 
                         "degree" = degree(net),
                         "in_degree"= strength(net, mode = c('in')),
                         "out_degree"= strength(net, mode = c('out')),
                         "weighted_degree"= strength(net, mode = c('all')))
# nodes_data = nodes_data[order(-nodes_data$in_degree),] does this mess up our directed network? left out but interesting case
x=nodes_data[order(nodes_data$in_degree,decreasing=T),]
x$name[1:10]


#ratio of emails
nodes_data$in_plus_out = nodes_data$in_degree +  nodes_data$out_degree

nodes_data$email_ratio = (nodes_data$in_degree)/(nodes_data$in_plus_out)


library(ggplot2)
plot_ratio = ggplot(nodes_data,
                    aes(x=in_degree,y=email_ratio, size= out_degree)) +
  geom_point()+
  guides(size = guide_legend("Size (Emails sent)"))+
  labs(title="Emails Received vs Email Ratio", y="Email Ratio", x="Emails Received")
plot_ratio


# suspects
at_least_20 = nodes_data[nodes_data$in_degree >= 20,]
at_least_09 = at_least_20[at_least_20$email_ratio >= 0.9,]
at_least_09
at_least_09$name


#change color and size accordingly
net_suspects = graph_from_data_frame(d=edges, vertices=nodes, directed=T)

V(net_suspects)$color = 'black'
V(net_suspects)[at_least_09$name]$color = 'red'
V(net_suspects)$size = 2
V(net_suspects)[at_least_09$name]$size = 5


plot(net_suspects, edge.arrow.size=0.1, vertex.label=NA)


#visgraph = toVisNetworkData(net_suspects) 
#visNetwork(nodes = visgraph$nodes, edges = visgraph$edges) 


### Step 2
#non-suspects
net_non_suspects = graph_from_data_frame(d=edges, vertices=nodes, directed=T)
#want all the nonsuspects so we can highlight them. invert=T selects non Enron emails
non_sus = nodes_data[grep('enron.com',nodes_data$name,invert=T),]
?V()
V(net_non_suspects)$color = 'black' #make them all black first
V(net_non_suspects)[non_sus$name]$color = 'red' #non enron are red now
V(net_non_suspects)$size = 2 #all size 2 first
V(net_non_suspects)[non_sus$name]$size = 5 #non enron are size 5

plot(net_non_suspects, edge.arrow.size=0.1, vertex.label=NA) #plot the network


net_del = delete_vertices(net_non_suspects,non_sus$name) #delete non enron

plot(net_del, edge.arrow.size=0.1, vertex.label=NA)

#visgraph1 = toVisNetworkData(net_del) 
#visNetwork(nodes = visgraph1$nodes, edges = visgraph1$edges, vertex.label=NA) 


### Alternative Method - we discussed in a zoom meeting this other way that also works
data_test = nodes_data
data_test$in_enron = grepl("enron.com", data_test$name)
table(data_test$in_enron)

#start all the same
data_test$color = 'black'
data_test$size = 2

data_test[data_test$in_enron=="FALSE",]$color = "red"
data_test[data_test$in_enron=="FALSE",]$size = 5 

net_test = graph_from_data_frame(d=edges, vertices=nodes, directed=T)


V(net_test)$size = data_test$size
V(net_test)$color = data_test$color


#par(mfrow = c(1, 2))
plot(net_test, edge.arrow.size = 0.1,vertex.label = NA)
plot(net_non_suspects, edge.arrow.size=0.1, vertex.label=NA)


#identical_graphs(net_test,net_non_suspects)

#2-2
net2_test = delete_vertices(net_test, data_test$in_enron=="FALSE")
plot(net2_test, edge.arrow.size = 0.1,vertex.label = NA)
plot(net_del, edge.arrow.size = 0.1,vertex.label = NA)




### Step 3 - need to just enron emails AND only those in first component
filtered = nodes_data[grep('enron.com',nodes_data$name),] #only enron emails


#just in first component
comp = components(net_del) #inspect membership
in.max.comp = (comp$membership == 1) #employees in largest connected component
sg = induced_subgraph(net_del,in.max.comp) #take the subgraph
?as_data_frame
table(in.max.comp)

first_comp = as_data_frame(sg,what='vertices')
first_comp#want names just in first component
str(first_comp$name) #their emails to find in only enron email dataframe

filtered_comp = filtered[filtered$name %in% first_comp$name,] #make dataframe with only first comp nodes

#closeness metric
sg_cl = closeness(sg)
summary(sg_cl)
cl_quantile = quantile(sg_cl,0.95) #95% percentile value for closeness

filtered_comp$net_cl = sg_cl

filtered_close = filtered_comp[filtered_comp$net_cl > cl_quantile,]
filtered_close$name #who's above 95% percentile closeness?

#betweenness metric
sg_bn = betweenness(sg)
summary(sg_bn)
sg_bn
bn_quantile = quantile(sg_bn,0.95) #95% percentile value for betweenness

filtered_comp$net_bn = sg_bn

filtered_bn = filtered_comp[filtered_comp$net_bn > bn_quantile,]

filtered_bn$name #who's above 95% quantile betweenness?


### Alt Method
# Take a subset of data where in_enron == TRUE
data_testdf = data_test[data_test$in_enron=="TRUE",]

# Compute the people that are part of the large network
comp_test = components(net2_test)
in.max.comp_test = comp_test$membership == 1
sg_test = induced_subgraph(net2_test, in.max.comp_test)

# Create bew column T/F if part of large network, and keep only "TRUE"
data_testdf$sub = in.max.comp_test
data_testdf = data_testdf[data_testdf$sub=="TRUE",]


close = closeness(sg_test)
summary(close)

# Find the 95th percentile of all values of closeness
a = quantile(close, 0.95)

# Apply the value to the df and keep only thos above the 95th percentile
data_testdf$closeness = close
data_testdf_cl = data_testdf[(data_testdf$closeness > a),]

data_testdf_cl$name














#final network
final_net = sg
plot(final_net,edge.arrow.size=0.1, vertex.label=NA)

#find who is in both top quantiles (note equal size sets)
in_both = filtered_close[filtered_close$name %in% filtered_bn$name,]
in_both$name

just_cl = filtered_close[!(filtered_close$name %in% filtered_bn$name),] #strictly in top closeness
just_bn = filtered_bn[!(filtered_bn$name %in% filtered_close$name),] #strictly in top betweenness


V(final_net)$color = 'black' #all black to start
V(final_net)$size = 2 #all size 2 to start
V(final_net)[in_both$name]$color = 'green' #if in both then green
V(final_net)[just_cl$name]$color = 'red' #if just in close -> red
V(final_net)[just_bn$name]$color = 'blue'  #if just in bn -> blue

#non black are size 5
V(final_net)[in_both$name]$size = 5
V(final_net)[just_cl$name]$size = 5
V(final_net)[just_bn$name]$size = 5

plot(final_net, edge.arrow.size=0.1, vertex.label=NA)

visgraph = toVisNetworkData(final_net) 
visNetwork(nodes = visgraph$nodes, edges = visgraph$edges) 


