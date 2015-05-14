edges = read.csv('../data/edges.csv')
users = read.csv('../data/users.csv')

install.packages("igraph")
library(igraph)

g = graph.data.frame(edges, FALSE, users) 

plot(g, vertex.size=5, vertex.label=NA)

degree(g)

table(degree(g))
table(degree(g) >= 10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "blue"
V(g)$color[V(g)$school == "B"] = "green"
plot(g, vertex.label=NA)

