#######################################################################################################################################
###############################################     ANÁLISIS DE REDES SOCIALES EN R     ###############################################
###############################################                                         ###############################################   
###############################################             BENJAMÍN  MUÑOZ             ###############################################
###############################################               09/01/2018                ###############################################
###############################################   ESCUELA DE VERANO DE MÉTODOS MIXTOS   ###############################################
#######################################################################################################################################

#1) PRELIMINARES: EL PAQUETE IGRAPH.

#El paquete fundamental que utilizaremos en este taller es igraph. Contiene múltiples funciones para la creación, manipulación,
#visualización y análisis de redes. Está escrito en C++ y opera en Python y R. Fue elaborado por Gábor Csárdi y Tamás Nepusz. 
#Producto de su diseño es eficiente en el manejo de redes (incluso las de gran tamaño) y permite distintos tipos de análisis.

library(igraph) #Sí no lo han instalado, deben utilizar install.packages("igraph")

load("Terrorist.rda")


plot(terrorist)
legend(x = -1.8, y = 1.5 , c("AA-11 (WTC North)","AA-77 (Pentagon)","UA-175 (WTC South)","UA-93 (Pennsylvania"),
       pch = 21 , col ="#777777" , pt.bg = c("orange","red","blue","green"), pt.cex = 2, cex = 0.8 , 
       bty = "n", ncol = 1)

#Red de 19 atacantes en el 11/9 (Valdis Krebs, 2002)
#Los círculos representan a los objetos (vertex/nodes; vértices/nodos) y las líneas representan relaciones
#(ties/edges).

summary(terrorist)

#El output describe suscintamente, pero de un modo un tanto críptico, la estructura general del objeto igraph.
#La primera línea describe la estrucutra global de la red.
#La segunda línea los atributos de la red (sean de vértices o bordes).
#Por último se presentan los vínculos entre nodos.


######################################################################################################################################
##############################################     CONSTRUCCIÓN DE OBJETOS DE REDES     ##############################################
######################################################################################################################################

#1) USO DE GRAPH-FORMULA

grafo_01 <-  graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)

#Pequeño análisis descriptivo
summary(grafo_01)
plot(grafo_01)
V(grafo_01)
E(grafo_01)


grafo_02 <- graph.formula(1-+2, 1-+3, 2++3)

#Pequeño análisis descriptivo
summary(grafo_02)
plot(grafo_02)
V(grafo_02)
E(grafo_02)

#Este grafo es direccionado (digrafo). Los bordes están direccionados (se les conoce también como arcos) y 
#tienen cabeza y cola. Si dos arcos entre un par de vértices tienen roles opuestos de cabeza y cola se dice
#que son mutuos. 

#2) USO DE FUNCIONES DE MANIPULACIÓN DE DATOS

#Sociomatriz
net_01 <- rbind(c(0,1,1,0,0),
                c(0,0,1,1,0),
                c(0,1,0,0,0),
                c(0,0,0,0,0),
                c(0,0,1,0,0))
net_01  #También se podría usar comando matrix.                       

rownames(net_01) <- c("A","B","C","D","E")
colnames(net_01) <- c("A","B","C","D","E")

net_01_igraph <- igraph::graph_from_adjacency_matrix(net_01)  #Es lo mismo graph.adjacency

summary(net_01_igraph)
V(net_01_igraph)
E(net_01_igraph)
plot(net_01_igraph)

################################################################################
#                                                                              #
# graph_from_adjacency_matrix(adjmatrix, mode = c("directed", "undirected",    #
#    "max", "min", "upper", "lower", "plus"), weighted = NULL, diag = TRUE,    #
#    add.colnames = NULL, add.rownames = NA)                                   #
################################################################################

#¿Buscabamos una red direccionada? (Hint: simetría)

#Listado de bordes
net_02 <- rbind(c(1,2),
                c(1,3),
                c(2,3),
                c(2,4),
                c(3,2),
                c(5,3))

net_02_igraph <- igraph::graph_from_edgelist(net_02)  #Es lo mismo graph.edgelist

#############################################       
#                                           #
# graph_from_edgelist(el, directed = TRUE)  #
#                                           #  
#############################################

summary(net_02_igraph)
V(net_02_igraph)
E(net_02_igraph)
plot(net_02_igraph)

#Los atributos deben ingresarse posteriormente
#¿Por qué puede ser recomendable usar edgelists?

#Atributos. Se puede ingresar todo tipo de información, para vértices, bordes e incluso el grafo. Sin embargo,
#hay algunos especiales (name, color, etc.) que plot.igraph los usará por defecto:

#Del grafo
grafo_01$name <- "Mi primer grafo"
summary(grafo_01)

#De los vértices
V(grafo_01)

V(grafo_01)$name <- c("Benjamín","Francisco","Felipe","Helena","Loreto","Ignacio","Valentina")
V(grafo_01)$gender <- c("M","M","M","F","F","M","F")

plot(grafo_01)

#De los bordes
E(grafo_01)
E(grafo_01)$type <- c("Amistad","Amistad","Trabajo","Pareja","Pareja","Amistad","Trabajo","Amistad","Amistad","Trabajo")
E(grafo_01)$type


#3) DATOS PROVENIENTES DE OTRAS FUENTES (Ejemplo de Kosuke Imai)

florence <- read.csv(file = "florentine.csv", row.names = "FAMILY")
View(florence)
class(florence)  #Es una base de datos (data.frame)

#Ahora la convertire en una matriz (coerción) para usarlo como una matriz de adyacencia.
florence <- as.matrix(florence) 
View(florence)
class(florence)
florence[1:5, 1:5]

#¿Cuántos matrimonios?
rowSums(florence)

#Construir una red desde una sociomatriz
florence_igraph <- igraph::graph.adjacency(adjmatrix = florence, mode = "undirected", 
                                           weighted = NULL, diag = FALSE, 
                                           add.colnames = NULL , add.rownames = NA)

florence_igraph
florence_igraph[]
plot(florence_igraph)

# ANÁLISIS DESDE UNA LISTA DE BORDES

load("Sponsorship_Example.rda")
View(sponsorship_chile_07)

#Para manipular este objeto hay opciones más sencillas. Sin embargo, lo haremos del modo más extenso para entender todos los 
#paso asociados.

#Genero primero elementos temporales que contienen todos los pares de vértices
temporal01 <-dplyr::select(sponsorship_chile_07,-c(sponsor03,sponsor04,sponsor05))    #1-2
temporal02 <-dplyr::select(sponsorship_chile_07,-c(sponsor02,sponsor04,sponsor05))    #1-3
temporal03 <-dplyr::select(sponsorship_chile_07,-c(sponsor02,sponsor03,sponsor05))    #1-4
temporal04 <-dplyr::select(sponsorship_chile_07,-c(sponsor02,sponsor03,sponsor04))    #1-5

temporal05 <-dplyr::select(sponsorship_chile_07,-c(sponsor01,sponsor04,sponsor05))    #2-3
temporal06 <-dplyr::select(sponsorship_chile_07,-c(sponsor01,sponsor03,sponsor05))    #2-4
temporal07 <-dplyr::select(sponsorship_chile_07,-c(sponsor01,sponsor03,sponsor04))    #2-5

temporal08 <-dplyr::select(sponsorship_chile_07,-c(sponsor01,sponsor02,sponsor05))    #3-4
temporal09 <-dplyr::select(sponsorship_chile_07,-c(sponsor01,sponsor02,sponsor04))    #3-5

temporal10 <-dplyr::select(sponsorship_chile_07,-c(sponsor01,sponsor02,sponsor03))    #4-5

colnames(temporal01) <- colnames(temporal02) <- colnames(temporal03) <- colnames(temporal04) <-
  colnames(temporal05) <- colnames(temporal06) <- colnames(temporal07) <- colnames(temporal08) <- 
  colnames(temporal08) <- colnames(temporal09) <- colnames(temporal10)  <-
  c("period_n","period_c","day","month","year","date","n_bill1","n_bill2","bill_comp","title","type",
    "status","sponsortype","sponsor01","sponsor02")

#Combino todos los objetos temporales
temporal_net_01  <- dplyr::bind_rows(temporal01,temporal02,temporal03,temporal04,temporal05,
                              temporal06,temporal07,temporal08,temporal09,temporal10)

rm(temporal01,temporal02,temporal03,temporal04,temporal05,temporal06,temporal07,temporal08,temporal09,temporal10)

#Lo que busco son vínculos. No me sirven proyectos con un único autor. De manera más general, no me
#sirven pares de vértices con un sólo vértice.
summary(is.na(temporal_net_01$sponsor01))
summary(is.na(temporal_net_01$sponsor02))

#Debo limpiar los datos
temporal_net_01 %>% dplyr::filter(is.na(sponsor01)==F) -> temporal_net_02

#Evidentemente hay redundancias en el listado
temporal_net_02$filter <- paste(temporal_net_02$sponsor01,"-",temporal_net_02$sponsor02,"-",temporal_net_02$bill_comp)

temporal_net_02 %>% dplyr::distinct(filter, .keep_all = T) -> temporal_net_03

temporal_net_03 %>% dplyr::filter(is.na(sponsor02)==F) -> temporal_net_04

dim(temporal_net_01)
dim(temporal_net_02) #463 filas eliminadas por sponsor01 vacío
dim(temporal_net_03) #409 filas eliminadas por redundancia
dim(temporal_net_04) #309 filas eliminadas por sponsor02 vacío

rm(temporal_net_01,temporal_net_02)

#Cargar atributos de vértices
load("Vertex_Sponsorship_Example.rda")

#Usaré los ids de los diputados
vertex_2007 %>% dplyr::select(full_name, id_nuevo) -> ids

#Combino los elementos de modo tal que agrego los IDs
temporal_net_04$full_name <- temporal_net_04$sponsor01
temporal_net_05 <- dplyr::left_join(temporal_net_04, ids , by ="full_name")
temporal_net_05$full_name <- temporal_net_05$sponsor02
temporal_net_06 <- dplyr::left_join(temporal_net_05, ids , by ="full_name")

temporal_net_06 %>% dplyr::filter(is.na(id_nuevo.y)==FALSE, is.na(id_nuevo.x)==FALSE) -> temporal_net_07

#Construyo la edgelist
temporal_net_07 %>% dplyr::select(id_sponsor_01 = id_nuevo.x, id_sponsor_02 = id_nuevo.y) -> edge_list_sponsor

#Genero el objeto igraph
igraph_2007 <- graph_from_data_frame(d = edge_list_sponsor, directed = FALSE, vertices = vertex_2007)
plot(igraph_2007) #¿Cuál es el problema?

vertex_2007$filtro <- car::recode(vertex_2007$id_nuevo,"c('036','035','008','002','034','014','006','070',
                                  '017','025','027','015','021','016','009','037','005','048','031','054',
                                  '004','010','081','003','047','001','033','020','057','023','011','039',
                                  '060','052','077','071','018','044','097','064','076','028','061','056',
                                  '082','086','045','043','092','068','091','089','078','026','090','087',
                                  '019','042','041','093','058','038','095','055','102','101','063','062',
                                  '079','069','099','111','110','106','084','113','032','067','116','107','072',
                                  '094','104','112','059','080','117','108')=1;else=0",as.factor.result=T)

vertex_2007 %>% dplyr::filter(filtro==1) -> vertex_2007_depurado

igraph_2007_final <- graph_from_data_frame(d = edge_list_sponsor, directed = FALSE, vertices = vertex_2007_depurado)

plot(igraph_2007_final)

#LECCIÓN: EL MANEJO DE DATOS ES FUNDAMENTAL.

#Aspectos complementarios

#Si una red tiene bordes que ambos finales conecten a un mismo vértice (bucles o loops) o tiene 
#pares de vértices con más de un borde entre ellos (multi-bordes) se tiene un MULTI-GRÁFICO.

#Este aspecto es importante, ya que muchos de los indicadores y análisis suponen redes SIMPLES (y bordes propios)
igraph::is.simple(igraph_2007_final) 

#¿Cuál es el origen de este problema?
#¿Cómo resolverlo? Aquí una solución sencilla generando una red ponderada

E(igraph_2007_final)$weight <- 1
igraph_2007_ponderada <- igraph::simplify(igraph_2007_final)
igraph::is.simple(igraph_2007_ponderada)
plot(igraph_2007_ponderada)

#Dos vértices son adyacentes si comparten un borde
igraph::neighbors(graph = igraph_2007_ponderada, v =1)
igraph::neighbors(graph = igraph_2007_ponderada, v =5)

#Dos bordes son adyacentes si comparten un vértice como punto de cierre (endpoint).
#Un vértice es incidente en un borde si es un punto de cierre de dicho borde.

igraph::ego_size(graph = igraph_2007_ponderada , order = 1 , nodes = V(igraph_2007_ponderada))


#MOVIMIENTOS dentro de un gráfico. Es una secuencia de pasos en que los puntos de término son v1 y v2.
#El número de pasos corresponde a su longitud. Los trails son los pasos sin repetir bordes y 
#los path son los trails en que no se repiten vértices. 

#Un vértice es alcanzable desde otro vértice si existe un camino que los conecte. 
#Un grafo está CONECTADO si cada vértice es alcanzable por los restantes.

igraph::is.connected(graph = grafo_01)
igraph::is.connected(graph = igraph_2007_ponderada)

#Con redes direccionadas hay dos versiones
igraph::is.connected(graph = grafo_02, mode = "weak")   #Ignorando las direcciones de arcos hay conexión.
igraph::is.connected(graph = grafo_02, mode = "strong") #Se consideran las direcciones para ser alcanzable.

#La DISTANCIA entre vértices es la longitud del camino más corto entre vértices (se conoce técnicamente
#como distancia geodésica).

igraph::diameter(graph = grafo_01)
igraph::diameter(graph = florence_igraph)
igraph::diameter(graph = igraph_2007_ponderada)


#############################################
#TIPOS DE REDES (NO NECESARIO, ES ÚTIL PARA ADENTRARSE EN LA LITERTURA).

#Usaremos funciones de igraph para generar redes ficticias con el objeto de describir algunos tipos clasicos
#Ejemplo proveniente de Csárdi

igraph_full <- graph.full(n = 7 , directed = FALSE, loops = FALSE)
igraph_ring <- graph.ring(n = 7 , directed = FALSE, mutual = FALSE, circular = TRUE)
igraph_tree <- graph.tree(n = 7 , children = 2, mode = "undirected")
igraph_star <- graph.star(n = 7, mode = "undirected", center = 1)
par(mfrow = c(2,2))

plot(igraph_full) #Grafo completo: todos los vértices se vinculan con todos los vértices.
plot(igraph_ring) #Grafo regular:  todos los vértices tienen el mismo grado (número de bordes).
plot(igraph_tree) #Grafo árbol:    NO tiene ciclos (camino de longitud 3 o más dónde el vértice de
                  #                inicio y término es el mismo).
plot(igraph_star) #Grafo estrella: tipo especial de árbol, dónde hay una raíz y k-ramas.

#Un tipo muy especial de grafos son los bipartitos

grafo_bipartito <- graph.formula(actor1:actor2:actor3, movie1:movie2, actor1:actor2 - movie1,
                                 actor2:actor3 - movie2)
V(grafo_bipartito)$type <- grepl("^movie", V(grafo_bipartito)$name)
grafo_bipartito

par(mfrow=c(1,1))
plot(grafo_bipartito)

proj <- bipartite_projection( grafo_bipartito)
proj

#Eliminar elementos sobrantes
rm(edge_list_sponsor, grafo_bipartito, ids, igraph_full, igraph_ring, igraph_star, igraph_tree, 
   net_01, net_02, proj, temporal_net_03, temporal_net_04, temporal_net_05, temporal_net_06, 
   temporal_net_07)

######################################################################################################################################
###################################################     VISUALIZACIÓN DE REDES     ###################################################
######################################################################################################################################


#Retomemos el ejemplo de matrimonios en la Florencia renacentista
plot(terrorist)
plot(florence_igraph) 
plot(igraph_2007_ponderada)

#Ahora, veamos la función plot. Se le pueden sumar argumentos generales (xlim, ylim, main, xlab, ylab)
#y otros específicos para el trabajo con redes.

#Saquemos las familias
plot(florence_igraph, vertex.label = NA)

#Manipulemos los vértices
plot(florence_igraph, vertex.label.color = "black" , vertex.label.dist = 1 ,
     vertex.size = 15 , vertex.label = V(florence_igraph)$name, vertex.label.cex=.7,
     vertex.color ="blue", vertex.frame.color ="yellow")

#vertex.label:      etiquetas de vértices (es algún atributo)
#vertex.label.color: color de las etiquetas
#vertex.label.dist: ampliar la distancia predeterminada entre etiquetas
#vertex.label.cex:  manipular el tamaño de las etiquetas (fuente)

#vertex.color : color de relleno de los círculos (vértices)
#vertex.size:   tamaño de los círculos (vértices)
#vertex.frame.color: color del borde de los círculos (vértices).

#Alteremos los bordes
plot(florence_igraph, vertex.label.color = "black" , vertex.label.dist = 1 ,
     vertex.size = 15 , vertex.label = V(florence_igraph)$name , edge.curved = .3)


#Los layouts o marcos son los modos/algorritmos en que se dibujan las redes (cómo se sitúan los vértices
#y bordes en el espacio).

plot(igraph_2007_ponderada)

#Un layout básico
plot(igraph_2007_ponderada, layout = layout_randomly)


#Ahora manipulemos el color como un atributo de los vértices
V(igraph_2007_ponderada)$color <- V(igraph_2007_ponderada)$pact
V(igraph_2007_ponderada)$color=gsub("Concertacion/NM","red",V(igraph_2007_ponderada)$color)
V(igraph_2007_ponderada)$color=gsub("Alianza","blue",V(igraph_2007_ponderada)$color)
V(igraph_2007_ponderada)$color=gsub("Otro","yellow",V(igraph_2007_ponderada)$color)

#Formato de círculo
plot(igraph_2007_ponderada, layout = layout_in_circle,
     vertex.shape = "none", vertex.label =V(igraph_2007_ponderada)$lastname1)

#Formato esférico, aproximadamente uniforme en base a IDs
plot(igraph_2007_ponderada, layout = layout_on_sphere, vertex.label = V(igraph_2007_ponderada)$lastname1,
     vertex.color = V(igraph_2007_ponderada)$color)

#Formato Fruchterman-Reingold
plot(igraph_2007_ponderada, layout_with_fr(igraph_2007_ponderada),
     vertex.label = V(igraph_2007_ponderada)$lastname1, vertex.color = V(igraph_2007_ponderada)$color)


######################################################################################################################################
###############################################     ANÁLISIS DESCRIPTIVO DE REDES     ################################################
######################################################################################################################################

#Describir la red de matrimonios con medidas de centralidad.
#CENTRALIDAD: grado en que cada nodo está conectado con otros nodos (por lo que se vuelve el centro del grafo).

#COMANDO DEGREE
igraph::degree(florence_igraph, mode ="all")     #Número de bordes según vértices adyacentes (con vínculos).
                                                 #Descripción más local que global sobre la red.

#########################################################################
#                                                                       #
# degree(graph, v = V(graph), mode = c("all", "out", "in", "total"),    #
#        loops = TRUE, normalized = FALSE)                              #  
#                                                                       #
#v permite individualizar los vértices para el cálculo.                 #
#mode es pertinente según el tipo de red (en redes direccionadas, 'out' #
# e 'in' son relevantes).                                               #
#loop alude a bucles de bordes en el conteo.                            #
#normalized es para transformar los resultados (dividr por n-1, siendo  # 
#n el número de vértices).                                              #
#########################################################################


#COMANDO CLOSENESS
igraph::closeness(florence_igraph)   #Es un indicador global de la estructura de la red. 
                                     #Es el inverso de farness, suma de bordes de un cierto nodo respecto
                                     #a otros nodos, incluyendo los no directamente conectados.

#Como indicador de centralidad, mide cuántos pasos se requieren para acceder a cada vértices desde un determinado
#vértice. 

#closeness(v) = 1 / farness(v)

#closeness(v) = 1 / Sumatoria distancia entre v1 y v2

############################################################################
#                                                                          #
# closeness(graph, vids = V(graph), mode = c("out", "in", "all", "total"), #
#          weights = NULL, normalized = FALSE)                             #  
#                                                                          #
############################################################################

#Se simplifica la interpretación al dividir por el número promedio de bordes de un nodo a otro
#Para esto, dividimos por el número de OTROS nodos en el grafo. Si hay 16 nodos, se divide por 15. 


1 / (igraph::closeness(florence_igraph) * 15)  

#Los 2.7 bordes entre la familia Médici y cualquier otra familia florentina refleja su centralidad
#en la red. 


#COMANDO BETWEENNESS

igraph::betweenness(florence_igraph)   #Otro indicador global de centralidad.

#En betweenness, un nodo es considerado central si permite la conexión entre otros nodos.
#Se utiliza como supuesto que la comunicación/conexión entre nodos se da entre el camino más
#corto entre ambos. 
#Se deine como el número de geodesias (caminos más cortos) a través de vértices (también existe una
#definición equivalente para edge betweenness, lo que se hace por medio de edge_betweenness).

######################################################################
#                                                                    #
# betweenness(graph, v = V(graph), directed = TRUE, weights = NULL,  #
#            nobigint = TRUE, normalized = FALSE)                    #
#                                                                    #
# ?edge_betweenness                                                  #
######################################################################

# 1) Calcular la proporción de caminos más cortos entre pares de nodos t y u que contienen v.
# 2) Calcular la proporción para cada par unico de nodos t y u en el grafo, excluyendo a v.
# 3) Sumar todas las proporciones.

# betweenness(v) = nª de caminos más cortos que contienen nodo v
#                   ----------------------------------------------
#                   nº de caminos más cortos entre nodos t y u


47.5/105*100

#Cada nodo (familia) puede ser pareado con otros 105 nodos. Familia Médici está en el camino
#más corto para vincular a más del 45% de los posibles pares de familias. 


#Visualización. Usaremos los indicadores de centralidad previamente descritos para visualizar.
#En específico, manipularemos el tamaño de los vértices.
plot.igraph(florence_igraph, vertex.size = igraph::closeness(florence_igraph) * 1000, 
     main = "Closeness Measure. Marriage Network in Renaissance Florence")

plot.igraph(florence_igraph, vertex.size = igraph::betweenness(florence_igraph), 
     main = "Betweenness Measure. Marriage Network in Renaissance Florence")


#También es posible construir indicadores globales para la red


#Ahora un breve ejemplo con redes direccionadas
load("senators_twitter.rda")

#Calcular degree in y out
senator$indegree <- degree(twitter.adj, mode = "in")
senator$outdegree <- degree(twitter.adj, mode = "out") 

#Ordenando los datos
in.order <- order(senator$indegree, decreasing = TRUE)
out.order <- order(senator$outdegree, decreasing = TRUE)

#Indegree no tiene porque coincidir con outdegree
senator[in.order[1:3], ]
senator[out.order[1:3], ]


#A modo de cierre

#DENSIDAD: proporción de bordes presentes de todos los posibles en la red

igraph::edge_density(florence_igraph, loops=F) #Red no dirigida
ecount(senators_us_twitter)/(vcount(senators_us_twitter)*(vcount(senators_us_twitter)-1)) #Red direccionadas

#RECIPROCIDAD: proporción de vínculos recíprocos en red direccionada

igraph::reciprocity(senators_us_twitter)
igraph::dyad_census(senators_us_twitter) # Mutual, asymmetric, and null pares de vértices

#TRANSITIVIDAD: global razón de triángulos a conexiones triples; local razón de triangulos conectados tripos por vértice

igraph::transitivity(florence_igraph, type="global")  
igraph::transitivity(florence_igraph, type="local")

igraph::triad_census(senators_us_twitter) 

