library('bnlearn')
library('networkD3')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
source('error.bar.R')


shinyServer(function(input, output,session) {

  bn.hc.boot.fit = readRDS("CHILD&MATERNAL_HEALTH_RESULTS_bnfit.RData")
  DiscreteData = readRDS("CHILD&MATERNAL_HEALTH_RESULTS_data.RData")
  bn.hc.boot.average = readRDS("CHILD&MATERNAL_HEALTH_RESULTS_bnaverage.RData")
  NetworkGraph = read.csv('bootPrunedNetwork.txt')
  nodeNames = names(bn.hc.boot.average$nodes)
  moduleData = read.csv('modularity2.csv')
  updateSelectInput(session,'module',choices = c("graph",paste("Module",c(1:max(moduleData[,2])),sep=" ")))
  moduleData[,2] = paste("Module",moduleData[,2],sep=" ")
  inserted  = c()
  insertedV = c()
  EventNode = nodeNames[1]
  EvidenceNode = c()
  updateSelectInput(session,'event',choices = nodeNames)
  updateSelectInput(session,'variable',choices = c("Codebook","Documentation","Description"))
  output$pdf<- renderUI({tags$iframe(style="height:600px; width:100%", src=paste(input$variable,".pdf",sep = ""))})
  rvs = reactiveValues(evidence = list(),values = list(),evidenceObserve = list(),valueObserve = list())
  observeEvent(input$insertBtn, {
    if(input$module == "graph")
    {
      nodeNamesNM = names(bn.hc.boot.average$nodes)
    }
    else
    {
      selectedNodes = moduleData[which(moduleData[,2]== input$module),1]
      nodeNamesNM = selectedNodes
    }
    btn = input$insertBtn
    id <- paste0('Evidence', btn)
    idL <- paste("Evidence", btn)
    idV <- paste0('Value', btn)
    idVL <- paste("Value", btn)
    insertUI(selector = '#placeholder1',
             ui = tags$div(selectInput(id,'Evidence',nodeNamesNM),
                           id = id
             )
    )
    insertUI(selector = '#placeholder2',
             ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNamesNM[1]])),
                           id = idV
             )
    )
    inserted <<- c(id, inserted)
    insertedV <<- c(idV,insertedV)
    rvs$evidence = c(rvs$evidence,id)
    rvs$value = c(rvs$value,id)
    rvs$evidenceObserve = c(rvs$evidenceObserve,observeEvent(input[[id]],{
      valID = insertedV[which(inserted == id)]
      print(valID)
      updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
    }))
  })

  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', insertedV[length(insertedV)])
    )
    insertedV <<- insertedV[-length(insertedV)]
    rvs$evidence = rvs$evidence[-length(inserted)]
    rvs$value = rvs$value[-length(insertedV)]
    rvs$evidenceObserve = rvs$evidenceObserve[-length(inserted)]
    rvs$valueObserve = rvs$valueObserve[-length(insertedV)]
  })
  observeEvent(input$plotBtn,{
    tryCatch({
      str1 <<- ""
      count =1
      for(elem in inserted)
      {
        vid = insertedV[which(inserted == elem)]
        str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
        if(count!=length(inserted))
        {
          str1 <<- paste0(str1," & ")
        }
        count = count + 1
      }
      probs = sort(prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1))))),decreasing = T)[1:10]
      output$distPlot = renderPlot({par(mar=c(5,3,3,3))
        par(oma=c(5,3,3,3))
        barplot(probs,
                col = "lightblue",
                main = "Conditional Probabilities",
                border = NA,
                xlab = "",
                ylab = "Probabilities",
                #ylim = c(0,1),
                las=2)})

    },error = function(e){
      print("error 4")
      output$distPlot<- renderPlot({validate("Error: in processing your request of inference.Please set an evidence node first")})
    })

  })
  observeEvent(input$plotStrengthBtn,{
    tryCatch({
      probT = c()
      for(i in 1:input$numInterval)
      {
        str1 <<- ""
        count =1
        for(elem in inserted)
        {
          vid = insertedV[which(inserted == elem)]
          str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
          if(count!=length(inserted))
          {
            str1 <<- paste0(str1," & ")
          }
          count = count + 1
        }
        probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))
        probT = rbind(probT,probs)
      }
      ee = 1
      ee$mean = colMeans(probT)
      ee$sd = apply(probT, 2, sd)
      nm = names(sort(ee$mean,decreasing = T))[1:10]
      output$distPlot = renderPlot({par(mar=c(5,3,3,3))
        par(oma=c(5,3,3,3))
        barx <-barplot(ee$mean[nm],
                       col = "lightblue",
                       main = "Conditional Probabilities",
                       border = NA,
                       xlab = "",
                       ylab = "Probabilities",
                       #ylim = c(0,1),
                       las=2)
        error.bar(barx,ee$mean[nm], 1.96*ee$sd[nm]/sqrt(input$numInterval))})

    },error = function(e){
      print("error 4")
      output$distPlot<- renderPlot({validate("Error: in processing your request of inference.Please set an evidence node first")})
    })

  })
  observeEvent(input$degree,{
    for(elem in inserted)
    {
      EvidenceNode = c(EvidenceNode,input[[elem]])
    }
    EventNode = input$event
    if(input$module == "graph")
    {
      subgraphNodes = 1:nrow(NetworkGraph)
      selectedNodes = names(bn.hc.boot.average$nodes)
    }
    else
    {
      selectedNodes = moduleData[which(moduleData[,2]== input$module),1]
      subgraphNodes = intersect(which(NetworkGraph$from %in% selectedNodes),which(NetworkGraph$to %in% selectedNodes))
    }

    networkData = NetworkGraph[subgraphNodes,1:2]
    src <- NetworkGraph$from[subgraphNodes]
    target <- NetworkGraph$to[subgraphNodes]
    nodes <- data.frame(name = selectedNodes)
    nodes$id <- 0:(nrow(nodes) - 1)
    colnames(networkData) = c("src","target")
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)

    edges$width <- 1

    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    ColourScale <- 'd3.scaleOrdinal().domain(["not in use","Event","Evidence"]).range(["#0E5AE8", "#50E80E","#FF0000"]);'
    #output$netPlot <- networkD3::renderSimpleNetwork({
    #  networkD3::forceNetwork(Links = edges, Nodes = nodes,
    #                          Source = "source",
    #                          Target = "target",
    #                          NodeID ="name",
    #                          Group = "group",
    #                          Value = "width",
    #                          opacity = 0.9,
    #                          arrows = TRUE,
    #                          opacityNoHover = 0.9,
    #                          zoom = TRUE,
    #                          legend = T,
    #                          colourScale = JS(ColourScale))
    #})
    visNodes<- data.frame(id = selectedNodes,
                          label = selectedNodes,
                          group = nodes$group)
    visEdges<- data.frame(from = NetworkGraph$from[subgraphNodes],
                          to = NetworkGraph$to[subgraphNodes])
    output$netPlot<-renderVisNetwork({
      visNetwork(visNodes, visEdges, width = "200%") %>%
        visEdges(arrows ="to",smooth = T,color = list(color = "black",highlight = "yellow",hover = "yellow"))%>%
        visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'yellow', hover = "yellow")) %>%
        visGroups(groupname = "Event", color = list(background = "green",highlight = "yellow", hover = "yellow"))%>%
        visGroups(groupname = "Evidence", color = list(background = "red",highlight = "yellow", hover = "yellow")) %>%
        visLegend(width = 0.1, position = "left")%>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
        #visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout()
    })

  })
  observeEvent(input$module,{
    if(input$module == "graph")
    {
      subgraphNodes = 1:nrow(NetworkGraph)
      selectedNodes = names(bn.hc.boot.average$nodes)
    }
    else
    {
      selectedNodes = moduleData[which(moduleData[,2]== input$module),1]
      subgraphNodes = intersect(which(NetworkGraph$from %in% selectedNodes),which(NetworkGraph$to %in% selectedNodes))
    }
    networkData = NetworkGraph[subgraphNodes,1:2]
    #print(networkData)
    src <- NetworkGraph$from[subgraphNodes]
    target <- NetworkGraph$to[subgraphNodes]
    nodes <- data.frame(name = selectedNodes)
    nodes$id <- 0:(nrow(nodes) - 1)
    colnames(networkData) = c("src","target")
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)

    edges$width <- 1
    EventNode = selectedNodes[1]
    EvidenceNode = c()
    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    nd = nodes
    nd$color<- 'blue'
    nd[which(nd$name %in% EvidenceNode),4] = "red"
    nd[which(nd$name == EventNode),4] = "green"
    ColourScale <- 'd3.scaleOrdinal().domain(["not in use","Event","Evidence"]).range(["#0E5AE8", "#50E80E","#FF0000"]);'
    #output$netPlot <- networkD3::renderSimpleNetwork({
    #  networkD3::forceNetwork(Links = edges, Nodes = nodes,
    #                          Source = "source",
    #                          Target = "target",
    #                          NodeID ="name",
    #                          Group = "group",
    #                          Value = "width",
    #                          opacity = 0.9,
    #                          arrows = TRUE,
    #                          opacityNoHover = 0.9,
    #                          legend = T,
    #                          colourScale = JS(ColourScale),
    #                          zoom = TRUE)
    #})
    visNodes<- data.frame(id = selectedNodes,
                          label = selectedNodes,
                          group = nodes$group)
    visEdges<- data.frame(from = NetworkGraph$from[subgraphNodes],
                          to = NetworkGraph$to[subgraphNodes])
    output$netPlot<-renderVisNetwork({
      visNetwork(visNodes, visEdges, width = "200%") %>%
        visEdges(arrows ="to",smooth = T,color = list(color = "black",highlight = "yellow",hover = "yellow"))%>%
        visGroups(groupname = "not in use", color = list(background ="lightblue",highlight = 'yellow', hover = "yellow")) %>%
        visGroups(groupname = "Event", color = list(background = "green",highlight = "yellow", hover = "yellow"))%>%
        visGroups(groupname = "Evidence", color = list(background = "red",highlight = "yellow", hover = "yellow")) %>%
        visLegend(width = 0.1, position = "left")%>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = 2,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
        #visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout()
    })
    for(elem in 1:length(inserted))
    {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[elem])
      )

    }
    inserted <<- c()
    for(elem2 in 1:length(insertedV))
    {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedV[elem2])
      )

    }
    insertedV <<- c()
    rvs$evidence <<- c()
    rvs$value <<- c()
    rvs$evidenceObserve <<- c()
    rvs$valueObserve <<- c()
    output$distPlot <<- renderPlot(NULL)
    updateSelectInput(session,'event',choices = selectedNodes)
    EventNode = selectedNodes[1]
    EvidenceNode = c()

  })

  observeEvent(input$variable,{
    output$pdf<- renderUI({tags$iframe(style="height:600px; width:100%", src=paste(input$variable,".pdf",sep = ""))})
  })

  observeEvent(input$graphBtn,{
    for(elem in inserted)
    {
      EvidenceNode = c(EvidenceNode,input[[elem]])
    }
    EventNode = input$event
    if(input$module == "graph")
    {
      subgraphNodes = 1:nrow(NetworkGraph)
      selectedNodes = names(bn.hc.boot.average$nodes)
    }
    else
    {
      selectedNodes = moduleData[which(moduleData[,2]== input$module),1]
      subgraphNodes = intersect(which(NetworkGraph$from %in% selectedNodes),which(NetworkGraph$to %in% selectedNodes))
    }

    networkData = NetworkGraph[subgraphNodes,1:2]
    src <- NetworkGraph$from[subgraphNodes]
    target <- NetworkGraph$to[subgraphNodes]
    nodes <- data.frame(name = selectedNodes)
    nodes$id <- 0:(nrow(nodes) - 1)
    colnames(networkData) = c("src","target")
    edges <- networkData %>%
      left_join(nodes, by = c("src" = "name")) %>%
      select(-src) %>%
      rename(source = id) %>%
      left_join(nodes, by = c("target" = "name")) %>%
      select(-target) %>%
      rename(target = id)

    edges$width <- 1

    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    ColourScale <- 'd3.scaleOrdinal().domain(["not in use","Event","Evidence"]).range(["#0E5AE8", "#50E80E","#FF0000"]);'
    #output$netPlot <- networkD3::renderSimpleNetwork({
    #  networkD3::forceNetwork(Links = edges, Nodes = nodes,
    #                          Source = "source",
    #                          Target = "target",
    #                          NodeID ="name",
    #                          Group = "group",
    #                          Value = "width",
    #                          opacity = 0.9,
    #                          arrows = TRUE,
    #                          opacityNoHover = 0.9,
    #                          zoom = TRUE,
    #                          legend = T,
    #                          colourScale = JS(ColourScale))
    #})
    visNodes<- data.frame(id = selectedNodes,
                          label = selectedNodes,
                          group = nodes$group)
    visEdges<- data.frame(from = NetworkGraph$from[subgraphNodes],
                          to = NetworkGraph$to[subgraphNodes])
    output$netPlot<-renderVisNetwork({
      visNetwork(visNodes, visEdges, width = "200%") %>%
        visEdges(arrows ="to",smooth = T,color = list(color = "black",highlight = "yellow",hover = "yellow"))%>%
        visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'yellow', hover = "yellow")) %>%
        visGroups(groupname = "Event", color = list(background = "green",highlight = "yellow", hover = "yellow"))%>%
        visGroups(groupname = "Evidence", color = list(background = "red",highlight = "yellow", hover = "yellow")) %>%
        visLegend(width = 0.1, position = "left")%>%
        visNodes(shape = "dot") %>%
        visOptions(highlightNearest = list(enabled =TRUE, degree = input$degree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection = TRUE)%>%
        #visInteraction(navigationButtons = TRUE)%>%
        visIgraphLayout()
    })

  })



})
