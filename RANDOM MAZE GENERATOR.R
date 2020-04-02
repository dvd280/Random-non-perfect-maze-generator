library(dplyr)
library(plotly)
library(orca)
MAZE<- -2 %>% matrix(50,50)
start <- c(1,1)
visited <- FALSE %>% matrix(50,50)
Stack <- data.frame()
pic_counter <- 1
Neighbors <- function(my_location){
  
  Neighbors <- list(c(my_location[1], my_location[2] - 1),
                    c(my_location[1] - 1, my_location[2]),
                    c(my_location[1] + 1, my_location[2]),
                    c(my_location[1],my_location[2] + 1))
  Neighbors <- lapply(Neighbors,function(x){
                if((x %>% between(1,nrow(MAZE)) %>% sum())== 2){
                  x
                }
               }) %>% do.call(what = "cbind")%>% as.matrix()
  Neighbors %>% return()
} 
check_visited <- function(N_MAT){
  N_MAT %>% apply(MARGIN = 2,function(x){
    visited[x[1],x[2]] == FALSE
  })%>% return()
}
Stacker <- function(df,element_to_add){
  rbind(df,element_to_add) %>% return()
}
UNStacker <- function(df){
  df[-nrow(df),] %>% return()
}

traverse_maze <- function(current){
  while(visited %>% sum() !=50*50){
    #plot_ly(z =~MAZE,type = "heatmap") %>% orca(file = as.character(pic_counter))
    #pic_counter <<- pic_counter + 1
    visited[current[1],current[2]] <<- TRUE
    MAZE[current[1],current[2]] <<- 0
    Neighbor_mat <- Neighbors(current) 
    possible <- Neighbor_mat %>% check_visited()
    filtered <- Neighbor_mat[,possible]
    if(possible %>% sum() == 0 ){
      current <- Stack[nrow(Stack),]%>% unlist()
      Stack<<- UNStacker(Stack)
      next
    }
    
    if(possible %>% sum() == 1 ){
      return(traverse_maze(filtered))
      
    }
    if(possible %>% sum() > 1 ){
      rand_chosen <- sample(1:(possible %>% sum()),2)
      #SET VARIABLES
      visited[filtered[1,rand_chosen[1]],filtered[2,rand_chosen[1]]] <<- TRUE 
      MAZE[filtered[1,rand_chosen[1]],filtered[2,rand_chosen[1]]] <<- -2
      if(possible %>% sum() >2){
      Stack <<- Stacker(Stack,current)
      }
      return( traverse_maze(filtered[,rand_chosen[2]]) )
    }
  }
}
traverse_maze(start)

