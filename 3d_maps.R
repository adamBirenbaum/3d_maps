library(rayshader)
library(dplyr)
library(ggplot2)
library(sp)
library(microbenchmark)


fill_region <- function(df){
  n <- 100
  new_df <- data.frame(long = rep(0,(nrow(df)-1)*n), lat = rep(0,(nrow(df)-1)*n))
  for (i in 1:(nrow(df) - 1)){
    start <- ((i - 1)*n + 1)
    new_df[start:(start + n-1),] <- data.frame(long = seq(from = df$long[i], to = df$long[i + 1], length.out = n),
                                               lat =seq(from = df$lat[i], to = df$lat[i + 1], length.out = n))
    
  }
  new_df
  
}

in_polygon <- function(x,y,lon,lat){
  point.in.polygon(x,y,lon,lat)
}

convert_df_to_matrix <- function(df,n){
  matrix(df$elevation,nrow = n,ncol = n,byrow = T)
  
}


df <- read.csv("~/minn_map_data.csv",stringsAsFactors = F)
zips <- read.csv("~/Desktop/filtered_map.csv",stringsAsFactors = F)
zips <- read.csv("~/small_set.csv",stringsAsFactors = F)
zips <- read.csv("~/medium_set.csv",stringsAsFactors = F)
df <- df %>% filter(Zip_Code %in% zips$Zip_Code)

df <- df %>% filter(center_long > -96.710704,center_long < -91.8742,center_lat > 43.71954980, center_lat < 47.0200)



# good ratio is 218.5777 per square degree
width <- abs(max(df$long) - min(df$long))
height <- abs(max(df$lat) - min(df$lat))
rec_npoint <- 218.577 * (width * height)


npoints <- 800


# df3 <- expand.grid(x = seq(from = -94.30019, to = -94.0517414, length.out = lat_long_ratio), 
#                    y = seq(from =  44.931395, to = 45.234210, length.out = 100))

offset <- .1
df3 <- expand.grid(x = seq(from = min(df$long) - offset, to = max(df$long) + offset, length.out = npoints), 
                   y = seq(from = min(df$lat) - offset, to = max(df$lat) + offset, length.out = npoints))


# x <- -94.5444
# y <- 45.0919

get_height <- function(x,y){
  progr_value <<- progr_value + progress_inc
  setTxtProgressBar(pb,progr_value)
  poly_df_list <- df %>% group_by(group) %>% mutate(in_zip = point.in.polygon(x,y,long,lat))  %>% ungroup() %>% 
    filter(in_zip == 1) %>% select(Freq)
  
  if (nrow(poly_df_list) == 0) return(0)
  
  poly_df_list$Freq[1]
}


get_elevation <- function(x,y,nsplits,start = 1){
  if (nsplits == 0){
    elevation <- mapply(get_height,x,y)
  }else{
    n <- length(x)
    
    if (nsplits %% n != 0){
      vec_splits <- nsplits:(nsplits + 1000)
      mod_vec <- n %% vec_splits
      nsplits<- vec_splits[which(mod_vec == 0)[1]]
      
    }
    
    split_length <- n / nsplits

    for (i in start:nsplits){
      starti <- ((i-1) * split_length) + 1
        newx <- x[starti:(starti + split_length - 1)]
        newy <- y[starti:(starti + split_length - 1)]
        temp_elevation <- mapply(get_height, newx,newy)
        save(temp_elevation, file = paste0("~/3d_maps/temp_elevation_files/elev_",i,".RData"))
    }
    
    elevation <- c()
    for (i in 1:nsplits){
      load(file = paste0("~/3d_maps/temp_elevation_files/elev_",i,".RData"))
      elevation <- c(elevation,temp_elevation)
    }
    
    
    
  }
  elevation
  
}

pb <- txtProgressBar(style = 3)
progress_inc <- 1 / nrow(df3)
progr_value <- 0


df3$elevation <- get_elevation(df3$x,df3$y,nsplits = 600)
close(pb)




save(df3,file = "~/saved_map.RData")
df3$elevation <- df3$elevation + 5

#microbenchmark(mapply(get_height,df3$x,df3$y),times = 10)


m <- convert_df_to_matrix(df3,npoints)

save(m,file = "~/3d_maps/m_400.RData")

m %>% sphere_shade() %>% 
  add_shadow(ray_shade(m)) %>% 
  add_shadow(ambient_shade(m)) %>% 
  plot_3d(m)


save_3dprint("~/3d_maps/map_400_pts.stl")


#new_df2 <- fill_region(df2)


avg_time <- 1.449
est_time <- (nrow(df3) / 100) * 1.449

# final plot
ggplot() + geom_point(data = df3, aes(x = x, y = y, color = factor(val))) + 
  geom_point(data = new_df2,aes(x = long,y = lat))




## show poly model with dist
dd %>% 
  ggplot(aes(x = 1:nrow(dd), y = dist)) + 
  geom_line()  + geom_smooth(formula =dd$dist ~ poly(dd$index,8) )


## show points of region and line
new_df2 %>% 
  ggplot(aes(x = long,y = lat)) + geom_point() +
  geom_line(data = dd, aes(x = x, y =y))  + geom_point(data = dd[400,], aes(x = x, y = y), color = "red")
