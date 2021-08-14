library(magick)
# the formula for the changing of angle over time 
# https://en.wikipedia.org/wiki/Pendulum
theta <- function(t,period,theta0){
  theta0*cos(2*pi*t/period)
}

# for drawing the pendulum at a given point (x,y), i.e. drawing a line between (0,0) and (x,y)
draw.pendulum <- function(pen.x, pen.y){
  
  pivot.x <- 0
  pivot.y <- 0

  plot(x=pivot.x,y=pivot.y,type = 'l',ylim = c(-1.5,0.1),asp = 1)
  lines(x=c(pivot.x,pen.x),y=c(pivot.y,pen.y))
  points(x = pen.x, y=pen.y,col='red')
}

# calculate the position of the pendulum over time
# l-length of the pendulum
# theta0 - the starting angle (in radian)
# runtime - the simulation time (in seconds)
# step - time step
pendulum <- function(theta0,l,runtime,step = 0.1){
  g <- 9.8
  period <- 2*pi*sqrt(l/g)
  
  times <- seq(0.0,runtime, by = step)

  pen.x <- l*sin(theta(times, period = period,theta0 = theta0)) 
  pen.y <- -l*cos(theta(times,period = period,theta0 = theta0))
  
  output <- matrix(c(times,pen.x,pen.y),ncol = 3)
  return(output)
}


pendulum.sim <- function(theta0, l, runtime, step = 0.1){
  
  sim.data <- pendulum(theta0 = theta0, l=l, runtime = runtime)
  
  ## export each frame as png
  for(i in 1:nrow(sim.data)){
    png(filename = paste0(tempdir(),"/",i,".png"))
    draw.pendulum(sim.data[i,2],sim.data[i,3])
    dev.off()
  }
  
  frames <- image_read(paste0(tempdir(),"/",1:nrow(sim.data),".png"))
  animate <- image_animate(image = frames, fps = 1/step)
  
  return(animate)
  
}

animate <- pendulum.sim(theta0 = pi/4, l = 1, runtime = 10)
print(animate)
