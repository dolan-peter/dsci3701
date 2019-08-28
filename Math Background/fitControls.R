FitControls=function(n, b=c(min= 0.4,max=3.6,val=0,step=0.1),
                        m=c(min=-0.4,max=5.1,val=1,step=0.1),
                    theta=c(min=-180,max=180,val=6,step=1),
                      phi=c(min=-180,max=180,val=24,step=1),
                    explore=FALSE){
  
  paste0(
    fluidRow(
      with(data.frame(t(b)),{
        column(width=4,sliderInput(paste0("model.intercept",n),"Intercept",min=min,max=max,step=step,val=val))
      }),with(data.frame(t(m)),{
        column(width=4,sliderInput(paste0("model.slope",n),"Slope",min=min,max=max,step=step,val=val))
      })
    ),
    fluidRow(
      with(data.frame(t(theta)),{
        column(width=4,sliderInput(paste0("model.theta",n),"Theta",min=min,max=max,step=step,val=val))
      }),with(data.frame(t(m)),{
        column(width=4,sliderInput(paste0("model.phi",n),"Phi",min=min,max=max,step=step,val=val))
      })
    ),ifelse(explore,paste0(
    fluidRow(
        column(width=4,sliderInput(paste0("model.min.intercept",n),"Min intercept",min=-10,max=10,step=0.1,val=b["min"])),
        column(width=4,sliderInput(paste0("model.max.intercept",n),"Max intercept",min=-10,max=10,step=0.1,val=b["max"]))
    ),
    fluidRow(
      column(width=4,sliderInput(paste0("model.min.slope",n),"Min intercept",min=-10,max=10,step=0.1,val=m["min"])),
      column(width=4,sliderInput(paste0("model.max.slope",n),"Max intercept",min=-10,max=10,step=0.1,val=m["max"]))
    )
    ),''),
  checkboxInput(paste0("model.exponentiate",n),"Exponentiate",val=TRUE),
  fluidRow(
    column(width=8,plotOutput(paste0("paramExplore",n))),
    column(width=4,plotOutput(paste0("lineFit2",n)))
  ),
  plotOutput(paste0("paramContour",n))
  )
}