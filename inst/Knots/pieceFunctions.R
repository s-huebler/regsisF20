rsq = function(knots, data){ # data=spruce.df
  df=within(data, {
    X=(BHDiameter-knots[1])*(BHDiameter>knots[1]) 
    X2=(BHDiameter-knots[2])*(BHDiameter>knots[2])
  
  })
  ret=summary(lm(Height ~ BHDiameter + X+X2, data=df))
  ret$r.squared
}


twoKnotReg = function(x,xk,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}


r = function(xk,xk2,data){ # data=spruce.df
  df=within(data, {
    X=(BHDiameter-xk)*(BHDiameter>xk) 
    X2=(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  ) 
  ret=summary(lm(Height ~ BHDiameter + X + X2, data=df))
  ret$r.squared
}

coeff = function(xk,xk2,data){ # data=spruce.df
  df=within(data, {
    X=(BHDiameter-xk)*(BHDiameter>xk) 
    X2=(BHDiameter-xk2)*(BHDiameter>xk2)
  }
  ) 
  ret=coef(lm(Height ~ BHDiameter + X + X2, data=df))
  ret
}


grid=function(int1,int2,data){
  dff=expand.grid(x=seq(int1[1], int1[2], length.out=50), y=seq(int2[1], int2[2], length.out=50))
    dff$r2<-map2_dbl(dff$x, dff$y,  ~r(.x,.y, data))
    dff
}
