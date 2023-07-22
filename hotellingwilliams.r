#  HWTEST FUNCTION FOR S-PLUS and R .

#  John Van Sickle
#  USEPA, National Health and Environmental Effects
#  Laboratory, Western Ecology Division
#  200 SW 35th St. Corvallis, OR 97333
#  VanSickle.John@epa.gov

#  PURPOSE;
#  Performs the Hotelling/Williams test for the equality 
#  of 2 dependent product-moment correlations, Rxz and Ryz.
#  Based on Eq. 7 of  Steiger, J.H. 1980. Tests for Comparing 
#  Elements of a Correlation Matrix. Psychological Bulletin 87:245-251. 


#  USAGE: hwtest(X,Y,Z)

# -- X,Y,Z are equal-length vectors. 
# -- Function computes bivariate sample correlations among X, Y, and Z.
# -- Function then tests H0:(RHOzx-RHOzy)=0, against 2-sided alternative. 

#  NOTES;
#  -- Prior to usage, delete any observation with missing values for ;
#      one or more of X, Y and Z.
#  -- Must be 4 or more observations.
#  -- Loading the function: In Splus or R, execute the command 
#   > source("hwtest.ssc"), 
#        prior to function usage, where "hwtest.ssc" ;
#        may need to include the full pathname of this file;


#  EXAMPLE USAGE (see Van Sickle(2003) JAWRA paper); 
#  hwtest(agnetwrk, agwshed, ibi);


#   EXAMPLE OUTPUT;

#   HW Test result
#   Rzx = -0.63 ,  Rzy = -0.292 ,  Rxy = 0.404
#   Rzx-Rzy = -0.338     HW-statistic = 1.7522
#   2-sided P-value = 0.0951

#####################################;

hwtest<-function(X,Y,Z) { ;
                         
     zx<-cor(Z,X); zy<-cor(Z,Y); xy<-cor(X,Y); #compute corrs;
#spearman correlation accomodation                         
#     zx<-cor(Z,X,method = "spearman"); zy<-cor(Z,Y,method = "spearman"); xy<-cor(X,Y,method = "spearman"); #compute corrs;              
     detc<-1+2*zx*zy*xy-zx*zx-zy*zy-xy*xy; # determinant;
     if(detc<=0)return("Determinant failed -- exact linear dependence in data");
     nn<-length(X); # sample size;
     obsdif<-zx-zy; #observed difference in Corr coef;
     ra<-0.5*(zx+zy); #average correlation;
     denom<-2*detc*(nn-1)/(nn-3)+ra*ra*(1-xy)^3; 
     numer<-(nn-1)*(1+xy);
     hw<-abs(obsdif)*sqrt(numer/denom); #hw-statistic, drop sign;
     pval<-2*(1-pt(hw,nn-3)); #2-sided p-value;
     print("HW Test result",quote=F);
     out5<-paste("Rzx =",as.character(round(zx,3)),
                 ",  Rzy =",as.character(round(zy,3)),
                 ",  Rxy =",as.character(round(xy,3)),collapse="");
      print(out5,quote=F);
     out1<-paste("Rzx-Rzy =",as.character(round(obsdif,3)), 
            "    HW-statistic =",as.character(round(hw,4)), collapse="");
     print(out1,quote=F);
     out2<-paste("2-sided P-value =", as.character(round(pval,4)), collapse="");
     print(out2,quote=F);
     return(NULL);
   }; #end of function;
     
