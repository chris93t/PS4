library(Rtauchen)
library("plyr")

# PARAMETERS
beta = .99; ##discount factor 
alpha=1/3; ##production elasticity
sigma = 2; ## coefficient of risk aversion
delta=.025; ##depreciation rate
rho_epsi=0.5; ##AR(1) autoregressive
sigma_epsi=0.2; ##AR(1) std. devs of residual

##exogenous variable grid
num_z= 5;
PI<-Rtauchen(num_z,rho_epsi,sigma_epsi,3);
z_grid<-Tgrid(num_z,rho_epsi,sigma_epsi,3);
z_grid=exp(z_grid);
## invariant distribution is 
PI_inv=PI^1000;
PI_inv=PI_inv[1,];
           N_s=z_grid*PI_inv; # aggregate labor supply (Q3.)
           ## ASSET VECTOR
           a_lo = 0; ##lower bound of grid points
           a_hi = 80; ##(guess) upper bound of grid points
           num_a = 500;
           
           a = seq(from = a_lo, to = a_hi, length.out = num_a); ## asset (row) vector
           
           #INITIAL GUESS OF K AND FACTOR PRICES
           K_min=20;
           K_max=50;
           K_tol=1;
          while (abs(K_tol)>.01){
             
           if (K_max-K_min<0.01){
           break}
         
           # K_guess and correspoding factor prices
           K_guess=(K_min+K_max)/2;
           interest= alpha*K_guess^(alpha-1)*N_s^(1-alpha)+(1-delta);
           wage=(1-alpha)*K_guess^alpha*N_s^(-alpha);
           
     
           
           
           cons = do.call(rbind,lapply( (interest[1]* a), function (x) x-(a))) #Good
           cons = lapply(z_grid*wage,function (x) x+cons) #Potential Error
           ret = lapply(cons, function (x) (x ^ (1-sigma)) / (1 - sigma)); #% current period utility
           
           ret[[1]][cons[[1]]<0]<- -Inf
           ret[[2]][cons[[2]]<0]<--Inf
           ret[[3]][cons[[3]]<0]<- -Inf
           ret[[4]][cons[[4]]<0]<--Inf
           ret[[5]][cons[[5]]<0]<- -Inf
         
           
           
                         # INITIAL VALUE FUNCTION GUESS
                         v_guess = matrix(0,num_z,num_a)
                         
                         # VALUE FUNCTION ITERATION
                         v_tol = 1;
                         while (v_tol >.001){
                      
                           
                           
                           value<-lapply(alply(PI%*%v_guess,1), function(x) matrix( rep(x,num_a),num_a, length(x), byrow = TRUE ) )
                           
                           value_mat<-list()
                           vfn<-list()
                           policy<-list()
                           
                           diffrence<-list()
                           
                           for (i in 1:length(ret)){
                           
                           value_mat[[i]] = ret[[i]] +  beta*value[[i]]
                           
                           
                           #% find the optimal k' for every k:
                           vfn[[i]] = apply(value_mat[[i]], 1, max);
                           
                           policy[[i]]<-apply(value_mat[[i]], 1, which.max)
                           
                           diffrence[[i]]<-abs(vfn[[i]] - v_guess[i,])
                           

                           }
                       
                           
                     
                           dis<-do.call(max,diffrence)
                           #% if distance is larger than tolerance, update current guess and
                           #% continue, otherwise exit the loop
                           
                           
                           
                           
                           for (i in 1:length(ret)){
                           
                           v_guess[i,] <- vfn[[i]];
                          
                           
                           }
                           
           
           
           }
           
           # KEEP DECSISION RULE
            pol_fn<-list()  
            
            for (i in 1:length(ret)){
              
              pol_fn[[i]]<-a[policy[[i]]]
              
              
            }
                         
        
           
           #% SET UP INITITAL DISTRIBUTION
           
           mu<-matrix(1/(2*num_a),num_z,num_a)
           
           
           
           
         
           dis=1;
           while (dis>0.01){ 
             
           # ITERATE OVER DISTRIBUTIONS
             MuNew = matrix(0,num_z,num_a);
             
             muv<-as.vector(mu)
             
             mass<-muv[muv>0]
             
             
             emp_ind<-mu_row[mu>0]
             
             
             a_ind<-mu_col[mu>0]
           
           for (ii in (1:length(z_ind)) ){
             
             
             apr_ind <- pol_indx[emp_ind[ii],a_ind[ii]]; #% which a prime does the policy fn prescribe?
             
             MuNew[, apr_ind] <- MuNew[, apr_ind] + (pi[emp_ind[ii], ]*mass[ii]);
             
             
           
           }
           
             dis = max(abs(mu-MuNew));
             mu=MuNew
             
           
           }
           #Market clears
           
           k<-sum(unlist(lapply(pol_fn, function(x) x%*%mu)))
          
           K_tol=K-K_guess;
           if (K_tol>0){
           K_min=K_guess}
           else {K_max=K_guess}
           
           }

