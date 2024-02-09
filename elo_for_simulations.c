#include <R.h>
#include <math.h>
#include <Rmath.h>

void elo_inflation(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,int*adaptive,double*A,double*B,double*cumsum,double*exp_bias){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     double dif=0;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify probabilities for item selection*/
                if(adaptive[0]==0){
                    Mp=M[0];
                }
                if(adaptive[0]==1){
                    Mp=0;		
                    for(int s=0;s<M[0];s++){
                        dif=t[i+r*N[0]]-d[i+r*M[0]];
                        L=exp(dif*dif*A[0]+dif*B[0]);
                        Mp=Mp+L;
                        cumsum[s+1]=cumsum[s]+L;
                    }
                }
                for(int s=0;s<M[0];s++){
                    exp_bias[i+g*N[0]]=exp_bias[i+g*N[0]]+(cumsum[s+1]-cumsum[s])/Mp*(d[s+r*M[0]]-delta[s])/reps[0];
                }
                /* sample an item given the selection probabilities*/
                p=runif(0,Mp);
                j=0;
                for(int s=1;s<M[0];s++){
                    if(p>cumsum[s]){
                        j=j+1;
                    }
                }
                /* compute the true probability correct*/
                L=1/(1+exp((delta[j]-theta[i]))); 
                /*generate the observed response*/
                p=runif(0,1);
                x=1*(L>p);
                /*compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j+r*M[0]]-t[i+r*N[0]]));
                /* update the ratings*/
                t[i+r*N[0]]=t[i+r*N[0]]+K[0]*(x-e);
                d[j+r*M[0]]=d[j+r*M[0]]-K[0]*(x-e);
                /*add to the mean of person i at time point g*/
                mT[i+g*N[0]]=mT[i+g*N[0]]+t[i+r*N[0]]/reps[0];
            }            
        }
        /*compute the variances of the item and person ratings at time point g*/
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                vT[i+g*N[0]]=vT[i+g*N[0]]+(t[i+r*N[0]]-mT[i+g*N[0]])*(t[i+r*N[0]]-mT[i+g*N[0]])/(reps[0]-1);
            }
        }
     }
     PutRNGstate();
}

void elo(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,int*adaptive,double*A,double*B,double*cumsum,double*mD){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     double dif=0;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify probabilities for item selection*/
                if(adaptive[0]==1){
                    Mp=0;		
                    for(int s=0;s<M[0];s++){
                        dif=t[i+r*N[0]]-d[s+r*M[0]];
                        L=exp(dif*dif*A[0]+dif*B[0]);
                        Mp=Mp+L;
                        cumsum[s+1]=cumsum[s]+L;
                    }
                }
                if(adaptive[0]==0){
                    Mp=M[0];
                }
                /* sample an item given the selection probabilities*/
                p=runif(0,Mp);
                j=0;
                for(int s=1;s<M[0];s++){
                    if(p>cumsum[s]){
                        j=j+1;
                    }
                }
                /* compute the true probability correct*/
                L=1/(1+exp((delta[j]-theta[i]))); 
                /*generate the observed response*/
                p=runif(0,1);
                x=1*(L>p);
                /*compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j+r*M[0]]-t[i+r*N[0]]));
                /* update the ratings*/
                t[i+r*N[0]]=t[i+r*N[0]]+K[0]*(x-e);
                d[j+r*M[0]]=d[j+r*M[0]]-K[0]*(x-e);
                /*add to the mean of person i at time point g*/
                mT[i+g*N[0]]=mT[i+g*N[0]]+t[i+r*N[0]]/reps[0];
            }
            /*for each item, add the current value to the mean of item j at time point g*/
            for(int j=0;j<M[0];j++){
              mD[j+g*M[0]]=mD[j+g*M[0]]+d[j+r*M[0]]/reps[0];
            }
        }
        /*compute the variances of the item and person ratings at time point g*/
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                vT[i+g*N[0]]=vT[i+g*N[0]]+(t[i+r*N[0]]-mT[i+g*N[0]])*(t[i+r*N[0]]-mT[i+g*N[0]])/(reps[0]-1);
            }
        }
     }
     PutRNGstate();
}

void elo_double(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,double*A,double*B,double*cumsum,double*mD){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     double dif=0;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int h=0;h<2;h++){
            for(int r=0;r<reps[0];r++){    
                for(int i=0;i<N[0];i++){
                    /*specify probabilities for item selection*/
                    Mp=0;		
                    for(int s=0;s<M[0];s++){
                        /*item selection depends on the current ratings via a normal kernel*/
                        dif=t[i+r*N[0]+(1-h)*reps[0]*N[0]]-d[s+r*M[0]+(1-h)*reps[0]*M[0]];
                        L=exp(dif*dif*A[0]+dif*B[0]);
                        Mp=Mp+L;
                        cumsum[s+1]=cumsum[s]+L;
                    }
                    /* sample an item given the selection probabilities*/
                    p=runif(0,Mp);
                    j=0;
                    for(int s=1;s<M[0];s++){
                        if(p>cumsum[s]){
                            j=j+1;
                        }
                    }
                    /* compute the true probability correct*/
                    L=1/(1+exp((delta[j]-theta[i]))); 
                    /*generate the observed response*/
                    p=runif(0,1);
                    x=1*(L>p);
                    /*compute the expected accuracy based on the current ratings*/
                    e=1/(1+exp(d[j+r*M[0]+h*reps[0]*M[0]]-t[i+r*N[0]+h*reps[0]*N[0]]));
                    /* update the ratings*/
                    t[i+r*N[0]+h*reps[0]*N[0]]=t[i+r*N[0]+h*reps[0]*N[0]]+K[0]*(x-e);
                    d[j+r*M[0]+h*reps[0]*M[0]]=d[j+r*M[0]+h*reps[0]*M[0]]-K[0]*(x-e);
                    /*add to the mean of person i at time point g*/
                    mT[i+g*N[0]+h*games[0]*N[0]]=mT[i+g*N[0]+h*games[0]*N[0]]+t[i+r*N[0]+h*reps[0]*N[0]]/reps[0];
                }
                for(int j=0;j<M[0];j++){
                  mD[j+g*M[0]+h*games[0]*M[0]]=mD[j+g*M[0]+h*games[0]*M[0]]+d[j+r*M[0]+h*reps[0]*M[0]]/reps[0];
                }
            }
            /*compute the variances of the item and person ratings at time point g*/
            for(int r=0;r<reps[0];r++){
                for(int i=0;i<N[0];i++){
                    vT[i+g*N[0]+h*games[0]*N[0]]=vT[i+g*N[0]+h*games[0]*N[0]]+(t[i+r*N[0]+h*reps[0]*N[0]]-mT[i+g*N[0]+h*games[0]*N[0]])*(t[i+r*N[0]+h*reps[0]*N[0]]-mT[i+g*N[0]+h*games[0]*N[0]])/(reps[0]-1);
                }
            }
        }    
     }
     PutRNGstate();
}



void elo_history(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*t_hist,double*d_hist,double*mT,double*vT,double*A,double*B,double*cumsum,int*use_mean,int*LH,double*mD){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     GetRNGstate();
     int gg=0;
     double avT=0;
     double avD=0;
     double dif=0;
     for(int g=0;g<games[0];g++){
        gg=gg+1;
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify probabilities for item selection*/
                Mp=0;		
                avT=0;
                if(use_mean[0]==1){
                    for(int h=0;h<LH[0];h++){
                        avT=avT+t_hist[i+r*N[0]+h*N[0]*reps[0]]/LH[0];
                    }
                }
                for(int s=0;s<M[0];s++){
                    if(g<LH[0]){
                            L=1.00;
                    }
                    if(g>(LH[0]-1)){
                        if(use_mean[0]==1){
                            avD=0;
                            for(int h=0;h<LH[0];h++){
                                avD=avD+d_hist[s+r*M[0]+h*M[0]*reps[0]]/LH[0];
                            }
                            dif=avT-avD;
                        }
                        if(use_mean[0]==0){
                            dif=t_hist[i+r*N[0]+(gg-1)*N[0]*reps[0]]-d_hist[s+r*M[0]+(gg-1)*M[0]*reps[0]];
                        }
                        L=exp(dif*dif*A[0]+dif*B[0]);
                    }
                    Mp=Mp+L;
                    cumsum[s+1]=cumsum[s]+L;
                }
                /* sample an item given the selection probabilities*/
                p=runif(0,Mp);
                j=0;
                for(int s=1;s<M[0];s++){
                    if(p>cumsum[s]){
                        j=j+1;
                    }
                }
                /* compute the true probability correct*/
                L=1/(1+exp((delta[j]-theta[i]))); 
                /*generate the observed response*/
                p=runif(0,1);
                x=1*(L>p);
                /*compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j+r*M[0]]-t[i+r*N[0]]));
                /* update the ratings*/
                t[i+r*N[0]]=t[i+r*N[0]]+K[0]*(x-e);
                d[j+r*M[0]]=d[j+r*M[0]]-K[0]*(x-e);
                /*add to the mean of person i at time point g*/
                mT[i+g*N[0]]=mT[i+g*N[0]]+t[i+r*N[0]]/reps[0];
                /*save the value as one of the 10 last values*/
                t_hist[i+r*N[0]+(gg-1)*N[0]*reps[0]]=t[i+r*N[0]];
                d_hist[j+r*M[0]+(gg-1)*M[0]*reps[0]]=d[j+r*M[0]];
            }
            /*for each item, add the current value to the mean of item j at time point g*/
            for(int j=0;j<M[0];j++){
              mD[j+g*M[0]]=mD[j+g*M[0]]+d[j+r*M[0]]/reps[0];
            }
        }
        /*compute the variances of the item and person ratings at time point g*/
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                vT[i+g*N[0]]=vT[i+g*N[0]]+(t[i+r*N[0]]-mT[i+g*N[0]])*(t[i+r*N[0]]-mT[i+g*N[0]])/(reps[0]-1);
            }        
        }
        if(gg==LH[0]){gg=0;}
     }
     PutRNGstate();
}


void elo_MH(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,double*A,double*B,double*cumsum,double*P,double*P_star,double*mD){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     double Mp_star=0;
     double t_star=0;
     double d_star=0;
     double u=0;
     double dif;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify probabilities for item selection*/
                Mp=0;		
                for(int s=0;s<M[0];s++){
                    dif=t[i+r*N[0]]-d[s+r*M[0]];
                    L=exp(dif*dif*A[0]+dif*B[0]);
                    P[s]=L;
                    Mp=Mp+L;
                    cumsum[s+1]=cumsum[s]+L;
                }
                /* sample an item given the selection probabilities*/
                p=runif(0,Mp);
                j=0;
                for(int s=1;s<M[0];s++){
                    if(p>cumsum[s]){
                        j=j+1;
                    }
                }
                /* compute the true probability correct*/
                L=1/(1+exp((delta[j]-theta[i]))); 
                /*generate the observed response*/
                p=runif(0,1);
                x=1*(L>p);
                /*compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j+r*M[0]]-t[i+r*N[0]]));
                /* proposed update for the ratings*/
                t_star=t[i+r*N[0]]+K[0]*(x-e);
                d_star=d[j+r*M[0]]-K[0]*(x-e);
                /*compute the selection probability for the proposed values*/
                dif=t_star-d_star;
                P_star[j]=exp(dif*dif*A[0]+dif*B[0]);
                Mp_star=P_star[j];
                for(int s=0;s<M[0];s++){
                    if(s!=j){
                        dif=t_star-d[s+r*M[0]];
                        P_star[s]=exp(dif*dif*A[0]+dif*B[0]);
                        Mp_star=Mp_star+P_star[s];
                    }
                }
                /*MH step, accept the proposed value with probability equal to the ration of selection probabilities*/
                u=runif(0,1);
                if(u<(P_star[j]/Mp_star)/(P[j]/Mp)){
                    t[i+r*N[0]]=t_star;
                    d[j+r*M[0]]=d_star;
                }
                /*add to the mean of person i at time point g*/
                mT[i+g*N[0]]=mT[i+g*N[0]]+t[i+r*N[0]]/reps[0];
            }
            /*for each item, add the current value to the mean of item j at time point g*/
            for(int j=0;j<M[0];j++){
              mD[j+g*M[0]]=mD[j+g*M[0]]+d[j+r*M[0]]/reps[0];
            }
        }
        /*compute the variances of the item and person ratings at time point g*/
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                vT[i+g*N[0]]=vT[i+g*N[0]]+(t[i+r*N[0]]-mT[i+g*N[0]])*(t[i+r*N[0]]-mT[i+g*N[0]])/(reps[0]-1);
            }
        }
     }
     PutRNGstate();
}


void elo_fixed_items(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,int*adaptive,double*A,double*B,double*cumsum){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double dif=0;
     double Mp=0;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify the selection probabilities for the items*/
                if(adaptive[0]==0){
                    Mp=M[0];
                }		
                if(adaptive[0]==1){
                    Mp=0;
                    for(int s=0;s<M[0];s++){
                        dif=t[i+r*N[0]]-d[s];
                        L=exp(dif*dif*A[0]+dif*B[0]);
                        Mp=Mp+L;
                        cumsum[s+1]=cumsum[s]+L;
                    }
                }
                /* sample an item given the selection probabilities*/
                p=runif(0,Mp);
                j=0;
                for(int s=1;s<M[0];s++){
                    if(p>cumsum[s]){
                        j=j+1;
                    }
                }
                /* compute the true probability correct*/
                L=1/(1+exp((delta[j]-theta[i]))); 
                /*generate a response*/
                p=runif(0,1);
                x=1*(L>p);
                /*compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j]-t[i+r*N[0]]));
                /* update the rating of the person*/
                t[i+r*N[0]]=t[i+r*N[0]]+K[0]*(x-e);
                /*add to the mean of the ratings of person i at timepoint g*/
                mT[i+g*N[0]]=mT[i+g*N[0]]+t[i+r*N[0]]/reps[0];
            }
        }
        /*compute the variances of the ratings of all person at timepoint g*/
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                vT[i+g*N[0]]=vT[i+g*N[0]]+(t[i+r*N[0]]-mT[i+g*N[0]])*(t[i+r*N[0]]-mT[i+g*N[0]])/(reps[0]-1);
            }
        }
     }
     PutRNGstate();
}