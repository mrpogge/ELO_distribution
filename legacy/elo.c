#include <R.h>
#include <math.h>
#include <Rmath.h>

void elo(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*T,double*D,int*adaptive,double*mP,double*sP,double*cumsum){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     GetRNGstate();
     for(int r=0;r<reps[0];r++){
        for(int i=0;i<N[0];i++){
            t[i]=0;
        }
        for(int j=0;j<M[0];j++){
            d[j]=0;
        }
        for(int g=0;g<games[0];g++){
            for(int i=0;i<N[0];i++){
                /*specify item selection probabilities*/
                Mp=0;		
                for(int s=0;s<M[0];s++){
                    if(adaptive[0]==0){/*select a random item*/
                        L=1.00;
                    }
                    if(adaptive[0]==1){/*select an item using the current rating via a normal kernel*/
                        L=dnorm(1/(1+exp(d[s]-t[i])),mP[0],sP[0],0);
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
                L=1/(1+exp((delta[j+g*M[0]]-theta[i+g*N[0]]))); 
                /*generate the observed response*/
                p=runif(0,1);
                x=1*(L>p);
                /*compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j]-t[i]));
                /* update the ratings*/
                t[i]=t[i]+K[0]*(x-e);
                d[j]=d[j]-K[0]*(x-e);
                /*save the ratings of the item and the person at timepoint g in replication r*/
                D[j+g*M[0]+r*M[0]*games[0]]=d[j]; 
                T[i+g*N[0]+r*N[0]*games[0]]=t[i];
            }
        }
     }
     PutRNGstate();
}

void elo_fixed_items(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*T,int*adaptive,double*mP,double*sP,double*cumsum){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     GetRNGstate();
     for(int r=0;r<reps[0];r++){
        for(int i=0;i<N[0];i++){
            t[i]=0;
        }
        for(int g=0;g<games[0];g++){
            for(int i=0;i<N[0];i++){
                /*specify item selection probabilities*/
                Mp=0;		
                for(int s=0;s<M[0];s++){
                    if(adaptive[0]==0){/*select a random item*/
                        L=1.00;
                    }
                    if(adaptive[0]==1){/*select an item using the current rating via a normal kernel*/
                        L=dnorm(1/(1+exp(d[s]-t[i])),mP[0],sP[0],0);
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
                L=1/(1+exp((delta[j+g*M[0]]-theta[i+g*N[0]]))); 
                /*generate the observed response*/
                p=runif(0,1);
                x=1*(L>p);
                /* compute the expected accuracy based on the current ratings*/
                e=1/(1+exp(d[j]-t[i]));
                /* update the person's rating*/
                t[i]=t[i]+K[0]*(x-e);
                /*save the rating of person i at timepoint g in replication r*/
                T[i+g*N[0]+r*N[0]*games[0]]=t[i];
            }
        }
     }
     PutRNGstate();
}



void elo2(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*mD,double*vT,double*vD,int*adaptive,double*mP,double*sP,double*cumsum){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify probabilities for item selection*/
                Mp=0;		
                for(int s=0;s<M[0];s++){
                    if(adaptive[0]==1){/*item selection depends on the current ratings via a normal kernel*/
                        L=dnorm(1/(1+exp((d[s+r*M[0]]-t[i+r*N[0]]))),mP[0],sP[0],0);
                    }
                    if(adaptive[0]==0){ /*all items are equal likely*/
                        L=1.00;
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
                L=1/(1+exp((delta[j+g*M[0]]-theta[i+g*N[0]]))); 
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
            for(int j=0;j<M[0];j++){
                vD[j+g*M[0]]=vD[j+g*M[0]]+(d[j+r*M[0]]-mD[j+g*M[0]])*(d[j+r*M[0]]-mD[j+g*M[0]])/(reps[0]-1);
            }
        }
     }
     PutRNGstate();
}


void elo2_fixed_items(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,int*adaptive,double*mP,double*sP,double*cumsum){
     int x=0;
     double e=0;
     double L=0;
     double p=0;
     int j=0;
     double Mp=0;
     GetRNGstate();
     for(int g=0;g<games[0];g++){
        for(int r=0;r<reps[0];r++){
            for(int i=0;i<N[0];i++){
                /*specify the selection probabilities for the items*/
                Mp=0;		
                for(int s=0;s<M[0];s++){
                    if(adaptive[0]==1){/*select proportional to the normal kernel*/
                        L=dnorm(1/(1+exp(d[s]-t[i+r*N[0]])),mP[0],sP[0],0);
                    }
                    if(adaptive[0]==0){/*select an item randomly with equal probabilities*/
                        L=1.00;
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
                L=1/(1+exp((delta[j+g*M[0]]-theta[i+g*N[0]]))); 
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
