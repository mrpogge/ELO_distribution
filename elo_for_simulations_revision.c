#include <R.h>
#include <math.h>
#include <Rmath.h>

void urnings_simple_reps(int*u,int*v,double*theta,double*delta,int*N,int*K,int*Rep,int*k1,int*k2,double*P,double*cumsum,int*nrep,double*Rel,double*true_logit,double*mean_true_logit,double*s2,double*U,double*V){
  double L=0;
  double p=0;
  double Mp=0;
  double Mp1=0;
  int oldU=0;
  int oldV=0;
  int newV=0;
  int newU=0;
  int j=0;
  int x=0;
  int y=0;
  int success=0;
  int Q=0;
  int s=0;
  int jj=0;
  double Correct=0;
  double Incorrect=0;
  double mean1=0;
  double s1=0;
  double s12=0;
  GetRNGstate();
  for(int rep=0;rep<Rep[0];rep++){/* loop over iterations*/
    for(int r=0;r<nrep[0];r++){
      for(int i=0;i<N[0];i++){/* loop over persons*/
          Mp=0;		
          for(int s=0;s<K[0];s++){
            Mp=Mp+P[u[i+r*N[0]]+v[s+r*K[0]]*(k1[0]+1)];
            cumsum[s+1]=cumsum[s]+P[u[i+r*N[0]]+v[s+r*K[0]]*(k1[0]+1)];
          }
          p=runif(0,Mp);
          j=0;
          for(int s=1;s<K[0];s++){
            if(p>cumsum[s]){
              j=j+1;
            }
          }
        
        /* save the current values of the person and the item */
        oldU=u[i+r*N[0]];
        oldV=v[j+r*K[0]];
        /* generate the observed accuracy*/
        L=1/(1+exp(delta[j]-theta[i])); /* true probability correct*/
        p=runif(0,1.00);
        x=1*(L>p);
        /* add the balls based on the observed response to the urns*/                      
        u[i+r*N[0]]=u[i+r*N[0]]+x;                             
        v[j+r*K[0]]=v[j+r*K[0]]+(1-x);	
        /* compute the probability of X=1 given the urn configurations*/
        Correct=1.00;
        Incorrect=1.00;   
        Correct=Correct*(u[i+r*N[0]])*(k2[0]+1-v[j+r*K[0]]);
        Incorrect=Incorrect*(k1[0]+1-u[i+r*N[0]])*(v[j+r*K[0]]);
        
        L=Correct/(Correct+Incorrect);      
        /* generate the simulated response */
        p=runif(0,1.00);
        y=1*(L>p);
        /* remove the balls based on the simulated response from the urns: These would be the proposed values*/
        u[i+r*N[0]]=u[i+r*N[0]]-y;
        v[j+r*K[0]]=v[j+r*K[0]]-(1-y);
        
        
          newU=u[i+r*N[0]];
          newV=v[j+r*K[0]];
          u[i+r*N[0]]=oldU;
          v[j+r*K[0]]=oldV;
          if(newU!=oldU){
            /*Compute the normalising constant for the selection probability given the proposed values*/
            Mp1=P[newU+newV*(k1[0]+1)];
            for(int s=0;s<K[0];s++){
              if(s!=j){
                Mp1=Mp1+P[newU+v[s+r*K[0]]*(k1[0]+1)];
              }
            }
            /* compute the MH acceptance probability*/
            L=P[newU+newV*(k1[0]+1)]/Mp1*Mp/P[oldU+oldV*(k1[0]+1)];
            /* Generate a random uniform (0,1) number to decide whether to accept the proposal*/
            p=runif(0,1);
            if(p<L){
              u[i+r*N[0]]=newU;
              v[j+r*K[0]]=newV;
            }
          }
       }
     
    }

    for(int r=0;r<nrep[0];r++){
      for(int i=0;i<N[0];i++){
        U[i+rep*N[0]]=U[i+rep*N[0]]+1.00*u[i+r*N[0]]/(k1[0]*nrep[0]);
      }  
      for(int j=0;j<K[0];j++){
        V[j+rep*K[0]]=V[j+rep*K[0]]+1.00*v[j+r*K[0]]/(k2[0]*nrep[0]);
      }
      mean1=0;
      for(int i=0;i<N[0];i++){
        mean1=mean1+1.00*u[i+r*N[0]]/(k1[0]*N[0]);
      }
      s1=0;
      s12=0;
      for(int i=0;i<N[0];i++){
        s1=s1+(1.00*u[i+r*N[0]]/k1[0]-mean1)*(1.00*u[i+r*N[0]]/k1[0]-mean1);
        s12=s12+(1.00*u[i+r*N[0]]/k1[0]-mean1)*(true_logit[i]-mean_true_logit[0]);
      }
      Rel[rep]=Rel[rep]+s12*s12/(s1*s2[0]*nrep[0]);
    }
  }
  PutRNGstate();
}  


void elo_double_extras(double*K,int*reps,int*games,int*N,int*M,double*theta,double*delta,double*t,double*d,double*mT,double*vT,double*A,double*B,double*cumsum,double*mD,double*Rel1,double*Rel1_logit,double*Rel2,double*Rel2_logit,double*true_logit,double*mean_true,double*mean_true_logit,double*s3,double*s3_logit,double*mse){
  int x=0;
  double e=0;
  double L=0;
  double p=0;
  int j=0;
  double Mp=0;
  double dif=0;
  double mean_delta1=0;
  double mean_delta2=0;
  double mean1=0;
  double mean2=0;
  double s1=0;
  double s2=0;
  double s12=0;
  double s13=0;
  double s23=0;
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
    
    for(int r=0;r<reps[0];r++){
      mean_delta1=0;
      mean_delta2=0;
      for(int j=0;j<M[0];j++){
        mean_delta1=mean_delta1+d[j+r*M[0]]/M[0];
        mean_delta2=mean_delta2+d[j+r*M[0]+reps[0]*M[0]]/M[0];
      }
      for(int i=i;i<N[0];i++){
        mse[g]=mse[g]+0.5*(1/(1+exp(-t[i+r*N[0]]+mean_delta1))-true_logit[i])*(1/(1+exp(-t[i+r*N[0]]+mean_delta1))-true_logit[i])/(N[0]*reps[0])+0.5*(1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))-true_logit[i])*(1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))-true_logit[i])/(N[0]*reps[0]);
      }
      mean1=0;
      mean2=0;
      for(int i=0;i<N[0];i++){
        mean1=mean1+t[i+r*N[0]]/N[0];
        mean2=mean2+t[i+r*N[0]+reps[0]*N[0]]/N[0];
      }
      s1=0;
      s2=0;
      s12=0;
      s13=0;
      s23=0;
      for(int i=0;i<N[0];i++){
        s1=s1+(t[i+r*N[0]]-mean1)*(t[i+r*N[0]]-mean1);
        s2=s2+(t[i+r*N[0]+reps[0]*N[0]]-mean2)*(t[i+r*N[0]+reps[0]*N[0]]-mean2);
        s12=s12+(t[i+r*N[0]]-mean1)*(t[i+r*N[0]+reps[0]*N[0]]-mean2);
        s13=s13+(t[i+r*N[0]]-mean1)*(theta[i]-mean_true[0]);
        s23=s23+(t[i+r*N[0]+reps[0]*N[0]]-mean2)*(theta[i]-mean_true[0]);
      }
      Rel1[g]=Rel1[g]+s12/sqrt(s1*s2)/reps[0];
      Rel2[g]=Rel2[g]+(s13/(sqrt(s3[0]*s1)))*(s13/(sqrt(s3[0]*s1)))/(reps[0]*2)+(s23/(sqrt(s3[0]*s2)))*(s23/(sqrt(s3[0]*s2)))/(reps[0]*2);
      
      mean1=0;
      mean2=0;
      for(int i=0;i<N[0];i++){
        mean1=mean1+1/(1+exp(-t[i+r*N[0]]+mean_delta1))/N[0];
        mean2=mean2+1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))/N[0];
      }
      s1=0;
      s2=0;
      s12=0;
      s13=0;
      s23=0;
      for(int i=0;i<N[0];i++){
        s1=s1+(1/(1+exp(-t[i+r*N[0]]+mean_delta1))-mean1)*(1/(1+exp(-t[i+r*N[0]]+mean_delta1))-mean1);
        s2=s2+(1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))-mean2)*(1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))-mean2);
        s12=s12+(1/(1+exp(-t[i+r*N[0]]+mean_delta1))-mean1)*(1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))-mean2);
        s13=s13+(1/(1+exp(-t[i+r*N[0]]+mean_delta1))-mean1)*(true_logit[i]-mean_true_logit[0]);
        s23=s23+(1/(1+exp(-t[i+r*N[0]+reps[0]*N[0]]+mean_delta2))-mean2)*(true_logit[i]-mean_true_logit[0]);
      }
      Rel1_logit[g]=Rel1_logit[g]+s12/sqrt(s1*s2)/reps[0];
      Rel2_logit[g]=Rel2_logit[g]+s13*s13/(s3_logit[0]*s1)/(reps[0]*2)+s23*s23/(s3_logit[0]*s2)/(reps[0]*2);
    }
    
  }
  PutRNGstate();
}
