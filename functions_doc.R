fee_val <- function(type, ror,bm_ror, eop_val, sop_val, i, trf, hurd_rate,hwm,wm ) {
  switch(type,
         abs_perf         = if(ror[i]>0 & eop_val[i]>100){return((eop_val[i]-100)* trf / 100)}else{return(0)},
         gain             = if(ror[i]>0 & (eop_val[i] > sop_val[i])){return((eop_val[i]-sop_val[i]) * trf / 100)}else{return(0)},
         abs_perf_hwm     = if(ror[i]>0 & eop_val[i] > hwm){((eop_val[i]-hwm) * trf / 100)}else{return(0)},
         abs_perf_wm      = if(ror[i]>0 & eop_val[i] > wm ){((eop_val[i]-wm) * trf / 100)}else{return(0)},
         bm               = if(ror[i]>bm_ror[i]){((ror[i]-bm_ror[i]) * trf / 100)}else{return(0)},
         hr               = if(ror[i]>hurd_rate){return((ror[i]-hurd_rate) * trf / 100)}else{return(0)},
         bm_hwm           = if(ror[i]>bm_ror[i] & eop_val[i] > hwm ){return((ror[i]-bm_ror[i])*(eop_val[i]-hwm) * trf / 100)}else{return(0)},
         hr_hwm           = if(ror[i]>hurd_rate & eop_val[i] > hwm ){return((ror[i]-hurd_rate)*(eop_val[i]-hwm) * trf / 100)}else{return(0)},
         bm_wm            = if(ror[i]>bm_ror[i] & eop_val[i] > wm ){return((ror[i]-bm_ror[i])*(eop_val[i]-wm) * trf / 100)}else{return(0)},
         hr_wm            = if(ror[i]>hurd_rate & eop_val[i] > wm ){return((ror[i]-hurd_rate)*(eop_val[i]-wm) * trf / 100)}else{return(0)}
  )}

simulator <- function(periods = 10,
                      trf = 0.05,
                      trf2 = 0.05,
                      riskfree=3,
                      bm_std=0.03,
                      port_std=25,
                      hurd_rate = -1,
                      # typus1 ="abs_perf", 
                      typus1, 
                      # typus2="gain",
                      typus2) {
  ######## variable list ########
  ror = rnorm(periods, mean = riskfree, sd = port_std)/100
  per <- c(1:periods)
  sop_val <- c(100)
  eop_val <- c()
  bm_sop_val <- c(100)
  bm_eop_val <- c()
  tariff1 <- numeric(periods)
  tariff2 <- numeric(periods)
  bm_ror <- rnorm(periods, mean = riskfree, sd = bm_std)/100
  wm=100
  hwm = 100
  ######## end variable list ########
  
  for (i in 1:periods){
    if(i!=1) sop_val = c(sop_val,tail(eop_val, n=1))
    eop_val = c(eop_val,tail(sop_val, n=1)*(1+ror[i]))
    
    tariff1[i]=fee_val(type=typus1,ror=ror,bm_ror=bm_ror,
                       eop_val=eop_val, sop_val=sop_val, i=i,
                       trf=trf, hurd_rate=hurd_rate,hwm=hwm,wm=wm)
    tariff2[i]=fee_val(type=typus2,ror=ror,bm_ror=bm_ror,
                       eop_val=eop_val, sop_val=sop_val, i=i,
                       trf=trf2, hurd_rate=hurd_rate,hwm=hwm,wm=wm)
  }
  return(data.frame(per,sop_val,eop_val,ror,tariff1,tariff2))
}


