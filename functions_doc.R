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