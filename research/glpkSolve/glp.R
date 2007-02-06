require(glpk)

glp.make.list <- function(c,A,b,const.dir,dir,integer){
  list(dir=dir,objectives=c,const.matrix=A,const.dir=const.dir,rhs=b,integer=integer)
}

"glp" <- function(c,A,b,const.dir,dir,integer){
  lp <- as.lp(glp.make.list(c,A,b,const.dir,dir,integer))
  glp.solve(lp)
}

## the R-ported GNU Linear Programming kit
## solve function

glp.solve <- function(lp, ...){
  if (!inherits(lp, "lp"))
        stop("'lp' must inherit from class \"lp\"")
  ## create problem object
  p.obj<-lpx_create_prob()
  ## add the optimization direction to the problem object
  if(lp$dir=="max") lpx_set_obj_dir(p.obj, LPX_MAX)
  else lpx_set_obj_dir(p.obj, LPX_MIN)
  ## is it an integer problem?
  if(any(lp$integer))
    lpx_set_class(p.obj, LPX_MIP)
  ## add rows to the problem object
  lpx_add_rows(p.obj, lp$n.const)
  for(i in lp$n.const)
    switch(lp$const.dir[i],
           "default"=stop("'dir' must be either '<', '<=', ..."),
           "<" = lpx_set_row_bnds(p.obj, i, LPX_UP, 0.0, lp$rhs[i]),
           "<="= lpx_set_row_bnds(p.obj, i, LPX_UP, 0.0, lp$rhs[i]),
           ">" = lpx_set_row_bnds(p.obj, i, LPX_LO, lp$rhs[i], 0.0),
           ">=" = lpx_set_row_bnds(p.obj, i, LPX_LO, lp$rhs[i], 0.0),
           "=="=lpx_set_row_bnds(p.obj, i, LPX_FX, lp$rhs[i], lp$rhs[i])
           )
  ## add columns to the problem object
  lpx_add_cols(p.obj, lp$n.obj)
  for(i in 1:lp$n.obj){
    lpx_set_col_bnds(p.obj, i, LPX_LO, 0.0, 0.0)
    lpx_set_obj_coef(p.obj, i, lp$objectives[i])
    if(lp$integer[i])
      lpx_set_col_kind(p.obj, i, LPX_IV)
  }
  ## load the matrix
  lpx_load_matrix(p.obj, length(lp$const.mat.v), lp$const.mat.i, lp$const.mat.j, lp$const.mat.v)

  ## run simplex algorithm
  lpx_simplex(p.obj)
  opt<-lpx_get_obj_val(p.obj)
  obj<-NULL
  ## rc<-NULL
  ## sp<-NULL
  for(i in 1:lp$n.obj){
    obj<-c(obj,lpx_get_col_prim(p.obj,i))
    ## rc<-c(rc,lpx_get_col_dual(lp,i))
  }
  ##for(i in 1:length(b))
  ##  sp<-c(sp,lpx_get_row_dual(lp,i))
  if(any(lp$integer)){
    lpx_integer(p.obj)
    opt<-lpx_mip_obj_val(p.obj)
    obj<-NULL
    ##rc<-NA
    ##sp<-NA
    for(i in 1:lp$n.obj){
    obj<-c(obj,lpx_mip_col_val(p.obj,i))
    }
  }
  lpx_delete_prob(p.obj)
  lp$optimum <- opt
  lp$solution <- obj
  lp
}
