#!/usr/bin/env Rscript
require(RNetCDF)

Calc_SimpleBIO=function(EMIS3D,UPDSMK=T){
  print("Generating BVOC emission...")
  iswater_temp=16
  isice_temp=24
  UPDMETHOD="ADD" # "REP" / "ADD"
  MDSLU=T
  
  MET1=open.nc(paste0(gddir,"METCRO2D"))
  MET2=open.nc(paste0(gddir,"GRIDCRO2D"))
  MET3=open.nc(paste0(gddir,"GRIDDOT2D"))
  SMK1=open.nc(EMIS3D,write = T)

  GSW=var.get.nc(MET1,"GSW")
  T2=var.get.nc(MET1,"TEMP2")
  PA=var.get.nc(MET1,"PRSFC")*0.01
  VG=var.get.nc(MET1,"VEG")
  LATD=var.get.nc(MET2,"LAT")
  LOND=var.get.nc(MET2,"LON")
  COL=length(T2[,1,1])
  ROW=length(T2[1,,1])
  TSP=length(T2[1,1,])
  DX=att.get.nc(MET1,"NC_GLOBAL","XCELL")

  print("CASEINFO:")
  print(paste0("ROW=",ROW,"; COL=",COL,"; DX=",DX,"; TS=",TSP))

  GRID_S=length(LATD[,1])*length(LATD[1,])*DX*DX/1000000 #KM2

  aefiso=array(0,dim=c(25))
  aefmter=array(0,dim=c(25))
  aefovoc=array(0,dim=c(25))
  aef_n=array(0,dim=c(25))
  ixxxlu=array(0,dim=c(25))
  modismap=array(0,dim=c(20))

  city_plant_rate=0.3
  aefiso[1] = 4400*city_plant_rate  
  aefiso[2] = 8.
  aefiso[3] = 8.
  aefiso[4] = 8.
  aefiso[5] = 4.
  aefiso[6] = 2204.
  aefiso[7] = 0.
  aefiso[8] = 0.
  aefiso[9] = 0.
  aefiso[10] = 0.
  aefiso[11] = 4400.
  aefiso[12] = 780.
  aefiso[13] = 4400.
  aefiso[14] = 780.
  aefiso[15] = 5775.
  aefiso[16] = 0.
  aefiso[17] = 0.
  aefiso[18] = 5775.
  aefiso[19] = 0.
  aefiso[20] = 70.
  aefiso[21] = 70.
  aefiso[22] = 70.
  aefiso[23] = 0.
  aefiso[24] = 0.
  aefiso[25] = 0.

  aefmter[1] = 385*city_plant_rate
  aefmter[2] = 20.
  aefmter[3] = 20.
  aefmter[4] = 20.
  aefmter[5] = 20.
  aefmter[6] = 202.5
  aefmter[7] = 20.
  aefmter[8] = 20.
  aefmter[9] = 20.
  aefmter[10] = 0
  aefmter[11] = 385.
  aefmter[12] = 1380.
  aefmter[13] = 385.
  aefmter[14] = 1380.
  aefmter[15] = 1001.
  aefmter[16] = 0.
  aefmter[17] = 0.
  aefmter[18] = 1001.
  aefmter[19] = 0.
  aefmter[20] = 0.
  aefmter[21] = 0.
  aefmter[22] = 0.
  aefmter[23] = 0.
  aefmter[24] = 0.
  aefmter[25] = 0.

  aefovoc[1] = 0.
  aefovoc[2] = 12.
  aefovoc[3] = 12.
  aefovoc[4] = 12.
  aefovoc[5] = 46.
  aefovoc[6] = 363.5
  aefovoc[7] = 80.
  aefovoc[8] = 80.
  aefovoc[9] = 80.
  aefovoc[10] = 0
  aefovoc[11] = 715.
  aefovoc[12] = 840.
  aefovoc[13] = 715.
  aefovoc[14] = 840.
  aefovoc[15] = 924.
  aefovoc[16] = 0.
  aefovoc[17] = 0.
  aefovoc[18] = 924.
  aefovoc[19] = 0.
  aefovoc[20] = 0.
  aefovoc[21] = 0.
  aefovoc[22] = 0.
  aefovoc[23] = 0.
  aefovoc[24] = 0.
  aefovoc[25] = 0.

  aef_n[1] = 0.07*city_plant_rate
  aef_n[2] = 9.
  aef_n[3] = 9.
  aef_n[4] = 9.
  aef_n[5] = 4.95
  aef_n[6] = 4.535
  aef_n[7] = 0.9
  aef_n[8] = 0.07
  aef_n[9] = 0.07
  aef_n[10] = 0.
  aef_n[11] = 0.07
  aef_n[12] = 0.07
  aef_n[13] = 0.07
  aef_n[14] = 0.07
  aef_n[15] = 0.07
  aef_n[16] = 0.
  aef_n[17] = 0.
  aef_n[18] = 0.07
  aef_n[19] = 0.
  aef_n[20] = 0.
  aef_n[21] = 0.
  aef_n[22] = 0.
  aef_n[23] = 0.
  aef_n[24] = 0.
  aef_n[25] = 0.

  ixxxlu[1] = 4
  ixxxlu[2] = 2
  ixxxlu[3] = 2
  ixxxlu[4] = 2
  ixxxlu[5] = 2
  ixxxlu[6] = 4
  ixxxlu[7] = 3
  ixxxlu[8] = 6
  ixxxlu[9] = 3
  ixxxlu[10] = 6
  ixxxlu[11] = 4
  ixxxlu[12] = 5
  ixxxlu[13] = 4
  ixxxlu[14] = 5
  ixxxlu[15] = 5
  ixxxlu[16] = 0
  ixxxlu[17] = 6
  ixxxlu[18] = 4
  ixxxlu[19] = 1
  ixxxlu[20] = 6
  ixxxlu[21] = 4
  ixxxlu[22] = 6
  ixxxlu[23] = 1
  ixxxlu[24] = 0
  ixxxlu[25] = 1

  modismap[1]=14
  modismap[2]=13
  modismap[3]=12
  modismap[4]=11
  modismap[5]=15
  modismap[6]=8
  modismap[7]=9
  modismap[8]=8
  modismap[9]=10
  modismap[10]=7
  modismap[11]=17
  modismap[12]=2
  modismap[13]=1
  modismap[14]=5
  modismap[15]=24
  modismap[16]=19
  modismap[17]=16
  modismap[18]=21
  modismap[19]=22
  modismap[20]=23

  biogen=function(iland,ta,rad,pa,mminlu,vegflag,ludf,dx,vegfrc,bid){
    alpha=0.0027
    cl1=1.066
    r=8.314
    ct1=95000
    ct2=230000
    tm1=314
    ts1=303
    beta=0.09
    cl=1
    ct=1

    if(ixxxlu[iland]==4 || ixxxlu[iland]==5){
      #print(paste0("dtermlxxx:4,5"))
      par = 2.0*rad
      cl = alpha*cl1*par/sqrt(1+alpha*alpha*par*par)
      ct = exp(ct1*(ta-ts1)/(r*ts1*ta))/(1+exp(ct2*(ta-tm1)/(r*ts1*ta)))
      ecf_iso = cl*ct
      ecf_mter = exp(beta*(ta-ts1))
      ecf_ovoc = ecf_mter
      tsoil = 0.84*(ta-273.15) + 3.6
      ecf_n = exp(0.071*tsoil)
    }

    if(ixxxlu[iland]==2){
      #print(paste0("dtermlxxx:2"))
      ecf_iso = exp(0.1*(ta-30.-273.15))
      ecf_mter = ecf_iso
      ecf_ovoc = ecf_iso
      tsoil = 0.72*(ta-273.15) + 5.8
      ecf_n = exp(0.071*tsoil)
    }

    if(ixxxlu[iland]==3 || ixxxlu[iland]==6){
      #print(paste0("dtermlxxx:3,6"))
      ecf_iso = exp(0.1*(ta-30.-273.15))
      ecf_mter = ecf_iso
      ecf_ovoc = ecf_iso
      tsoil = 0.66*(ta-273.15) + 8.8
      ecf_n = exp(0.071*tsoil)
    }

    if(ixxxlu[iland]==1 || iland==iswater_temp || iland==isice_temp){
      #print(paste0("dtermlxxx:1"))
      ecf_iso=0
      ecf_mter = 0.
      ecf_ovoc = 0.
      ecf_n = 0.
    }

    tvgf=vegfrc

    for(j in 1:length(vegfrc[1,1,])){
      tvgf[,,j]=vegfrc[,,j]*ludf
    }

    rat=ta/pa

    coniso = 68.11/60/1E12  #rat*2.3095E-5
    eisoc = aefiso[iland]*ecf_iso
    eiso = coniso*eisoc*tvgf*dx*dx #/68.117/3600 #Mole/s NEW
    conter = 134.3564/120/1E12 #rat*1.1548E-5
    emterc = aefmter[iland]*ecf_mter
    emter = conter*emterc*tvgf*dx*dx #/134.3564/1000/3600
    conovoc = 142/96/1E12 #rat*1.4435E-5 #3-hexenyl-acetate,C8H14O2
    eovocc = aefovoc[iland]*ecf_ovoc
    eovoc = conovoc*eovocc
    vocsc = eisoc + emterc + eovocc
    conn = 30/14/1E15*3600 #rat*3.5633E-4
    e_nn = aef_n[iland]*ecf_n
    e_n = conn*e_nn*tvgf*dx*dx #/30/1000/3600

    if(bid==1){
      return(eiso)
    }

    if(bid==2){
      return(emter)
    }

    if(bid==3){
      return(e_n)
    }

  }

  vegflag=FALSE

  getlu=function(luid){
    if(luid<10){
      LUNAME=paste0("LUFRAC_0",luid)
    }else{
      LUNAME=paste0("LUFRAC_",luid)
    }
    LUDF=var.get.nc(MET2,LUNAME)
    return(LUDF)
  }


  genbiovoc=function(bioid,mdslu){
    #1=ISOP,2=TERP,3=NO
    if(mdslu==FALSE){
      mxlu=24
      i=1
      ild=1
      outdf=biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)

      for( i in 2:mxlu){
        ild=i
        outdf=outdf+biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)
      }
    }else{
      mxlu=20
      i=1
      ild=modismap[1]
      outdf=biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)
      for( i in 2:mxlu){
        ild=modismap[i]
        outdf=outdf+biogen(ild,T2,GSW,PA,mminlu[ild],vegflag,getlu(i),DX,VG,bioid)
      }
    }
    return(outdf)
  }

  CALGRIDSUM=function(GDDF){
    tsumdf=GDDF[,,1]
    for(tis in 2:length(GDDF[1,1,])){
      tsumdf=tsumdf+GDDF[,,tis]
    }
    return(tsumdf)
  }

  ISOPB=genbiovoc(1,MDSLU) # 68.1100
  TERPB=genbiovoc(2,MDSLU) #134.3564
  NOB=genbiovoc(3,MDSLU)   # 30.0000

  EMR=sum(CALGRIDSUM(ISOPB))*12/GRID_S

  rep=data.frame(POL="ISOP",VAL=mean(ISOPB)*length(T2[1,1,])*length(T2[1,,1])*length(T2[,1,1]),UNIT="t")
  rep=rbind(rep,data.frame(POL="TERP",VAL=mean(TERPB)*length(T2[1,1,])*length(T2[1,,1])*length(T2[,1,1]),UNIT="t"))
  rep=rbind(rep,data.frame(POL="NO",VAL=mean(NOB)*length(T2[1,1,])*length(T2[1,,1])*length(T2[,1,1]),UNIT="t"))

  SMKISOP=var.get.nc(SMK1,"ISOP")
  SMKTERP=var.get.nc(SMK1,"TERP")
  SMKNO=var.get.nc(SMK1,"NO")

  SMKISOP[,,1,]=SMKISOP[,,1,]+ISOPB*1E6/68.11/3600 #==> mols/s
  SMKTERP[,,1,]=SMKTERP[,,1,]+TERPB*1E6/134.3564/3600
  SMKNO[,,1,]=SMKNO[,,1,]+NOB*1E6/30/3600

  if (UPDSMK==T){
    var.put.nc(SMK1,variable = "ISOP",data = SMKISOP)
    var.put.nc(SMK1,variable = "TERP",data = SMKTERP)
    var.put.nc(SMK1,variable = "NO",data = SMKNO)
  }

  TISOP=CALGRIDSUM(ISOPB)
  TTERP=CALGRIDSUM(TERPB)
  TNO=CALGRIDSUM(NOB)
      RESDF=data.frame(LAT=as.vector(LATD),LON=as.vector(LOND),ISOP=as.vector(TISOP),TERP=as.vector(TTERP),NO=as.vector(TNO))

  close.nc(MET1)
  close.nc(MET2)
  close.nc(MET3)
  close.nc(SMK1)

  print(rep)
}


gen_vnames=function(unames,subfix="",numc=16){
  namefix=unames
  for(ni in 1:length(unames)){
    tmpun=paste0(unames[ni],subfix)
    namefix[ni]=paste0(tmpun,paste0(rep(" ",16-nchar(tmpun)),collapse = ""))
  }
  return(paste0(namefix,collapse = ""))
}

GenCMAQ=function(GDNAME,BSEMIS,EMISFILE,TLAY){
  GDCRO=open.nc(paste0(gddir,"GRIDCRO2D"))
  MTCRO=open.nc(paste0(gddir,"METCRO3D"))
  MTC2D=open.nc(paste0(gddir,"METCRO2D"))

  PC=var.get.nc(MTC2D,"RN")+var.get.nc(MTC2D,"RC")
  PCFIX=PC*0+1
  PCFIX[PC>0.254]=0

  close.nc(MTC2D)
  DayMon=c(31,28,31,30,31,30,31,31,30,31,30,31)
  TFG=var.get.nc(MTCRO,"TFLAG")
  cmaqtime=TFG[,,1][1,1]
  STDATE=as.Date(paste0(substr(cmaqtime,1,4),"-01-01"))+as.numeric(substr(cmaqtime,5,7))-1
  THOUR=dim.inq.nc(MTCRO,"TSTEP")$length
  close.nc(MTCRO)
  
  STMO=as.numeric(substr(STDATE,6,7)) #Month id
  ROWS=dim.inq.nc(GDCRO,"ROW")$length
  COLS=dim.inq.nc(GDCRO,"COL")$length
  LAYS=TLAY
  TSTP=THOUR
  TVAL=length(VNAMS)
  NEMI=length(BSEMIS)
  NV=length(VARBASEEMI)
  NVCMAQ=length(VNAMS)
  
  print(paste("Grid Info: Start Date:",STDATE,", THour:",THOUR,", TLay:",TLAY,", COL:",COLS,", ROW:",ROWS,"."))

  TEMI=array(0,dim=c(COLS,ROWS,LAYS,TSTP,NV))
  REMI=array(0,dim=c(COLS,ROWS,NEMI,NV))
  for(EMI in 1:length(BSEMIS)){
    BSF=paste0(bsdir,"BASE_",BSEMIS[EMI],".ncf")
    em=open.nc(BSF)
    for(VN in 1:length(VARBASEEMI)){
      REMI[,,EMI,VN]=var.get.nc(em,VARBASEEMI[VN])*SCALEFAC[EMI]
    }
    close.nc(em)
  }
  print("Done.")
  REMM=REMI # A dunplication of raw emis data.
  REMH=array(0,dim=c(COLS,ROWS,TSTP,NEMI,NV))
  for(EMI in 1:length(BSEMIS)){
    REMM[,,EMI,]=REMM[,,EMI,]*TPROM[[BSEMIS[EMI]]][STMO]/DayMon[STMO] # Daily Emission
  }
  
  ttag=1
  dtag=0
  for(TI in 1:TSTP){
    if(ttag>24){
      ttag=1
      dtag=dtag+1
    }
    cday=as.Date(STDATE)+dtag
    cwkd=as.POSIXlt(cday)$wday
    if(cwkd==0) cwkd=7
    for(EMI in 1:length(BSEMIS)){
      REMH[,,TI,EMI,]=REMM[,,EMI,]*TPROH[[BSEMIS[EMI]]][ttag]*TPROW[[BSEMIS[EMI]]][cwkd] # Hourly Emission
    }
    ttag=ttag+1
  }
  ncvarlist=gen_vnames(c(VNAMS,"PMC"))
  #Vertical and speciation.
  EMIS3D=create.nc(EMISFILE,format="offset64")
  
  dim.def.nc(EMIS3D,"TSTEP",TSTP) #0
  dim.def.nc(EMIS3D,"DATE-TIME",2)   #1
  dim.def.nc(EMIS3D,"LAY",LAYS)         #2
  dim.def.nc(EMIS3D,"VAR",as.numeric(NVCMAQ+1))         #3
  dim.def.nc(EMIS3D,"ROW",ROWS)  #4
  dim.def.nc(EMIS3D,"COL",COLS)  #5
  
  att.copy.nc(GDCRO,"NC_GLOBAL","IOAPI_VERSION",EMIS3D,"NC_GLOBAL")
  att.put.nc(EMIS3D,"NC_GLOBAL","EXEC_ID","NC_CHAR","MEIC2CMAQ")
  att.copy.nc(GDCRO,"NC_GLOBAL","FTYPE",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","CDATE",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","CTIME",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","WDATE",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","WTIME",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","SDATE",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","STIME",EMIS3D,"NC_GLOBAL")
  att.put.nc(EMIS3D,"NC_GLOBAL","TSTEP","NC_INT",as.numeric(10000))
  att.copy.nc(GDCRO,"NC_GLOBAL","NTHIK",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","NCOLS",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","NROWS",EMIS3D,"NC_GLOBAL")
  att.put.nc(EMIS3D,"NC_GLOBAL","NLAYS","NC_INT",as.numeric(LAYS))
  att.put.nc(EMIS3D,"NC_GLOBAL","NVARS","NC_INT",as.numeric(NVCMAQ+1))
  att.copy.nc(GDCRO,"NC_GLOBAL","GDTYP",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","P_ALP",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","P_BET",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","P_GAM",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","XCENT",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","YCENT",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","XORIG",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","YORIG",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","XCELL",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","YCELL",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","VGTYP",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","VGTOP",EMIS3D,"NC_GLOBAL")
  att.put.nc(EMIS3D,"NC_GLOBAL","VGLVLS","NC_FLOAT",c(MCIPLAYS[1:(LAYS+1)]))
  att.copy.nc(GDCRO,"NC_GLOBAL","GDNAM",EMIS3D,"NC_GLOBAL")
  att.copy.nc(GDCRO,"NC_GLOBAL","UPNAM",EMIS3D,"NC_GLOBAL")
  att.put.nc(EMIS3D,"NC_GLOBAL","HISTORY","NC_CHAR","                      ")
  att.put.nc(EMIS3D,"NC_GLOBAL","VAR-LIST","NC_CHAR",ncvarlist)
  att.put.nc(EMIS3D,"NC_GLOBAL","FILEDESC","NC_CHAR","CMAQ Emission File by MEIC2CMAQ.")
  
  #Generating TFLAG, not finished
  TFG=array(0,dim=c(2,as.numeric(NVCMAQ+1),TSTP))
  ttag=0
  dtag=1
  for(TI in 1:TSTP){
    if(ttag>23){
      ttag=0
      dtag=dtag+1
    }
    cday=as.Date(STDATE)+dtag
    cdai=as.POSIXlt(cday)$yday
    cjul=paste0(substr(cday,1,4),paste0(rep("0",3-nchar(cdai)),collapse = ""),cdai)
    ctim=as.integer(ttag*10000)
    ttag=ttag+1
    TFG[1,,TI]=as.integer(cjul)
    TFG[2,,TI]=as.integer(ctim)
  }
  
  var.def.nc(EMIS3D,"TFLAG","NC_INT",c(1,3,0))
  att.put.nc(EMIS3D,"TFLAG","long_name","NC_CHAR","TFLAG")
  att.put.nc(EMIS3D,"TFLAG","units","NC_CHAR","<YYYYDDD,HHMMSS>")
  att.put.nc(EMIS3D,"TFLAG","var_desc","NC_CHAR","Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS")
  var.put.nc(EMIS3D,"TFLAG",TFG)
  #Process VOCs
  VN=1
  for(SPC in 1:length(VOCN$VNAM)){
    tmpval=array(0,dim=c(COLS,ROWS,LAYS,TSTP))
    for(EMI in 1:length(BSEMIS)){
      if(substr(BSEMIS[EMI],1,3)=="IND"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_IND[LY]*VPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
        }
      }else if(substr(BSEMIS[EMI],1,3)=="POW"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_POW[LY]*VPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
        }
      }else{
        tmpval[,,1,]=tmpval[,,1,]+REMH[,,,EMI,VN]*VPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
      }
    }
    #merged iVOC
    ncvar=as.vector(VOCN$VNAM[SPC])
    print(paste0("Generating ",ncvar," ..."))
    molmas=as.vector(VOCN$MMAS[SPC])
    var.def.nc(EMIS3D,ncvar,"NC_FLOAT",c(5,4,2,0))
    att.put.nc(EMIS3D,ncvar,"long_name","NC_CHAR",ncvar)
    att.put.nc(EMIS3D,ncvar,"units","NC_CHAR","moles/s         ")
    att.put.nc(EMIS3D,ncvar,"var_desc","NC_CHAR",paste0("Model species ",ncvar))
    tmpval=tmpval*1000*1000/molmas/3600 #mol/s
    var.put.nc(EMIS3D,ncvar,tmpval)
  }
  #Process NOx
  VN=2
  for(SPC in 1:length(NOXN$VNAM)){
    tmpval=array(0,dim=c(COLS,ROWS,LAYS,TSTP))
    for(EMI in 1:length(BSEMIS)){
      if(substr(BSEMIS[EMI],1,3)=="IND"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_IND[LY]*NPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
        }
      }else if(substr(BSEMIS[EMI],1,3)=="POW"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_POW[LY]*NPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
        }
      }else{
        tmpval[,,1,]=tmpval[,,1,]+REMH[,,,EMI,VN]*NPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
      }
    }
    #merged iVOC
    ncvar=as.vector(NOXN$VNAM[SPC])
    print(paste0("Generating ",ncvar," ..."))
    molmas=as.vector(NOXN$MMAS[SPC])
    var.def.nc(EMIS3D,ncvar,"NC_FLOAT",c(5,4,2,0))
    att.put.nc(EMIS3D,ncvar,"long_name","NC_CHAR",ncvar)
    att.put.nc(EMIS3D,ncvar,"units","NC_CHAR","moles/s         ")
    att.put.nc(EMIS3D,ncvar,"var_desc","NC_CHAR",paste0("Model species ",ncvar))
    tmpval=tmpval*1000*1000/molmas/3600 #mol/s
    var.put.nc(EMIS3D,ncvar,tmpval)
  }
  #Process OTHER GAS
  for(VN in c(3,4,7)){
    if(VN==3) SPC=1
    if(VN==4) SPC=2
    if(VN==7) SPC=3
    
    
    tmpval=array(0,dim=c(COLS,ROWS,LAYS,TSTP))
    for(EMI in 1:length(BSEMIS)){
      if(substr(BSEMIS[EMI],1,3)=="IND"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_IND[LY] # 4D Emission
        }
      }else if(substr(BSEMIS[EMI],1,3)=="POW"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_POW[LY] # 4D Emission
        }
      }else{
        tmpval[,,1,]=tmpval[,,1,]+REMH[,,,EMI,VN] # 4D Emission
      }
    }
    #merged iVOC
    ncvar=as.vector(OTHN$VNAM[SPC])
    print(paste0("Generating ",ncvar," ..."))
    molmas=as.vector(OTHN$MMAS[SPC])
    var.def.nc(EMIS3D,ncvar,"NC_FLOAT",c(5,4,2,0))
    att.put.nc(EMIS3D,ncvar,"long_name","NC_CHAR",ncvar)
    att.put.nc(EMIS3D,ncvar,"units","NC_CHAR","moles/s         ")
    att.put.nc(EMIS3D,ncvar,"var_desc","NC_CHAR",paste0("Model species ",ncvar))
    tmpval=tmpval*1000*1000/molmas/3600 #mol/s
    var.put.nc(EMIS3D,ncvar,tmpval)
    
  }
  
  #Process PMFine
  VN=6
  for(SPC in 1:length(PMFN$VNAM)){
    tmpval=array(0,dim=c(COLS,ROWS,LAYS,TSTP))
    for(EMI in 1:length(BSEMIS)){
      if(substr(BSEMIS[EMI],1,3)=="IND"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_IND[LY]*PPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
        }
      }else if(substr(BSEMIS[EMI],1,3)=="POW"){
        for(LY in 1:4){
          tmpval[,,LY,]=tmpval[,,LY,]+REMH[,,,EMI,VN]*LF_POW[LY]*PPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
        }
      }else{
      if(substr(BSEMIS[EMI],1,2)=="FD" & FugiPC){
        tmpval[,,1,]=tmpval[,,1,]+REMH[,,,EMI,VN]*PPRO[[BSEMIS[EMI]]][SPC]*PCFIX # 4D Emission
      }else{
        tmpval[,,1,]=tmpval[,,1,]+REMH[,,,EMI,VN]*PPRO[[BSEMIS[EMI]]][SPC] # 4D Emission
      }
        
      }

    }
    ncvar=as.vector(PMFN$VNAM[SPC])
    print(paste0("Generating ",ncvar," ..."))
    var.def.nc(EMIS3D,ncvar,"NC_FLOAT",c(5,4,2,0))
    att.put.nc(EMIS3D,ncvar,"long_name","NC_CHAR",ncvar)
    att.put.nc(EMIS3D,ncvar,"units","NC_CHAR","g/s            ")
    att.put.nc(EMIS3D,ncvar,"var_desc","NC_CHAR",paste0("Model species ",ncvar))
    tmpval=tmpval*1000*1000/3600 #g/s
    var.put.nc(EMIS3D,ncvar,tmpval)
  }
  
  #Process PMC
  tmpval=array(0,dim=c(COLS,ROWS,LAYS,TSTP))
  for(EMI in 1:length(BSEMIS)){
    if(substr(BSEMIS[EMI],1,3)=="IND"){
      for(LY in 1:4){
        tmpval[,,LY,]=tmpval[,,LY,]+(REMH[,,,EMI,5]*LF_IND[LY]-REMH[,,,EMI,6]*LF_IND[LY])
      }
    }else if(substr(BSEMIS[EMI],1,3)=="POW"){
      for(LY in 1:4){
        tmpval[,,LY,]=tmpval[,,LY,]+(REMH[,,,EMI,5]*LF_POW[LY]-REMH[,,,EMI,6]*LF_POW[LY])
      }
    }else{
      if(substr(BSEMIS[EMI],1,2)=="FD" & FugiPC){
        tmpval[,,1,]=tmpval[,,1,]+(REMH[,,,EMI,5]-REMH[,,,EMI,6])*PCFIX # 4D Emission
      }else{
        tmpval[,,1,]=tmpval[,,1,]+(REMH[,,,EMI,5]-REMH[,,,EMI,6])
      }
    }
  }
  ncvar="PMC"
  print(paste0("Generating ",ncvar," ..."))
  var.def.nc(EMIS3D,ncvar,"NC_FLOAT",c(5,4,2,0))
  att.put.nc(EMIS3D,ncvar,"long_name","NC_CHAR",ncvar)
  att.put.nc(EMIS3D,ncvar,"units","NC_CHAR","g/s            ")
  att.put.nc(EMIS3D,ncvar,"var_desc","NC_CHAR",paste0("Model species ",ncvar))
  tmpval=tmpval*1000*1000/3600 #g/s
  var.put.nc(EMIS3D,ncvar,tmpval)
  
  close.nc(GDCRO)
  close.nc(EMIS3D)
  print("Done.")
}


#########################################################################
print("XEMIS v1.0")
print("==============================")
print("xemis is a tool to generate cmaq ready emission file based on MEIC by lcw from cdaes.")
print("some codes are copied from my project simplebio to generate biogenic vocs emission.")
print("modifications can be made so original voc speciate information of MEIC can be used.")
print("the program is open source, so use at your risk.")
print("contact lcw@cdaes.cn if necessary.")
print("==============================")
GENBIO=F
FugiPC=T
Args <- commandArgs()

if(length(Args)>=7){
  MRDIR=Args[6]
  if(nchar(MRDIR)!=18){
    print("Wrong casename!")
    quit()
  }
  GDNAME=substr(MRDIR,16,18)
  GB=Args[7]
  if(GB=="T") GENBIO=T
}else{
  print("Useage: xemis casename genbio.")
  quit()
}


gddir=paste0("/home/lcw/MODELS/OUTPUT/cmaq/",MRDIR,"/mcip/")
bsdir=paste0("/home/lcw/MODELS/xemis/emis/",GDNAME,"/")
VARBASEEMI=c("VOC","NOx","CO","SO2","PM10","PM2p5","NH3")
MCIPLAYS=c(1.0000,0.9974,0.9940,0.9890,0.9820,0.9720,0.9590,0.9430,0.9230,0.8990,0.8710,0.8390,0.7630,0.6680,0.5180,0.3680,0.2180,0.1230,0.0000)

VOCN=data.frame(VNAM=c("ALDX","CH4"  ,"ETH"  ,"ETHA","ETOH" ,"FORM" ,"IOLE" ,"ISOP","ALD2","MEOH" ,"NVOL" ,"OLE"  ,"PAR"  ,"TERP"  ,"TOL"  ,"UNR"  ,"XYL"   ,"BENZENE"),
                MMAS=c(46.752,16.0425,33.1419,30.069,45.5934,29.8945,55.3036,68.117,46.752,31.9666,15.9125,32.2818,16.8563,134.3564,97.6694,25.8865,108.9851,78.1118))

LF_IND=c(0.7,0.2,0.1,0  )
LF_POW=c(0.2,0.4,0.3,0.1)

VPRO=data.frame(
  AGR=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  IND=c(0,0,0.044,0.021,0,0.002,0.011,0,0,0,0,0.036,0.447,0,0.029,0.394,0.016,0),
  POW=c(0,0,0.044,0.021,0,0.002,0.011,0,0,0,0,0.036,0.447,0,0.029,0.394,0.016,0),
  RES=c(0,0,0.178,0.069,0,0,0,0,0,0,0,0.044,0.237,0,0.015,0.453,0.004,0),
  TRA=c(0,0,0.0342,0.0338,0,0.0042,0.0096,0.0018,0.0024,0,0,0.0114,0.6642,0,0.0276,0.176,0.0348,0),
  FDL=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  FDR=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

PMFN=data.frame(VNAM=c("PEC","PNO3","POC","PSO4","PMFINE","PH2O","PCL","PNCOM","PCA","PSI","PMG","PMN","PNA","PNH4","PAL","PFE","PTI","PK","PMOTHR"))
PPRO=data.frame(
AGR=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
IND=c(0.0206,0.0206,0.622,0.015,0.3218,0.0001,0.0034,0.2488,0.0127,0.0049,0.0019,0.0001,0.0021,0.015,0.0035,0.005,0.0006,0.003,0.0206),
POW=c(0.0019,0.0185,0.0233,0.0811,0.8752,0.0001,0.0337,0.0093,0.3709,0.0692,0.0251,0.0007,0.0091,0.0176,0.1196,0.0424,0.0035,0.0122,0.1618),
RES=c(0.0206,0.0206,0.622,0.015,0.3218,0.0001,0.0034,0.2488,0.0127,0.0049,0.0019,0.0001,0.0021,0.015,0.0035,0.005,0.0006,0.003,0.0206),
TRA=c(0.3365,0.0044,0.4088,0.0427,0.2076,0.0001,0,0.1635,0.0066,0.0077,0.0024,0.0002,0.0033,0,0.0029,0.0131,0.0011,0.0025,0.0042),
FDL=c(0.0163,0.0007,0.0137,0.0053,0.964,0.0001,0,0.0055,0.093,0.568,0.0132,0.001,0.0276,0,0.1702,0.0536,0.0047,0.0263,0.0008),
FDR=c(0.0163,0.0007,0.0137,0.0053,0.964,0.0001,0,0.0055,0.093,0.568,0.0132,0.001,0.0276,0,0.1702,0.0536,0.0047,0.0263,0.0008)
)

NOXN=data.frame(VNAM=c("NO","NO2","HONO"),
                MMAS=c(30,  46,   47))
NPRO=data.frame(
  AGR=c(0,0,0),
  IND=c(0.90,0.082,0.008),
  POW=c(0.90,0.082,0.008),
  RES=c(0.90,0.082,0.008),
  TRA=c(0.85,0.082,0.058),
  FDL=c(0,0,0),
  FDR=c(0,0,0)
)

OTHN=data.frame(VNAM=c("CO","SO2","NH3"),
                MMAS=c(28,64,17))

VNAMS=c(as.vector(VOCN$VNAM),as.vector(PMFN$VNAM),as.vector(NOXN$VNAM),as.vector(OTHN$VNAM))

TPROM=data.frame(
  AGR=c(83,83,83,83,83,83,83,83,83,83,83,83)/1000,
  IND=c(87,67,80,80,81,85,81,82,88,89,89,92)/1000,
  POW=c(73,60,81,81,80,73,103,121,88,82,79,78)/1000,
  RES=c(216,111,70,58,62,60,43,27,41,49,91,166)/1000,
  TRA=c(82,81,77,73,86,89,94,94,86,73,77,81)/1000,
  FDL=c(216,111,70,58,62,60,43,27,41,49,91,166)/1000,
  FDR=c(216,111,70,58,62,60,43,27,41,49,91,166)/1000
)

TPROW=data.frame(
  AGR=c(143,143,143,143,143,143,143)/1000,
  IND=c(148,135,125,137,154,154,146)/1000,
  POW=c(143,143,143,143,143,143,143)/1000,
  RES=c(143,143,143,143,143,143,143)/1000,
  TRA=c(148,135,125,137,154,154,146)/1000,
  FDL=c(143,143,143,143,143,143,143)/1000,
  FDR=c(148,135,125,137,154,154,146)/1000
)

TPROW=TPROW/colMeans(TPROW)

TPROH=data.frame(
  AGR=c(31,34,38,42,45,47,49,50,52,52,51,49,47,45,43,41,39,38,37,36,35,34,33,32)/1000,
  IND=c(77,89,98,89,77,89,89,89,89,89,44,24,24,12,12,0,0,0,0,0,0,0,0,0)/1000,
  POW=c(42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42)/1000,
  RES=c(44,34,14,68,82,55,27,14,20,41,89,123,96,68,41,27,20,20,14,7,7,20,27,41)/1000,
  TRA=c(97,86,67,55,43,44,51,53,57,72,85,42,40,35,26,16,9,6,4,4,11,11,19,54)/1000,
  FDL=c(31,34,38,42,45,47,49,50,52,52,51,49,47,45,43,41,39,38,37,36,35,34,33,32)/1000,
  FDR=c(60,64,65,71,70,78,86,85,49,35,31,28,21,15,12,10,18,16,14,17,22,34,36,50)/1000
)


SCALEFAC=c(1.0,1.0,1.0,1.0,1.0,1.0,1.0)
BASEEMIS=c("AGR","IND","POW","RES","TRA","FDL","FDR")

suppressWarnings({dir.create(paste0("/home/lcw/MODELS/OUTPUT/cmaq/",MRDIR,"/emis"),recursive=T)})
GenCMAQ(GDNAME,BSEMIS = BASEEMIS,TLAY=15,EMISFILE = paste0("/home/lcw/MODELS/OUTPUT/cmaq/",MRDIR,"/emis/emis.ncf"),SCALEFAC)

if(GENBIO) Calc_SimpleBIO(EMIS3D=paste0("/home/lcw/MODELS/OUTPUT/cmaq/",MRDIR,"/emis/emis.ncf"))

print("xemis done.")
print("==============================")
