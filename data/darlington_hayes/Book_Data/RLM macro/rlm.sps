
/* RLM for SPSS version 1.01 */.
/* Written by Andrew F. Hayes */.
/* Department of Psychology */.
/* The Ohio State University */.
/* afhayes@gmail.com */. 
/* Copyright 2017 */.
/* See Darlington and Hayes (2017) */.
/* Regression Analysis and Linear Models */.
/* for documentation http://www.guilford.com/p/darlington */.
/* This code should not be posted online except through afhayes.com */.
/* without written permission. Commercial, for-profit distribution is not authorized */.

/* Permission is hereby granted, free of charge, to any person obtaining a copy of this software */.
/* and associated documentation files (the "Software"), to use the software in this form.  Distribution */.
/* after modification is prohibited, as is its use for any commercial purpose without authorization */.  
/* This software should not be posted or stored on any webpage, server, or directory accessible to */.
/* the public whether free or for a charge unless written permission has been granted by the copyright */.
/* holder.  The copyright holder requests that this software be distributed by directing users to */.
/* afhayes.com where the latest release of the software and documentation is archived and */.
/* can be downloaded.  Permission is granted to install this software in university computing labs for */.
/* noncommercial/nonprofit use */.

/* THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, */
/* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF */.
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT */.
/* IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, */.
/*  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT */.
/* OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE */.
/* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE */.

/* The above text should be included in any distribution of the software */.

set printback=off.

DEFINE descrip (descdat=!tokens(1)).
compute nnnn=nrow(!descdat).
compute desc1 = (nrow(!descdat)*sscp(!descdat))-(t(csum(!descdat))*(csum(!descdat))).
compute desc1 = desc1/(nrow(!descdat)*(nrow(!descdat)-1)).
compute desc2=diag(desc1).
compute desc3 = sqrt(desc2).
compute desc4 = csum(!descdat)/nrow(!descdat).
compute desc5 = desc1&/(desc3*t(desc3)). 
!ENDDEFINE.

DEFINE domin (dx=!charend('/')/dy=!charend('/')).
compute iv5={!dx,!dy}.
compute numbx=ncol(iv5)-1.
descrip descdat=iv5.
compute desc5x=desc5(1:numbx,1:numbx).
compute dommat=make(numbx,numbx,0).
compute rsqrmat = make(((2&**numbx)-1),numbx+1,0).
compute allcomp=(2&**(numbx-2)).
compute domx = make(1,numbx,0).
loop j = 1 to ((2&**numbx)-1).
  compute domx(1,1)=domx(1,1)+1.
  loop i = 1 to numbx.
    do if domx(1,i) = 2.
      compute domx(1,i)=0.
      compute domx(1,i+1)=domx(1,i+1)+1.
    end if.
  end loop.
compute rii=(t(domx)*domx)&*desc5x.
compute bin=make(1,numbx,0).
loop k = 1 to numbx.
   compute rii(k,k)=1.
   compute bin(1,k)=2&**(k-1).
end loop.
compute riy=desc5((numbx+1),1:numbx)&*domx.
compute rsqrmat(j,numbx+1)=sqrt((riy*inv(rii)*t(riy))).
compute rsqrmat(j,1:numbx)=domx.
end loop.
do if (subsets = 1).
  compute prednam={nms(2:(nrow(nms)-1),1);"R"}.
  print/title = "***************************************************************************".
  compute temp = rsqrmat.  
  compute temp(GRADE(rsqrmat(:,(ncol(rsqrmat)))),:) = rsqrmat.
  compute rsqrmats = temp. 
  print rsqrmats/cnames = prednam/format=F6.4/title = "All subsets regression results".
end if.
do if (dominate = 1).
loop i = 1 to (numbx-1).
   loop j = (i+1) to numbx.
   compute critdiff=bin(1,j)-bin(1,i).
     loop k = 1 to nrow(rsqrmat).
       do if (rsqrmat(k,i)=1 and rsqrmat(k,j)=0).
         compute tmps={rsqrmat(k,(numbx+1)),rsqrmat((k+critdiff),(numbx+1))}.
         do if (tmps(1,1) > tmps(1,2)).
           compute dommat(i,j)=dommat(i,j)+1.
         end  if.
         do if (tmps(1,1) < tmps(1,2)).
           compute dommat(j,i)=dommat(j,i)+1.
         end if.
      end if.
     end loop.
   end loop.
end loop.
compute prednam=nms(2:(nrow(nms)-1),1).
compute dommat=dommat/allcomp.
    print/title = "***************************************************************************".
print dommat/title "Dominance matrix"/cnames=prednam/rnames=prednam/format = F4.3.
end if.
!ENDDEFINE.

DEFINE RLM (y = !charend ('/')/x = !charend ('/')/modval = !charend ('/') !default (9999)/jn = !charend ('/') !default (0)/conf = !charend ('/') !default (95)
  /center = !charend('/') !default (0)/est = !charend ('/') !default (0)/change = !charend ('/') !default(1)/mcfoc=!charend('/') !default(0)/mcmod = !charend('/') !default(0)
  /mod=!charend('/') !default(0)/plot=!charend('/') !default(0)/decimals = !charend('/') !default(F10.4)/settest=!charend('/') !default(0)
  /ptiles=!charend('/') !default(0)/hc=!charend('/') !default(5)/mcx=!charend('/') !default(0)/zpp=!charend('/') !default(1)/stand=!charend('/') !default(0)
  /crossv=!charend('/') !default(0)/longname=!charend('/') !default(0)/covcoeff = !charend('/') !default(0)/dominate = !charend('/') !default(0)
  /subsets=!charend('/') !default(0)/diagnose=!charend('/') !default(0)/contrast=!charend('/') !default("-999")/spline=!charend('/') !default("-999")).
PRESERVE.
set mxloop = 100000000.
set printback = off.
matrix.
get dd/file = */variables = !y !x/names = vnames/missing = 99111999.
compute ninit = nrow(dd). 
compute spl={!spline}.
compute nspl=ncol(spl).
do if (spl(1,1) = -999).
  compute nspl=0.
end if.
compute hypmat={!contrast}.
compute longname=trunc(!longname).
compute nhypmat=ncol(hypmat).
do if (hypmat(1,1) = -999).
  compute nhypmat=0.
end if.
compute rownum=make(ninit,1,0).
compute rowmiss=make(ninit,1,0).
compute good=1.
compute bad=1.
loop i = 1 to ninit.
  compute missv=0.
  loop j = 1 to ncol(dd).
    do if (dd(i,j)=99111999).
      compute missv=missv+1.
    end if.
  end loop.
  do if (missv = 0).
    compute rownum(good,1)=i.
    compute good=good+1.
  end if.
  do if (missv > 0).
    compute rowmiss(bad,1)=i.
    compute bad=bad+1.
  end if.
end loop.
do if (good > 1).
   compute rownum=rownum(1:(good-1),1).
end if.
do if (bad > 1).
   compute rowmiss=rowmiss(1:(bad-1),1).
end if.
compute hc3=0.
get dd/variables = !y !x/names = nm/MISSING = OMIT.
get x/variables = !x/names=tpx/missing=omit.
get tempy/variables = !y/names=tpy/missing=omit.
compute n = nrow(dd).
compute note = make(10,1,0).
compute notes = 1.
compute itprob=0.
compute conf=!conf.
compute settest = abs(trunc(!settest)).
compute p0=-.322232431088.
compute p1 = -1.
compute p2 = -.342242088547.
compute p3 = -.0204231210245.
compute p4 = -.0000453642210148.
compute q0 = .0993484626060.
compute q1 = .588581570495.
compute q2 = .531103462366.
compute q3 = .103537752850.
compute q4 = .0038560700634.
do if (trunc(conf) >= 100 or (trunc(conf) <= 50)).
  compute conf = 95.
  compute note(notes,1) = 8.
  compute notes = notes + 1.
end if.
compute alpha2 = (1-(conf/100))/2.
compute cilm=alpha2*2.
compute y5=sqrt(-2*ln(alpha2)).
compute xp2=(y5+((((y5*p4+p3)*y5+p2)*y5+p1)*y5+p0)/((((y5*q4+q3)*y5+q2)*y5+q1)*y5+q0)).
do if (n < ninit).
  compute nmiss = ninit-n.
  compute note(notes,1) = 1.
  compute notes = notes + 1.
end if.
compute nx=1.
compute criterr=0.
compute errs=0.
compute errsm=make(10,1,0).
compute nyv=ncol(tpy).
compute nxv=ncol(x).
compute savplot=(!plot = 1).
compute ptiles=(!ptiles=1).
compute stand=(!stand=1).
compute covcoeff=(!covcoeff=1).
compute nomod=(!mod=0).
compute mcx=trunc(!mcx).
compute hc=trunc(!hc).
do if (longname=0).
  !let !toomany=0.
  !do !i !in (!x).
    !do !j = 1 !to !length(!i).
      !if ((!j > 8) !and (!toomany = 0)) !then.
        compute criterr = 1.
        compute errs=errs+1.
        compute errsm(errs,1)=13.
        !let !toomany = 1.
      !ifend.
    !doend.
  !doend.
end if.
do if (longname=0 and criterr=0).
  !let !toomany=0.
  !do !i !in (!y).
    !do !j = 1 !to !length(!i).
      !if ((!j > 8) !and (!toomany = 0)) !then.
        compute criterr = 1.
        compute errs=errs+1.
        compute errsm(errs,1)=13.
        !let !toomany = 1.
      !ifend.
    !doend.
  !doend.
end if.
compute diagnose=(!diagnose=1).
do if (hc >= 0 and hc < 5).
  compute note(notes,1) = 5.
  compute notes = notes + 1.
end if.
do if (hc > 5 or hc < 0).
  compute hc=5.
end if.
compute hclab={"se(HC0)","se(HC1)","se(HC2)","se(HC3)","se(HC4)","se"}.
compute hcflab={"F(HC0)","F(HC1)","F(HC2)","F(HC3)","F(HC4)","F"}.
compute hclab=hclab(1,(hc+1)).
compute hcflab=hcflab(1,(hc+1)).
compute zpp=(!zpp=1).
compute subsets=(!subsets=1).
compute dominate=(!dominate=1).
compute crossv=(!crossv = 1).
do if (nyv <> 1).
  compute criterr=1.
  compute errs=errs+1.
  compute errsm(errs,1)=1.
end if.
do if (good < 2).
  compute criterr=1.
  compute errs=errs+1.
  compute errsm(errs,1)=8.
end if.
compute desc1 = (nrow(dd)*sscp(dd))-(t(csum(dd))*(csum(dd))).
compute desc1 = desc1/(nrow(dd)*(nrow(dd)-1)).
compute desc2=csum(diag(desc1) = 0).
do if (desc2 > 0).
  compute criterr=1.
  compute errs=errs+1.
  compute errsm(errs,1)=5.
end if.
compute ddd = {"D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9"}.
compute ddd1 = {"int_1", "int_2", "int_3", "int_4", "int_5", "int_6", "int_7", "int_8", "int_9"}.
compute dumok = 0.
compute mcfoc=trunc(!mcfoc).
compute mcmod=trunc(!mcmod).
do if (mcfoc < 0).
  compute mcfoc=0.
end if.
do if (mcfoc > 6).
  compute mcfoc=0.
end if.
do if (mcmod < 0).
  compute mcmod=0.
end if.
do if (mcmod > 6).
  compute mcmod=0.
end if.
do if (mcmod > 0 or mcfoc > 0).
  do if (mcx <> 0).
    compute note(notes,1) = 6.
    compute notes = notes + 1.
  end if.
  compute mcx=0.
  compute nomod=0.
end if.
do if (mcmod=0 and mcx > 0).
  compute mcmod=mcx.
  do if (mcmod < 0).
    compute mcmod=0.
  end if.
  do if (mcmod > 6).
    compute mcmod=0.
  end if.
end if.
do if (mcmod > 0 and mcfoc>0).
  compute errs=errs+1.
  compute errsm(errs,1)=3.
  compute criterr=1.
end if.
compute con = make(n,1,1).
do if (nxv=1).
  compute mcfoc=0.
  compute nomod=1.
end if.
compute mcloc=(mcfoc>0).
do if (mcfoc > 0 or mcmod > 0 or mcx > 0 or nomod = 0).
  do if (dominate=1).
    compute note(notes,1) = 2.
    compute notes = notes + 1.
    compute dominate=0.
  end if.
  do if  (subsets=1).
    compute note(notes,1) = 3.
    compute notes = notes + 1.
    compute subsets=0.
  end if.
end if.
do if ((dominate = 1 or subsets = 1) and (nxv > 15 or nxv < 2) and (nspl = 0)).
  compute dominate=0.
  compute subsets= 0.
    compute note(notes,1) = 7.
    compute notes = notes + 1.
end  if.
compute ncovs=ncol(x)-2+(nxv=1).
do if (nxv < 2 and (!mod <> 0)).
  compute criterr=1.
  compute errs=errs+1.
  compute errsm(errs,1)=2.
end if.
do if (nspl > 0).
  compute spld3=make(n,nspl,0).
  compute spld2=make(n,nspl,0).
  compute xsplmin=cmin(dd(:,ncol(dd))).
  compute mcheck=0.
  compute ocheck=0.
  loop i = 1 to nspl.
    compute spld3(:,i)=(dd(:,ncol(dd)) > spl(1,i)).
    compute spld2(:,i)=dd(:,ncol(dd)) - spl(1,i).
    compute spld=spld3&*spld2.
    compute mcheck=mcheck+(spl(1,i)=xsplmin).
    do if (i > 1).
       do if (spl(1,i) <= spl(1,(i-1))).
         compute ocheck=1.
       end if.
    end if.
  end loop.
  compute spld4=csum(spld3).
  compute fff=rsum((csum(spld3) < 2) > 0).
  do if (ocheck=0).
    loop i = 1 to nspl.
      do if (i > 1).
        do if (spld4(1,i) >= ((spld4(1,i-1))-1)).
          compute fff=1.
        end if.
      end if.
    end  loop.
  end if.
  do if (fff > 0).
    compute errs=errs+1.
    compute errsm(errs,1)=9.
    compute criterr=1.
  end if.
  do if (mcheck > 0).
    compute errs=errs+1.
    compute errsm(errs,1)=10.
    compute criterr=1.
  end if.
  do if (ocheck > 0).
    compute errs=errs+1.
    compute errsm(errs,1)=11.
    compute criterr=1.
  end if.
  do if (nspl > 10).
    compute errs=errs+1.
    compute errsm(errs,1)=12.
    compute criterr=1.
  end if.
  release spld2, spld3.
  compute dd={dd,spld}.
  compute xnmspl=nm(1,ncol(nm)).
  do if (nspl < 11).
    compute jlab={"Joint1","Joint2","Joint3","Joint4","Joint5","Joint6","Joint7","Joint8","Joint9","Joint10"}.
    compute jlab=jlab(1,1:nspl).
    compute nm={nm,jlab}.
    compute nmspl=jlab.
    compute tpx={tpx,jlab}.
    compute nxv=nxv+nspl.
    compute settest=nspl+1.
  end if.
  compute mcfoc=0.
  compute mcmod=0.
  compute nomod=1.
  compute dominate=0.
  compute subsets=0.
end if.
do if (mcfoc > 0 or mcmod > 0).
  compute temp = dd.
  compute temp2 =rownum.
  compute temp(GRADE(dd(:,(ncol(dd)-mcloc))),:) = dd.
  compute temp2(GRADE(dd(:,(ncol(dd)-mcloc))),:) = rownum.
  compute dd = temp.
  compute rownum=temp2.
  release temp.
  release temp2.
  compute dummy = design(dd(:,ncol(dd)-mcloc)).
  compute nvls = ncol(dummy).
  compute nnvls = csum(dummy).
  compute nnsum=make(1,ncol(nnvls),0).
  loop i = 1 to ncol(nnvls).
     compute nnsum(1,i)=rsum(nnvls(1,i:ncol(nnvls))).
  end loop.
  compute nnvls2=nnvls.
  compute toosmall=rsum(nnvls < 2).
  compute mnvls = cmin(t(nnvls)).
  do if (rsum(nnvls < 2)) > 0).
    compute criterr = 1.
    compute errs=errs+1.
    compute errsm(errs,1)=6.
  end if.
  do if (nvls > 10).
    compute criterr = 1.
    compute errs=errs+1.
    compute errsm(errs,1)=4.
  end if.
  do if (criterr=0).
    compute dumok = 1.
    compute nnvls=make(nvls,1,0).
    compute nnvls(1,1)=dd(1,ncol(dd)-mcloc).
    compute temp = 2.
    loop i = 2 to n.
      do if (dd(i,ncol(dd)-mcloc) <> nnvls((temp-1),1)).
        compute nnvls(temp,1)=dd(i,ncol(dd)-mcloc).
        compute temp = temp+1.
      end if.
    end loop.
    compute dummy3=dummy.
    compute dummy=dummy(:,1:(ncol(dummy)-1)).
    compute dummy2 = dummy3(:,2:ncol(dummy3)).
    release dummy3.
    do if (mcfoc=4 or mcmod=4 or mcfoc=6 or mcmod=6).
      compute minus1=make(1,ncol(dummy),-1).
      do if (mcfoc=6 or mcmod=6).
        compute minus1=minus1&*(nnvls2(1,1:(ncol(nnvls2)-1))/nnvls2(1,ncol(nnvls2))).
      end if.
      release dummy2.
      loop k = 1 to n.
        do if (rsum(dummy(k,:)) = 0).
          compute dummy(k,:)=minus1.
        end if.
      end loop.
    end if.
    do if (mcfoc = 5 or mcmod = 5).
      compute conmat1=ident(nvls)*-1.
      loop i = 1 to nvls-1.
        loop j = 2+(i-1) to nvls.
         compute conmat1(i,j)=nnvls2(1,j)/nnsum(1,(i+1)).
        end loop.
     end loop.
     compute conmat1=conmat1(1:(nrow(conmat1)-1),:).
     compute conmat1=(t(conmat1)*inv(conmat1*t(conmat1))).
    end if.
    do if (mcfoc = 2 or mcfoc = 3 or mcfoc = 5) or (mcmod = 2 or mcmod = 3 or mcmod = 5)).
      compute dummy=dummy2.
      loop k = 1 to n.
        do if (rsum(dummy(k,:)) > 0).
          loop i = 1 to ncol(dummy).
            do if (dummy(k,i) = 0).
              compute dummy(k,i) = 1.
            else.
              break.
            end if.
          end loop.
        end if.
      end loop.
      release dummy2.
      do if (mcfoc = 3 or mcmod=3).
        compute conmat1={-8,1,1,1,1,1,1,1,1;
                                        0,-7,1,1,1,1,1,1,1;
                                        0,0,-6,1,1,1,1,1,1;
                                        0,0,0,-5,1,1,1,1,1;
                                        0,0,0,0,-4,1,1,1,1;
                                        0,0,0,0,0,-3,1,1,1;
                                        0,0,0,0,0,0,-2,1,1;
                                        0,0,0,0,0,0,0,-1,1}.
        loop i = 1 to 8.
          compute conmat1(i,:)=conmat1(i,:)/(10-i).
        end loop.
        compute conmat1=t(conmat1((10-nvls):8,(10-nvls):9)).
        loop k=1 to n.
          compute dummy(k,:)=conmat1((rsum(dummy(k,:))+1),:). 
        end loop.        
      end if.
    end if.
    do if (mcfoc = 5 or mcmod = 5).
      loop k=1 to n.
        compute dummy(k,:)=conmat1((rsum(dummy(k,:))+1),:). 
      end loop.  
    end if.
    compute nx = ncol(dummy).
    compute xname = ddd(1,1:nx).
    /* compute xdata = dummy */.
    compute xname = ddd(1,1:nx).
    compute xname2={nm(1,(ncol(nm)-mcloc)), xname}.   
    compute indlbs = t(xname).
    compute dummat = make((nx+1),nx,0).
    compute dummat2=dummat.
    compute dummat2(1:(nrow(dummat)-1),:)=ident(nx).
    compute dummat((2:nrow(dummat)),:)=ident(nx).
    do if (mcfoc = 1 or mcmod = 1).
      compute dummat=dummat2.
    end if.
    do if (mcfoc = 2 or mcmod=2).
      loop i = 2 to nrow(dummat).
        loop j = 1 to (i-1).
          compute dummat(i,j) = 1.
        end loop.
      end loop.
    end if.
    do if (mcfoc = 3 or mcmod=3).
      compute dummat=conmat1.
    end if.
    do if (mcfoc = 4 or mcmod = 4 or mcfoc = 6 or mcmod = 6).
       compute dummat=dummat2.
       compute dummat(nrow(dummat2),:)=minus1.
    end if.
    do if (mcfoc = 5 or mcmod = 5).
       compute dummat=conmat1.
    end if.
    compute dummat={nnvls, dummat}.
  end if.
end if.
do if (!modval <> 9999 and mcmod <> 0).
  compute note(notes,1) = 4.
  compute notes = notes + 1.
end if.
do if (mcx <> 0 and mcfoc <> 0).
  compute note(notes,1) = 6.
  compute notes = notes + 1.
end if.


print/title = "******************* RLM Procedure for SPSS Release 1.01 *******************"/space=newpage.
print/title = "          Written by Andrew F. Hayes, Ph.D.       www.afhayes.com".
print/title = "***************************************************************************".
compute y = dd(:,1).
compute ovals = ncol(design(dd(:,1))).
do if (ovals = 2).
  compute criterr=1.
  compute errs=errs+1.
  compute errsm(errs,1)=7.
end if.
do if (criterr=0).
  compute jnerr = 0.
  compute jn = (!jn = 1).
  compute alperr = 0.
  compute sstotal = csum((y-(csum(y)/n))&**2).
  compute outv = t(nm(1,1)).
  compute mdtr = nm(1,ncol(dd)).
  compute fciv = nm(1,(ncol(dd)-1)).
  compute centerv={" "}.
  do if (!center = 1 and mcmod = 0 and nomod=0).
    compute dd(:,ncol(dd)) = dd(:,ncol(dd))-(csum(dd(:,ncol(dd)))/n).
    compute centerv={centerv,mdtr}.
  end if.
  do if (!center = 1 and mcfoc=0 and nomod=0).
    compute dd(:,(ncol(dd)-1)) = dd(:,(ncol(dd)-1))-(csum(dd(:,(ncol(dd)-1)))/n).
    compute centerv={centerv, fciv}.
  end if.
  do if (mcfoc=0 and mcmod=0).
    compute x = {con,dd(:,2:ncol(dd))}.
    do if (nomod = 0).
       compute inter = dd(:,(ncol(dd)-1))&*dd(:,ncol(dd)).
      compute x={x,inter}.
    end if.
    compute nms = t(nm(1,2:ncol(dd))).
  end if.
  do if (mcfoc <> 0 or mcmod <> 0).
    compute inter=make(n,nx,0).
    loop i = 1 to nx.
      compute inter(:,i)=dummy(:,i)&*dd(:,ncol(dd)-(1-mcloc)).
    end loop.
    do if (ncovs = 0).
      compute x={con,dummy}.
      do if (nxv > 1).
        compute x={con,dd(:,ncol(dd)-(1-mcloc)),dummy}.
      end if.
      do if (nomod = 0).
        compute x={x,inter}.
      end if.
      compute nms={nm(1,ncol(dd)-(1-mcloc)); t(ddd(1,1:ncol(dummy)));t(ddd1(1,1:ncol(dummy)))}.
      do if (nxv=1).
        compute nms=t(ddd(1,1:ncol(dummy))).
      end if.
    end if.
    do if (ncovs > 0).
        compute x={con,dd(:,2:(1+ncovs)),dummy}.
      do if (nxv > 1).
        compute x={con,dd(:,2:(1+ncovs)),dd(:,ncol(dd)-(1-mcloc)),dummy}.
      end if.
      do if (nomod = 0).
        compute x={x,inter}.
      end if.   
      compute nms={t(nm(1,2:(1+ncovs))); nm(1,ncol(dd)-(1-mcloc)); t(ddd(1,1:ncol(dummy)));t(ddd1(1,1:ncol(dummy)))}.
      do if (nxv=1).
        compute nms={t(nm(1,2:(1+ncovs))); t(ddd(1,1:ncol(dummy)))}.
      end if.
    end if.
  release dummy.
  end if.

  compute xmns = csum(x)&/n.
  compute focvals = {1,0,0}.
  compute highwarn = 0.
  compute lowwarn = 0.
  do if (ncol(x) > (2*nx+2)).
    compute covmns = {1, xmns(1,2:(ncol(x)-1-(2*nx)))}.
    compute focvals = {covmns,0,0}.
  end if.
  compute dfres = n-ncol(x). 
  compute invXtX=inv(t(x)*x).
  compute b = invXtX*t(x)*y.
  compute pred=x*b.
  compute resid = y-(x*b).
  compute ssresid=csum(resid&*resid).
  compute msresid=ssresid/dfres.
  compute hat=diag(x*invXtX*t(x)).
  compute yh=((x*b)-hat&*y)&/(1-hat).
  compute stri=resid&/sqrt((1-hat)*msresid).
  compute tri=stri&*sqrt((dfres-1)/(dfres-(stri&*stri))).
  compute ptri = 2*(1-tcdf(abs(tri), (dfres-1))).
  compute bp=n*ptri.
  compute bpm=bp-1.
  compute bp=bp-(bpm&*(bp > 1)).
  compute xmins=cmin(x).
  compute xmaxs=cmax(x).
  compute ymin=cmin(y).
  compute ymax=cmax(y).
  compute yhmin=cmin(pred).
  compute yhmax=cmax(pred).
  compute resmin=cmin(resid).
  compute resmax=cmax(resid).
  compute trmin=cmin(tri).
  compute trmax=cmax(tri).
  compute varb = msresid*invXtX.
  compute sehom = sqrt(diag(varb)).
  compute seint=sqrt(varb(nrow(varb),nrow(varb))).
  compute k3 = nrow(b).
  do if (hc <> 5).
    compute xhc=x.
    compute h = xhc(:,1).
    do if (hc = 0 or hc =1).
      loop i3 = 1 to k3.
        compute xhc(:,i3)=xhc(:,i3)&*resid.
      end  loop.
    end if.
    do if (hc =3 or hc=2).
      loop i3=1 to k3.
        compute xhc(:,i3) = (resid&/(1-hat)&**(1/(4-hc)))&*xhc(:,i3).
      end loop.
    end if.
    do if (hc = 4).
      compute hcmn=make(n,2,4).
      compute hcmn(:,2)=(n*hat)/k3.
      loop i3= 1 to k3.
        compute xhc(:,i3) = (resid&/(1-hat)&**(rmin(hcmn)/2))&*xhc(:,i3).
      end loop.
    end if.
    compute varb=(invXtX*t(xhc)*xhc*invXtX).
    do if (hc=1).
      compute varb=(n/dfres)&*varb.
    end if.
  end if.
  compute seb = sqrt(diag(varb)).
  compute r2 = 1-(ssresid/sstotal).
  do if (mcfoc <> 0 or mcmod <> 0).
    compute ytp=y.
    compute xtp=x(:,1:(ncol(x)-nx)).
    compute btp = inv(t(xtp)*xtp)*t(xtp)*ytp.
    compute residt = ytp-(xtp*btp).
    compute ssresidt=csum(residt&*residt).
    compute r2noint = 1-(ssresidt/sstotal).
  end if.
  compute pr = ncol(x)-1.
  compute adjr2=r2-((pr*(1-r2))/dfres).
  compute adjr2=adjr2*(1-(adjr2 < 0)).
  compute pratt2=1-((((n-3)*(1-r2))/(n-pr-1))*(1+((2*(1-r2))/(n-pr-(2/3))))).
  compute pratt2=pratt2*(1-(pratt2 < 0)).
  compute seest=sqrt(msresid).
  do if (zpp <> 0).
    compute srrat=b&/sehom.
    compute sign=b&/abs(b).
    compute semir=srrat*sqrt((1-r2)/(n-pr-1)).
    compute partialr=sqrt((srrat&*srrat)&/((srrat&*srrat)+dfres))&*sign.
    compute part={semir, partialr}.
    compute iv5=x(:,2:ncol(x)).
    compute iv5={iv5,y}.
    descrip descdat=iv5.
    compute btilde=b(2:nrow(desc3))&*(desc3(1:(nrow(desc3)-1)))&/desc3(nrow(desc3),1).
    compute ivnms=nms.
    compute zppr=desc5(nrow(desc5),1:(ncol(desc5)-1)).
    compute part={t(zppr),part(2:nrow(part),:),btilde}.
  end if.
  do if (crossv <> 0).
    compute browne=0.
    compute iv5={y,yh}.
    descrip descdat=iv5.
    compute lvout1=desc5(2,1).
    do if (adjr2 >= 0).
      compute rho4=(adjr2*adjr2)-((2*pr*(1-adjr2)*(1-adjr2))/((n-1)*(n-pr+1))).
      do if (rho4 >=0).
        compute browne=sqrt(((n-pr-3)*rho4+adjr2)/((n-2*pr-2)*adjr2+pr)).
      end if.
    end if.
    compute newsse=ssresid&*(n-pr-2)&/((tri&*tri)+n-pr-2).
    compute my=csum(y)/n.
    compute dleave=(my-y)&/(n-1).
    compute yd=y-my.
    compute newtss=sstotal+n&*(dleave&*dleave)-((y-my-dleave)&*(y-my-dleave)).
    compute newvhaty = (newtss&/n)&*(1-newsse&/newtss).
    compute zh=(yh-dleave-my)&/sqrt(newvhaty).
    compute temp={y, zh}.
    descrip descdat=temp.
    compute lvout2=desc5(2,1).
    compute crossr={browne,lvout1, lvout2}.
  end if.
  compute lmat = ident(nrow(b)).
  compute lmat = lmat(:,2:ncol(lmat)).
  compute f = (t(t(lmat)*b)*inv(t(lmat)*varb*lmat)*((t(lmat)*b)))/pr).
  compute pf = 1-fcdf(f,pr,dfres).
  compute pf = {sqrt(r2),r2,f,pf, seest}.
  compute rsqm = {adjr2, pratt2}.
  compute tval =  sqrt(dfres* (exp((dfres-(5/6))*((xp2/(dfres-(2/3)+(.11/dfres)))*(xp2/(dfres-(2/3)+(.11/dfres)))))-1)).
  do if (itprob = 0).  
    compute tstat = b&/seb.
    compute p = 2*(1-tcdf(abs(tstat), dfres)).
    compute outp = {b,seb,tstat,p}.
    compute nms = {"constant"; nms; "interact"}.
    compute outp={outp,(b-tval&*seb),(b+tval&*seb)}.
    compute bb = tval*tval.
    print outv/title = "Dependent Variable"/format a8.
      do if (nomod=0).
        print fciv/title = "Focal Predictor"/format a8.
        print mdtr/title = "Moderator"/format a8.
      end if.
    do if (dumok = 1).
      do if (mcfoc > 0 or mcmod > 0 or mcx > 0).
        print dummat/title = "Coding of categorical regressor for analysis:"/cnames = xname2/format = F5.3.
     end if.
    end if.
    print n/title = "Sample size"/format = F10.0.
    do if (nspl > 0).
      print spl/title = "Location of spline joints:"/rnames=xnmspl/cnames=nmspl/format=!decimals.
    end if.
    print pf/title = "Complete Model Regression Summary"/clabels = "R" "R-sq" "F" "p" "SEofEst"/format !decimals.
    print rsqm/title = "Population R-squared estimates"/clabels = "Adj. Rsq" "PrattRsq"/format !decimals.
    do if (crossv <> 0).
      print crossr/clabels = "Browne" "LvOut1" "LvOut2"/title = "Shrunken R estimates"/format !decimals.
    end if.
    compute sumtab={(sstotal-ssresid),pr,(sstotal-ssresid)/pr;ssresid, dfres, ssresid/dfres; sstotal,(pr+dfres),sstotal/(pr+dfres)}.
    print sumtab/title = "ANOVA summary table"/rlabels = "Regress" "Residual" "Total"/clabels = "SS" "df" "MS"/format !decimals.
    compute lmat=make(nrow(b),1,0).
    compute lmat(nrow(lmat),1)=1.
    compute fcha = (t(t(lmat)*b)*inv(t(lmat)*varb*lmat)*((t(lmat)*b)))/1).
    compute rcha=((b(nrow(b),1)/seint)*(b(nrow(b),1)/seint))*(1-r2)/dfres.
    compute rcha = {rcha, fcha, 1, dfres, outp((pr+1),4)}.
    compute outlab={"Coeff", hclab, "t","p","LLCI","ULCI"}.
    print outp/title = "Regression Model"/rnames = nms/cnames= outlab/format !decimals.
    do if (mcfoc > 0 or mcmod > 0).
      compute rcha=r2-r2noint.
      compute lmat=make((nrow(b)-nx),nx,0).
      compute lmat2=ident(nx).
      compute lmat={lmat;lmat2}.
      compute fcha = (t(t(lmat)*b)*inv(t(lmat)*varb*lmat)*((t(lmat)*b)))/nx).
      compute pvalr2c=1-fcdf(fcha,nx,dfres).
      compute rcha = {rcha, fcha, nx, dfres,pvalr2c}.
    end if.
    do if (mcfoc = 0 and mcmod=0 and nomod=0).
      compute cprod = {nms((ncol(dd)-1),1), "X",  nms(ncol(dd),1)}.
      print cprod/title = "Interact is defined as:"/format = a8.
    end if.
    do if ((mcfoc > 0 or mcmod > 0) and nomod=0).
      compute intkey = {"a", "b", "c", "d", "e"}.
      loop i = 1 to nx.
        compute intkey={intkey; ddd1(1,i), " : ", ddd(1,i), " X ", nm(1,ncol(dd)-(1-mcloc))}.
      end loop.
      compute intkey=intkey(2:nrow(intkey),:).
      print intkey/title="Product terms key:"/format = a8.
    end if.
    do if (!change <> 0 and nomod = 0).
        print rcha/title = "R-square increase due to interaction:"/clabels "R2-chng" "F" "df1" "df2" "p"/format !decimals.
    end if.
    compute ivnms=nms(2:nrow(nms),:).
    do if (zpp = 1).
      do if (stand = 1).
        print/title = "Simple (r), semipartial (sr), partial (pr) correlations with outcome".
        print part/title = "and standardized regression coefficients (stand)"/rnames=ivnms/clabels = "r" "sr" "pr" "stand"/format !decimals/space=0.
      end if.
      do if (stand = 0).
        compute part=part(:,1:(ncol(part)-1)).
        print part/title = "Simple (r), semipartial (sr), and partial (pr) correlations with outcome"/rnames=ivnms/clabels = "r" "sr" "pr"/format !decimals.
      end if.
    end if.
    do if (nomod = 0 and settest > 0).
      compute settest=0.
      compute note(notes,1) = 9.
      compute notes = notes + 1.
    end if.
    do if (settest > nxv).
      compute settest=0.
      compute note(notes,1) = 10.
      compute notes = notes + 1.
    end if.
    do if (settest >0).
      do if (mcx > 0).
        compute settest=settest+nx-1.
      end if.
      compute lmat=make((nrow(b)-settest),settest,0).
      compute lmat2=ident(settest).
      compute lmat={lmat;lmat2}.
      compute fcha = (t(t(lmat)*b)*inv(t(lmat)*varb*lmat)*((t(lmat)*b)))/settest).
      compute pvalr2c=1-fcdf(fcha,settest,dfres).
      compute pf = {fcha,settest,dfres,pvalr2c}.
      print/title = "***************************************************************************".
      do if (nspl= 0).
        print pf/title = "Hypothesis test for regressor set"/clabels = "F" "df1" "df2" "p"/format !decimals.
        print tpx(1,(ncol(tpx)+1-settest+(nx-1)):ncol(tpx))/title = "Regressors in the set:"/format=A8.
      end if.
      do if (nspl> 0).
        print pf/title = "Hypothesis test for spline function"/clabels = "F" "df1" "df2" "p"/format !decimals.
        print tpx(1,(ncol(tpx)+1-settest):ncol(tpx))/title = "Regressors defining the spine set:"/format=A8.
      end if.
      release pvalr2c, fcha, lmat, lmat2.
    end if.
    do if (covcoeff=1).
      print/title = "***************************************************************************".
      print varb/title = "Variance-covariance matrix of regression coefficients"/rnames = nms/cnames=nms/format = !decimals.
    end if.
    do if (subsets=1 or dominate = 1).
      compute iv5=x(:,2:ncol(x)).
      domin dx=iv5/dy=y.
    end if.
    do if (nhypmat > 0) and (nhypmat <> nrow(b))).
      compute note(notes,1) = 11.
      compute notes = notes + 1.
    end if.
    do if (nhypmat = nrow(b)).
      print/title = "***************************** LINEAR HYPOTHESIS *****************************".
      print t(hypmat)/title= "Contrast vector:"/rnames=nms/clabels="weight"/format !decimals.
      compute hypval=hypmat*b.
      compute seoft=sqrt(hypmat*varb*t(hypmat)).
      compute outp={outp,(b-tval&*seb),(b+tval&*seb)}.
      compute pval = 2*(1-tcdf(abs(hypval/seoft), dfres)).
      compute conres={hypval,seoft,hypval/seoft,pval, (hypval-tval*seoft),(hypval+tval*seoft)}.
      compute outlab2={"Value", hclab, "t", "p", "LLCI", "ULCI"}.
      print conres/title = "Linear Hypothesis Test"/cnames=outlab2/format = !decimals. 
    end if.

    do if (nomod=0).
      compute mdvar = x(:,(ncol(x)-1)).
      do if (mcfoc = 0 and mcmod = 0).
        compute g1 = b((ncol(x)-2),1).
        compute g3 = b(ncol(x),1).
        compute vg1 = varb((ncol(x)-2),(ncol(x)-2)).
        compute vg3 = varb(ncol(x),ncol(x)).
        compute covg1g3 = varb((ncol(x)-2), ncol(x)).
      end if.
      do if (mcfoc > 0 or mcmod > 0).
        compute mdvar = x(:,(ncol(x)-(2*nx))).
      end if.
      compute mdmin = cmin(mdvar).
      compute mdmax = cmax(mdvar).
      compute fvar = x(:,(ncol(x)-2)).
      compute nval = ncol(design(mdvar)).
      compute fvmin = cmin(fvar).
      compute fvmax = cmax(fvar).
      do if (!modval = 9999 and jn < 1).
        compute mnmd = csum(mdvar)/n.
        compute tmp = make(n,1,mnmd).
        compute sdmd = sqrt(csum(((mdvar-tmp)&**2))/(n-1)).
        compute probeval = {mnmd-sdmd; mnmd; mnmd+sdmd}.
        do if (ptiles = 1).
          compute tmp = mdvar.
          compute tmp(GRADE(mdvar(:,1)),:) = mdvar.
          compute mdvar = tmp.
          compute probeval={mdvar(trunc(0.25*n),1);mdvar(trunc(0.5*n),1);mdvar(trunc(0.75*n),1)}.
        end if.
      end if.
      do if (nval = 2).
        compute probeval = make(2,1,0).
        compute probeval(1,1)=cmin(mdvar).
        compute jn = 0.
        loop i = 1 to n.
          do if (mdvar(i,1) <> probeval(1,1)).
            compute probeval(2,1) = mdvar(i,1).
            BREAK.
          end if.
        end loop.
      end if.
      do if (!modval <> 9999 and jn < 1).
        compute probeval = !modval.
      end if.
      do if (jn < 1).
        compute outp = make(nrow(probeval),7,0).
        do if (mcfoc = 0 and mcmod=0).
          loop i = 1 to nrow(probeval).
            compute x2 = probeval(i,1).
            compute w1 = g1+g3*x2.
            compute varw1 = vg1+(2*x2*covg1g3)+((x2*x2)*vg3).
            compute sew1 = sqrt(varw1).
            compute t1 = w1/sew1.
            compute LLCI = (w1-tval&*sew1).
            compute ULCI = (w1+tval&*sew1).
            compute p = 2*(1-tcdf(abs(t1), dfres)).
            loop j = 0 to 20.
              compute temp = (fvmin+j*((fvmax-fvmin)/20)).
              do if (ncol(x) > 4).
                compute focvals = {focvals; covmns, temp, probeval(i,1)}.
              else.
                compute focvals = {focvals; 1, temp, probeval(i,1)}.
              end if.
            end loop.
            compute outp(i,:) = {x2, w1, sew1, t1, p, LLCI, ULCI}.
          end loop.
          compute focvals = focvals(2:nrow(focvals),:).
          compute inter2 = focvals(:,(ncol(focvals)-1))&*focvals(:,ncol(focvals)).
          compute focvals = {focvals, inter2}.
          compute yhat = focvals*b.
          compute seyhat=sqrt(diag(focvals*varb*t(focvals))).
          compute focvals = {focvals(:,(ncol(focvals)-2):(ncol(focvals)-1)), yhat,seyhat}.
        end if.
        do if (mcfoc > 0 or mcmod > 0).
          compute focvals=make(1,ncol(x)+1,1).
          do if (mcfoc > 0).
            print/title = "***************************************************************************".
            print/title = "Conditional Effect of Focal Predictor at Values of the Moderator Variable".
            print/title = " "/space=0.
            compute rnn2=mdtr.
            compute matt=make(nx,6,0).
          end if.
          loop jj=1 to nrow(probeval).
            do if (mcfoc > 0).
              loop ii=1 to nx.
                compute g1=b((ncol(x)-(2*nx)+ii),1).
                compute g3=b((ncol(x)-nx+ii),1).
                compute vg1=varb((ncol(x)-(2*nx)+ii),(ncol(x)-(2*nx)+ii)).
                compute vg3=varb((ncol(x)-nx+ii),(ncol(x)-nx+ii)).
                compute covg1g3=varb((ncol(x)-(2*nx)+ii),(ncol(x)-nx+ii)).
                compute x2 = probeval(jj,1).
                compute w1 = g1+g3*x2.
                compute varw1 = vg1+(2*x2*covg1g3)+((x2*x2)*vg3).
                compute sew1 = sqrt(varw1).
                compute t1 = w1/sew1.
                compute LLCI = (w1-tval&*sew1).
                compute ULCI = (w1+tval&*sew1).
                compute p = 2*(1-tcdf(abs(t1), dfres)).
                compute cnms = {"Coeff", hclab, "t", "p", "LLCI", "ULCI"}.
                compute matt(ii,:)={w1,sew1,t1,p,llci,ulci}.             
              end loop.
              compute rnms=t(xname).
              compute mdvalpr=probeval(jj,1).
              print mdvalpr/title = "Moderator value:"/rnames=rnn2/format=!decimals/space=0.
              print matt/title=" "/cnames=cnms/rnames=rnms/format=!decimals/space=0.
              compute xprob=x(:,(ncol(x)-(2*nx)))-mdvalpr.
              loop kk = 1 to nx.
                compute xprob={xprob, (xprob(:,1)&*x(:,(ncol(x)-(2*nx)+kk)))}.          
              end loop.
              compute xprob={x(:,1:((ncol(x)-(2*nx))-1)),xprob}.
              compute bmultc = inv(t(xprob)*xprob)*t(xprob)*y.
              compute residc = y-(xprob*bmultc).
              compute ssresidc=csum(residc&*residc).
              compute r2c = r2-(1-(ssresidc/sstotal)).
              compute fcha2=(dfres*r2c)/(nx*(1-r2)).
              do if (hc <> 5).
                loop kk = 1 to nx.
                  compute xprob={xprob, x(:,(ncol(x)-(2*nx)+kk))}.          
                end loop.
                compute bmultc = inv(t(xprob)*xprob)*t(xprob)*y.
                compute k3 = nrow(bmultc).
                compute xhc=xprob.
                compute hat = xhc(:,1).
                loop i3=1 to n.
                  compute hat(i3,1)= xhc(i3,:)*inv(t(xprob)*xprob)*t(xhc(i3,:)). 
                end loop.
                do if (hc = 0 or hc = 1).
                  loop i3=1 to k3.
                    compute xhc(:,i3)=xhc(:,i3)&*resid.
                  end loop.
                end if.
                do if (hc=3 or hc=2).
                  loop i3=1 to k3.
                    compute xhc(:,i3) = (resid&/(1-hat)&**(1/(4-hc)))&*xhc(:,i3).
                  end loop.
                end if.
                do if (hc =4).
                  compute hcmn=make(n,2,4).
                  compute hcmn(:,2)=(n*hat)/k3.
                  loop i3=1 to k3.
                    compute xhc(:,i3) = (resid&/(1-hat)&**(rmin(hcmn)/2))&*xhc(:,i3).
                  end loop.
                end if.
                compute varbc=(inv(t(xprob)*xprob)*t(xhc)*xhc*inv(t(xprob)*xprob)).
                do if (hc=1).
                  compute varbc=(n/dfres)*varbc.
                end if.        
                compute lmat=make((nrow(bmultc)-nx),nx,0).
                compute lmat2=ident(nx).
                compute lmat={lmat;lmat2}.
                compute fcha2 = (t(t(lmat)*bmultc)*inv(t(lmat)*varbc*lmat)*((t(lmat)*bmultc)))/nx).
              end if.
              compute pvalr2cc=1-fcdf(fcha2,nx,dfres).
              compute rcha2 = {r2c, fcha2, nx, dfres,pvalr2cc}.
              print rcha2/title = "Test of equality of conditional means at this value of the moderator"/clabels "R2-chng" "F" "df1" "df2" "p"/format !decimals.  
            end if.
            /* end of do if mcfoc > 0 */.
            compute ttttt=make(nrow(dummat),1,probeval(jj,1)).
            compute ttttt={ttttt,dummat(:,2:ncol(dummat))}.
            loop kkk=1 to nx.
              compute ttttt={ttttt,ttttt(:,1)&*ttttt(:,1+kkk)}.
            end loop.
            compute ones=make(nrow(dummat),1,1).
            do if (ncol(x) > (2*nx+2)).
              compute covmnmat=make(nrow(ttttt),ncol(covmns),0).
              loop kkk=1 to nrow(ttttt).
                compute covmnmat(kkk,:)=covmns.  
              end loop.
              compute ttttt={covmnmat,ttttt}.
            else.
              compute ttttt={ones,ttttt}.
            end if.
            compute focvals={focvals;dummat(:,1),ttttt}.  
            do if (mcfoc > 0).
              compute yhat={dummat(:,1),(ttttt*b)}.
              compute cmnms={fciv, "yhat"}.
                print yhat/title = "Estimated conditional means at this value of the moderator"/cnames=cmnms/format !decimals.
              do if (jj <> nrow(probeval)).
                print/title="-------------"/space=0.
              end if.
            end if.
          end loop.
          compute focvals=focvals(2:nrow(focvals),:).
          compute yhat=focvals(:,2:ncol(focvals))*b.
          compute seyhat=sqrt(diag(focvals(:,2:ncol(focvals))*varb*t(focvals(:,2:ncol(focvals))))).
          compute focvals={focvals(:,1),focvals(:,(ncol(focvals)-(2*nx))),yhat,seyhat}.
          compute cnms={fciv,mdtr,"yhat", "se(yhat)"}.
          do if (mcmod > 0).
            compute cnms={mdtr,fciv,"yhat", "se(yhat)"}.
          end if.
        end if.
        do if (mcmod > 0).
          compute outp=make((nx+1),7,0).
          compute bcatm={b((ncol(x)-(2*nx)),1);b((ncol(x)-nx+1):ncol(x),1)}.
          compute outp(:,1)=dummat(:,1).
          compute bcatcov=varb((ncol(x)-nx):ncol(x),(ncol(x)-nx):ncol(x)).
          compute bcatcov(1,1)=varb((ncol(x)-(2*nx)),(ncol(x)-(2*nx))).
          compute bcatcov(2:nrow(bcatcov),1)=varb((ncol(x)-nx+1):ncol(x),(ncol(x)-2*nx)).
          compute bcatcov(1,2:nrow(bcatcov))=t(varb((ncol(x)-nx+1):ncol(x),(ncol(x)-2*nx))).
          loop i = 1 to nrow(dummat).
            compute catmval={1,dummat(i,2:ncol(dummat))}.
            compute condeff=catmval*bcatm.
            compute condse=sqrt(catmval*bcatcov*t(catmval)).
            compute outp(i,2:3)={condeff,condse}.
          end loop.
          compute outp(:,4)=outp(:,2)&/outp(:,3).
          compute outp(:,5) = 2*(1-tcdf(abs(outp(:,4)), dfres)).
          compute outp(:,6) = (outp(:,2)-tval&*outp(:,3)).
          compute outp(:,7) = (outp(:,2)+tval&*outp(:,3)).
          compute cnmms = {xname2(1,1), "effect", hclab, "t", "p", "LLCI", "ULCI"}.
          print/title = "***************************************************************************".
          print outp/title = "Conditional Effect of Focal Predictor in Groups Defined by the Moderator Variable:"/cnames=cnmms/format = !decimals.
        end if.
        do if (mcfoc = 0 and mcmod = 0).
          compute cnms = {nms((ncol(x)-1),1), "effect", hclab, "t", "p", "LLCI", "ULCI"}.
          print/title = "***************************************************************************".
          print outp/title = "Conditional Effect of Focal Predictor at Values of the Moderator Variable"/cnames = cnms/format = !decimals.
        end if.
        do if (probeval(1,1) < mdmin).
          compute lowwarn = 1.
        end if.
        do if (probeval(nrow(probeval),1) > mdmax).
          compute highwarn = 1.
        end if.
        do if (nval > 2 and (!modval = 9999) and mcmod = 0).
          do if (ptiles <> 1).
            print/title = "Moderator values are the sample mean and plus/minus one SD from mean".
          else.
            print/title = "Moderator values are 25th, 50th, and 75th percentiles of the moderator distribution".
          end if.
          do if (highwarn = 1 and ptiles = 0).
            print/title = "Warning: One SD above the mean is beyond the available data".
          end if.
          do if (lowwarn = 1 and ptiles = 0).
            print/title = "Warning: One SD below the mean is beyond the available data".
          end if.
        end if.
        do if (nval = 2 and (!modval = 9999)).
          print/title = "The moderator variable is dichotomous".
        end if.
      end if.
      do if (jn > 0 and nval > 2 and (mcmod=0 and mcfoc=0)).
        compute ajn =(bb*vg3)-(g3*g3).
        compute bjn = 2*((bb*covg1g3)-(g1*g3)).
        compute cjn = (bb*vg1)-(g1*g1).
        compute radarg = (bjn*bjn)-(4*ajn*cjn).
        compute den = 2*ajn.
        compute nrts = 0.
        do if (radarg >= 0 and den <> 0 and nval <> 2).
          compute x21 = (-bjn+sqrt(radarg))/den.
          compute x22 = (-bjn-sqrt(radarg))/den.
          compute roots = 0.
          do if (x21 >= mdmin and x21 <= mdmax).
            compute nrts = 1.
            compute roots = {roots; x21}.
          end if.
          do if (x22 >= mdmin and x22 <= mdmax).
            compute nrts = nrts + 1.
            compute roots = {roots; x22}.
          end if.
          do if (nrts > 0).
            compute roots = roots(2:nrow(roots),1).
            compute cuts=make(nrow(roots),2,0).
            loop j=1 to nrow(roots).
              compute cuts(j,1)=csum((dd(:,ncol(dd)) < roots(j,1)))/(.01*n).
              compute cuts(j,2)=csum((dd(:,ncol(dd)) > roots(j,1)))/(.01*n).
            end loop.
            compute roots={roots,cuts}.
            do if (jn = 1).
              print/title = "***************************************************************************".
              print roots/title = "Moderator Value(s) Defining Nonsimultaneous Johnson-Neyman Significance Region(s)"/
                 clabels "Value" "% below" "% above"/format F10.4.
            else if (jn = 2).
              print roots/title = "Moderator Value(s) Defining Simultaneous Johnson-Neyman Significance Region(s)"/
                  clabels "Value" "% below" "% above"/format F10.4.
            end if.
          end if.
          do if (nrts = 0).
            print/title = "There are no regions of significance for the focal predictor within the observed".
            print/title= "range of the moderator."/space=0.
          end if.
        end if.
        compute probeval = 0.
        loop j = 0 to 20.
          compute temp = (mdmin+j*((mdmax-mdmin)/20)).
          compute probeval = {probeval; temp}.
        end loop.
        compute probeval = {probeval; (mdmax+5)}.
        do if (nrts > 0).
          loop i = 1 to nrts.
            loop j = 1 to (nrow(probeval)-1).
              do if ((roots(i,1) > probeval(j,1)) and (roots(i,1) < probeval((j+1),1))).
                compute probeva2 = {probeval(1:j,1); roots(i,1); probeval((j+1):nrow(probeval),1)}.
              end if.
            end loop.
            compute probeval = probeva2.
          end loop.
        end if.
        compute probeval = probeval(2:(nrow(probeval)-1),1).
        compute outp = make(nrow(probeval),7,0).
        loop i = 1 to nrow(probeval).
          compute x2 = probeval(i,1).
          compute w1 = g1+g3*x2.
          compute varw1 = vg1+(2*x2*covg1g3)+((x2*x2)*vg3).
          compute sew1 = sqrt(varw1).
          compute t1 = w1/sew1.
          compute LLCI = (w1-tval&*sew1).
          compute ULCI = (w1+tval&*sew1).
          compute p = 2*(1-tcdf(abs(t1), dfres)).
          compute cnms = {nms((ncol(x)-1),1), "effect", hclab, "t", "p", "LLCI", "ULCI"}.
          compute outp(i,:) = {x2, w1, sew1, t1, p, LLCI, ULCI}.
        end loop.
        do if (jn = 2).
          compute outp = {outp(:,1), outp(:,6:7)}.
          compute cnms = {nms((ncol(x)-1),1), "LLCI", "ULCI"}.
        end if.
        print outp/title = "Conditional Effect of Focal Predictor at Values of Moderator Variable"/cnames = cnms/format = !decimals.
        print cilm/title = "Alpha level used for Johnson-Neyman method:"/format = F4.2.
      end if.
      do if (!modval <> 9999 and mcmod = 0 and ((!modval < mdmin) or (!modval > mdmax))).
        print/title = "Warning: MODVAL is outside of the range of the data".
      end if.
      do if (mcfoc = 0 and mcmod=0).
        do if (jn < 1).
          compute fvdes = ncol(design(fvar)).
          do if (fvdes = 2).
            compute fv1 = cmin(fvar).
            compute fv2 = cmax(fvar).
            compute r = 1.
            loop j = 1 to nrow(focvals).
              do if ((focvals(j,1) = fv1) or (focvals(j,1) = fv2)).
                compute focvals(r,:)=focvals(j,:).
                compute r = r + 1.
              end if.
            end loop.
            compute focvals = focvals(1:(r-1),:).
          end if.
        end if.
      end if.
      do if (savplot=1 and jn < 1).
        do if (mcfoc = 0 and mcmod=0).
          compute cnms = {t(nms((ncol(x)-2):(ncol(x)-1),1)), "yhat", "se(yhat)"}.
        end if.
        print/title = "***************************************************************************".
        print focvals/title = "Data for Visualizing Conditional Effect of Focal Predictor"/cnames = cnms/format = !decimals.
        do if (ncovs > 0).
          print/title = "NOTE: For data above, covariates are set to their sample means.".
        end if.
      end if.
    end if.
    do if (diagnose=1).
      print/title = "***************************** DIAGNOSTICS *********************************".
      compute cleric={t(xmins),t(xmaxs)}.
      compute cleric={cleric;ymin,ymax;yhmin,yhmax;resmin,resmax;trmin,trmax}.
      compute ivnms=ivnms(1:(ncol(x)-1),:).
      compute vnms={ivnms;tpy}.
      compute vnms={vnms;"y-hat"; "resid"; "t-resid"}.
      print cleric(2:nrow(cleric),:)/title = "Variable minimums and maximums"/rnames=vnms/clabels="Minimum" "Maximum"/format = F8.4.
      compute md=(n*hat-1)*((n-1)/n).
      compute cook=(stri&*stri)&*(hat&/((1-hat)*(ncol(x)))).
      compute ex=resid&/(1-hat).
      compute diagn={rownum,x(:,2:ncol(x)),y,pred,resid,ex,stri,tri,hat,md,cook}.
      compute temp = diagn.
      compute temp(GRADE(diagn(:,1)),:) = diagn.
      compute diagn = temp.
      compute nms={"casenum"; nms(2:(1+pr),1); outv; "pred"; "resid"; "de"; "str"; "tr"; "h"; "md"; "cook"}.
      save diagn/outfile = */names=nms.
      compute bp = {abs(tri),bp,rownum}.
      compute temp = bp.
      compute temp(GRADE(bp(:,1)),:) = bp.
      compute bp = temp.
      compute bp=bp(n,:).
      do if ((bp(1,1)=abs(trmin)) and (trmin < 0)).
        compute bp(1,1)=-bp(1,1).
      end if.
      print bp/title = "Bonferroni-corrected p for largest t-residual"/clabels "t-resid" "p-value" "Casenum"/format=F8.4.  
    end if.
    print/title = "********************* ANALYSIS NOTES AND WARNINGS *************************".
    print conf/title = "NOTE: Level of confidence for confidence intervals:"/format = F5.3.
    loop i = 1 to notes.
      do if (note(i,1) = 1).
        print nmiss/title = "NOTE: Some cases were deleted due to missing data.  The number of such cases was:"/space=0.
        /* print t(rowmiss)/title = "NOTE: Some cases were deleted due to missing data.  The case numbers are:"/space=0 */.
      end if.
      do if (note(i,1) = 2).
        print/title = "NOTE: Dominance analysis is not available for models with multicategorical"/space=0.
        print/title = " regressors or interactions."/space=0.
      end if.
      do if (note(i,1) = 3).
        print/title = "NOTE: All subsets regression is not available for models with multicategorical"/space=0.
        print/title = " regressors or interactions."/space=0.
      end if.
      do if (note(i,1) = 4).
        print/title = "NOTE: MODVAL option not available for use with a multicategorical moderator."/space=0.
      end if.
      do if (note(i,1) = 5).
        print/title = "NOTE: A heteroscedasticity consistent standard error estimator was used."/space=0.
      end if.
      do if (note(i,1) = 6).
        print/title = "NOTE: MCX option is ignored when focal predictor or moderator is multicategorical"/space=0.
      end if.
      do if (note(i,1) = 7).
        print/title = "NOTE: All subsets regression and dominance analysis are not available with fewer"/space=0.
        print/title = "than 2 or more than 15 regressors."/space=0.
      end if.
      do if (note(i,1) = 8).
        print/title = "NOTE: Confidence level restricted to between 50 and 99.9999%.  95% confidence is provided in output".
      end if.
      do if (note(i,1) = 9).
        print/title = "NOTE: Test of sets of regressors not implemented for models with an interaction.".
      end if.
      do if (note(i,1) = 10).
        print/title = "NOTE: Insufficient number of regressors for your requested test on a set.".
      end if.
      do if (note(i,1) = 11).
        print/title = "NOTE: Your contrast vector is not of the correct length for this model".
      end if.
    end loop.
    do if (alperr = 1 and jn > 0).
      print/title = "ERROR: Inappropriate alpha level requested.  Alpha set to 0.05.".
    end if.
  end if.
  do if (ncol(centerv) > 1).
    compute centerv=centerv(:,2:ncol(centerv)).
    print centerv/title = "NOTE: The following variables were mean centered prior to analysis:"/format = A8.
  end if.
end if. 
print.
loop i= 1 to errs.
  do if errsm(i,1)=1.
    print/title = "ERROR: Only one variable can be specifed as the outcome Y."/space=0.
  end if.
  do if errsm(i,1)=2.
    print/title = "ERROR: With moderation, you must specify at least two variables in the X= list."/space=0.
  end if.
  do if errsm(i,1)=3.
    print/title = "ERROR: Focal predictor and moderator cannot both be specified as multicategorical."/space=0.
  end if.
  do if errsm(i,1)=4.
    print/title = "ERROR: Categorical variables cannot have more than 10 categories."/space=0.
  end if.
  do if errsm(i,1)=5.
    print/title = "ERROR: One of the variables in the model exhibits no variation."/space=0.
  end if.
  do if errsm(i,1)=6.
    print/title = "ERROR: Each group must have at least two cases."/space=0.
  end if.
  do if errsm(i,1)=7.
    print/title = "ERROR: Y cannot be a dichotomous variable."/space=0.
  end if.
  do if errsm(i,1)=8.
    print/title = "ERROR: No cases without missing data available for analysis."/space=0.
  end if.
  do if errsm(i,1)=9.
    print/title = "ERROR: A spline segment must contain at least two cases."/space=0.
  end if.
  do if errsm(i,1)=10.
    print/title = "ERROR: All spline joints must be larger than the minimum observed value."/space=0.
  end if.
  do if errsm(i,1)=11.
    print/title = "ERROR: Spline joints must be listed in ascending order with no ties."/space=0.
  end if.
  do if errsm(i,1)=12.
    print/title = "ERROR: The RLM spline function works with 10 or fewer joints."/space=0.
  end if.
  do if errsm(i,1)=13.
   print/title = "ERROR: RLM requires all variable names to be eight characters or fewer."/space=0.
    print/title = "       Please shorten variable names and reexecute."/space=0.
  end if.
end loop.
end matrix.
set printback = on.

RESTORE.
!ENDDEFINE.




