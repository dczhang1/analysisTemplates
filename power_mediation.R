#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#


# Thanks to Jake Westfall of the University of Colorado at Boulder for suggestions.
# Small changes made April 26, 2014.


library(RGtk2)


rround  = function (num,dig)    # Rounding function
 {
if (dig==2) return(sprintf("%.2f", round(num,2)))
if (dig==3) return(sprintf("%.3f", round(num,3)))
if (dig==1) return(sprintf("%.1f", round(num,1)))
if (dig==4) return(sprintf("%.4f", round(num,4)))
if (dig==5) return(sprintf("%.5f", round(num,5)))


}
cpow  = function (r, numb,df,alph)   # Power calculation using non-central t distribution

# Error fixed in line below: .975 replaced with 1-alph/2.

 {z2T = qt(1-alph/2.,numb-2-df)
 tr = r*sqrt((numb-2-df)/(1- r*r))
 powr=  1 - pt(z2T,numb-2-df,tr) + 1 - pt(z2T,numb-2-df,-tr)
 return(powr)

}

rrround  = function (num,dig)   # Truncate leading zero for power
 {
if (substring(as.character(rround(num,dig)),1,2)=="0.") return(substring(as.character(rround(num,dig)),2,7))
if (substring(as.character(rround(num,dig)),1,2)=="-0") return(gsub("-0","-", rround(num,dig)))
if (substring(as.character(rround(num,dig)),1,2)=="1.") return(substring(as.character(rround(num,dig)),1,7))
if (substring(as.character(rround(num,dig)),1,2)=="-1") return(substring(as.character(rround(num,dig)),1,8))
}

MedP <- function(button,user.data)    # Power calculation
{

cat("Power calculations have begun...", sep="\n")

bnum=999000

alpha <- as.numeric(alph$getText())
aaa <- as.numeric(patha$getText())
bbb <- as.numeric(pathb$getText())
cpc <- as.numeric(pathcp$getText())
nxn <- as.numeric(samN$getText())
dpow <- as.numeric(dp$getText())
butty <- rs$active





ccc = aaa*bbb + cpc
bbbp=0
if (aaa^2<1 & ccc^2<1 )bbbp=bbb*sqrt((1-aaa^2)/(1-ccc^2))
bbbb=bbb+aaa*cpc
cpcp=0
if (aaa^2<1 & bbbb^2<1 ) cpcp=cpc*sqrt((1-aaa^2)/(1-bbbb^2))
if (butty==TRUE) nxn=-1

# Test a,b,c,c' and N 
runok=1
if (abs(aaa)>=1)
{
runok=0
cat("Absolute value of the path a is greater than or equal to 1. Please re-enter values.", sep="\n")
}

if (abs(ccc)>=1)
{
runok=0
cat("Absolute value of the path c or ab + c' is greater than or equal to 1. Please re-enter values.", sep="\n")
}

if (abs(bbbp)>=1)
{
runok=0
cat("Absolute value of the partial correlation of M with Y controlling for X is greater than or equal to 1. Please re-enter values.", sep="\n")
}

if (abs(bbbb)>=1)
{
runok=0
cat("Absolute value of the correlation of M with Y is greater than or equal to 1. Please re-enter values.", sep="\n")
}


if (abs(cpcp)>=1)
{
runok=0
cat("Absolute value of the partial correlation of M with Y controlling for X is greater than or equal to 1. Please re-enter values.", sep="\n")
}

if (nxn<10 & butty!=TRUE)
{
runok=0
cat("The sample size must be at least 10.  Please re-enter values.", sep="\n")
}




if (runok==1)
{
# Step 1 


 if (nxn !=-1) 
 {
   pow1= cpow(ccc,nxn,0,alpha)
   ttc= rrround(pow1,3)
   if (ttc =="1.000") ttc= "virtually 1" 
   nxn_c=nxn 
 }

if (abs(ccc)<.005)

    {if (nxn==-1) {nxn_c=0
      ttc=rrround(dpow,3)}
      if (nxn !=-1) ttc=rrround(alpha,3)}

       
    
    if (nxn ==-1 & abs(ccc)>.005) 

 { nxn_c=0
       for (ii in 10: bnum) 
   { 
       pow1= cpow(ccc,ii,0,alpha)
       if (pow1 > dpow & nxn_c==0)
      {
           nxn_c = ii
           ttc= rrround(dpow,3)
           break
      }
   }
 }




# Step 2
 if (nxn != -1) {

  pow2= cpow(aaa,nxn,0,alpha)
  tta= rrround(pow2,3)
  if (tta=="1.000") tta= "virtually 1"
  nxn_a=nxn}

if (abs(aaa)<.005)
    {if (nxn==-1) {nxn_a=0
      tta=rrround(dpow,3)}
      if (nxn !=-1) tta=rrround(alpha,3)}



   if (nxn==-1 & abs(aaa)>.005) 
 {        nxn_a=0
       for (ii in 10: bnum) 
   { 
       pow1= cpow(aaa,ii,0,alpha)
       if (pow1 > dpow & nxn_a==0)
      {
           nxn_a = ii

           tta= rrround(dpow,3)
           break
      }
   }
 }




# Step 3
 if (nxn !=-1) 

{
  pow3= cpow(bbbp,nxn,1,alpha)
 ttb= rrround(pow3,3)
 if (ttb=="1.000") ttb= "virtually 1"
 nxn_b=nxn
}
if (abs(bbbp)<.005)
    {if (nxn==-1) {nxn_b=0
      ttb=rrround(dpow,3)}
      if (nxn  !=-1) ttb=rrround(alpha,3)}



   if (nxn==-1 & abs(bbbp)>.005) 
 {        nxn_b=0
       for (ii in 10: bnum) 
   { 
       pow1= cpow(bbbp,ii,0,alpha)
       if (pow1 > dpow & nxn_b==0)
      {
           nxn_b = ii
           ttb= rrround(dpow,3)

           break
      }
   }
 }




# Step 4

 if (nxn !=-1) {
 pow4= cpow(cpcp,nxn,1,alpha)
 ttcp= rrround(pow4,3)
 if (ttcp=="1.000") ttcp= "virtually 1"
 nxn_cp=nxn }


if (abs(cpcp)<.005)

  { if (nxn==-1) {nxn_cp=0
      ttcp=rrround(dpow,3)}
      if (nxn  !=-1) ttcp=rrround(alpha,3)}


   if (nxn==-1 & abs(cpcp)>.005 ) 
 {        nxn_cp=0
       for (ii in 10: bnum) 
   { 
       pow1= cpow(cpcp,ii,0,alpha)
       if (pow1 > dpow & nxn_cp==0)
      {
           nxn_cp = ii
           ttcp = rrround(dpow,3)

           break
      }
   }
 }



# Indirect Effect
 if (nxn  !=-1) {
 pow_IE=pow2*pow3 
 ttab= rrround(pow_IE,3)
 if (ttab=="1.000") ttab= "virtually 1"
  nxn_ab=nxn
 }

if (abs(aaa*bbb)<.00005)
   

 {if (nxn==-1)  {nxn_ab=0
      ttab=rrround(dpow,3)}
      if (nxn  !=-1) ttab=rrround(alpha,3)}



   if (nxn==-1 & abs(aaa*bbb)>.00005) 
 {        nxn_ab=0
       for (ii in 10: bnum) 
   { 
       pow1= cpow(aaa,ii,0,alpha)
       pow1= pow1*cpow(bbbp,ii,1,alpha)
       if (pow1 > dpow & nxn_ab==0)
      {
           nxn_ab = ii
           ttab= rrround(dpow,3)

           break
      }
   }
 }


# Output results

tab1 <- matrix(c(
 "Effect","Size", "Power" , "N",
"c",rrround(ccc,3),ttc,nxn_c,
"a",rrround(aaa,3),tta,nxn_a,
"b",rrround(bbb,3),ttb,nxn_b,
"c'",rrround(cpc,3),ttcp,nxn_cp,
"ab",rrround(aaa*bbb,3),ttab,nxn_ab)
,ncol=4,byrow=TRUE)


rownames(tab1) <- rep("", nrow(tab1))
colnames(tab1) <- rep("", ncol(tab1))
print(noquote(tab1))



cat("Alpha for all power calculations set to ",rrround(alpha,3),".", sep="")

cat( sep="\n")

cat("Power calculations complete.", sep="\n")


}

}

# Create window
window = gtkWindow()
# Add title
window["title"] = "MedPow: Power Computation for Mediation Analysis"

# Add a frame
 frame = gtkFrameNew("")
window$add(frame)
# Create vertical container for file name entry
vbox = gtkVBoxNew(FALSE, 8)
vbox$setBorderWidth(54)
frame$add(vbox)







hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("Causal Paths (standardized values)")
hbox$packStart(label,FALSE,FALSE,0)



hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("                                     Path from X to M or a:")
hbox$packStart(label,FALSE,FALSE,0)
patha = gtkEntryNew()
patha$setWidthChars(10)
patha$setText(".3")
label$setMnemonicWidget(patha)
hbox$packStart(patha,FALSE,FALSE,0)


hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("    Path from M to Y (controlling for X) or b:")
hbox$packStart(label,FALSE,FALSE,0)
pathb = gtkEntryNew()
pathb$setWidthChars(10)
pathb$setText(".3")
label$setMnemonicWidget(pathb)
hbox$packStart(pathb,FALSE,FALSE,0)

hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("   Path from X to Y (controlling for M) or c':")
hbox$packStart(label,FALSE,FALSE,0)
pathcp = gtkEntryNew()
pathcp$setWidthChars(10)
pathcp$setText(".3")
label$setMnemonicWidget(pathb)
hbox$packStart(pathcp,FALSE,FALSE,0)




hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("Sample size (at least 10):")
hbox$packStart(label,FALSE,FALSE,0)
samN = gtkEntryNew()
samN$setWidthChars(10)
samN$setText("100")
label$setMnemonicWidget(samN)
hbox$packStart(samN,FALSE,FALSE,0)





hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("Or Determine N for Desired Power? (check if yes):")
hbox$packStart(label,FALSE,FALSE,0)
rs = gtkCheckButton()
rs$active <- FALSE
hbox$packStart(rs,FALSE,FALSE,0)
label$setMnemonicWidget(rs)






hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("            Desired Power?")
hbox$packStart(label,FALSE,FALSE,0)
dp = gtkEntryNew()
dp$setWidthChars(10)
dp$setText(".80")
label$setMnemonicWidget(dp)
hbox$packStart(dp,FALSE,FALSE,0)


hbox = gtkHBoxNew(FALSE,8)
vbox$packStart(hbox, FALSE, FALSE, 0)
label = gtkLabelNewWithMnemonic("Alpha?")
hbox$packStart(label,FALSE,FALSE,0)
alph = gtkEntryNew()
alph$setWidthChars(10)
alph$setText(".05")
label$setMnemonicWidget(alph)
hbox$packStart(alph,FALSE,FALSE,0)


# Add button
the.buttons = gtkHButtonBoxNew()
the.buttons$setBorderWidth(5)
vbox$add(the.buttons)
the.buttons$setLayout("spread")
the.buttons$setSpacing(40)
 buttonOK = gtkButtonNewFromStock("gtk-ok")
 gSignalConnect(buttonOK, "clicked", MedP)
 the.buttons$packStart(buttonOK,fill=F)
buttonCancel = gtkButtonNewFromStock("gtk-close")
gSignalConnect(buttonCancel, "clicked", window$destroy)
the.buttons$packStart(buttonCancel,fill=F)







