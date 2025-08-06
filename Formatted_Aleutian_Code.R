#### Code to calculate magma flux, tailored to the Aleutian arc ####
#### Submitted to G3 ####

#### Begin code to calculate magma flux ####
library(tidyverse)
library(egg)
## Step 1: Define the number of iterations and ranges of each variable ##
# Decide on number of iterations
nrun<-500000

# First, define all parameters in this model: param_range=c(min, max)

# First, F for wet and dry melts

Fwet_range=c(1, 20)
Fdry_range=c(1,20)

# mantle and liquid densities in kg/m3

rhomantle_range=c(3.2e3, 3.3e3) # mantle density

rho_melt_range=c(2.19e3,3.03e3) # melt densities with a variety of hydration

# grain sizes (in m)

dtotal_range=c(1e-3,10e-3)

# viscosity (in Pa s)
mu_range=c(1, 10)

# slab dip (in degrees) for all arcs
dip_range=c(45.6, 55.9)

# LAB depth (in m) for all arcs

depthLAB_range=c(35.6e3,41.1e3)

# ARC-PERPENDICULAR radius (in m)
# In this version, r_perp is set at 28km, based on de Bremond d'ars estimates for aleutian arc width
r_perp_range=c(28e3,28e3)

# This describes the length of the arc. For an output with unit km3/km/myr, keep at .5e3

r_along_range=c(0.5e3,0.5e3)

# Potential channel radii for Stokes flow (define in m).

channelradius_range=c(1.5e3, 5e3)

#also need gravity (in m/s2)
g<-9.8

# And convergence rate in m/year 

ConvergenceRate_range=c(2e-2,7.5e-2)

# If comparison of model results to a value is desired, input that here

Reported_Flux= 211# Average value of the aleutians

Reported_Flux_Uncertainty=21 # 10%  of that value

##Step 2: Prepare calculation of secondary variables from equations of lines ##
## Calculate H ##
# We will calculate a value of H (in km) based on the line of form a*cos(lambda)+b=H, and add to that a random number
# from a normal distribution with mean of zero (or really the mean of the residuals, which is on the order of 1e-16) and s.d. of the r.s.e.
set.seed(11230)
Hcorrection<-rnorm(nrun,mean=3.95e-16,sd=23.62) # This is the "uncertainty" value

Hslope<-41.745 # This is the slope of our fit line

Hintercept<-59.072 # This is the intercept

# Calculate t_equilibration from F

# Instead of using the uncertainties of the coefficients, we are going to use the standard error of the residual
# for a line with the form time=a*melt fraction + b. 
# fit the line to experiments where F was between 1 and 30, which is the range of melt fraction we're considering.
# The mean residual is approximately zero (2.9e-15), so the average value offset is 0, plus or minus 48.77.
set.seed(10023)
tcorrectwet<-rnorm(nrun,mean=2.9e-15,sd=48.77) # Correction for times calculated using F_wet
set.seed(44074)
tcorrectdry<-rnorm(nrun,mean=2.9e-15,sd=48.77) # Correction for times calculated using F_dry

tslope=-4.407 # this is the slope of our fit line
tintercept=153.670 # this is the intercept


# Calculate permeability

# Permeability is calculated by a power function fit, involving grain size and F
# it has 2 coefficients, n (basically describes melt connectivity) and C (a geometric constant)
# C has asymmetric error, but we'll approximate it as a normal distribution 
# with standard deviation equal to the average of the two errors
# We'll use these random values normally distributed around the mean to calculate permeability
set.seed(63130)
nfitrand<-rnorm(nrun,mean=2.6,sd=0.2)
set.seed(85281)
Cfitrand<-rnorm(nrun,mean=58,sd=29)

# Calculate Hot Mantle Depth (HMD; in m)
# We are going to calculate HMD from H, since they are linearly correlated, 
# and add to it a normally distributed correction factor
HMDIntercept<-1.837e4 # our line's intercept
HMDSlope<-6.656e-1 # its slope
set.seed(20815)
HMDCorrect<-rnorm(nrun,mean=-5.16184e-13,sd=11600) # the correction factor

## Step 3: Prepare for for-loop ##

#initialize progress bar
pb=txtProgressBar(min = 0, max = nrun, initial = 0, style=3) 

# below, we are going to generate a set of random numbers
# the "set.seed" function will ensure reproducibility by producing the same random numbers each time
# the value of this seed has no impact on the calculation results
set.seed(91424)
nrA=runif(nrun,min=0,max=1)
set.seed(61624)
nrB=runif(nrun,min=0,max=1)
set.seed(820)
nrC=runif(nrun,min=0,max=1)
set.seed(918)
nrD=runif(nrun,min=0,max=1)
set.seed(830)
nrE=runif(nrun,min=0,max=1)
set.seed(118)
nrF=runif(nrun,min=0,max=1)
set.seed(424)
nrG=runif(nrun,min=0,max=1)
set.seed(316)
nrH=runif(nrun,min=0,max=1)
set.seed(28)
nrI=runif(nrun,min=0,max=1)
set.seed(814)
nrJ=runif(nrun,min=0,max=1)
set.seed(48)
nrK=runif(nrun,min=0,max=1)
set.seed(119)
nrL=runif(nrun,min=0,max=1)
set.seed(116)
nrM=runif(nrun,min=0,max=1)

## Step 4: Run for-loop (heart of the calculation) ##
for(i in 1:nrun){
  #update progress bar
  setTxtProgressBar(pb,i)
  # first, we will define all random variables from their ranges
  Fwet=(min(Fwet_range)+((max(Fwet_range)-min(Fwet_range)))*nrA)
  Fdry=(min(Fdry_range)+((max(Fdry_range)-min(Fdry_range)))*nrI)
  rhomantle=(min(rhomantle_range)+((max(rhomantle_range)-min(rhomantle_range)))*nrB)
  rho_melt=(min(rho_melt_range)+((max(rho_melt_range)-min(rho_melt_range)))*nrC)
  dtotal=(min(dtotal_range)+((max(dtotal_range)-min(dtotal_range)))*nrD)
  mu=(min(mu_range)+((max(mu_range)-min(mu_range)))*nrE)
  dip=(min(dip_range)+((max(dip_range)-min(dip_range)))*nrF)
  depthLAB=(min(depthLAB_range)+((max(depthLAB_range)-min(depthLAB_range)))*nrG)
  r_perp=(min(r_perp_range)+((max(r_perp_range)-min(r_perp_range)))*nrH)
  channelradius=(min(channelradius_range)+((max(channelradius_range)-min(channelradius_range)))*nrJ)
  ConvergenceRate=(min(ConvergenceRate_range)+((max(ConvergenceRate_range)-min(ConvergenceRate_range)))*nrK)
  r_along=(min(r_along_range)+((max(r_along_range)-min(r_along_range)))*nrM)
  
  
  # Step 1: Calculate equilibration time and scale for grain size
  # do this first for wet calculation
  experimenttimehourwet=((Fwet*tslope)+tintercept)+tcorrectwet # calculate experimental equilibration time
  experimenttimehourwet=ifelse(experimenttimehourwet<0,NA,experimenttimehourwet) # NA the time if the correction causes it to be below 0
  experimenttimesecondwet=experimenttimehourwet*60*60 # convert time to seconds
  ordermagdiff=1/(1e-6/dtotal) # this step calculates an order of magnitude difference between mantle grain size in nature and grain size in experiments. we assume experimental grain size is on the order of 10-6
  t_equilibration_wet=(experimenttimesecondwet*ordermagdiff^2)*3.2e-14 # scale time for grain size, now in myr
  # now for dry calculation
  experimenttimehourdry=((Fdry*tslope)+tintercept)+tcorrectdry
  experimenttimehourdry=ifelse(experimenttimehourdry<0,NA,experimenttimehourdry)
  experimenttimeseconddry=experimenttimehourdry*60*60
  t_equilibration_dry=(experimenttimeseconddry*ordermagdiff^2)*3.2e-14
  
  # Step 2: calculate travel times via darcy flow (wet) and stokes flow (dry)
  # let's start with Darcy flux
  # Intermediate step 1: calculate permeability using equation from Miller et al 2014
  k=(((Fwet/100)^nfitrand)*dtotal^2)/Cfitrand # n and C are fit coefficients, see Step 2
  k=ifelse(k<0,NA,k) # if the corrections to n and C cause this value to be negative, NA
  deltarho=rhomantle-rho_melt
  Darcy_Velocity=((k*deltarho*g)/((Fwet/100)*mu)) 
  DarcyTime=(1/Darcy_Velocity*(HotMantleDepth-depthLAB))*3.2e-14 # The amount of time in million years it takes for melt to make it to the crust traveling by Darcy Flow from hot mantle depth
  Stokes_Velocity=((2*deltarho*g*rdry^2)/(9*mu)) # Stokes Velocity
  StokesTime=(1/Stokes_Velocity*Hdry)*3.2e-14 #The amount of time in million years it takes for melt to get through the entire cylinder of dry melting via channelized flow
  
  # Step 3: Calculate total times in myr
  t_wet=DarcyTime+t_equilibration_wet
  t_dry=StokesTime+t_equilibration_dry
  
  # Step 4: Calculate volume of mantle that is melting
  # Intermediate step: calculate depth to slab (H) given dips in m
  H=((((Hslope*1/cos(dip*pi/180))+Hintercept))+Hcorrection)*10^3 # ok we calculate H from the dip, then we add the correction based on the residual standard error. Then convert to meters.
  H=ifelse(H<0,NA,H)
  # Calculate Hot Mantle Depth from H
  HotMantleDepth=(H*HMDSlope)+HMDIntercept+HMDCorrect # this is the formula as defined above with a correction factor
  #Suggest following along with Figure 2 from here on out
  Heff=H-HotMantleDepth # calculate height of wet melting at center of cylinder
  Heff=ifelse(Heff<0,NA,Heff) # NA this value if it's less than 0
  #Calculate height of decompression melting mantle
  Hdry=HotMantleDepth-depthLAB 
  Hdry=ifelse(Hdry<0,NA,Hdry)
  # Define r_perp based on r_max, the distance to the slab at HMD. r_perp CANNOT be greater than the distance to the slab 
  r_max=Heff/tan(dip*pi/180)
  # Because in this version we're setting r_perp to 28km, we get rid of this next line.
  # r_perp is going to range somewhere from .01 km to r_max
  #r_perp=(min(r_perp_range)+((r_max-min(r_perp_range)))*nrH)
  
  
  # Calculate volume of dry melting mantle (a cylinder). Its arc perpendicular radius is half that Wet Melt, but along arc is the same
  rdry=r_perp/2
  volumedry=pi*(rdry)*r_along*Hdry
  volumedry=ifelse(volumedry<0,NA,volumedry) # if for any reason this is <0, NA
  
  # Calculate volume of wet melting mantle
  #Depth of the greatest depth of possible melting Hwet
  Hwet=(r_max+r_perp)*tan(dip*pi/180)
  # Depth of the point the cylinder intersects the slab, Hcyl. Above this depth, we calculate volume as a standard cylinder
  Hcyl=(r_max-r_perp)*tan(dip*pi/180)
  # Depth to the actual depth of wet melting, given a radius less than the Max
  Hwedge=Hwet-Hcyl
  # Volume of the upper cylinder
  VolumeCyl=pi*r_perp*r_along*Hcyl
  #Volume of the wedge, which is a bisected cylinder
  VolumeWedge=(pi*r_perp*r_along*Hwedge)/2
  volumewet=VolumeCyl+VolumeWedge
  volumewet=ifelse(volumewet<0,NA,volumewet) # if for any reason this is <0, NA
  
  
  # Step 5: calculate N_Cycle
  # Assume that model time/(convergence rate*maximum slab depth) gives the number of times the mantle is replenished and melting can occur
  N_Cycle_Wet=(t_wet*1e6)/((1/ConvergenceRate)*sqrt(Hwedge^2+(2*r_perp)^2))
  N_Cycle_Dry=(t_dry*1e6)/((1/ConvergenceRate)*sqrt(Hwedge^2+(2*r_perp)^2)) 
  
  # Step 6: Calculate flux in km3/myr 
  WetFlux=((Fwet/100)/(t_wet))*(volumewet*1e-9)*N_Cycle_Wet
  DryFlux=((Fdry/100)/(t_dry))*(volumedry*1e-9)*N_Cycle_Dry
  TotalFlux=WetFlux+DryFlux
  
  
  # Step 7: Calculate deviation from a reported flux
  Residualabs=abs(TotalFlux-Reported_Flux)/Reported_Flux_Uncertainty
  Residual=(TotalFlux-Reported_Flux)/Reported_Flux_Uncertainty
  #close progress bar
  close(pb)
}

## Step 5: Export everything to a dataframe ##
Residualdf<-as.data.frame(Residual)
Residualabsdf<-as.data.frame(Residualabs)
TotalFluxdf<-as.data.frame(TotalFlux)
DryFluxdf<-as.data.frame(DryFlux)
WetFluxdf<-as.data.frame(WetFlux)
Stokes_Velocitydf<-as.data.frame(Stokes_Velocity)
Darcy_Velocitydf<-as.data.frame(Darcy_Velocity)
deltarhodf<-as.data.frame(deltarho)
kdf<-as.data.frame(k)
volumewetdf<-as.data.frame(volumewet)
volumedrydf<-as.data.frame(volumedry)
Hwetdf<-as.data.frame(Hwet)
Hdrydf<-as.data.frame(Hdry)
Heffdf<-as.data.frame(Heff)
Hdf<-as.data.frame(H)
HMDdf<-as.data.frame(HotMantleDepth)
Fwetdf<-as.data.frame(Fwet)
Fdrydf<-as.data.frame(Fdry)
rhomantledf<-as.data.frame(rhomantle)
rho_meltdf<-as.data.frame(rho_melt)
dtotaldf<-as.data.frame(dtotal)
mudf<-as.data.frame(mu)
dipdf<-as.data.frame(dip)
depthLABdf<-as.data.frame(depthLAB)
r_perpdf<-as.data.frame(r_perp)
experimenttimehourwetdf<-as.data.frame(experimenttimehourwet)
ordermagdiffdf<-as.data.frame(ordermagdiff)
t_equilibration_wetdf<-as.data.frame(t_equilibration_wet)
experimenttimehourdrydf<-as.data.frame(experimenttimehourdry)
t_equilibration_drydf<-as.data.frame(t_equilibration_dry)
t_wetdf<-as.data.frame(t_wet)
t_drydf<-as.data.frame(t_dry)
r_maxdf<-as.data.frame(r_max)
r_alongdf<-as.data.frame(r_along)
ConvergenceRatedf<-as.data.frame(ConvergenceRate)
DarcyTimedf<-as.data.frame(DarcyTime)
StokesTimedf<-as.data.frame(StokesTime)
N_Cycle_Wetdf<-as.data.frame(N_Cycle_Wet)
N_Cycle_Drydf<-as.data.frame(N_Cycle_Dry)
Channel_Radius<-as.data.frame(channelradius)

Model_Output_Aleutians_20<-bind_cols(Residualdf,
                                     Residualabsdf,
                                     TotalFluxdf,
                                     DryFluxdf,
                                     WetFluxdf,
                                     Stokes_Velocitydf,
                                     Darcy_Velocitydf,
                                     deltarhodf,
                                     kdf,
                                     volumewetdf,
                                     volumedrydf,
                                     Hwetdf,
                                     Hdrydf,
                                     Heffdf,
                                     Hdf,
                                     HMDdf,
                                     Fwetdf,
                                     Fdrydf,
                                     rhomantledf,
                                     rho_meltdf,
                                     dtotaldf,
                                     mudf,
                                     dipdf,
                                     depthLABdf,
                                     r_perpdf,
                                     experimenttimehourwetdf,
                                     experimenttimehourdrydf,
                                     ordermagdiffdf,
                                     t_equilibration_wetdf,
                                     t_equilibration_drydf,
                                     t_wetdf,
                                     t_drydf,
                                     r_maxdf,
                                     r_alongdf,
                                     ConvergenceRatedf,
                                     DarcyTimedf,
                                     StokesTimedf,
                                     N_Cycle_Drydf,
                                     N_Cycle_Wetdf,
                                     Channel_Radius
)%>%
  mutate(Within_2_SD=case_when(
    Residualabs<=2~"Yes",
    TRUE~"No"
  ))

#### Create Data Frames to Analyze ####
# The output from the model step is a dataframe called "Model_Output_Aleutians_20"
# Here, we're going to filter that output
# First let's filter to remove all that are NA or that have r_perp>r_max
Aleutian_Output_No_NA_20<-Model_Output_Aleutians_20%>%
  filter(is.na(TotalFlux)==FALSE)%>% # Selecting rows where flux is NOT NA
  filter(r_perp<=r_max) # Selecting rows were r_perp is less than or equal to r_max
# Now, let's filter on the basis of H, which we want to be between 90 and 110 km
Aleutian_Output_H_Filtered_20<-Aleutian_Output_No_NA_20%>%
  filter(H>=90e3&H<=110e3) # Select rows where H is between 90 and 110 km (inclusive)

#### Make Figures ####
# Figure 5 # 
Fwet_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=Fwet,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=Fwet,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(y="Normalized Count",x=expression(F["Wet"]~"(%)"))

Fdry_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=Fdry,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=Fdry,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(y="Normalized Count",x=expression(F["Dry"]~"(%)"))

Convergence_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=ConvergenceRate,after_stat(ncount)),
                 binwidth=.005,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=ConvergenceRate,after_stat(ncount)),
                 binwidth=.005,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(y="Normalized Count", x="Convergence rate (m/yr)")

Dip_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=dip,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=dip,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(y="Normalized Count", x=expression("Dip ("*degree*")"))

LAB_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=depthLAB/1000,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=depthLAB/1000,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(y="Normalized Count", x="LAB (km)")


ggarrange(Fwet_Hist,Fdry_Hist,Dip_Hist,LAB_Hist, Convergence_Hist)

# Figure 6 #
ggplot(data=Aleutian_Output_H_Filtered_20)+
  geom_histogram(aes(x=TotalFlux), fill="grey", color="black", binwidth=50)+
  geom_boxplot(aes(x=TotalFlux,y=2500), width=400)+
  theme_article()+
  labs(title="H-filtered Runs",x=expression("Mantle-derived magma flux (km"^"3"*"/km/myr)"))+
  theme(axis.text=element_text(color="black"))+
  theme(axis.title.y=element_blank())

# Figure 7 # 
Fig7A<-ggplot(data=filter(Aleutian_Output_H_Filtered_20))+
  geom_point(aes(x=Fwet,y=TotalFlux, color=ConvergenceRate))+
  theme_article()+
  scale_color_viridis_b(n.breaks=5)+
  labs(x=expression("F"["Wet"]), y=expression("Mantle-Derived Magma Flux (km"^"3"*"/km/myr)"), color="Convergence Rate (m/yr)")+
  geom_text(x=2,y=400,label="(a)",size=8)
Fig7B<-ggplot(data=filter(Aleutian_Output_H_Filtered_20))+
  geom_point(aes(color=Fwet,y=TotalFlux, x=ConvergenceRate))+
  theme_article()+
  scale_color_viridis_b(n.breaks=5, option="cividis")+
  labs(color=expression("F"["Wet"]), y=expression("Mantle-Derived Magma Flux (km"^"3"*"/km/myr)"), x="Convergence Rate (m/yr)")+
  geom_text(x=.025,y=400,label="(b)",size=8)

ggarrange(Fig7A,Fig7B,nrow=1)

# Supplementary Figure 2 # 
Rho_Mantle_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=rhomantle,after_stat(ncount)),
                 binwidth=10,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=rhomantle,after_stat(ncount)),
                 binwidth=10,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(rho["Mantle"]~"(kg/m"^"3"*")"),y="Normalized Count")

Rho_Melt_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=rho_melt,after_stat(ncount)),
                 binwidth=100,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=rho_melt,after_stat(ncount)),
                 binwidth=100,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(rho["Melt"]~"(kg/m"^"3"*")"),y="Normalized Count")

D_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=dtotal*1000,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=dtotal*1000,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression("d (mm)"),y="Normalized Count")


Mu_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=mu,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=mu,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(mu~"(Pa-S)"),y="Normalized Count")



Channel_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_No_NA_20,aes(x=channelradius/1000,after_stat(ncount)),
                 binwidth=.5,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=channelradius/1000,after_stat(ncount)),
                 binwidth=.5,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x="Channel Radius (km)",y="Normalized Count")


ggarrange(Rho_Mantle_Hist,Rho_Melt_Hist,D_Hist,Mu_Hist,Channel_Hist)


H_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=H/1000,after_stat(ncount)),
                 binwidth=5,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=H/1000,after_stat(ncount)),
                 binwidth=5,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x="H (km)",y="Normalized Count")

HMD_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=HotMantleDepth/1000,after_stat(ncount)),
                 binwidth=5,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=HotMantleDepth/1000,after_stat(ncount)),
                 binwidth=5,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x="HMD (km)",y="Normalized Count")

t_wet_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=t_wet,after_stat(ncount)),
                 binwidth=.1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=t_wet,after_stat(ncount)),
                 binwidth=.1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(t["wet"]~"(myr)"),y="Normalized Count")

t_dry_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=t_dry,after_stat(ncount)),
                 binwidth=.1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=t_dry,after_stat(ncount)),
                 binwidth=.1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(t["dry"]~"(myr)"),y="Normalized Count")

Wet_Ncycle_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=N_Cycle_Wet,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=N_Cycle_Wet,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(N["Cycle"["Wet"]]),y="Normalized Count")+
  lims(x=c(0,50))

Dry_Ncycle_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=N_Cycle_Dry,after_stat(ncount)),
                 binwidth=1,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=N_Cycle_Dry,after_stat(ncount)),
                 binwidth=1,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(N["Cycle"["Dry"]]),y="Normalized Count")+
  lims(x=c(0,50))

Wet_Flux_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=WetFlux,after_stat(ncount)),
                 binwidth=20,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=WetFlux,after_stat(ncount)),
                 binwidth=20,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(Flux["Wet"]~(km^"3"/km/myr)),y="Normalized Count")


Dry_Flux_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=DryFlux,after_stat(ncount)),
                 binwidth=20,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=DryFlux,after_stat(ncount)),
                 binwidth=20,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(Flux["Dry"]~(km^"3"/km/myr)),y="Normalized Count")

Wet_to_Dry_Flux_Hist<-ggplot()+
  geom_histogram(data=Aleutian_Output_H_Filtered_20,aes(x=WetFlux/DryFlux,after_stat(ncount)),
                 binwidth=.5,color="black",fill="grey")+
  geom_histogram(data=Aleutian_Output_Likely_20,aes(x=WetFlux/DryFlux,after_stat(ncount)),
                 binwidth=.5,color="black",fill="gold", alpha=.5)+
  theme_article()+
  labs(x=expression(Flux["Wet"]/Flux["Dry"]),y="Normalized Count")+lims(x=c(0,20))

ggarrange(H_Hist,HMD_Hist,t_wet_Hist,t_dry_Hist,Wet_Ncycle_Hist,Dry_Ncycle_Hist,
          Wet_Flux_Hist,Dry_Flux_Hist,Wet_to_Dry_Flux_Hist)

#### Supplementary Figure 1 ####
# Ok, to make supplementary Figure 1, we're actually going to need to run a whole other set of models
# This is the same model as we ran before, but with a different r_perp
## First, we run the model ##
## Step 1: Define the number of iterations and ranges of each variable ##
# Decide on number of iterations
nrun<-500000

# First, define all parameters in this model: param_range=c(min, max)

# First, F for wet and dry melts

Fwet_range=c(1, 20)
Fdry_range=c(1,20)

# mantle and liquid densities in kg/m3

rhomantle_range=c(3.2e3, 3.3e3) # mantle density

rho_melt_range=c(2.19e3,3.03e3) # melt densities with a variety of hydration

# grain sizes (in m)

dtotal_range=c(1e-3,10e-3)

# viscosity (in Pa s)
mu_range=c(1, 10)

# slab dip (in degrees) for all arcs
dip_range=c(45.6, 55.9)

# LAB depth (in m) for all arcs

depthLAB_range=c(35.6e3,41.1e3)

# ARC-PERPENDICULAR radius (in m)
# In this version, r_perp is set at 12km, based on minimum de Bremond d'ars estimate for oceanic arcs 
r_perp_range=c(12e3,12e3)

# This describes the length of the arc. For an output with unit km3/km/myr, keep at .5e3

r_along_range=c(0.5e3,0.5e3)

# Potential channel radii for Stokes flow (define in m).

channelradius_range=c(1.5e3, 5e3)

#also need gravity (in m/s2)
g<-9.8

# And convergence rate in m/year 

ConvergenceRate_range=c(2e-2,7.5e-2)

# If comparison of model results to a value is desired, input that here

Reported_Flux= 211# Average value of the aleutians

Reported_Flux_Uncertainty=21 # 10%  of that value

## Step 2: Prepare calculation of secondary variables from equations of lines ##
## Calculate H ##
# We will calculate a value of H (in km) based on the line of form a*cos(lambda)+b=H, and add to that a random number
# from a normal distribution with mean of zero (or really the mean of the residuals, which is on the order of 1e-16) and s.d. of the r.s.e.
set.seed(11230)
Hcorrection<-rnorm(nrun,mean=3.95e-16,sd=23.62) # This is the "uncertainty" value

Hslope<-41.745 # This is the slope of our fit line

Hintercept<-59.072 # This is the intercept

# Calculate t_equilibration from F

# Instead of using the uncertainties of the coefficients, we are going to use the standard error of the residual
# for a line with the form time=a*melt fraction + b. 
# fit the line to experiments where F was between 1 and 30, which is the range of melt fraction we're considering.
# The mean residual is approximately zero (2.9e-15), so the average value offset is 0, plus or minus 48.77.
set.seed(10023)
tcorrectwet<-rnorm(nrun,mean=2.9e-15,sd=48.77) # Correction for times calculated using F_wet
set.seed(44074)
tcorrectdry<-rnorm(nrun,mean=2.9e-15,sd=48.77) # Correction for times calculated using F_dry

tslope=-4.407 # this is the slope of our fit line
tintercept=153.670 # this is the intercept


# Calculate permeability

# Permeability is calculated by a power function fit, involving grain size and F
# it has 2 coefficients, n (basically describes melt connectivity) and C (a geometric constant)
# C has asymmetric error, but we'll approximate it as a normal distribution 
# with standard deviation equal to the average of the two errors
# We'll use these random values normally distributed around the mean to calculate permeability
set.seed(63130)
nfitrand<-rnorm(nrun,mean=2.6,sd=0.2)
set.seed(85281)
Cfitrand<-rnorm(nrun,mean=58,sd=29)

# Calculate Hot Mantle Depth (HMD; in m)
# We are going to calculate HMD from H, since they are linearly correlated, 
# and add to it a normally distributed correction factor
HMDIntercept<-1.837e4 # our line's intercept
HMDSlope<-6.656e-1 # its slope
set.seed(20815)
HMDCorrect<-rnorm(nrun,mean=-5.16184e-13,sd=11600) # the correction factor

## Step 3: Prepare for for-loop ##

#initialize progress bar
pb=txtProgressBar(min = 0, max = nrun, initial = 0, style=3) 

# below, we are going to generate a set of random numbers
# the "set.seed" function will ensure reproducibility by producing the same random numbers each time
# the value of this seed has no impact on the calculation results
set.seed(91424)
nrA=runif(nrun,min=0,max=1)
set.seed(61624)
nrB=runif(nrun,min=0,max=1)
set.seed(820)
nrC=runif(nrun,min=0,max=1)
set.seed(918)
nrD=runif(nrun,min=0,max=1)
set.seed(830)
nrE=runif(nrun,min=0,max=1)
set.seed(118)
nrF=runif(nrun,min=0,max=1)
set.seed(424)
nrG=runif(nrun,min=0,max=1)
set.seed(316)
nrH=runif(nrun,min=0,max=1)
set.seed(28)
nrI=runif(nrun,min=0,max=1)
set.seed(814)
nrJ=runif(nrun,min=0,max=1)
set.seed(48)
nrK=runif(nrun,min=0,max=1)
set.seed(119)
nrL=runif(nrun,min=0,max=1)
set.seed(116)
nrM=runif(nrun,min=0,max=1)

## Step 4: Run for-loop (heart of the calculation) ##
for(i in 1:nrun){
  #update progress bar
  setTxtProgressBar(pb,i)
  # first, we will define all random variables from their ranges
  Fwet=(min(Fwet_range)+((max(Fwet_range)-min(Fwet_range)))*nrA)
  Fdry=(min(Fdry_range)+((max(Fdry_range)-min(Fdry_range)))*nrI)
  rhomantle=(min(rhomantle_range)+((max(rhomantle_range)-min(rhomantle_range)))*nrB)
  rho_melt=(min(rho_melt_range)+((max(rho_melt_range)-min(rho_melt_range)))*nrC)
  dtotal=(min(dtotal_range)+((max(dtotal_range)-min(dtotal_range)))*nrD)
  mu=(min(mu_range)+((max(mu_range)-min(mu_range)))*nrE)
  dip=(min(dip_range)+((max(dip_range)-min(dip_range)))*nrF)
  depthLAB=(min(depthLAB_range)+((max(depthLAB_range)-min(depthLAB_range)))*nrG)
  r_perp=(min(r_perp_range)+((max(r_perp_range)-min(r_perp_range)))*nrH)
  channelradius=(min(channelradius_range)+((max(channelradius_range)-min(channelradius_range)))*nrJ)
  ConvergenceRate=(min(ConvergenceRate_range)+((max(ConvergenceRate_range)-min(ConvergenceRate_range)))*nrK)
  r_along=(min(r_along_range)+((max(r_along_range)-min(r_along_range)))*nrM)
  
  
  # Step 1: Calculate equilibration time and scale for grain size
  # do this first for wet calculation
  experimenttimehourwet=((Fwet*tslope)+tintercept)+tcorrectwet # calculate experimental equilibration time
  experimenttimehourwet=ifelse(experimenttimehourwet<0,NA,experimenttimehourwet) # NA the time if the correction causes it to be below 0
  experimenttimesecondwet=experimenttimehourwet*60*60 # convert time to seconds
  ordermagdiff=1/(1e-6/dtotal) # this step calculates an order of magnitude difference between mantle grain size in nature and grain size in experiments. we assume experimental grain size is on the order of 10-6
  t_equilibration_wet=(experimenttimesecondwet*ordermagdiff^2)*3.2e-14 # scale time for grain size, now in myr
  # now for dry calculation
  experimenttimehourdry=((Fdry*tslope)+tintercept)+tcorrectdry
  experimenttimehourdry=ifelse(experimenttimehourdry<0,NA,experimenttimehourdry)
  experimenttimeseconddry=experimenttimehourdry*60*60
  t_equilibration_dry=(experimenttimeseconddry*ordermagdiff^2)*3.2e-14
  
  # Step 2: calculate travel times via darcy flow (wet) and stokes flow (dry)
  # let's start with Darcy flux
  # Intermediate step 1: calculate permeability using equation from Miller et al 2014
  k=(((Fwet/100)^nfitrand)*dtotal^2)/Cfitrand # n and C are fit coefficients, see Step 2
  k=ifelse(k<0,NA,k) # if the corrections to n and C cause this value to be negative, NA
  deltarho=rhomantle-rho_melt
  Darcy_Velocity=((k*deltarho*g)/((Fwet/100)*mu)) 
  DarcyTime=(1/Darcy_Velocity*(HotMantleDepth-depthLAB))*3.2e-14 # The amount of time in million years it takes for melt to make it to the crust traveling by Darcy Flow from hot mantle depth
  Stokes_Velocity=((2*deltarho*g*rdry^2)/(9*mu)) # Stokes Velocity
  StokesTime=(1/Stokes_Velocity*Hdry)*3.2e-14 #The amount of time in million years it takes for melt to get through the entire cylinder of dry melting via channelized flow
  
  # Step 3: Calculate total times in myr
  t_wet=DarcyTime+t_equilibration_wet
  t_dry=StokesTime+t_equilibration_dry
  
  # Step 4: Calculate volume of mantle that is melting
  # Intermediate step: calculate depth to slab (H) given dips in m
  H=((((Hslope*1/cos(dip*pi/180))+Hintercept))+Hcorrection)*10^3 # ok we calculate H from the dip, then we add the correction based on the residual standard error. Then convert to meters.
  H=ifelse(H<0,NA,H)
  # Calculate Hot Mantle Depth from H
  HotMantleDepth=(H*HMDSlope)+HMDIntercept+HMDCorrect # this is the formula as defined above with a correction factor
  #Suggest following along with Figure 2 from here on out
  Heff=H-HotMantleDepth # calculate height of wet melting at center of cylinder
  Heff=ifelse(Heff<0,NA,Heff) # NA this value if it's less than 0
  #Calculate height of decompression melting mantle
  Hdry=HotMantleDepth-depthLAB 
  Hdry=ifelse(Hdry<0,NA,Hdry)
  # Define r_perp based on r_max, the distance to the slab at HMD. r_perp CANNOT be greater than the distance to the slab 
  r_max=Heff/tan(dip*pi/180)
  # Because in this version we're setting r_perp to 28km, we get rid of this next line.
  # r_perp is going to range somewhere from .01 km to r_max
  #r_perp=(min(r_perp_range)+((r_max-min(r_perp_range)))*nrH)
  
  
  # Calculate volume of dry melting mantle (a cylinder). Its arc perpendicular radius is half that Wet Melt, but along arc is the same
  rdry=r_perp/2
  volumedry=pi*(rdry)*r_along*Hdry
  volumedry=ifelse(volumedry<0,NA,volumedry) # if for any reason this is <0, NA
  
  # Calculate volume of wet melting mantle
  #Depth of the greatest depth of possible melting Hwet
  Hwet=(r_max+r_perp)*tan(dip*pi/180)
  # Depth of the point the cylinder intersects the slab, Hcyl. Above this depth, we calculate volume as a standard cylinder
  Hcyl=(r_max-r_perp)*tan(dip*pi/180)
  # Depth to the actual depth of wet melting, given a radius less than the Max
  Hwedge=Hwet-Hcyl
  # Volume of the upper cylinder
  VolumeCyl=pi*r_perp*r_along*Hcyl
  #Volume of the wedge, which is a bisected cylinder
  VolumeWedge=(pi*r_perp*r_along*Hwedge)/2
  volumewet=VolumeCyl+VolumeWedge
  volumewet=ifelse(volumewet<0,NA,volumewet) # if for any reason this is <0, NA
  
  
  # Step 5: calculate N_Cycle
  # Assume that model time/(convergence rate*maximum slab depth) gives the number of times the mantle is replenished and melting can occur
  N_Cycle_Wet=(t_wet*1e6)/((1/ConvergenceRate)*sqrt(Hwedge^2+(2*r_perp)^2))
  N_Cycle_Dry=(t_dry*1e6)/((1/ConvergenceRate)*sqrt(Hwedge^2+(2*r_perp)^2)) 
  
  # Step 6: Calculate flux in km3/myr 
  WetFlux=((Fwet/100)/(t_wet))*(volumewet*1e-9)*N_Cycle_Wet
  DryFlux=((Fdry/100)/(t_dry))*(volumedry*1e-9)*N_Cycle_Dry
  TotalFlux=WetFlux+DryFlux
  
  
  # Step 7: Calculate deviation from a reported flux
  Residualabs=abs(TotalFlux-Reported_Flux)/Reported_Flux_Uncertainty
  Residual=(TotalFlux-Reported_Flux)/Reported_Flux_Uncertainty
  #close progress bar
  close(pb)
}

## Step 5: Export everything to a dataframe ##
Residualdf<-as.data.frame(Residual)
Residualabsdf<-as.data.frame(Residualabs)
TotalFluxdf<-as.data.frame(TotalFlux)
DryFluxdf<-as.data.frame(DryFlux)
WetFluxdf<-as.data.frame(WetFlux)
Stokes_Velocitydf<-as.data.frame(Stokes_Velocity)
Darcy_Velocitydf<-as.data.frame(Darcy_Velocity)
deltarhodf<-as.data.frame(deltarho)
kdf<-as.data.frame(k)
volumewetdf<-as.data.frame(volumewet)
volumedrydf<-as.data.frame(volumedry)
Hwetdf<-as.data.frame(Hwet)
Hdrydf<-as.data.frame(Hdry)
Heffdf<-as.data.frame(Heff)
Hdf<-as.data.frame(H)
HMDdf<-as.data.frame(HotMantleDepth)
Fwetdf<-as.data.frame(Fwet)
Fdrydf<-as.data.frame(Fdry)
rhomantledf<-as.data.frame(rhomantle)
rho_meltdf<-as.data.frame(rho_melt)
dtotaldf<-as.data.frame(dtotal)
mudf<-as.data.frame(mu)
dipdf<-as.data.frame(dip)
depthLABdf<-as.data.frame(depthLAB)
r_perpdf<-as.data.frame(r_perp)
experimenttimehourwetdf<-as.data.frame(experimenttimehourwet)
ordermagdiffdf<-as.data.frame(ordermagdiff)
t_equilibration_wetdf<-as.data.frame(t_equilibration_wet)
experimenttimehourdrydf<-as.data.frame(experimenttimehourdry)
t_equilibration_drydf<-as.data.frame(t_equilibration_dry)
t_wetdf<-as.data.frame(t_wet)
t_drydf<-as.data.frame(t_dry)
r_maxdf<-as.data.frame(r_max)
r_alongdf<-as.data.frame(r_along)
ConvergenceRatedf<-as.data.frame(ConvergenceRate)
DarcyTimedf<-as.data.frame(DarcyTime)
StokesTimedf<-as.data.frame(StokesTime)
N_Cycle_Wetdf<-as.data.frame(N_Cycle_Wet)
N_Cycle_Drydf<-as.data.frame(N_Cycle_Dry)
Channel_Radius<-as.data.frame(channelradius)

Model_Output_Aleutians_22<-bind_cols(Residualdf,
                                     Residualabsdf,
                                     TotalFluxdf,
                                     DryFluxdf,
                                     WetFluxdf,
                                     Stokes_Velocitydf,
                                     Darcy_Velocitydf,
                                     deltarhodf,
                                     kdf,
                                     volumewetdf,
                                     volumedrydf,
                                     Hwetdf,
                                     Hdrydf,
                                     Heffdf,
                                     Hdf,
                                     HMDdf,
                                     Fwetdf,
                                     Fdrydf,
                                     rhomantledf,
                                     rho_meltdf,
                                     dtotaldf,
                                     mudf,
                                     dipdf,
                                     depthLABdf,
                                     r_perpdf,
                                     experimenttimehourwetdf,
                                     experimenttimehourdrydf,
                                     ordermagdiffdf,
                                     t_equilibration_wetdf,
                                     t_equilibration_drydf,
                                     t_wetdf,
                                     t_drydf,
                                     r_maxdf,
                                     r_alongdf,
                                     ConvergenceRatedf,
                                     DarcyTimedf,
                                     StokesTimedf,
                                     N_Cycle_Drydf,
                                     N_Cycle_Wetdf,
                                     Channel_Radius
)%>%
  mutate(Within_2_SD=case_when(
    Residualabs<=2~"Yes",
    TRUE~"No"
  ))

## Then, we filter ##
Aleutian_Output_No_NA_22<-Model_Output_Aleutians_22%>% 
  filter(is.na(TotalFlux)==FALSE)%>%
  filter(r_perp<=r_max)
Aleutian_Output_H_Filtered_22<-Aleutian_Output_No_NA_22%>%
  filter(H>=90e3&H<=110e3)
## Great, now we will make the figures ##

FigSupplement_A<-ggplot(data=filter(Aleutian_Output_H_Filtered_22))+
  geom_point(aes(x=Fwet,y=TotalFlux, color=ConvergenceRate))+
  theme_article()+
  scale_color_viridis_b(n.breaks=5)+
  labs(x=expression("F"["Wet"]), y=expression("Mantle-Derived Magma Flux (km"^"3"*"/km/myr)"), color="Convergence Rate (m/yr)")+
  geom_text(x=2,y=400,label="(a)",size=8)
FigSupplement_B<-ggplot(data=filter(Aleutian_Output_H_Filtered_22))+
  geom_point(aes(color=Fwet,y=TotalFlux, x=ConvergenceRate))+
  theme_article()+
  scale_color_viridis_b(n.breaks=5, option="cividis")+
  labs(color=expression("F"["Wet"]), y=expression("Mantle-Derived Magma Flux (km"^"3"*"/km/myr)"), x="Convergence Rate (m/yr)")+
  geom_text(x=.025,y=400,label="(b)",size=8)

ggarrange(FigSupplement_A,FigSupplement_B,nrow=1)
