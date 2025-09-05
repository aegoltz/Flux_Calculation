#### Analyze Sensitivity Tests ####
library(tidyverse)

# To run this code, you must run all sensitivity tests and retain results in your global environment
#### Generate functions to extract relevant info ####
# custom function to remove rows with NA values and where rperp>rmax
remove_nas<-function(df){
  df%>%
    filter(.,is.na(TotalFlux)==FALSE)%>%
    filter(r_perp<=r_max)
 # print(500000-nrow(df))
}
# custom function to determine pearson coefficient
determine_r<-function(df,varname){
  Result<-cor.test(x=getElement(df,varname),
           y=getElement(df,"TotalFlux"), method="pearson")
  Result[["estimate"]]
  df<-df%>%
    mutate(pearson=Result[["estimate"]])
}
# custom function to determine spearman coefficient
determine_spearman<-function(df,varname){
  Result<-cor.test(x=getElement(df,varname),
                   y=getElement(df,"TotalFlux"), 
                   method="spearman",
                   exact=FALSE)
  Result[["estimate"]]
  df<-df%>%
    mutate(spearman=Result[["estimate"]])
}
# custom function to extract coefficients, leave behind extra data
extract_coefficients<-function(df, varname){
  df%>%
    mutate(Variable=varname)%>%
    distinct(pearson, spearman,Variable)
}

#### Calculate the pearson and spearman coefficients ####
Sensitivity_Result_FWet<-Model_Sensitivity_FWet%>%
  remove_nas(.)%>%
  determine_r(.,"Fwet")%>%
  determine_spearman(.,"Fwet")

Sensitivity_Result_FDry<-Model_Sensitivity_FDry%>%
  remove_nas(.)%>%
  determine_r(.,"Fdry")%>%
  determine_spearman(.,"Fdry")

Sensitivity_Result_RPerp<-Model_Sensitivity_RPerp%>%
  remove_nas(.)%>%
  determine_r(.,"r_perp")%>%
  determine_spearman(.,"r_perp")



Sensitivity_Result_Rho_Mantle<-Model_Sensitivity_Rho_Mantle%>%
  remove_nas(.)%>%
  determine_r(.,"rhomantle")%>%
  determine_spearman(.,"rhomantle")

Sensitivity_Result_Rho_Melt<-Model_Sensitivity_Rho_Melt%>%
  remove_nas(.)%>%
  determine_r(.,"rho_melt")%>%
  determine_spearman(.,"rho_melt")

Sensitivity_Result_d<-Model_Sensitivity_d%>%
  remove_nas(.)%>%
  determine_r(.,"dtotal")%>%
  determine_spearman(.,"dtotal")

Sensitivity_Result_mu<-Model_Sensitivity_mu%>%
  remove_nas(.)%>%
  determine_r(.,"mu")%>%
  determine_spearman(.,"mu")

Sensitivity_Result_dip_only<-Model_Sensitivity_dip_only%>%
  remove_nas(.)%>%
  determine_r(.,"dip")%>%
  determine_spearman(.,"dip")

Sensitivity_Result_LAB<-Model_Sensitivity_LAB%>%
  remove_nas(.)%>%
  determine_r(.,"depthLAB")%>%
  determine_spearman(.,"depthLAB")

Sensitivity_Result_channel<-Model_Sensitivity_Channel_Rad%>%
  remove_nas(.)%>%
  determine_r(.,"channelradius")%>%
  determine_spearman(.,"channelradius")

Sensitivity_Result_Conv<-Model_Sensitivity_Conv_Rate%>%
  remove_nas(.)%>%
  determine_r(.,"ConvergenceRate")%>%
  determine_spearman(.,"ConvergenceRate")

#### Simplify these dataframes by extracting relevant information, unify them ####

Coefficients_FWet<-Sensitivity_Result_FWet%>%
  extract_coefficients(.,"Fwet")

Coefficients_FDry<-Sensitivity_Result_FDry%>%
  extract_coefficients(.,"Fdry")

Coefficients_RPerp<-Sensitivity_Result_RPerp%>%
  extract_coefficients(.,"Rperp")



Coefficients_Rho_Mantle<-Sensitivity_Result_Rho_Mantle%>%
  extract_coefficients(.,"Mantle density")

Coefficients_Rho_Melt<-Sensitivity_Result_Rho_Melt%>%
  extract_coefficients(.,"Melt density")

Coefficients_d<-Sensitivity_Result_d%>%
  extract_coefficients(.,"Mantle grain size")

Coefficients_mu<-Sensitivity_Result_mu%>%
  extract_coefficients(.,"Melt viscosity")

Coefficients_dip_only<-Sensitivity_Result_dip_only%>%
  extract_coefficients(.,"Slab dip")

Coefficients_LAB<-Sensitivity_Result_LAB%>%
  extract_coefficients(.,"LAB depth")

Coefficients_channel<-Sensitivity_Result_channel%>%
  extract_coefficients(.,"Channel radius")

Coefficients_Conv<-Sensitivity_Result_Conv%>%
  extract_coefficients(.,"Convergence rate")

All_Coefficients<-bind_rows(Coefficients_FWet,
                        Coefficients_FDry,
                        Coefficients_RPerp,
                        Coefficients_Rho_Mantle,
                        Coefficients_Rho_Melt,
                        Coefficients_d,
                        Coefficients_mu,
                        Coefficients_dip_only,
                        Coefficients_LAB,
                        Coefficients_channel,
                        Coefficients_Conv)%>%
  mutate(r2=pearson^2)



#### Recreate Figure 4####
library(egg)
library(tidyverse)

ggplot(data=All_Coefficients, aes(y=abs(spearman)))+
  geom_point(aes(y=abs(spearman),x=reorder(Variable,-abs(spearman))), size=3, color='black')+
  theme_article()+
  theme(axis.text = element_text(size=10, color="black"), axis.title = element_text(size=12, color="black"))+
  theme(axis.title.x = element_blank())+
  labs(y=expression(abs(rho)))+ 
  guides(x =  guide_axis(angle = -20))+
  theme(plot.margin = unit(c(0.5,1.2,0.5,0.5), "cm"))

