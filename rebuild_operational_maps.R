library(ggplot2);library(sf);library(dplyr);library(systemfonts)
register_font('Manrope',plain='assets/Manrope-Regular.ttf',bold='assets/Manrope-Bold.ttf')
a<-readRDS('../outputs/sampling_app_cache/Abia.rds');st<-a$state_ll
s<-read.csv('../outputs/sampling_protocol/probability_first/Abia/K30/selected_sites.csv')
stopifnot(nrow(s)==30,all(s$mask_all_feasible))
sp<-st_as_sf(s,coords=c('longitude','latitude'),crs=4326)
d<-st_distance(st_transform(sp,32632));diag(d)<-Inf;stopifnot(min(as.numeric(d))>=4999)
g<-a$grid%>%mutate(eligible=dist_to_hydroriver_km<=5 & dist_to_road_km<=2 & dist_to_community_km<=5);gs<-st_as_sf(g,coords=c('longitude','latitude'),crs=4326)
t<-theme_void(base_family='Manrope',base_size=20)+theme(plot.background=element_rect(fill='#FAFAFA',colour=NA),legend.position='top',legend.title=element_text(size=17),legend.text=element_text(size=16),plot.margin=margin(8,8,8,8))
export<-function(p,n){for(ext in c('svg','png','pdf'))ggsave(paste0('assets/',n,'.',ext),p,width=7,height=7,device=if(ext=='svg')svglite::svglite else if(ext=='pdf')cairo_pdf else 'png',dpi=300,bg='#FAFAFA')}
p<-ggplot()+geom_sf(data=st,fill='#ECEFF1',colour=NA)+geom_sf(data=gs[which(gs$eligible),],colour='#92BDDD',shape=15,size=.7,stroke=0)+geom_sf(data=st,fill=NA,colour='#58595B',linewidth=.5)+t
export(p,'operational-access')
p<-ggplot()+geom_sf(data=gs,aes(colour=p_presence),shape=15,size=.7,stroke=0)+scale_colour_gradientn(colours=c('#EEF5FA','#C7DEEF','#92BDDD','#4E96C8','#0057BF'),limits=c(0,1),breaks=c(0,.5,1),labels=scales::label_percent(),name='Probability of finding flies',guide=guide_colourbar(direction='horizontal',title.position='top',barwidth=grid::unit(8,'cm'),barheight=grid::unit(.3,'cm')))+geom_sf(data=a$rivers_ll,colour='#58595B',linewidth=.35)+geom_sf(data=st,fill=NA,colour='#58595B',linewidth=.5)+geom_sf(data=sp,shape=21,fill='#F07F3C',colour='#FAFAFA',stroke=.7,size=3.5)+t
export(p,'operational-plan')
system2('python3','embed_svg_fonts.py')
