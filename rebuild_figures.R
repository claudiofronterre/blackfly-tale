library(ggplot2)
library(sf)
library(dplyr)
library(systemfonts)
root <- '..'
out <- 'assets'
register_font('Manrope', plain=file.path(out,'Manrope-Regular.ttf'),bold=file.path(out,'Manrope-Bold.ttf'))
ink <- '#1B1B1B'; teal <- '#00ACA9'; gold <- '#F07F3C'; blue <- '#2581C4'; muted <- '#58595B'
cols <- c('Forest'='#00ACA9','Mosaic transition'='#F07F3C','Savanna'='#5E4F9C')
base <- theme_minimal(base_size=21,base_family='Manrope') + theme(text=element_text(colour=ink),panel.grid.minor=element_blank(),panel.grid.major=element_line(colour='#E8EEEB',linewidth=.3),plot.background=element_rect(fill='#FAFAFA',colour=NA),panel.background=element_rect(fill='#FAFAFA',colour=NA),axis.title=element_text(size=19),axis.text=element_text(size=17,colour=muted),legend.position='bottom',legend.title=element_text(size=18),legend.text=element_text(size=17),plot.margin=margin(14,20,14,14),strip.text=element_text(size=21,face='bold'),plot.title=element_text(size=25,face='bold'),plot.subtitle=element_text(size=18,colour=muted))
save <- function(p,name,w=12,h=5.5){ggsave(file.path(out,paste0(name,'.svg')),p,width=w,height=h,device=svglite::svglite,bg='#FAFAFA');ggsave(file.path(out,paste0(name,'.png')),p,width=w,height=h,dpi=300,bg='#FAFAFA');ggsave(file.path(out,paste0(name,'.pdf')),p,width=w,height=h,device=cairo_pdf,bg='#FAFAFA')}
loc <- readRDS(file.path(root,'outputs/covariates/locations.rds'))
states <- st_transform(loc$states,4326)
locdata <- loc$tcc_s
stopifnot(all(is.finite(locdata$hlc_count)),all(locdata$hlc_count>=0))
monthly <- read.csv(file.path(root,'seasonality_adapative_sampling/cohort_audit/seasonal_cohort_664.csv'))
stopifnot(nrow(monthly)==7513,n_distinct(monthly$site_id)==664,all(monthly$y_presence==as.integer(monthly$y_count>0)),!anyDuplicated(monthly[c('site_id','year','calendar_month')]))
totals <- read.csv(file.path(root,'seasonality_adapative_sampling/cohort_audit/model_cohort_676.csv')) %>% group_by(site_id) %>% summarise(total=sum(y_count),.groups='drop')
n <- nrow(totals)
y <- sort(totals$total,decreasing=T)
con <- data.frame(site_pct=c(0,seq_along(y)/n*100),catch_pct=c(0,cumsum(y)/sum(y)*100))
top <- sum(head(y,ceiling(n*.1)))/sum(y)*100
p<-ggplot(con,aes(site_pct,catch_pct))+geom_abline(slope=1,intercept=0,colour='#B9C7C1',linetype=2)+geom_area(fill=teal,alpha=.08)+geom_line(colour=teal,linewidth=1.7)+geom_vline(xintercept=10,colour=gold,linewidth=.8,linetype=3)+annotate('point',x=10,y=approx(con$site_pct,con$catch_pct,10)$y,colour=gold,size=4)+annotate('text',x=42,y=65,label=paste0('Highest-catch 10% of locations\naccount for ',round(top),'% of flies'),family='Manrope',size=6.3,hjust=0,colour=ink)+scale_x_continuous(breaks=seq(0,100,20),labels=function(x)paste0(x,'%'))+scale_y_continuous(breaks=seq(0,100,20),labels=function(x)paste0(x,'%'))+labs(x='Collection locations, ranked by total catch',y='Cumulative share of flies')+base
save(p,'concentration')
maptheme<-base+theme(panel.grid=element_blank(),panel.grid.major=element_blank(),axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=16),legend.text=element_text(size=15))
pts<-st_as_sf(locdata,coords=c('longitude','latitude'),crs=4326)
pts$status<-factor(ifelse(pts$hlc_count>0,'At least one fly','No flies'),levels=c('No flies','At least one fly'))
p<-ggplot()+geom_sf(data=states,fill='#F3F3F3',colour='#858688',linewidth=.55)+geom_sf(data=pts,aes(colour=status),size=1.7,alpha=.85)+scale_colour_manual(values=c('No flies'='#8A8B8C','At least one fly'=teal),name=NULL)+maptheme
save(p,'sites',w=7.4,h=7)
g<-readRDS(file.path(root,'outputs/maps/pred_grid__hurdle_best__H_raw_raw__negbin.rds'))
gsf<-st_as_sf(g,coords=c('longitude','latitude'),crs=4326)
pal<-scale_fill_gradientn(colours=c('#F0F9F9','#B3E6E5','#66CDCB','#00ACA9','#00788E'),limits=c(0,1),breaks=c(0,.5,1),labels=scales::label_percent(accuracy=1),name='Probability of a positive collection')
p<-ggplot()+geom_sf(data=gsf,aes(colour=p_presence),size=.64,shape=15,stroke=0)+geom_sf(data=states,fill=NA,colour='#697975',linewidth=.24)+scale_colour_gradientn(colours=c('#F0F9F9','#B3E6E5','#66CDCB','#00ACA9','#00788E'),limits=c(0,1),breaks=c(0,.5,1),labels=scales::label_percent(accuracy=1),name='Probability of a positive collection')+maptheme+guides(colour=guide_colourbar(title.position='top',barwidth=grid::unit(3.8,'cm'),barheight=grid::unit(.25,'cm')))
save(p,'presence',w=10,h=6.2)
a<-readRDS(file.path(root,'outputs/sampling_app_cache/Abia.rds'))
sites<-read.csv(file.path(root,'outputs/ranked_reserve_sites/abia_primary_and_ranked_reserves.csv'))
sites_sf<-st_as_sf(sites,coords=c('longitude','latitude'),crs=4326)
ag<-st_as_sf(a$grid,coords=c('longitude','latitude'),crs=4326)
state<-a$state_ll
base_map<-function(){ggplot()+geom_sf(data=ag,aes(colour=p_presence),shape=15,size=.5,stroke=0)+scale_colour_gradientn(colours=c('#F0F9F9','#B3E6E5','#66CDCB','#00ACA9','#00788E'),limits=c(0,1),breaks=c(0,.5,1),labels=scales::label_percent(accuracy=1),name='Positive collection')+geom_sf(data=state,fill=NA,colour=ink,linewidth=.55)+maptheme+guides(colour=guide_colourbar(title.position='top',barwidth=grid::unit(3.8,'cm'),barheight=grid::unit(.25,'cm')))}
p<-base_map()+geom_sf(data=sites_sf[sites_sf$site_role=='Primary',],shape=21,fill=gold,colour='white',size=3.1,stroke=.65)
save(p,'abia',w=7,h=6.2)
p<-base_map()+geom_sf(data=sites_sf[sites_sf$site_role=='Primary',],shape=21,fill=gold,colour='white',size=3,stroke=.6)+geom_sf(data=sites_sf[sites_sf$site_role!='Primary' & sites_sf$site_rank<=30,],shape=21,fill='white',colour=ink,size=2.7,stroke=.8)
save(p,'reserves',w=7,h=6.2)
p<-ggplot()+geom_sf(data=state,fill='#EEF3E9',colour=ink,linewidth=.55)+geom_sf(data=a$hydrorivers_ll,colour='#AFC9D4',linewidth=.35)+geom_sf(data=a$rivers_ll,colour=blue,linewidth=.65)+geom_sf(data=sites_sf[sites_sf$site_role=='Primary',],shape=21,fill=gold,colour='white',size=3.3,stroke=.6)+maptheme+guides(colour=guide_colourbar(title.position='top',barwidth=grid::unit(3.8,'cm'),barheight=grid::unit(.25,'cm')))
save(p,'app',w=9,h=6.2)
access<-a$grid %>% mutate(eligible=dist_to_hydroriver_km<=5 & dist_to_road_km<=2 & dist_to_community_km<=5)
accesssf<-st_as_sf(access,coords=c('longitude','latitude'),crs=4326)
p<-ggplot()+geom_sf(data=state,fill='#E8EEEB',colour=NA)+geom_sf(data=accesssf[accesssf$eligible,],colour=teal,shape=15,size=.48,stroke=0)+geom_sf(data=state,fill=NA,colour=ink,linewidth=.6)+maptheme+guides(colour=guide_colourbar(title.position='top',barwidth=grid::unit(3.8,'cm'),barheight=grid::unit(.25,'cm')))+labs(subtitle='River ≤5 km · road ≤2 km · community ≤5 km')
save(p,'access',w=10,h=6.2)
# Original adaptive input recovered from Downloads; retained byte-for-byte under data/.
summ<-monthly %>% group_by(eco_zone,calendar_month) %>% summarise(n=n(),pos=sum(y_presence),p=pos/n,.groups='drop')
summ$eco_zone<-factor(summ$eco_zone,levels=names(cols))
p<-ggplot(summ,aes(calendar_month,p,colour=eco_zone))+geom_line(linewidth=1.25)+geom_point(size=3.1)+facet_wrap(~eco_zone,nrow=1)+scale_colour_manual(values=cols,guide='none')+scale_x_continuous(breaks=c(1,3,5,7,9,11),labels=month.abb[c(1,3,5,7,9,11)])+scale_y_continuous(labels=scales::label_percent(),limits=c(0,1))+labs(x=NULL,y='Collections yielding flies',subtitle='7,513 site-months · 664 sites · 2020–2025')+base+theme(axis.text.x=element_text(size=16),panel.grid.major.x=element_blank())
save(p,'season')
tab<-read.csv(file.path(root,'seasonality_adapative_sampling/cohort_audit/corrected664/appendixV2_seasonal_summary_table.csv')) %>% filter(k==3)
tab$zone<-factor(tab$zone,levels=names(cols))
p<-ggplot(tab,aes(zone,mean,colour=zone))+geom_linerange(aes(ymin=min,ymax=max),linewidth=2.2)+geom_point(size=5.5)+geom_text(aes(label=paste0('+',round(mean),'%')),colour=ink,nudge_x=.28,hjust=0,size=6,family='Manrope')+scale_colour_manual(values=cols,guide='none')+scale_y_continuous(labels=function(x)paste0(x,'%'),limits=c(0,310))+coord_flip(clip='off')+labs(x=NULL,y='Expected catch gain from replacing rather than continuing',subtitle='Three zero months · mean and range across calendar start months')+base+theme(axis.text.y=element_text(size=22,colour=ink),plot.margin=margin(14,75,14,14))
save(p,'gain')
write.csv(summ,'seasonality-values.csv',row.names=F)
writeLines(c(paste('Saved training locations:',n),paste('Top 10% catch share:',round(top,2)),paste('Seasonality site-months:',nrow(monthly)),paste('Seasonality sites:',n_distinct(monthly$site_id))),'figure-data-audit.txt')

system2('python3', 'embed_svg_fonts.py')
