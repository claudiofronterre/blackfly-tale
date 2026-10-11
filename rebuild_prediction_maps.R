library(ggplot2);library(sf);library(dplyr);library(systemfonts)
register_font('Manrope',plain='assets/Manrope-Regular.ttf',bold='assets/Manrope-Bold.ttf')
g<-readRDS('../outputs/maps/pred_grid__hurdle_best__H_raw_raw__negbin.rds');l<-readRDS('../outputs/covariates/locations.rds');st<-st_transform(l$states,4326);pts<-st_as_sf(g,coords=c('longitude','latitude'),crs=4326)
t<-theme_void(base_family='Manrope',base_size=20)+theme(plot.background=element_rect(fill='#FAFAFA',colour=NA),legend.position='top',legend.title=element_text(size=18,colour='#1B1B1B'),legend.text=element_text(size=16,colour='#1B1B1B'),plot.margin=margin(5,8,5,8))
export<-function(p,n,w=7,h=7){ggsave(paste0('assets/',n,'.svg'),p,width=w,height=h,device=svglite::svglite,bg='#FAFAFA');ggsave(paste0('assets/',n,'.png'),p,width=w,height=h,dpi=300,bg='#FAFAFA');ggsave(paste0('assets/',n,'.pdf'),p,width=w,height=h,device=cairo_pdf,bg='#FAFAFA')}
bar<-guide_colourbar(title.position='top',direction='horizontal',barwidth=grid::unit(10.5,'cm'),barheight=grid::unit(.35,'cm'))
p<-ggplot()+geom_sf(data=pts,aes(colour=p_presence),size=1.05,shape=15,stroke=0)+geom_sf(data=st,fill=NA,colour='#8F9AA2',linewidth=.22)+scale_colour_gradientn(colours=c('#EEF5FA','#C7DEEF','#92BDDD','#4E96C8','#0057BF'),limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=scales::label_percent(),name='Probability of finding blackflies',guide=bar)+t
export(p,'prediction-probability')
p<-ggplot()+geom_sf(data=pts,aes(colour=mu_count),size=1.05,shape=15,stroke=0)+geom_sf(data=st,fill=NA,colour='#8F9AA2',linewidth=.22)+scale_colour_gradientn(colours=c('#FCECED','#F4B5B9','#E30513','#C20019'),trans=scales::log1p_trans(),limits=c(0,5000),breaks=c(0,10,100,1000,5000),labels=scales::label_comma(),name='Expected catch over 12 months',guide=bar,na.value='#D4D4D4')+t
export(p,'prediction-productivity')
library(reliabilitydiag)
x<-readRDS('../outputs/selection/binary/multivariate/stack_preds.rds');y<-l$tcc_s$y_bin[x$row_id]
set.seed(20261011)
r<-reliabilitydiag(Ensemble=x$pred_stack,y=y,region.level=.9,region.position='diagonal',region.method='resampling',n.boot=1000)
saveRDS(r,'data/presence-calibration-reliabilitydiag.rds')
d<-r[[1]]
write.csv(d$regions,'data/presence-calibration-consistency-band.csv',row.names=F)
write.csv(d$bins,'data/presence-calibration-isotonic-curve.csv',row.names=F)
b<-d$bins;curve<-data.frame(x=as.vector(t(as.matrix(b[c('x_min','x_max')]))),p=rep(b$CEP_pav,each=2))
p<-ggplot()+geom_ribbon(data=d$regions,aes(x=x,ymin=lower,ymax=upper),fill='#2581C4',alpha=.18)+geom_abline(slope=1,intercept=0,colour='#58595B',linetype=2,linewidth=.6)+geom_line(data=curve,aes(x=x,y=p),colour='#C20019',linewidth=1.1)+scale_x_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=scales::label_percent())+scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=scales::label_percent())+labs(x='Predicted probability',y='Observed proportion with flies',title='Presence-model calibration')+theme_minimal(base_family='Manrope',base_size=21)+theme(plot.background=element_rect(fill='#FAFAFA',colour=NA),panel.grid.minor=element_blank(),plot.title=element_text(size=20),plot.margin=margin(12,12,12,12))
export(p,'presence-calibration',7,7)
system2('python3','embed_svg_fonts.py')
