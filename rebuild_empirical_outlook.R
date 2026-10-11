library(dplyr);library(ggplot2);library(systemfonts)
register_font('Manrope',plain='assets/Manrope-Regular.ttf',bold='assets/Manrope-Bold.ttf')
d<-read.csv('../seasonality_adapative_sampling/cohort_audit/seasonal_cohort_664.csv');stopifnot(nrow(d)==7513,n_distinct(d$site_id)==664)
windows<-list()
for(id in unique(d$site_id)){
 a<-d[d$site_id==id,];a<-a[order(a$year,a$calendar_month),];tt<-a$year*12+a$calendar_month
 if(nrow(a)<12)next
 starts<-which(sapply(1:(nrow(a)-11),function(i)all(diff(tt[i:(i+11)])==1)))
 if(!length(starts))next
 w<-a[starts[1]+0:11,];w$followup_month<-1:12;windows[[length(windows)+1]]<-w
}
w<-bind_rows(windows);ns<-n_distinct(w$site_id);stopifnot(nrow(w)==12*ns)
rows<-lapply(1:4,function(k){ss<-w%>%group_by(site_id)%>%summarise(early_zero=all(y_presence[followup_month<=k]==0),later_positive=any(y_presence[followup_month>k]==1),.groups='drop')
 ss<-ss[ss$early_zero,];data.frame(k=k,n=nrow(ss),positive=sum(ss$later_positive),proportion=mean(ss$later_positive))})
s<-bind_rows(rows);write.csv(w[,c('site_id','year','calendar_month','followup_month','y_count','y_presence')],'data/empirical-outlook-site-windows.csv',row.names=FALSE);write.csv(s,'data/empirical-outlook-summary.csv',row.names=FALSE)
print(s);cat('Eligible sites:',ns,'\n')
p<-ggplot(s,aes(k,proportion))+geom_line(colour='#00ACA9',linewidth=1.2)+geom_point(colour='#00ACA9',size=5)+geom_text(aes(label=paste0(round(proportion*100),'%')),nudge_y=.035,family='Manrope',size=8,colour='#1B1B1B')+geom_text(aes(label=paste0(positive,' / ',n,' sites')),nudge_y=-.035,family='Manrope',size=6,colour='#58595B')+scale_x_continuous(breaks=1:4,limits=c(.7,4.3))+scale_y_continuous(limits=c(0,.4),breaks=seq(0,.4,.1),labels=scales::label_percent())+labs(x='Initial months with zero flies (k)',y='Sites yielding flies later (%)')+theme_minimal(base_family='Manrope',base_size=23)+theme(plot.background=element_rect(fill='#FAFAFA',colour=NA),panel.grid.minor=element_blank(),panel.grid.major=element_line(colour='#E2E6E8',linewidth=.3),panel.grid.major.x=element_blank(),axis.text=element_text(colour='#58595B',size=20),axis.title=element_text(size=22),plot.margin=margin(20,40,15,20))
for(e in c('svg','png','pdf'))ggsave(paste0('assets/empirical-outlook.',e),p,width=12,height=4.6,device=if(e=='svg')svglite::svglite else if(e=='pdf')cairo_pdf else 'png',dpi=300,bg='#FAFAFA')
writeLines(paste('First complete consecutive 12-month window per site;',ns,'sites from the unchanged corrected 664-site cohort.'),'data/empirical-outlook-audit.txt')
system2('python3','embed_svg_fonts.py')
