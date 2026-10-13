library(ggplot2);library(dplyr);library(systemfonts)
register_font('Manrope',plain='assets/Manrope-Regular.ttf',bold='assets/Manrope-Bold.ttf')
d<-read.csv('data/empirical-outlook-site-windows.csv');g<-d%>%group_by(site_id)%>%summarise(early=sum(y_count[followup_month<=3]),later=sum(y_count[followup_month>3]),.groups='drop');stopifnot(nrow(g)==374)
a<-g%>%group_by(early_positive=early>0)%>%summarise(n=n(),positive=sum(later>0),p=mean(later>0),.groups='drop')
a$label<-factor(ifelse(a$early_positive,'Flies in\nmonths 1–3','No flies in\nmonths 1–3'),levels=c('Flies in\nmonths 1–3','No flies in\nmonths 1–3'))
write.csv(a,'data/early-late-comparison.csv',row.names=FALSE)
base<-theme_minimal(base_family='Manrope',base_size=18)+theme(plot.background=element_rect(fill='#FAFAFA',colour=NA),panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(colour='#E2E6E8',linewidth=.35),axis.text=element_text(size=16,colour='#58595B'),axis.title=element_text(size=17),plot.margin=margin(22,12,8,8),legend.position='none')
ys<-scale_y_continuous(limits=c(0,1.12),breaks=c(0,.25,.5,.75,1),labels=scales::label_percent(),expand=expansion(mult=c(0,0)))
p<-ggplot(a,aes(label,p,fill=early_positive))+geom_col(width=.55)+geom_text(aes(label=paste0(round(p*100),'%')),nudge_y=.065,family='Manrope',size=7)+geom_text(aes(label=paste0(positive,'/',n)),vjust=1.4,family='Manrope',size=5.5,colour=ifelse(a$early_positive,'white','#1B1B1B'))+scale_fill_manual(values=c('FALSE'='#858688','TRUE'='#00ACA9'))+ys+labs(x=NULL,y='Sites yielding flies later (%)')+base
export<-function(p,n){for(e in c('svg','png','pdf'))ggsave(paste0('assets/',n,'.',e),p,width=6,height=4.2,device=if(e=='svg')svglite::svglite else if(e=='pdf')cairo_pdf else 'png',dpi=300,bg='#FAFAFA')}
export(p,'empirical-early-late')
s<-read.csv('data/empirical-outlook-summary.csv')
p<-ggplot(s,aes(k,proportion))+geom_line(colour='#00ACA9',linewidth=1.1)+geom_point(colour='#00ACA9',size=3.5)+geom_text(aes(label=paste0(round(proportion*100),'%')),nudge_y=.065,family='Manrope',size=6.5)+scale_x_continuous(breaks=1:4,limits=c(.7,4.3))+ys+labs(x='Initial zero months (k)',y='Sites yielding flies later (%)')+base
export(p,'empirical-zero-run')
system2('python3','embed_svg_fonts.py')
