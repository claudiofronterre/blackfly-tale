library(ggplot2);library(dplyr);library(systemfonts)
register_font('Manrope',plain='assets/Manrope-Regular.ttf',bold='assets/Manrope-Bold.ttf')
d<-read.csv('../seasonality_adapative_sampling/cohort_audit/seasonal_cohort_664.csv')
stopifnot(nrow(d)==7513,n_distinct(d$site_id)==664)
g<-d%>%group_by(site_id)%>%summarise(n=n(),pos=sum(y_presence),.groups='drop')
x<-pmin(pmax(1-g$pos/g$n,1/(2*g$n)),1-1/(2*g$n));a<-mean(qlogis(x));sig<-sd(qlogis(x))
M<-function(r)integrate(function(z)plogis(z)^r*dnorm(z,a,sig),-Inf,Inf,rel.tol=1e-9)$value
cols<-c('Forest'='#00ACA9','Mosaic transition'='#F07F3C','Savanna'='#5E4F9C')
rows<-list()
for(zone in names(cols)){
 sub<-d[d$eco_zone==zone,];pzero<-1-tapply(sub$y_presence,sub$calendar_month,mean)
 ex<-sapply(pzero,function(target)uniroot(function(r)M(r)-target,c(.0001,80),tol=1e-9)$root)
 for(start in 1:12){months<-((start-1+0:11)%%12)+1
  for(k in 0:4){Sk<-if(k==0)0 else sum(ex[months[1:k]]);Sr<-sum(ex[months[(k+1):12]])
   prob<-1-M(Sk+Sr)/M(Sk)
   rows[[length(rows)+1]]<-data.frame(zone=zone,start_month=start,k=k,probability=prob)
  }
 }
}
v<-bind_rows(rows);stopifnot(all(v$probability>=0 & v$probability<=1))
write.csv(v,'data/conditional-outlook-by-start-month.csv',row.names=FALSE)
tab<-v%>%group_by(zone,k)%>%summarise(mean=mean(probability),low=min(probability),high=max(probability),.groups='drop')
tab$zone<-factor(tab$zone,levels=names(cols));write.csv(tab,'data/conditional-outlook-summary.csv',row.names=FALSE)
p<-ggplot(tab,aes(k,mean,colour=zone,fill=zone))+geom_ribbon(aes(ymin=low,ymax=high),alpha=.16,colour=NA)+geom_line(linewidth=1.2)+geom_point(size=3)+facet_wrap(~zone,nrow=1)+scale_colour_manual(values=cols)+scale_fill_manual(values=cols)+scale_x_continuous(breaks=0:4)+scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=scales::label_percent())+labs(x='Initial months with zero flies (k)',y='Chance of a later positive catch')+theme_minimal(base_family='Manrope',base_size=22)+theme(plot.background=element_rect(fill='#FAFAFA',colour=NA),panel.grid.minor=element_blank(),panel.grid.major=element_line(colour='#E2E6E8',linewidth=.3),panel.grid.major.x=element_blank(),axis.text=element_text(colour='#58595B',size=18),axis.title=element_text(size=20),strip.text=element_text(size=23,face='bold'),plot.margin=margin(10,20,10,10),legend.position='none')
for(e in c('svg','png','pdf'))ggsave(paste0('assets/conditional-outlook.',e),p,width=12,height=4.6,device=if(e=='svg')svglite::svglite else if(e=='pdf')cairo_pdf else 'png',dpi=300,bg='#FAFAFA')
cat('Pooled zero-propensity parameters:',a,sig,'\n');print(tab)
system2('python3','embed_svg_fonts.py')
