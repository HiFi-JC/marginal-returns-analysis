#setup
  getwd()
  setwd("C:/Users/Connor/OneDrive/School/Boulder/Honors/Data/R")
  
  #library(DescTools)
  library(RDCOMClient)
  library(MASS)
  library(stargazer)
  library(nomclust)
  library(ca)
  library(broom)
  library(plotrix)
  library(heplots)
  library(candisc)
  library(ggfortify)
  
  library(ggpmisc)
  library(ggsci)
  library(ggpubr)
  library(RcmdrMisc)
  library(ggrepel)
  library(plyr)
  library(rstatix)
  library(tidyverse)
  library(wesanderson)
  library(ggplot2)
  library(mvnormtest)

  e <- exp(1)
  
  #Tsq data cleanup
  #-----------------
#import data for tsq
  tsq<-read.csv(file="htdata_tesuque.csv")
#view and check structure to get a sense of the data
  #View(tsq)
  #str(tsq)
  
  # Identify character columns if every char row needs to be modified
  #char_columnsf <- sapply(tsq, is.character)
  
  #identify char rows for this data
  char_columns <- c("Portion", "Xsection", "Edge", "Weight", "Breakage", "Rework")
  data_chars_as_num <- tsq      # Replicate data
  # Recode characters as numeric
  data_chars_as_num[ , char_columns] <- as.data.frame(
    apply(data_chars_as_num[, char_columns], 2, as.numeric))
  
  sapply(data_chars_as_num, class)                       
  # Print classes of all columns
  
  tsqsub <- subset(data_chars_as_num, Site=!NA) 
  
  #View(tsqsub)
  #View(data_chars_as_num)
  #str(tsqsub)
  
  #replace material with code and season with color in new columns
  tsqsub<- tsqsub %>%
    rename(Soil.Type = Soil.type) %>%
    rename(Catalog=Catalog..) %>%
    rename(Catalog=Catalog..) %>%
    mutate(Notching = Type)
  
  #redo factors for coded data portion and xsection
  tsqsub$Portion<-factor(ifelse(tsqsub$Portion==1,"Whole",ifelse(tsqsub$Portion==2,"Basal",ifelse(tsqsub$Portion==3,"Medial",ifelse(tsqsub$Portion==4,"Distal", "Ind")))))
  tsqsub$Xsection<-factor(ifelse(tsqsub$Xsection==1,"Oval",ifelse(tsqsub$Xsection==2,"Plano-convex",ifelse(tsqsub$Xsection==3,"Ridged",ifelse(tsqsub$Xsection==4,"Flattened", ifelse(tsqsub$Xsection==5,"Rhomboid",ifelse(tsqsub$Xsection==6,"Fluted", "Ind")))))))

  #change oas material codes to materials
  tsqsub<-tsqsub %>%
    mutate(Material=recode(Material, '200'='Obsidian','001'='Undifferentiated Chert','080'='Undifferentiated Chalcedony', '100'='0', '002'='Pedernal Chalcedony'))
  #color, season, edge, and  objects to factors
  tsqsub$Color<-factor(tsqsub$Color)
  tsqsub$Season<-factor(tsqsub$Season)
  tsqsub$Edge<-factor(tsqsub$Edge)
  tsqsub$Material<-factor(tsqsub$Material)
  #recode colors to seasons in season object
  tsqsub<-tsqsub %>%
    mutate(Season=recode(Season, 'Black'='Summer','Red'='Winter','White'='Winter'))
  #recode edge to str in edge object and notching
  tsqsub<-tsqsub %>%
    mutate(Edge=recode(Edge, '0'='Indeterminate','1'='Excurvate','2'='Straight',
        '3'='Incurvate','4'='Inward Recurvate','11'='Excurvate Serrated',
        '12'='Straight Serrated')) %>%
    mutate(Notching=recode(Notching, 'Bonito'='Notched','Archaic Stemmed'='NA',
        'Narrow'='Unnotched', 'Corner Notched'='Notched', 'Dolores Expanding Stem'='Notched',
        'ind'='NA','Stemmed'='NA','Leaf'='Unnotched','SW Triangular'='Unnotched',
        'Notched Dart'='Notched','Preform'='NA','Dolores Straight Stem'='Unnotched',
        'Temporal'='Notched', 'Side Notched'='Notched'))
  #join dfs for T and S site data with distances
  tsq_join<-rbind.data.frame(tsqsjoin,tsqtjoin)
  write.csv(tsq_join, file="tsq_data.csv")
  #-----------------
  
  #LA835 data cleanup
  #-----------------
  #import data for tsq
  la835<-read.csv(file="htdata_la835.csv")
  #view and check structure to get a sense of the data
  #View(la835)
  #str(la835)
  
  #identify char rows for this data
  char_columns <- c("Portion", "Xsection", "Edge", "Weight", "Breakage", "Rework")
  data_chars_as_num835 <- la835      # Replicate data
  # Recode characters as numeric
  data_chars_as_num835[ , char_columns] <- as.data.frame(
    apply(data_chars_as_num835[, char_columns], 2, as.numeric))
  
  sapply(data_chars_as_num835, class)                       
  # Print classes of all columns
  str(data_chars_as_num835)
  
  #not working but makes lasub
  lasub <- subset(data_chars_as_num835, Portion=!NA) 
  
  #replace material with code and season with color in new columns
  lasub<- lasub %>%
    mutate(Material = Material.OAS.code) %>%
    mutate(Season = Color) %>%
    mutate(Notching = Type)
  
  #redo factors for coded data portion and xsection
  lasub$Portion<-factor(ifelse(lasub$Portion==1,"Whole",ifelse(lasub$Portion==2,"Basal",ifelse(lasub$Portion==3,"Medial",ifelse(lasub$Portion==4,"Distal", "Ind")))))
  lasub$Xsection<-factor(ifelse(lasub$Xsection==1,"Oval",ifelse(lasub$Xsection==2,"Plano-convex",ifelse(lasub$Xsection==3,"Ridged",ifelse(lasub$Xsection==4,"Flattened", ifelse(lasub$Xsection==5,"Rhomboid",ifelse(lasub$Xsection==6,"Fluted", "Ind")))))))
  #color material and season objects to factors
  lasub$Color<-factor(lasub$Color)
  lasub$Season<-factor(lasub$Season)
  lasub$Material<-factor(lasub$Material)
  #change oas material codes to materials and notching
  lasub<-lasub %>%
    mutate(Material=dplyr::recode(Material, '200'='Obsidian','001'='Undifferentiated Chert',
        '080'='Undifferentiated Chalcedony', '100'='Silicified Wood', '002'='Pedernal Chalcedony')) %>%
    mutate(Notching=dplyr::recode(Notching, 'Bonito'='Notched','Alto'='Notched','Archaic'='NA',
        'Basal Notched'='Notched', 'Corner Notched'='Notched', 'Dolores Expanding Stem'='Notched',
        'ind'='NA','Kin Kletso'='Notched','Leaf'='Unnotched','SW Triangular'='Unnotched',
        'Side Notched'='Notched','Preform'='NA','Temporal'='Notched'))
  lasub$Notching<-factor(lasub$Notching)
  #recode colors to seasons in season object
  lasub<-lasub %>%
    mutate(Season=dplyr::recode(Season, 'Black'='Summer','Red'='Winter','White'='Winter'))
  #recode edge to str in edge object
  lasub<-lasub %>%
    mutate(Edge= dplyr::recode(Edge, '0'='Indeterminate','1'='Excurvate','2'='Straight','3'='Incurvate','4'='Inward Recurvate','11'='Excurvate Serrated','12'='Straight Serrated'))
  lasub1 <- lasub %>%
    filter(length(Catalog..)!=0)
  #str(lasub)
  #join dfs for A and D data with distances
  la_join<-rbind.data.frame(ladjoin,laajoin)
  write.csv(la_join, file="la_data.csv")
  
  la835<-read.csv(file="la_data2.csv")
  #complete portion column
  compport835 <- la835 %>%
    filter(!is.na(Portion))
  #complete breakage column
  compbrk835 <- la835 %>%
    filter(!is.na(Breakage))
  #-----------------
  
  #Faunal and Proj Point Arakawa Data
  #-----------------
  #import faunal data for tsq
  afau<-read.csv(file="Faunal Data and Site Used for PII vs PIIIcu.csv")
  
  #import data for tsq
  arakawaimp<-read.csv(file="Original Important PII-PIIIcu.csv")
  
  #remove irrelevant columns
  afaujoin<- afau %>%
    select(1,7,8,9) %>%
    filter(Artiodactyls!=0)
  arakawaimp<- arakawaimp %>%
    select(1:9) %>%
    filter(Points!=0)
  
  #rename Site.name
  arakawaimp<-arakawaimp%>%
    rename(Site.name=Site.Name)
  arakawaimp$District<-factor(arakawaimp$District)
  arakawaimp$Pecos<-factor(arakawaimp$Pecos)
  arakawaimp$Area<-factor(arakawaimp$Area)
  arakawaimp$site_type<-factor(arakawaimp$site_type)
  arakawaimp$Points<-as.integer(arakawaimp$Points)
  str(arakawaimp)
  str(afaujoin)
  
  #left join faunal data to main data
  arakawa<-left_join(arakawaimp,afaujoin, by="Site.name")
  
  #complete arakawa
  arakawaturkey <- na.omit(arakawa)
  arakawacomp <- arakawa %>%
    filter(is.na(Artiodactyls)==F)
  
  #no mv
  arakawacompnomv <- arakawacomp%>%
    filter(District!="Mesa Verde")
  
  #no mv
  arakawacompnocu <- arakawacompnomv%>%
    filter(District!="Central Utah")
  
  #no my turkey
  arakawaturkeynomy <- arakawaturkey%>%
    filter(District!="Mesa Verde")
  
  #create columns for proportions with projectiles and grayware
  arakawacompnomv <- arakawacompnomv %>%
    mutate(projctprop=Points/(Sherds+Points)) %>%
    mutate(artioctprop=Artiodactyls/(Sherds+Artiodactyls)) %>%
    mutate(ai=Artiodactyls/(Lagomorphs+Artiodactyls)) %>%
    mutate(turkeyprop=Turkey/(Sherds+Turkey))%>%
    filter(Sherds>=0)
    
  #columns for arakawaturkey
  arakawaturkey <- arakawaturkey %>%
    mutate(projctprop=Points/(Sherds+Points)) %>%
    mutate(artioctprop=Artiodactyls/(Sherds+Artiodactyls)) %>%
    mutate(ai=Artiodactyls/(Lagomorphs+Artiodactyls)) %>%
    mutate(turkeyprop=Turkey/(Sherds+Turkey))
    
  str(arakawacompnomv)
  #group by district and site type
  #------------
  arakawagroup1 <- arakawacompnomv %>%
    group_by(District, site_type) %>%
    summarise(shtotal=sum(Sherds), pointstotal=sum(Points), artiototal=sum(Artiodactyls), lagototal=sum(Lagomorphs))
  
  arakawagroup1 <- arakawagroup1 %>%
    mutate(projctprop=pointstotal/(shtotal+pointstotal)) %>%
    mutate(artioctprop=artiototal/(shtotal+artiototal)) %>%
    mutate(ai=artiototal/(lagototal+artiototal))
  
  #group by district, area, site type
  arakawagroup2 <- arakawacompnomv %>%
    group_by(District, site_type, Area) %>%
    summarise(shtotal=sum(Sherds), pointstotal=sum(Points), artiototal=sum(Artiodactyls), lagototal=sum(Lagomorphs))
  
  arakawagroup2 <- arakawagroup2 %>%
    mutate(projctprop=pointstotal/(shtotal+pointstotal)) %>%
    mutate(artioctprop=artiototal/(shtotal+artiototal)) %>%
    mutate(ai=artiototal/(lagototal+artiototal))
  
  #group by district only
  arakawagroup3 <- arakawacompnomv %>%
    group_by(District) %>%
    summarise(shtotal=sum(Sherds), pointstotal=sum(Points), artiototal=sum(Artiodactyls), lagototal=sum(Lagomorphs))
  
  arakawagroup3 <- arakawagroup3 %>%
    mutate(projctprop=pointstotal/(shtotal+pointstotal)) %>%
    mutate(artioctprop=artiototal/(shtotal+artiototal)) %>%
    mutate(ai=artiototal/(lagototal+artiototal))
    
  #filter for over 1000
  arakawagroup1<- arakawagroup1 %>%
    filter(shtotal>1000)
  arakawafilt<- arakawacompnomv %>%
    filter(Sherds>1000)
  #Stats for arakawacompnomv
  #-----------------
  #groups for arakawacompnomv
  glencanyon<-arakawacompnomv %>%
    filter(District=='Glen Canyon')
  mey<-arakawacompnomv %>%
    filter(District=='McElmo-Yellowjacket')
  tewabasin<-arakawacompnomv %>%
    filter(District=='Tewa Basin')
  pajarito<-arakawacompnomv %>%
    filter(District=='Pajarito Plateau')
  
  #Normality and statistical mean comparisons
  leveneTest(log(arakawacompnocu$projctprop)~arakawacompnocu$District, arakawacompnomv)
  leveneTest(log(arakawacompnocu$artioctprop)~arakawacompnocu$District, arakawacompnomv)
  
  
  shapiro.test(log(glencanyon$projctprop))
  shapiro.test(log(glencanyon$artioctprop))
  
  shapiro.test(log(mey$projctprop))
  shapiro.test(log(mey$artioctprop))
  
  shapiro.test(log(tewabasin$projctprop))
  shapiro.test(log(tewabasin$artioctprop))
  
  shapiro.test(log(pajarito$projctprop))
  shapiro.test(log(pajarito$artioctprop))
  
  vartest1<-var.test(log(tewabasin$projctprop), log(pajarito$projctprop))
  vartest1
  
  vartest1<-var.test(log(tewabasin$artioctprop), log(pajarito$artioctprop))
  vartest1
  
  ttest <- t.test(log(tewabasin$artioctprop), log(pajarito$artioctprop), var.equal = T)
  ttest
  
  ttest <- t.test(log(tewabasin$projctprop), log(pajarito$projctprop), var.equal = T)
  ttest
  
  #table for summaries of projctprop for nomv
  val<-arakawacompnomv$projctprop
  nomv_stats_table <- arakawacompnomv %>% 
    dplyr::group_by(District) %>% 
    dplyr::summarise(mean = mean(log(projctprop)), sd = sd(log(projctprop)),
                     'Max' = max(log(projctprop)),
                     'Min' = min(log(projctprop)),
                     n = n())
  #table for summaries of artioctprop for nomv
  val<-arakawacompnomv$projctprop
  nomv_stats_table <- arakawacompnomv %>% 
    dplyr::group_by(District) %>% 
    dplyr::summarise(mean = mean(log(projctprop)), sd = sd(log(projctprop)),
                     'Max' = max(log(projctprop)),
                     'Min' = min(log(projctprop)),
                     n = n())
  
  # Compute the analysis of variance
  nomv.aov <- aov(artioctprop ~ District, data = arakawacompnomv)
  
  # Summary of the analysis
  summary(nomv.aov)
  TukeyHSD(nomv.aov)
  
  arakawacompnomvcu<-arakawacompnomv %>%
    filter(District!='Central Utah') %>%
    mutate(artioctpropl = log(artioctprop)) %>%
    mutate(projctpropl = log(projctprop))
  
  # Pairwise comparisons
  pwc1<- arakawacompnomvcu %>% 
    pairwise_t_test(
      artioctpropl ~ District,
      comparisons = list(c("Tewa Basin", "Pajarito Plateau"), c("Glen Canyon", "McElmo-Yellowjacket")),
      p.adjust.method = "bonferroni"
    )
  pwc1
  
  pwc2<- arakawacompnomvcu %>% 
    pairwise_t_test(
      projctpropl ~ District,
      comparisons = list(c("Tewa Basin", "Pajarito Plateau"), c("Glen Canyon", "McElmo-Yellowjacket")),
      p.adjust.method = "bonferroni"
    )
  pwc2
  pwcbind <- rbind.data.frame(pwc1, pwc2)
  write.csv(pwcbind, file="pwcbind_arakawa.csv")
  
  #-----------------
  
  #Crow canyon data cleanup
  #-----------------
  #import data for castle rock
  castle<-read.csv(file="crowcanyondata.csv")
  #view and check structure to get a sense of the data
  #View(castle) and str for 835 and castle rock
  str(compport835)
  str(castle)
  #pg_cr_join is pg-> cr, and vice versa
  pg_cr_join<-compport835 %>%
    mutate(Site..=dplyr::recode(Site.., '835'='Pojoaque Grant'))
  cr_pg_join<-castle %>%
    mutate(Portion=dplyr::recode(Portion, 'complete'='Whole'))
  str(pg_cr_join)
  str(cr_pg_join)
  
  cr_pg_join$Portion<-factor(cr_pg_join$Portion)
  cr_pg_join$Notching<-factor(cr_pg_join$Notching)
  cr_pg_join$Site..<-factor(cr_pg_join$Site..)
  
  pg_cr_join$Site..<-factor(pg_cr_join$Site..)
  pg_cr_join$Notching<-factor(pg_cr_join$Notching)
  
  pg_cr<-rbind.data.frame(pg_cr_join,cr_pg_join)
  
  str(pg_cr)
  #only include whole points with weights and notching recorded;
  #outliers removed <3.2
  pg_cr_comp<-pg_cr%>%
    filter(Portion=='Whole')%>%
    filter(is.na(Weight)==F)%>%
    filter(is.na(Notching)==F)%>%
    filter(is.na(Type)==F)%>%
    filter(Weight<=3.1)%>%
    dplyr::rename(Site = Site..)
    
  #-----------------
  
  #Comparisons between Castle Rock (low) and 835 (high) projectile assemblages
  #-----------------
  myVAR=pg_cr_comp$Weight
  mydata=pg_cr_comp
  mybw=.1
  
  c1.t <- str_wrap("Projectile Weight Distributions Across High and Low Hunting Investment Sites", 65)
  ggplot(data=mydata,aes(x=myVAR, fill=Site..))+
    geom_histogram(binwidth=mybw, color="black")+
    ggtitle(c1.t)+
    scale_fill_manual(values = wes_palette(n=5, name="Cavalcanti1"))+
    geom_density(aes(y=..density..*(nrow(mydata))*mybw), alpha=.25, fill="#F85700")+
    theme(plot.title=element_text(hjust=0.5))+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Frequency")+xlab("Weight (g)")+
    #geom_vline(aes(xintercept=mean(myVAR)), color="#009CFF", linetype="solid", size=1)+
    #geom_vline(aes(xintercept=median(myVAR)),color="#009CFF", linetype="dashed", size=1)+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .5),
          panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))
  
  #castle confusion notching
  castletabnotchingsite <- xtabs(~Notching+Site, pg_cr_comp)
  castletabnotchingsite
  write.csv(castletabnotchingsite, file="castletabnotchingsite.csv")
  
  set.seed(2824)
  
  CastleRock <- filter(pg_cr_comp, Site=='Castle Rock')
  Pojoaque <- filter(pg_cr_comp, Site=='Pojoaque Grant')
  
  #tests between means of sites
  t.test(CastleRock$Weight, Pojoaque$Weight, paired = F, var.equal=T)
  vartest1<-var.test(CastleRock$Weight, Pojoaque$Weight)
  vartest1
  
  ttest <- t.test(CastleRock$Weight, Pojoaque$Weight, var.equal = T)
  ttest
  
  #manova for length and width
  
  mnorm.test<- pg_cr_comp %>% mshapiro.test(Length, Width)
  mnorm.test <- mshapiro_test(pg_cr_comp[, 19:20])
  mnorm.test
  
  res.man <- manova(cbind(Length, Width) ~ Type, data = pg_cr_comp)
  summary(res.man)
  
  myVAR=pg_cr_comp$Weight
  mydata=pg_cr_comp

  mu <- ddply(pg_cr_comp, 'Site', summarise, grp.mean=mean(Weight))
  mu
  names <- c("mean"="#F85700")
  c2.t <- str_wrap("Projectile Weight Distributions Across High and Low Hunting Investment Sites", 80)
  c3.t <- str_wrap("Projectile Width Distributions Across High and Low Hunting Investment Sites", 80)
  c4.t <- str_wrap("Projectile Length Distributions Across High and Low Hunting Investment Sites", 80)
 
   ggplot(data=mydata,aes(x=myVAR, fill=Site))+
    ggtitle(c2.t)+
    scale_fill_manual(values = wes_palette(n=2, name="Cavalcanti1"))+
    geom_density(alpha=.5)+
    theme(plot.title=element_text(hjust=0.5))+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Frequency")+xlab("Weight (g)")+
    geom_vline(data=mu, aes(xintercept=grp.mean, colour="mean"),
               color="#F85700", linetype="dashed", linewidth=1)+
    scale_colour_manual(name = "Statistics", values = names) +
    #scale_color_manual(values = c("Mean" = "#F85700"))+
    #scale_x_continuous(breaks=seq(0, 1400, 200), expand = c(.05,50))+
    #scale_y_continuous(breaks=seq(0, 12, 2))+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .5),
          panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))
  
   #linear model of x1 and x2 log
   x1<-pg_cr_comp$Length
   x2<-pg_cr_comp$Width
   ap.lm <- lm(x1~x2,pg_cr_comp)
   summary(ap.lm) 
   
   ap.lm2 <- lm(x2~x1,pg_cr_comp)
   summary(ap.lm2) 
   
   hist(ap.lm$residuals)
   shapiro.test(ap.lm$residuals)
   
   t_pgcr <- str_wrap("Length and Width for Castle Rock Pueblo and the Pojoaque Grant Site by Site and Type", 55)
   pgcr <- ggplot(pg_cr_comp, aes(x2, x1, color = Site, shape=Type))
   pgcr + geom_point(size=4) + geom_smooth(aes(group = 1), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
     theme(plot.title=element_text(hjust=0.45))+
     ggtitle(t_pgcr)+
     scale_color_startrek()+
     #geom_text(hjust=-0.1, vjust=0.1, size=3, color='black', family="serif")+
     theme(text=element_text(size=16, face="bold",  family="serif"))+
     ylab("Length")+
     xlab("Width")+
     theme(axis.text.x = element_text(face="bold", color="#000000", 
                                      size=13, angle=0),
           axis.text.y = element_text(face="bold", color="#000000", 
                                      size=13, angle=0),
           panel.background = element_rect(color = "black"),
           panel.grid.major = element_line(color = "black", size = .5),
           panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))
  #xtabs totals for site datasets by type
   pg.SitexType<-xtabs(~Site+Type,Pojoaque)
   pg.SitexType
   cr.SitexType<-xtabs(~Site+Type,CastleRock)
   cr.SitexType
   
   #numerical summaries of metrics for points by site and type, then type
   #site length
   pgcr_stats_table_length<-pg_cr_comp %>% 
     dplyr::group_by(Site, Type) %>% 
     dplyr::summarise(mean = mean(Length), sd = sd(Length),
               'Max' = max(Length),
               'Min' = min(Length),
               n = n())
   write.csv(pgcr_stats_table_length, file="pgcr_stats_table_length.csv")
   
   #site width
   pgcr_stats_table_width<-pg_cr_comp %>% 
     dplyr::group_by(Site, Type) %>% 
     dplyr::summarise(mean = mean(Width), sd = sd(Width),
                      'Max' = max(Width),
                      'Min' = min(Width),
                      n = n())
   write.csv(pgcr_stats_table_width, file="pgcr_stats_table_width.csv")
   
   #site weight
   pgcr_stats_table_weight2<-pg_cr_comp %>% 
     dplyr::group_by(Site) %>% 
     dplyr::summarise(mean = mean(Weight), sd = sd(Weight),
                      'Max' = max(Weight),
                      'Min' = min(Weight),
                      n = n())
   write.csv(pgcr_stats_table_weight2, file="pgcr_stats_table_weight2.csv")
   
   #type weight
   pgcr_stats_table_type_weight<-pg_cr_comp %>% 
     dplyr::group_by(Type) %>% 
     dplyr::summarise(mean = mean(Weight), sd = sd(Weight),
                      'Max' = max(Weight),
                      'Min' = min(Weight),
                      n = n())
   write.csv(pgcr_stats_table_type_weight, file="pgcr_stats_table_type_weight.csv")
   
   #type length
   pgcr_stats_table_type_length<-pg_cr_comp %>% 
     dplyr::group_by(Type) %>% 
     dplyr::summarise(mean = mean(Length), sd = sd(Length),
                      'Max' = max(Length),
                      'Min' = min(Length),
                      n = n())
   write.csv(pgcr_stats_table_type_length, file="pgcr_stats_table_type_length.csv")
   
   #type width
   pgcr_stats_table_type_width<-pg_cr_comp %>% 
     dplyr::group_by(Type) %>% 
     dplyr::summarise(mean = mean(Width), sd = sd(Width),
                      'Max' = max(Width),
                      'Min' = min(Width),
                      n = n())
   write.csv(pgcr_stats_table_type_width, file="pgcr_stats_table_type_width.csv")
   
   pg_corner <- pg_cr_comp %>%
     dplyr::filter(Site=='Pojoaque Grant') %>%
     dplyr::filter(Type=='Corner Notched')
   
   cr_corner <- pg_cr_comp %>%
     dplyr::filter(Site=='Castle Rock') %>%
     dplyr::filter(Type=='Corner Notched')
   
   pg_side <- pg_cr_comp %>%
     dplyr::filter(Site=='Pojoaque Grant') %>%
     dplyr::filter(Type=='Side Notched')
   
   cr_side <- pg_cr_comp %>%
     dplyr::filter(Site=='Castle Rock') %>%
     dplyr::filter(Type=='Side Notched')
   
   corner <- pg_cr_comp %>%
     dplyr::filter(Type=='Corner Notched')
   
   side <- pg_cr_comp %>%
     dplyr::filter(Type=='Side Notched')
   
   data1 <- corner$Weight
   data2 <- side$Weight
   data1log <- log(data1)
   data2log <-log(data2)
   
   vartest2<-var.test(data1, data2)
   vartest2
   
   ttest2 <- t.test(data1, data2, var.equal = T)
   ttest2
   
   vartest2<-var.test(data1log, data2log)
   vartest2
   
   ttest2 <- t.test(data1log, data2log, var.equal = T)
   ttest2
   
   shapiro.test(data1log)
   
   # creating a frequency table using col1 columns of data frame
   corner_side.tbl<-pg_cr_comp %>%
     # now required with changes to dplyr::count()
     group_by(Site) %>%
     count(Type) %>%        
     mutate(prop = prop.table(n))
   write.csv(corner_side.tbl, file="pgcr_type_prop.tbl.csv")
   
   
   #-----------------
  
  #Confusion tables
  #-----------------
  conftabl <- function(dataset, var1, var2, name) {
    name <- xtabs(~var1+var2, dataset)
    name
  }
  
  #tsq
  brxtabt <- xtabs(~Breakage+Rework, tsqsub)
  brxtabt
  write.csv(brxtabt, file="brxtabt.csv")
  #conftabl(tsqsub, Breakage, Rework, brxtabt)
  
  bsxtabt <- xtabs(~Breakage+Season, tsqsub)
  bsxtabt
  write.csv(bsxtabt, file="bsxtabt.csv")
  
  rsxtabt <- xtabs(~Rework+Season, tsqsub)
  rsxtabt
  write.csv(rsxtabt, file="rsxtabt.csv")
  
  bsitextabt <- xtabs(~Breakage+Site, tsqsub)
  bsitextabt
  write.csv(bsitextabt, file="bsitextabt.csv")
  
  mbxtabt <- xtabs(~Material+Breakage, tsqsub)
  mbxtabt
  write.csv(mbxtabt, file="mbxtabt.csv")
  
  #la835
  brxtabl <- xtabs(~Breakage+Rework, lasub)
  brxtabl
  write.csv(brxtabl, file="brxtabl.csv")
  
  bsxtabl <- xtabs(~Breakage+Season, lasub)
  bsxtabl
  write.csv(bsxtabl, file="bsxtabl.csv")
  
  rsxtabl <- xtabs(~Rework+Season, lasub)
  rsxtabl
  write.csv(rsxtabl, file="rsxtabl.csv")
  
  mrxtabl <- xtabs(~Material+Rework, lasub)
  mrxtabl

  mbxtabl <- xtabs(~Material+Breakage, lasub)
  mbxtabl
  write.csv(mbxtabl, file="mbxtabl.csv")
  
  #filter for no NA notching
  
  lasubfiltnotch<-lasub %>%
    filter(Notching!='NA')
  
  nrxtabl <- xtabs(~Notching+Rework, lasubfiltnotch)
  nrxtabl
  
  nbxtabl <- xtabs(~Notching+Breakage, lasubfiltnotch)
  nbxtabl
  
  nmxtabl <- xtabs(~Notching+Material, lasub)
  nmxtabl
  
  #breakage not na and include all relevant numeric variables so far
  br <- subset(data_chars_as_num,Breakage!="na",select = c(4,6,10:14))  
  #subset of br for rework not na and only rework and breakage columns
  br2 <- subset(br,Rework!="na",select = c(6:7))
  
  View(br2)
  #simple matching matrix for breakage and rework
  smbr2 <- as.matrix(sm(br2))
  confusion<-xtabs(~., smbr2)
  
  View(br2)
  #-----------------
  
  #Plot setup
  #----------------- 
  lm_eqn <- function(lm){
    m <- lm;
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  
  #linear model of proj and artio log
  ap.lm <- lm(log(artioctprop)~log(projctprop),arakawacomp)
  summary(ap.lm) 
  
  #linear model of proj and turkey log
  ap.lm <- lm(log(projctprop)~log(artioctprop),arakawacomp)
  summary(ap.lm)
  
  ap.lm <- lm(log(artioctprop)~log(projctprop),arakawacompnomv)
  summary(ap.lm) 
  
  hist(ap.lm$residuals)
  shapiro.test(ap.lm$residuals)
  
  arakawacompnomv$residuals <- ap.lm$residuals
  ggqqplot(arakawacomp, residuals, facet.by = arakawacomp$District)
  
  # Compute the analysis of variance for residuals
  nocu.aov <- aov(residuals ~ District, data = arakawacompnocu)
  summary(nocu.aov)
  
  t.resibp<-str_wrap("Log-scale Residuals of Linear Model for Study Region Sites by District", 75)
  resibp<-ggplot(arakawacompnomv, aes(x=District,y=residuals, fill=District))
  resibp+geom_boxplot(size=1)+
    scale_fill_npg()+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(t.resibp)+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Residuals")+
    xlab("District")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .25),
          panel.grid.minor = element_line(color = "black", size = .1, linetype = "dashed"))
  
  a <- ggplot(arakawacompnomv, aes(projctprop, artioctprop, color=Area))
  a + geom_point()
  #x, y, alpha, color, fill, shape, size, stroke
  b <- ggplot(arakawacompnomv, aes(projctprop, ai, color=Area))
  b + geom_point()
  
  c <- ggplot(arakawacompnomv, aes(log(artioctprop), log(projctprop)))
  c + geom_point() + geom_smooth(method = lm)
  
  #nomv proj for artio
  td <- str_wrap("Relative Ancestral Puebloan Projectile Point Use for Large Game Processing Across Districts", 55)
  d <- ggplot(arakawacompnomv, aes(log(artioctprop), log(projctprop), color = District))
  d + 
    geom_vline(aes(xintercept=mean(log(projctprop))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_hline(aes(yintercept=mean(log(artioctprop))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_point(size=4.5)+
    geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -1.75, label.x = -8.8, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -2.25, label.x = -8.8, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(td)+
    #scale_y_discrete(guide = guide_axis(n.dodge=3))+
    geom_label_repel(aes(label=Site.name),color = 'black', size = 3, family = "serif", label.size = NA, fill = NA)+
    scale_color_startrek()+
    #geom_text(hjust=-0.1, vjust=0.1, size=3, color='black', family="serif")+
    theme(text=element_text(size=20, face="bold",  family="serif"))+
    ylab("Projectile Point Use Rate")+
    xlab("Large Game Processing Rate")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", linewidth = .5),
          panel.grid.minor = element_line(color = "black", linewidth = .2, linetype = "dashed"))
  
  #nomv artio for proj
  te <- str_wrap("Relative Ancestral Puebloan Hunting Investment Across Districts", 65)
  e <- ggplot(arakawacompnomv, aes(log(projctprop),log(artioctprop), color = District))
  e + 
    geom_vline(aes(xintercept=mean(log(projctprop))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_hline(aes(yintercept=mean(log(artioctprop))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_point(size=4.5)+
    geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black',level=.9, formula = 'y ~ x')+
    stat_regline_equation(family = "serif", size = 6,
      label.y = -1.75, label.x = -6.8, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
    stat_regline_equation(family = "serif", size = 6,
      label.y = -2.25, label.x = -6.8, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(te)+
    geom_label_repel(aes(label=Site.name),color = 'black', size = 3.5, family = "serif", label.size = NA, fill = NA)+
    scale_color_startrek()+
    theme(text=element_text(size=20, face="bold",  family="serif"))+
    ylab("Large Game Processing Rate")+
    xlab("Projectile Point Use Rate")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", linewidth = .5),
          panel.grid.minor = element_line(color = "black", linewidth = .2, linetype = "dashed"))
  
  #nomv turkey for proj
  tf <- str_wrap("Log-scale Turkey and Projectile Point Grayware Proportions for Study Region Sites by District", 65)
  f <- ggplot(arakawaturkey, aes(log(projctprop),log(turkeyprop), color = District))
  f + geom_point(size=4)+
    geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -1.75, label.x = -7.4, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -2.25, label.x = -7.4, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(tf)+
    #scale_y_discrete(guide = guide_axis(n.dodge=3))+
    geom_label_repel(aes(label=Site.name),color = 'black', size = 3, family = "serif")+
    scale_color_startrek()+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Log-Transformed Turkey Proportions")+
    xlab("Log-Transformed Projectile Point Proportions")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", linewidth = .5),
          panel.grid.minor = element_line(color = "black", linewidth = .2, linetype = "dashed"))
  
  #nomv turkey for proj northern rio grande
  tg <- str_wrap("Log-scale Turkey and Projectile Point Grayware Proportions for Study Region Sites by District", 65)
  g <- ggplot(arakawaturkeynomy, aes(log(projctprop),log(turkeyprop), color = District, label=Site.name))
  g + geom_point(size=4)+
    geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -1.75, label.x = -7.4, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -2.25, label.x = -7.4, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(tg)+
    scale_color_startrek()+
    geom_text(hjust=-0.1, vjust=0.1, size=3, color='black', family="serif")+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Log-Transformed Turkey Proportions")+
    xlab("Log-Transformed Projectile Point Proportions")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", linewidth = .5),
          panel.grid.minor = element_line(color = "black", linewidth = .2, linetype = "dashed"))
  
  #nomv artio for sherds
  th <- str_wrap("Ancestral Puebloan Large Game Processing Rate for Sherd Counts", 65)
  h <- ggplot(arakawacompnomv, aes(y=log(artioctprop), x=log(Sherds), color = District))
  h + 
    geom_vline(aes(xintercept=mean(log(Sherds))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_hline(aes(yintercept=mean(log(artioctprop))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_point(size=4.5)+
    geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -6.25, label.x = 2.6, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -6.75, label.x = 2.6, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(th)+
    #scale_y_discrete(guide = guide_axis(n.dodge=3))+
    geom_label_repel(aes(label=Site.name),color = 'black', size = 3, family = "serif", label.size = NA, fill = NA)+
    scale_color_startrek()+
    #geom_text(hjust=-0.1, vjust=0.1, size=3, color='black', family="serif")+
    theme(text=element_text(size=20, face="bold",  family="serif"))+
    ylab("Large Game Processing Rate")+
    xlab("Sherd Count (log)")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", linewidth = .5),
          panel.grid.minor = element_line(color = "black", linewidth = .2, linetype = "dashed"))
  
  #nomv proj for sherds
  ti <- str_wrap("Ancestral Puebloan Projectile Point Use for Sherd Counts", 65)
  i <- ggplot(arakawacompnomv, aes(y=log(projctprop), x=log(Sherds), color = District))
  i + 
    geom_vline(aes(xintercept=mean(log(Sherds))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_hline(aes(yintercept=mean(log(projctprop))), color="#F85750", linetype="dashed", linewidth=1)+
    geom_point(size=4.5)+
    geom_smooth(aes(group = NULL), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -6.25, label.x = 2.6, aes(group = 1, label = ..eq.label..),show.legend = FALSE)+
    stat_regline_equation(family = "serif", size = 6,
                          label.y = -6.75, label.x = 2.6, aes(group=1,label = ..rr.label..),show.legend = FALSE)+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(ti)+
    #scale_y_discrete(guide = guide_axis(n.dodge=3))+
    geom_label_repel(aes(label=Site.name),color = 'black', size = 3, family = "serif", label.size = NA, fill = NA)+
    scale_color_startrek()+
    #geom_text(hjust=-0.1, vjust=0.1, size=3, color='black', family="serif")+
    theme(text=element_text(size=20, face="bold",  family="serif"))+
    ylab("Projectile Point Use Rate")+
    xlab("Sherd Count (log)")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=16, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", linewidth = .5),
          panel.grid.minor = element_line(color = "black", linewidth = .2, linetype = "dashed"))
  #grouped data
  a <- ggplot(arakawagroup1, aes(artioctprop, projctprop, color=District))
  a + geom_point()
  #x, y, alpha, color, fill, shape, size, stroke
  b <- ggplot(arakawagroup, aes(projctprop, ai, color=Area))
  b + geom_point()
  
  grouptc <- str_wrap("Log-transformed Artiodactyl and Projectile Point Proportions with Grayware for Study Region Sites by District and Habitation Size", 45)
  c <- ggplot(arakawagroup1, aes(log(artioctprop),log(projctprop), color=District, shape=site_type))
  c + geom_point(size=3) + geom_smooth(aes(group = 1), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(grouptc)+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Log-Transformed Projectile Point Proportion")+xlab("Log-Transformed Artiodactyl Proportions")+
    #scale_x_continuous(breaks=seq(0, 8, .5), expand = c(.05,.2))+
    #scale_y_continuous(breaks=seq(0, 12, 2))+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .5),
          panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))
  
  grouptc2 <- str_wrap("Log-transformed Artiodactyl and Projectile Point Proportions with Grayware for Study Region Sites by District", 65)
  c2 <- ggplot(arakawagroup3, aes(log(artioctprop),log(projctprop), color=District))
  c2 + geom_point(size=4.5) + geom_smooth(aes(group = 1), method = "lm", se=F, color = 'black', formula = 'y ~ x')+
    theme(plot.title=element_text(hjust=0.45))+
    ggtitle(grouptc2)+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Log-Transformed Projectile Point Proportion")+xlab("Log-Transformed Artiodactyl Proportions")+
    #scale_x_continuous(breaks=seq(0, 8, .5), expand = c(.05,.2))+
    #scale_y_continuous(breaks=seq(0, 12, 2))+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .5),
          panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))
  
  #grouped regression and boxplot of residuals
  apg3.lm <- lm(log(projctprop)~log(artioctprop),arakawagroup3)
  summary(apg3.lm)
  
  apg1.lm <- lm(log(projctprop)~log(artioctprop),arakawagroup1)
  summary(apg1.lm) 
  
  hist(apg3.lm$residuals)
  
  residualsg <- as.data.frame(apg3.lm$residuals)
  
  ggplot(residualsg, aes(y=residualsg))+
    geom_boxplot()
  
  d <- ggplot(arakawagroup, aes(log(projctprop), ai, color=District))
  d + geom_point()
  
  mydata=tsqsub
  myVAR=log(data_chars_as_num$Weight)
  myVAR=data_chars_as_num$Breakage
  h1.t = str_wrap("Weight", 40)
  mybw=.5
  
  #Plots
  
  shapiro.test(log(arakawacompnomv$projctprop))
  shapiro.test(log(arakawacompnomv$artioctprop))
  
  
  mydata=arakawacompnomv
  mybw=.5
  myVAR=log(arakawacompnomv$projctprop)
  myVAR=log(arakawacompnomv$artioctprop)
  
  myVAR=arakawacomp$ai
  mybw=.15
  
  myVAR=arakawacomp$Points
  mybw=20
  
  myVAR=arakawacomp$Sherds
  mybw=2000
  
  #distribution hist plots for arakawacomp data
  h1.t <- str_wrap("Distribution", 50)
  ggplot(data=mydata,aes(x=myVAR, fill=Area))+
    geom_histogram(binwidth=mybw, color="black")+
    ggtitle(h1.t)+
    scale_fill_manual(values = wes_palette(n=5, name="Cavalcanti1"))+
    geom_density(aes(y=..density..*(nrow(mydata))*mybw), alpha=.25, fill="#F85700")+
    theme(plot.title=element_text(hjust=0.5))+
    theme(text=element_text(size=16, face="bold",  family="serif"))+
    ylab("Frequency")+xlab("Value")+
    geom_vline(aes(xintercept=mean(myVAR)), color="#009CFF", linetype="solid", size=1)+
    geom_vline(aes(xintercept=median(myVAR)),color="#009CFF", linetype="dashed", size=1)+
    #scale_x_continuous(breaks=seq(0, 1400, 200), expand = c(.05,50))+
    #scale_y_continuous(breaks=seq(0, 12, 2))+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=13, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .5),
          panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))

  
  q1.1.t <- str_wrap("Weight and Portion", 50)
  
  ggplot(data=mydata,aes(x=Rework, y=Breakage))+
    geom_smooth(alpha=.2,method="lm", se=F)+
    geom_point(alpha=0.8, size=3.5, color='black', pch=21, fill='blue')+
    ggtitle(q1.1.t)+
    theme(plot.title=element_text(hjust=0.5))+
    theme(text=element_text(size=17.5, face="bold",  family="serif"))+
    ylab("Total People")+
    xlab("Number of Wives")+
    theme(axis.text.x = element_text(face="bold", color="#000000", 
                                     size=15, angle=0),
          axis.text.y = element_text(face="bold", color="#000000", 
                                     size=15, angle=0),
          panel.background = element_rect(color = "black"),
          panel.grid.major = element_line(color = "black", size = .5),
          panel.grid.minor = element_line(color = "black", size = .2, linetype = "dashed"))  

  shapiro.test(log(tsqsub$Weight))  
  
  #testm<-as.data.frame(complete.cases(data_chars_as_num[,14:17]))
  #View(testm)
  #-----------------
  
     