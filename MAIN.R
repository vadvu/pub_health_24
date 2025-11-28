# REPLICATION MATERIALS FOR ``MORTALITY IN RUSSIA: PROBLEM STATEMENT AND WAYS TO REDUCE IT``
# last update: 29.11.2025

# ---- Version of R ----
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          5.1                         
# year           2025                        
# month          06                          
# day            13                          
# svn rev        88306                       
# language       R                           
# version.string R version 4.5.1 (2025-06-13)
# nickname       Great Square Root 

# ---- Packages ----

library(demor) #1.0.6, to install run devtools::install_github("vadvu/demor")
library(ggplot2) #4.0.0
library(ggpubr) #0.6.1
library(dplyr) #1.1.4
library(tidyr) #1.3.1
library(ggsci) #3.2.0
library(readr) #2.1.5
library(openxlsx) #4.2.8
library(ggrepel) #0.9.6
`%notin%` <- Negate(`%in%`)


# ---- 1) FIG. 1 ----

rosbris5all <- demor::rosbris_mortality_pop_5

for (i in 1993:2022){
  for(j in c("m","f")){
    ledati = data.frame(year = i, sex = j, le = NA)
    ledati[1,3] <- LT(age = unique(rosbris5all$age), 
                      sex = j, 
                      mx = rosbris5all[rosbris5all$year==i & 
                                         rosbris5all$sex==j & 
                                         rosbris5all$territory == "t" & 
                                         rosbris5all$code==1100,]$mx)[1,"ex"]
    if(i==1993 & j=="m"){
      ledat <- ledati
    }else{
      ledat <- rbind(ledat,ledati)
    }
  }
}

rusall <- read_rds("data/HMD/Russia.rds")


for (i in 1960:1990){
  for(j in c("m","f")){
    ledati = data.frame(year = i, sex = j, le = NA)
    ledati[1,3] <- LT(age = unique(rusall$Age), 
                      sex = j, 
                      mx = rusall[rusall$Year==i & 
                                    rusall$sex==j,]$mx)[1,"ex"]
    ledat <- rbind(ledat,ledati)
  }
}

ledat$cnt = "RUS"

ggplot(ledat[ledat$cnt == "RUS",], aes(year, le))+
  geom_line(aes(color = sex), linewidth = 1.3)+
  theme_bw()+
  labs(x = "Год", y = "ОПЖ (при рождении, лет)", color = "Пол:")+
  scale_color_manual(values = c("red", "darkblue"), labels = c("Женский", "Мужской"))+
  geom_vline(xintercept = 2004, linetype = "dashed", alpha = 0.5, linewidth = 1.1)+
  scale_x_continuous(breaks = c(seq(1960, 2015, 5), 2019, 2022), guide = guide_axis(n.dodge = 2))

ggsave("plots/FIG_1.png", device = "png", dpi = 600, units = "cm", width = 20, height = 10)


# ---- 2) FIG. 2 + FIG 4a ----

ledat <- read_rds("data/HMD/ALL_e0.rds")
ledat1 <- ledat %>% filter(year >= 2000)
ledat1$le_pr <- NA

for (i in unique(ledat1$cnt)){
  for (j in c("m","f")){
    ledat1[ledat1$cnt==i & ledat1$sex == j,]$le_pr <- ledat1[ledat1$cnt==i & ledat1$sex == j,]$le/
      ledat1[ledat1$cnt==i & ledat1$sex == j & ledat1$year == 2000,]$le
  }
}

ledat1$sex = ifelse(ledat1$sex =="m", "Мужчины", "Женщины")

ggplot(ledat1 %>% mutate(line = ifelse(cnt == "RUS", 0, 1)), 
       aes(year, le_pr, color = cnt, linetype = factor(line)))+
  geom_line(size = 1)+
  facet_grid(~sex)+
  theme_bw()+
  scale_y_continuous(labels = scales::percent)+
  scale_color_aaas()+
  labs(x = "Год", y = "ОПЖ к 2000 году (%)", color = "Страна:")+
  guides(linetype = "none")

ggsave("plots/FIG_2.png", device = "png", dpi = 600, units = "cm", width = 20, height = 10)


ggplot(ledat %>% mutate(line = ifelse(cnt == "RUS", 0, 1)), 
       aes(year, le, color = cnt, linetype = factor(line)))+
  geom_line()+
  facet_wrap(~sex1, nrow = 1)+
  theme_bw()+
  labs(x = "Год", y = "ОПЖ при рождении (лет)", color = "Страна:")+
  scale_y_continuous(breaks = seq(55,90,5))+
  scale_x_continuous(breaks = c(seq(1960, 2020, 5)), guide = guide_axis(n.dodge = 2))+
  scale_color_aaas()+
  guides(linetype = "none")

ggsave("plots/FIG_4a.png", device = "png", dpi = 600, units = "cm", width = 20, height = 10)

# ---- 3) FIG. 3a, 3b ----

## ---- 3.1) 2000 data ----

cd00_05 <- readr::read_csv("data/CAUSE/RUS_m_short_idr.csv")

cd00_05 <- cd00_05 %>%
  select(-m85, -m90p, -m90, -m95p, -m95, -m100p) %>% 
  rename(m85 = m85p) %>% 
  pivot_longer(cols = starts_with("m"), names_to = "age",
               values_to = "mx") %>%
  mutate(age = gsub("m","", age)) %>%
  mutate (age = as.numeric(age),
          mx = mx / 1000000) %>%
  select(year, age, sex, cause, mx) %>% 
  mutate(cause = case_when(
    cause == 0 ~ 0,
    cause == 1 ~ 1,
    cause == 2 ~ 2,
    cause == 3 ~ 3,
    cause == 4 ~ 4,
    cause == 5 ~ 5,
    cause == 6 ~ 6,
    cause %in% 7:9 ~ 7,
    cause %in% 10:11 ~ 10,
    cause == 12 ~ 12, 
    cause == 13 ~ 13,
    cause == 14 ~ 14,
    cause == 15 ~ 15,
    cause == 16 ~ 16
  )) %>% 
  mutate(sex = case_when(
    sex == 1 ~ "m",
    sex == 2 ~ "f"
  )) %>% 
  drop_na()


cd00 <- aggregate(mx ~ age + sex + cause, 
                  data = cd00_05[cd00_05$cause!=0 & cd00_05$year == 2000,], FUN = sum) %>% as.data.frame()

## ---- 3.2) 2019 data ----

rostat190 <- openxlsx::read.xlsx("data/CAUSE/rus2019causes.xlsx")

rostat19 <- rbind(rostat190[,c(1:22)], rostat190[,c(1:2,23:42)])

rostat19 <- rostat19 %>% 
  filter(cause >= 1000) %>% 
  mutate(cause = cause - 1000) %>% 
  pivot_longer(cols = starts_with("m"), names_to = "age",
               values_to = "Dx") %>%
  mutate(age = gsub("m","", age)) %>% 
  mutate(age = as.numeric(age))

rostat19 <- left_join(rostat19, 
                      rosbris5all[rosbris5all$year == 2019 & 
                                    rosbris5all$code == 643 & 
                                    rosbris5all$territory == "t" & 
                                    rosbris5all$sex != "b",c("sex", "age", "N")], 
                      by = c("sex", "age")
)

rostat19$mx <- rostat19$Dx/rostat19$N

rostat_decomp <- rostat19 %>% filter(cause %in% c(0:16, 999))

redistr <- data.frame(sex = c(rep("m", 19), rep("f", 19)), 
                      age = rep(unique(rostat_decomp$age), 2),
                      Dx_w = c(
                        rostat_decomp[rostat_decomp$cause == 999 & 
                                        rostat_decomp$sex == "m",]$Dx,
                        rostat_decomp[rostat_decomp$cause == 999 & 
                                        rostat_decomp$sex == "f",]$Dx
                      )
)

rostat_decomp$c_w <- NA 

for (sex in c("m","f")){
  for(age in unique(rostat_decomp$age)){
    for(i in 1:16){
      rostat_decomp[rostat_decomp$sex == sex & 
                      rostat_decomp$age == age & 
                      rostat_decomp$cause == i,]$c_w <- 
        rostat_decomp[rostat_decomp$sex == sex &
                        rostat_decomp$age == age & 
                        rostat_decomp$cause == i,]$Dx/
        rostat_decomp[rostat_decomp$sex == sex &
                        rostat_decomp$age == age & 
                        rostat_decomp$cause == 0,]$Dx
    }
  }
}

rostat_decomp$Dx_new <- NA 

for (sex in c("m","f")){
  for(age in unique(rostat_decomp$age)){
    for(i in 1:16){
      rostat_decomp[rostat_decomp$sex == sex & 
                      rostat_decomp$age == age & 
                      rostat_decomp$cause == i,]$Dx_new <- 
        rostat_decomp[rostat_decomp$sex == sex &
                        rostat_decomp$age == age & 
                        rostat_decomp$cause == 999,]$Dx * 
        rostat_decomp[rostat_decomp$sex == sex & 
                        rostat_decomp$age == age & 
                        rostat_decomp$cause == i,]$c_w +
        rostat_decomp[rostat_decomp$sex == sex &
                        rostat_decomp$age == age & 
                        rostat_decomp$cause == i,]$Dx
    }
  }
}
rostat_decomp$Dx_new <- round(rostat_decomp$Dx_new)
rostat_decomp$mx_new <- rostat_decomp$Dx_new/rostat_decomp$N


## ---- 3.3) Merging ----

mx1 <- list()
mx2 <- list()

for(sex in c("m", "f")){
  for (i in 1:16){
    if(i %notin% unique(rostat_decomp$cause)){
      next
    }else{
      mx2[[paste0(sex, i)]] <- rostat_decomp[rostat_decomp$cause == i & 
                                               rostat_decomp$sex == sex,]$mx_new
      
      mx1[[paste0(sex, i)]] <- cd00[cd00$cause == i & 
                                      cd00$sex == sex,]$mx
    }
  }
  
  allin <- c()
  st <- ifelse(sex == "m", 1, 15)
  for (i in 1:19){
    specage = 0
    for(j in st:(st+12)){
      specage <- specage + mx2[[j]][i] 
    }
    allin <- c(allin, specage)
  }
  
  mx2[[paste0(sex, 0)]] <- allin
  rm(allin, specage)
  
  allin <- c()
  st <- ifelse(sex == "m", 1, 15)
  for (i in 1:19){
    specage = 0
    for(j in st:(st+12)){
      specage <- specage + mx1[[j]][i] 
    }
    allin <- c(allin, specage)
  }
  
  mx1[[paste0(sex, 0)]] <- allin
  
}

mx <- mx1[1:3]


## ---- 3.4) Decomposition ----

### ---- 3.4.1) Males ----

mxm1 <- mx1[c(14, 1:13)]
mxm2 <- mx2[c(14, 1:13)]

names(mxm1) <- c(
  "all",
  "Некоторые инфекционные\nи паразитарные болезни",
  "Новообразования",
  "Болезни крови, кроветворных органов\nи отдельные нарушения с\nвовлечением иммунного механизма",
  "Болезни эндокринной системы, \nрасстройства питания и\nнарушения обмена веществ",
  "Психические расстройства\nи расстройства поведения",
  "Болезни нервной системы\nи органов чувств",
  "Болезни системы кровообращения",
  "Болезни органов дыхания",
  "Болезни органов пищеварения",
  "Заболевания кожи и подкожной клетчатки,\nкостно-мышечной системы\nи соединительной ткани",
  "Заболевания мочеполовой системы\nи осложнения беременности,\nроды и послеродовой период",
  "Некоторые состояния,\nвозникшие в перинатальном периоде,\nи врожденные пороки развития/аномалии",
  "Внешние причины"
)
names(mxm2) <- c(
  "all",
  "Некоторые инфекционные и паразитарные болезни",
  "Новообразования",
  "Болезни крови, кроветворных органов\nи отдельные нарушения с\nвовлечением иммунного механизма",
  "Болезни эндокринной системы, \nрасстройства питания и\nнарушения обмена веществ",
  "Психические расстройства\nи расстройства поведения",
  "Болезни нервной системы\nи органов чувств",
  "Болезни системы кровообращения",
  "Болезни органов дыхания",
  "Болезни органов пищеварения",
  "Заболевания кожи и подкожной клетчатки,\nкостно-мышечной системы\nи соединительной ткани",
  "Заболевания мочеполовой системы\nи осложнения беременности,\nроды и послеродовой период",
  "Некоторые состояния,\nвозникшие в перинатальном периоде,\nи врожденные пороки развития/аномалии",
  "Внешние причины"
)


decm <- mdecomp(mx1 = mxm1, 
                mx2 = mxm2, 
                sex = "m", 
                age = unique(rostat_decomp$age)
)


plot(decm) +
  geom_bar(stat="identity", colour = "black", linewidth = 0.25)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Возраст", y = "Вклад в разницу в ОПЖ (лет)", fill = "Причина (вклад):"
  )+
  annotate("text", x = 6, y = 0.85, label = paste0("Общая разница в ОПЖ = ", sum(decm[,2])))

ggsave("plots/FIG_3a.png", device = "png", dpi = 600, units = "cm", width = 25, height = 15)


### ---- 3.4.2) Females ----

mxf1 <- mx1[c(28, 15:(28-1))]
mxf2 <- mx2[c(28, 15:(28-1))]

names(mxf1) <- c(
  "all",
  "Некоторые инфекционные\nи паразитарные болезни",
  "Новообразования",
  "Болезни крови, кроветворных органов\nи отдельные нарушения с\nвовлечением иммунного механизма",
  "Болезни эндокринной системы, \nрасстройства питания и\nнарушения обмена веществ",
  "Психические расстройства\nи расстройства поведения",
  "Болезни нервной системы\nи органов чувств",
  "Болезни системы кровообращения",
  "Болезни органов дыхания",
  "Болезни органов пищеварения",
  "Заболевания кожи и подкожной клетчатки,\nкостно-мышечной системы\nи соединительной ткани",
  "Заболевания мочеполовой системы\nи осложнения беременности,\nроды и послеродовой период",
  "Некоторые состояния,\nвозникшие в перинатальном периоде,\nи врожденные пороки развития/аномалии",
  "Внешние причины"
)
names(mxf2) <- c(
  "all",
  "Некоторые инфекционные и паразитарные болезни",
  "Новообразования",
  "Болезни крови, кроветворных органов\nи отдельные нарушения с\nвовлечением иммунного механизма",
  "Болезни эндокринной системы, \nрасстройства питания и\nнарушения обмена веществ",
  "Психические расстройства\nи расстройства поведения",
  "Болезни нервной системы\nи органов чувств",
  "Болезни системы кровообращения",
  "Болезни органов дыхания",
  "Болезни органов пищеварения",
  "Заболевания кожи и подкожной клетчатки,\nкостно-мышечной системы\nи соединительной ткани",
  "Заболевания мочеполовой системы\nи осложнения беременности,\nроды и послеродовой период",
  "Некоторые состояния,\nвозникшие в перинатальном периоде,\nи врожденные пороки развития/аномалии",
  "Внешние причины"
)

decf <- mdecomp(mx1 = mxf1, 
                mx2 = mxf2, 
                sex = "f", 
                age = unique(rostat_decomp$age)
)

plot(decf) +
  geom_bar(stat="identity", colour = "black", linewidth = 0.25)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3")+
  labs(x = "Возраст", y = "Вклад в разницу в ОПЖ (лет)", fill = "Причина (вклад):"
  )+
  annotate("text", x = 6, y = 0.85, label = paste0("Общая разница в ОПЖ = ", sum(decm[,2])))

ggsave("plots/FIG_3b.png", device = "png", dpi = 600, units = "cm", width = 25, height = 15)

## ---- 3.5) FIG. 3 Support ----

sdecm <- demor::decomp(rosbris5all[rosbris5all$year==2003 & 
                                     rosbris5all$code == 1100 & 
                                     rosbris5all$territory=="t" & 
                                     rosbris5all$sex=="m",]$mx,
                       rosbris5all[rosbris5all$year==2019 & 
                                     rosbris5all$code == 1100 & 
                                     rosbris5all$territory=="t" & 
                                     rosbris5all$sex=="m",]$mx,
                       sex = "m", age = unique(rosbris5all$age)) %>% 
  mutate(sex = "Мужчины")


sdecf <- demor::decomp(rosbris5all[rosbris5all$year==2003 & 
                                     rosbris5all$code == 1100 & 
                                     rosbris5all$territory=="t" & 
                                     rosbris5all$sex=="f",]$mx,
                       rosbris5all[rosbris5all$year==2019 & 
                                     rosbris5all$code == 1100 & 
                                     rosbris5all$territory=="t" & 
                                     rosbris5all$sex=="f",]$mx,
                       sex = "f", age = unique(rosbris5all$age)) %>% 
  mutate(sex = "Женщины")

sdec.all <- rbind(sdecm, sdecf)

ggplot(sdec.all %>% group_by(sex) %>% mutate(n = 1:n()), aes(x = n, y = ex12, fill = sex))+
  geom_bar(stat = "identity", width=1, alpha = 0.6, position = "dodge")+
  theme_classic()+
  scale_x_continuous(breaks = seq(1,19,1), labels = c(0, 1 , seq(5,85,5)))+
  scale_fill_manual(values = c("steelblue1", "deeppink"))+
  labs(x = "Возраст", y = "Вклад возраста в рост ОПЖ (лет)", fill = "")+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = mean(sdecf[,"ex12"]), 
             alpha = 0.6, size = 1.5, color = "deeppink2")+
  annotate("text", y = mean(sdecf[,"ex12"])*1.12, 
           x = 2, label = paste0("Общий рост: ", sum(sdecf$ex12)),
           color = "deeppink2")+
  geom_hline(yintercept = mean(sdecm[,"ex12"]), 
             size = 1.5, color = "steelblue2")+
  annotate("text", y = mean(sdecm[,"ex12"])*1.12, 
           x = 2, label = paste0("Общий рост: ", sum(sdecm$ex12)),
           color = "steelblue2")

ggsave("plots/SUPPORT_FIG_3.png", device = "png", dpi = 600, units = "cm", width = 20, height = 15)

# ---- 4) FIG 4b ----

## ---- 4.1) Main fig ----

wppe0 <- read_rds("data/WPP_e0.rds")
gdppc <- read.csv("data/WB_gdppc.csv") %>% rename(iso3c = Code, gdppc = 4, year = Year)

preston <- left_join(gdppc, wppe0, by = c("iso3c", "year"))

preston %>% 
  filter(year == 2019) %>% 
  mutate(rus = factor(ifelse(iso3c == "RUS", 1, 0)),
         lab = ifelse(iso3c == "RUS", "RUSSIA", "")) %>% 
  ggplot(aes(x = gdppc))+
  geom_point(aes(y = e0F, color = "Женщины", size = rus, alpha = rus))+
  geom_point(aes(y = e0M, color = "Мужчины", size = rus, alpha = rus))+
  theme_bw()+
  geom_smooth(aes(y = e0F, color = "Женщины"), 
              method = "lm", formula = y~log(x), 
              linetype = "twodash", se = F, linewidth = 1)+
  geom_smooth(aes(y = e0M, color = "Мужчины"), 
              method = "lm", formula = y~log(x), 
              linetype = "twodash", se = F, linewidth = 1)+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("red", "blue"))+
  scale_size_manual(values = c(2, 4))+
  scale_alpha_manual(values = c(0.1, 1))+
  guides(alpha = "none", size = "none")+
  labs(x = "ВВП на душу (2017$ по ППС) в 2019 году", y = "ОПЖ в 2019 году", color = "")

ggsave("plots/FIG_4b.png", device = "png", dpi = 600, units = "cm", width = 20, height = 15)

## ---- 4.2) Support fig ----

comp.dat = data.frame(iso3c = NA, e0b.2000 = NA, e0b.2019 = NA)

for(i in unique(wppe0$iso3c)){
  comp.dat <- rbind(comp.dat,
                    data.frame(iso3c = i, 
                               e0b.2000 = wppe0[wppe0$iso3c == i & wppe0$year == 2000,]$e0B/
                                 wppe0[wppe0$iso3c == "RUS" & wppe0$year == 2000,]$e0B, 
                               e0b.2019 = wppe0[wppe0$iso3c == i & wppe0$year == 2019,]$e0B/
                                 wppe0[wppe0$iso3c == "RUS" & wppe0$year == 2019,]$e0B
                    )
  ) %>% drop_na()
}

comp.dat %>% 
  mutate(rus = ifelse(iso3c == "RUS", 1, 0)) %>% 
  ggplot(aes(e0b.2000, e0b.2019))+
  geom_abline(intercept = 0, slope = 1)+
  geom_point(aes(color = factor(rus), size = factor(rus)))+
  ggrepel::geom_text_repel(aes(label = iso3c), 
                           size = 2, color = "darkgreen")+
  theme_classic()+
  scale_color_manual(values = c("blue", "red"))+
  scale_size_manual(values = c(1, 3))+
  guides(color = "none", size = "none")+
  labs(x = "ОПЖ в 2000 году (Россия = 1)", y = "ОПЖ в 2019 году (Россия = 1)")

ggsave("plots/SUPPORT_FIG_4.png", device = "png", dpi = 600, units = "cm", width = 20, height = 15)


# ---- 5) FIG. 5 ----

ledat0 <- ledat %>% filter(cnt == "RUS")

## ---- 5.1) Males ----

lcm <- leecart(data = 
                 rosbris5all[rosbris5all$territory=="t" & 
                               rosbris5all$code==1100 & 
                               rosbris5all$sex=="m" & 
                               rosbris5all$year %in% c(2003:2019),
                             c("age", "year", "mx", "Dx", "N")],
               ax_method = "classic",
               bx_method = "classic",
               ktadj = "Dmin", 
               n = 3
)

for(i in 2020:2022){
  lcm$ex0[lcm$ex0$year == i,]$e0.obs <- ledat0[ledat0$year == i & ledat0$sex == "m",]$le
}

# lcm$ex0$e0.obs - lcm$ex0$e0.hat
# lcm$ex0$e0.obs - lcm$ex0$conf.low
# lcm$ex0$e0.obs - lcm$ex0$conf.high

covm <- ggplot(lcm$ex0, aes(year, e0.obs))+
  geom_line(size = 1.3, color = "darkblue")+
  geom_line(data = lcm$ex0 %>% filter(year >= 2019), aes(x = year, y = e0.hat), color = "darkred", size = 1.3)+
  geom_ribbon(data = lcm$ex0 %>% filter(year >= 2019), 
              aes(x = year, ymin = conf.low, ymax = conf.high), 
              color = "darkred", linetype = "dashed", alpha = 0.2, fill = "darkred")+
  theme_bw()+
  labs(x = "Год", y = "ОПЖ (при рождении, лет)")+
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = 0.5, size = 1.1)+
  geom_hline(linetype = "dashed", yintercept = ledat0[ledat0$sex =="m" & ledat0$year == 2021,]$le)+
  annotate("text", 
           x = 2007, 
           y = ledat0[ledat0$sex =="m" & ledat0$year == 2021,]$le + 0.6, 
           label = paste0("Уровень 2021 года:\nОПЖ - ", ledat0[ledat0$sex =="m" & ledat0$year == 2021,]$le),
           size = 3
  )+
  scale_x_continuous(breaks = seq(2004,2022,2))+
  scale_y_continuous(breaks = seq(0,100,2))+
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))

## ---- 5.2) Females ----

lcf <- leecart(data = 
                 rosbris5all[rosbris5all$territory=="t" & 
                               rosbris5all$code==1100 & 
                               rosbris5all$sex=="f" & 
                               rosbris5all$year %in% c(2003:2019),
                             c("age", "year", "mx", "Dx", "N")],
               ax_method = "classic",
               bx_method = "classic",
               ktadj = "Dmin", n = 3)

for(i in 2020:2022){
  lcf$ex0[lcf$ex0$year == i,]$e0.obs <- ledat0[ledat0$year == i & ledat0$sex == "f",]$le
}

# lcf$ex0$e0.obs - lcf$ex0$e0.hat
# lcf$ex0$e0.obs - lcf$ex0$conf.low
# lcf$ex0$e0.obs - lcf$ex0$conf.high

covf <- ggplot(lcf$ex0, aes(year, e0.obs))+
  geom_line(size = 1.3, color = "red")+
  geom_line(data = lcf$ex0 %>% filter(year >= 2019), aes(x = year, y = e0.hat), color = "darkred", size = 1.3)+
  geom_ribbon(data = lcf$ex0 %>% filter(year >= 2019), 
              aes(x = year, ymin = conf.low, ymax = conf.high), 
              color = "darkred", linetype = "dashed", alpha = 0.2, fill = "darkred")+
  theme_bw()+
  labs(x = "Год", y = "")+
  geom_vline(xintercept = 2019, linetype = "dashed", alpha = 0.5, size = 1.1)+
  geom_hline(linetype = "dashed", yintercept = ledat0[ledat0$sex =="f" & ledat0$year == 2021,]$le)+
  annotate("text", 
           x = 2007, 
           y = ledat0[ledat0$sex =="f" & ledat0$year == 2021,]$le + 0.5, 
           label = paste0("Уровень 2021 года:\nОПЖ - ", ledat0[ledat0$sex =="f" & ledat0$year == 2021,]$le),
           size = 3
  )+
  scale_x_continuous(breaks = seq(2004,2022,2))+
  scale_y_continuous(breaks = seq(0,100,2))+
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust=1))

## ---- 5.3) Merging ----

ggarrange(covm, covf, nrow = 1)

ggsave("plots/FIG_5.png", device = "png", dpi = 600, units = "cm", width = 24, height = 11)



# ---- 6) FIG. 6 ----

## ---- 6.1) Main fig ----

yll_rates_19 <- data.frame(sex = c(rep("m", 13), rep("f", 13)),
                           cause = rep(unique(rostat19$cause)[-c(14:17)],2),
                           yll = NA, 
                           yll_p = NA)

for(i in 1:nrow(yll_rates_19)){
  
  yll_rates_19[i,"yll"] <- yll(type = "yll", 
                               Dx = rostat19[rostat19$cause == yll_rates_19[i,]$cause & 
                                               rostat19$sex == yll_rates_19[i,]$sex,]$Dx)[[1]]
  yll_rates_19[i,"yll_p"] <- yll(type = "yll.p", 
                                 Dx_all = rostat19[rostat19$cause == 0 & 
                                                     rostat19$sex == yll_rates_19[i,]$sex,]$Dx,
                                 Dx = rostat19[rostat19$cause == yll_rates_19[i,]$cause & 
                                                 rostat19$sex == yll_rates_19[i,]$sex,]$Dx)[[1]]
}


for (sex in c("m", "f")){
  for(i in unique(rostat19$cause)[-c(14:17)]){
    
    dd_i <- data.frame(sex = sex, age = unique(rostat19$age), cause = i, yll = NA, yll_p = NA)
    
    dd_i[,"yll"] <- yll(type = "yll",
                        Dx = rostat19[rostat19$cause == i & 
                                        rostat19$sex == sex,]$Dx)[[2]]
    
    dd_i[,"yll_p"] <- yll(type = "yll.p", 
                          Dx_all = rostat19[rostat19$cause == 0 &
                                              rostat19$sex == sex,]$Dx,
                          Dx = rostat19[rostat19$cause == i & 
                                          rostat19$sex == sex,]$Dx)[[2]]
    
    if(sex == "m" & i == 1){
      yll_gr <- dd_i
    }else{
      yll_gr <- rbind(yll_gr, dd_i)
    }
  }
}

yll_gr <- yll_gr %>% 
  mutate(age_gr = case_when(
    age %in% 0:19 ~ "0-19",
    age %in% 20:64 ~ "20-65",
    age >=65 ~ "65+" 
  ))

cas <- c(
  "Некоторые инфекционные и паразитарные болезни",
  "Новообразования",
  "Болезни крови, кроветворных органов\nи отдельные нарушения с\nвовлечением иммунного механизма",
  "Болезни эндокринной системы, \nрасстройства питания и\nнарушения обмена веществ",
  "Психические расстройства\nи расстройства поведения",
  "Болезни нервной системы\nи органов чувств",
  "Болезни системы кровообращения",
  "Болезни органов дыхания",
  "Болезни органов пищеварения",
  "Заболевания кожи и подкожной клетчатки,\nкостно-мышечной системы\nи соединительной ткани",
  "Заболевания мочеполовой системы\nи осложнения беременности,\nроды и послеродовой период",
  "Некоторые состояния,\nвозникшие в перинатальном периоде,\nи врожденные пороки развития/аномалии",
  "Внешние причины"
)

yll_gr$sex1 <- ifelse(yll_gr$sex == "m", "Мужчины", "Женщины")

ggplot(yll_gr, aes(x = as.factor(age), y = yll/1000, fill = as.factor(cause)))+
  geom_col(color = "black", linewidth = 0.1)+
  scale_fill_brewer(palette = "Set3", labels = cas)+
  theme_classic()+
  facet_wrap(.~sex1, ncol = 1)+
  labs(x = "Возраст", y = "YLL (тыс.)", fill = "Причина:")


ggsave("plots/FIG_6.png", device = "png", dpi = 600, units = "cm", width = 25, height = 20)


## ---- 6.2) Support fig ----

newyll22 = rbind(
  data.frame(age = unique(rosbris5all$age), sex = "m", 
             yll = yll(rosbris5all[rosbris5all$territory=="t" &
                                     rosbris5all$code==1100 &
                                     rosbris5all$year == 2019 & 
                                     rosbris5all$sex == "m",]$Dx, type = "yll")$yll),
  data.frame(age = unique(rosbris5all$age), sex = "f", 
             yll = yll(rosbris5all[rosbris5all$territory=="t" &
                                     rosbris5all$code==1100 &
                                     rosbris5all$year == 2019 & 
                                     rosbris5all$sex == "f",]$Dx, type = "yll")$yll)
)

ggplot(newyll22, aes(as.factor(age), yll/1000, fill = sex))+
  geom_bar(stat="identity", position=position_dodge(), color = "grey")+
  theme_bw()+
  scale_fill_manual(values = c("deeppink2", "steelblue3"), labels = c("Женский", "Мужской"))+
  labs(fill = "Пол:", x = "Возрастная группа", y = "YLL (в тыс.)")+
  annotate("text", x = 4, y = 3000, 
           label = paste0("YLL мужчин: ", round(sum(newyll22[newyll22$sex == "m",]$yll)/1000),"\n",
                          "YLL женщин: ", round(sum(newyll22[newyll22$sex == "f",]$yll)/1000)
           )
  )


ggsave("plots/SUPPORT_FIG_6.png", device = "png", dpi = 600, units = "cm", width = 24, height = 11)

# newyll22 %>% 
#   group_by(sex) %>% 
#   summarise(yll = sum(yll))


# ---- 6) FIG. 7 ----

alco <- read.xlsx("data/WHO_alcohol_consumption.xlsx")

alco %>% 
  mutate(deviation = 100*(FactValueNumeric/8)) %>% 
  mutate(sex = case_when(
    Dim1 == "Female" ~ "Женщины",
    Dim1 == "Male" ~ "Мужчины",
    Dim1 == "Both sexes" ~ "Общее"
  )) %>% 
  ggplot(aes(x = Period, y = FactValueNumeric, color = sex, fill = sex))+
  geom_col(position = position_dodge(), alpha = 0.8, color = "white")+
  geom_hline(yintercept = 8, linetype = "dashed")+
  theme_classic()+
  scale_y_continuous(
    name = "Абсолютное потребление\n(в литрах чистого спирта на душу)",
    sec.axis = sec_axis( trans=~.*(100/8), name = "Отклонение от 8л (в %)")
  )+
  scale_fill_manual(values = c("deeppink2", "steelblue3", "forestgreen"))+
  labs(fill = "", color = "", x = "Год")+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = seq(2000,2020,2))+
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))

ggsave("plots/FIG_7.png", device = "png", dpi = 600, units = "cm", width = 20, height = 10)


# ---- Cleaning ----

rm(list = ls()); gc()

