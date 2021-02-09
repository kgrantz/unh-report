
### DORM ACTIVE CASE THRESHOLDS ####
 xx<- ts_active_dorm %>%
      filter(!is.na(dorm)) %>%
      mutate(above5 = active_cases>=5) %>%
      mutate(above10 = active_cases>=10) %>%
      mutate(above3 = active_cases>=3) %>%
      mutate(above3_below5 = active_cases %in% 3:4) %>%
      filter(!is.na(active_cases))
 
 ggplot(xx, aes(y=dorm, x=testdate, fill=active_cases)) +
   geom_tile() +
   scale_x_date(breaks="2 weeks") +
   theme_bw() +
   scale_fill_continuous_sequential(name="Active Cases") +
   theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))
 ggsave("figures/dorm_thresh.pdf", width=8, height=8)
 
 ggplot(xx, aes(y=dorm, x=testdate, fill=above5)) +
    geom_tile() +
    scale_x_date(breaks="2 weeks") +
    theme_bw() +
    scale_fill_manual(name="Active Cases\n>=5", values=c("#e3e3e3", "#455e96")) +
    theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))
 ggsave("figures/dorm_thresh_5.pdf", width=8, height=8)
 
 ggplot(xx, aes(y=dorm, x=testdate, fill=above10)) +
    geom_tile() +
    scale_x_date(breaks="2 weeks") +
    theme_bw() +
    scale_fill_manual(name="Active Cases\n>=10", values=c("#e3e3e3", "#455e96")) +
    theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))
 ggsave("figures/dorm_thresh_10.pdf", width=8, height=8)
 
 ggplot(xx, aes(y=dorm, x=testdate, fill=above3)) +
    geom_tile() +
    scale_x_date(breaks="2 weeks") +
    theme_bw() +
    scale_fill_manual(name="Active Cases\n>=3", values=c("#e3e3e3", "#455e96")) +
    theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))
 ggsave("figures/dorm_thresh_3.pdf", width=8, height=8)
 
 
 dates <- c("2020-08-24", "2020-08-31", "2020-09-07", "2020-09-14", "2020-10-31", "2020-11-07")
 dates <- as.Date(dates)
 
 ggplot(ts_quar_campus %>% filter(campus_fac == "UNH Durham"), aes(x=date, y=nbed)) +
    geom_col(aes(fill=off_campus_fac))+
    theme_bw()+
    scale_fill_brewer("",palette="Dark2") +
    scale_y_continuous(name = "", breaks=breaks_pretty()) +
    scale_x_date(name="", breaks="1 week", limits = xaxis_entire) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_hline(aes(yintercept=quar_bed_limit), lty=2, col="red") +
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
    geom_vline(xintercept = dates, linetype="dashed")
 ggsave("figures/ts_quar_beds_labelled.pdf", width=8, height=6)
 
 ggplot(ts_isol_campus %>% filter(campus_fac == "UNH Durham"), aes(x=date, y=nbed)) +
    geom_col(aes(fill=off_campus_fac), width=0.9)+
    theme_bw()+
    scale_fill_brewer("",palette="Dark2") +
    scale_y_continuous(name = "", breaks=breaks_pretty()) +
    scale_x_date(name="", breaks="1 week", limits = xaxis_entire) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_hline(aes(yintercept=isol_bed_limit), lty=2, col="red") +
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
    geom_vline(xintercept = dates, linetype="dashed")
 ggsave("figures/ts_isol_beds_labelled.pdf", width=8, height=6)
 
 
 ### DORM QUARANTINE THRESHOLDS ####
 
 xx_active <- ts_active_dorm %>%
   filter(!is.na(dorm)) %>%
   mutate(above5 = active_cases>=5) %>%
   mutate(above10 = active_cases>=10) %>%
   mutate(above3 = active_cases>=3) %>%
   mutate(above3_below5 = active_cases %in% 3:4) %>%
   filter(!is.na(active_cases))
 
 # entries into quarantine
 xx <- ts_quar_ent_dorm %>%
       filter(!is.na(dorm)) %>%
       mutate(above5 = n>=5) %>%
       mutate(above10 = n>=10) %>%
       mutate(above3 = n>=3) %>%
       filter(!is.na(n))
 
 ggplot() +
   geom_tile(data=xx_active, aes(y=dorm, x=testdate, fill=active_cases)) +
   scale_x_date(breaks="2 weeks") +
   theme_bw() +
   scale_fill_continuous_sequential(name="Active Cases") +
   geom_point(data=xx, aes(y=dorm, x=date, col=above5)) +
   scale_color_manual(name="Quar entries\n>=5", values=c(NA, "red")) +
   theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1)) +
   labs(y="", x="")
 ggsave("figures/dorm_quar_entry_thresh5.pdf", width=8, height=8)

 ggplot() +
   geom_tile(data=xx_active, aes(y=dorm, x=testdate, fill=active_cases)) +
   scale_x_date(breaks="2 weeks") +
   theme_bw() +
   scale_fill_continuous_sequential(name="Active Cases") +
   geom_point(data=xx, aes(y=dorm, x=date, col=above3)) +
   scale_color_manual(name="Quar entries\n>=3", values=c(NA, "red")) +
   theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1)) +
   labs(y="", x="")
 ggsave("figures/dorm_quar_entry_thresh3.pdf", width=8, height=8)
 
 
 # total in quarantine
 xx <- ts_quar_dorm %>%
   filter(!is.na(dorm)) %>%
   mutate(above5 = n>=5) %>%
   mutate(above10 = n>=10) %>%
   mutate(above8 = n>=8) %>%
   filter(!is.na(n))

 ggplot() +
   geom_tile(data=xx_active, aes(y=dorm, x=testdate, fill=active_cases)) +
   scale_x_date(breaks="2 weeks") +
   theme_bw() +
   scale_fill_continuous_sequential(name="Active Cases") +
   geom_point(data=xx, aes(y=dorm, x=date, col=above5)) +
   scale_color_manual(name="In Quar\n>=5", values=c(NA, "red")) +
   theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1)) +
   labs(y="", x="")
 ggsave("figures/dorm_quar_thresh5.pdf", width=8, height=8)
 
 
 ggplot() +
   geom_tile(data=xx_active, aes(y=dorm, x=testdate, fill=active_cases)) +
   scale_x_date(breaks="2 weeks") +
   theme_bw() +
   scale_fill_continuous_sequential(name="Active Cases") +
   geom_point(data=xx, aes(y=dorm, x=date, col=above8)) +
   scale_color_manual(name="In Quar\n>=8", values=c(NA, "red")) +
   theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1)) +
   labs(y="", x="")
 ggsave("figures/dorm_quar_thresh8.pdf", width=8, height=8)
 
 