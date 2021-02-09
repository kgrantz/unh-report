
library(cowplot)

## Assuming you have gotten to the point of having `demo_quar`, `demo_isol` objects

## Note: this code is structured differently than the report, to produce
## comparisons of the projections and actual quarantine numbers

## Note: even though quarantine duration is now 10 days, these run with 
## 14 day durations, the standard in the fall

report_date <- as.Date("2020-11-07")
report_date_minus7 <- report_date - 7

## QUARANTINE PROJECTIONS ####
  # finding the number of individuals who entered quarantined (by on/off campus status)
  # in the 7 days prior to report period
  # then projecting those entries forward to act as 'new' entries in the 7 days
  # after the report period
  new_entry_quar <- demo_quar %>%
    filter(!is.na(quar_entrydate)) %>%
    filter(quar_entrydate <= report_date) %>%
    filter(quar_isol_bed==1) %>%
    group_by(quar_entrydate, campus_fac, off_campus_fac) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(quar_entrydate > report_date_minus7 & quar_entrydate <= report_date) %>%
    expand_grid(.,
                scaling_factor = c(0.5, 2, 4)) %>%
    rename(quar_entrydate_actual = quar_entrydate) %>%
    # shift everything forward 7 days
    mutate(quar_entrydate = quar_entrydate_actual + 7) %>%
    # assign quarantine exit date
    mutate(quar_exitdate = quar_entrydate + 13)
  
  new_entry_quar_long <- new_entry_quar %>%
    # apply scaling factor
    mutate(n = n*scaling_factor) %>%
    rowwise() %>% 
    do(data.frame(n = 1:.$n,
                  quar_entrydate = .$quar_entrydate,
                  quar_exitdate = .$quar_exitdate,
                  campus_fac = .$campus_fac,
                  off_campus_fac = .$off_campus_fac,
                  scaling_fac = .$scaling_factor)) %>%
    do(data.frame(date = .$quar_entrydate:.$quar_exitdate,
                  campus_fac = .$campus_fac,
                  off_campus_fac = .$off_campus_fac,
                  scaling_fac = .$scaling_fac)) %>%
    mutate(date = as.Date(date, origin="1970-01-01"))  
  
  # actual quarantine use for the next seven days
  to_add_long <- demo_quar %>%
                 filter(quar_entrydate <= report_date) %>%             
                 mutate(quar_exitdate = ifelse(is.na(quar_exitdate) & !is.na(quar_entrydate),
                                               quar_entrydate + 14,
                                               quar_exitdate)) %>%
                  mutate(quar_exitdate = as.Date(quar_exitdate, origin="1970-01-01")) %>%
                  rowwise() %>% 
                  do(data.frame(date = .$quar_entrydate:.$quar_exitdate,
                                uid = .$uid,
                                location = .$location,
                                dorm_isol = .$dorm_isol,
                                quar_isol_bed = .$quar_isol_bed,
                                campus_fac = .$campus_fac,
                                off_campus_fac = .$off_campus_fac,
                                user_status_fac = .$user_status_fac))
  
  to_add_quar  <- to_add_long %>%
                  group_by(date, campus_fac, off_campus_fac) %>%
                  summarize(nbed = sum(quar_isol_bed, na.rm=TRUE)) %>%
                  ungroup() %>%
                  mutate(date = as.Date(date, origin="1970-01-01")) %>%
                  expand_grid(scaling_fac = c(0.5, 1, 2, 3, 4)) 
  
  # adding in total
  to_add_quar <- to_add_quar %>%
    group_by(campus_fac, date, scaling_fac) %>%
    summarize(nbed=sum(nbed)) %>%
    mutate(off_campus_fac = "Total") %>%
    bind_rows(to_add_quar) %>%
    mutate(scaling_fac = glue::glue("{scaling_fac}"))
  
  # adding in total with 2x scaling factor, without off-campus
  to_add_quar <- to_add_quar %>%
      filter(off_campus_fac=="Total" & scaling_fac==4) %>%
      mutate(scaling_fac = "4 w/o off-campus") %>%
      bind_rows(to_add_quar) 
  
  # projected quarantine use (adding in projected new entries with actual known use)
  ts_quar_proj <- new_entry_quar_long %>%
    group_by(date, campus_fac, off_campus_fac, scaling_fac) %>%
    summarize(nbed = n()) %>%
    ungroup() %>%
    mutate(scaling_fac = glue::glue("{scaling_fac}"))
  
  # formatting to add in 'total' + 'total w/o off-campus' categories
  ts_quar_proj <- ts_quar_proj %>%
    group_by(campus_fac, date, scaling_fac) %>%
    summarize(nbed=sum(nbed)) %>%
    mutate(off_campus_fac = "Total") %>%
    bind_rows(ts_quar_proj)
  
  ts_quar_proj <- ts_quar_proj %>%
        filter(off_campus_fac=="On Campus" & scaling_fac==4) %>%
        mutate(off_campus_fac="Total") %>%
        mutate(scaling_fac = "4 w/o off-campus") %>%
        bind_rows(ts_quar_proj)
  
  ts_quar_proj_final <- ts_quar_proj %>%
      bind_rows(to_add_quar) %>%
      group_by(date, campus_fac, off_campus_fac, scaling_fac) %>%
      summarize(nbed=sum(nbed)) %>%
      filter(date >= report_date_minus7)
  
  # formatting to add in 'actual' categories
  # HERE ONLY (not in report): allow for dates up to report_date + 7
  ts_quar_actual <- ts_quar_campus %>%
                    filter(date >= report_date_minus7 & date <= report_date + 8) %>%
                    select(-n) %>%
                    mutate(scaling_fac = "Actual") %>%
                    distinct() 
  
  ts_quar_actual <- ts_quar_actual %>%
                    group_by(date, campus_fac, scaling_fac) %>%
                    summarise(nbed = sum(nbed)) %>%
                    mutate(off_campus_fac="Total") %>%
                    bind_rows(ts_quar_actual) 
  
  ts_quar_proj_final <- ts_quar_actual %>%
                        bind_rows(ts_quar_proj_final %>% filter(date >= report_date))
  
  
  ts_quar_proj_tmp <- ts_quar_proj_final %>%
                      filter(campus_fac == "UNH Durham") %>%
                      mutate(scaling_fac_lab = factor(scaling_fac,
                                                      levels = c("0.5", "2", "4 w/o off-campus", "4", "Actual"))) %>%
                      filter(date >= report_date_minus7 & date <= report_date + 8)
  
  ggplot(ts_quar_proj_tmp, aes(x=date, y=nbed, group=scaling_fac_lab, col=scaling_fac_lab)) +
    geom_line(lwd=1.15)+
    facet_wrap(.~off_campus_fac)+
    theme_bw()+
    scale_color_manual(name="", values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black")) +
    scale_y_continuous(name = "Quarantine Beds in Use", breaks=breaks_pretty()) +
    scale_x_date(name="", breaks="3 days", limits = c(report_date_minus7, report_date+8)) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
    annotate("rect", alpha=0.1, xmin=report_date, xmax=report_date+8, ymin=-Inf, ymax=Inf) +
    ggtitle(glue("Quarantine: {report_date}"))  
  
  ggsave(glue("figures/quarantine_{report_date}.pdf"), width = 8, height = 5.5)
  
## ISOLATION PROJECTIONS ####
  # finding the number of individuals who entered isolation (by on/off campus status)
  # in the 7 days prior to report period
  # then projecting those entries forward to act as 'new' entries in the 7 days
  # after the report period

  new_entry_isol <- demo_isol %>%
                    filter(!is.na(iso_entrydate)) %>%
                    filter(iso_entrydate <= report_date) %>%
                    filter(quar_isol_bed==1) %>%
                    group_by(iso_entrydate, campus_fac, off_campus_fac) %>%
                    summarise(n = n()) %>%
                    ungroup() %>%
                    filter(iso_entrydate > report_date_minus7 & iso_entrydate <= report_date) %>%
                    expand_grid(.,
                                scaling_factor = c(0.5, 2, 4)) %>%
                    rename(iso_entrydate_actual = iso_entrydate) %>%
                    # shift everything forward 7 days
                    mutate(iso_entrydate = iso_entrydate_actual + 7) %>%
                    # assign isolation exit date
                    mutate(iso_exitdate = iso_entrydate + 13)
  
  new_entry_isol_long <- new_entry_isol %>%
                         # apply scaling factor
                         mutate(n = n*scaling_factor) %>%
                         rowwise() %>% 
                         do(data.frame(n = 1:.$n,
                                       iso_entrydate = .$iso_entrydate,
                                       iso_exitdate = .$iso_exitdate,
                                       campus_fac = .$campus_fac,
                                       off_campus_fac = .$off_campus_fac,
                                       scaling_fac = .$scaling_factor)) %>%
                         do(data.frame(date = .$iso_entrydate:.$iso_exitdate,
                                       campus_fac = .$campus_fac,
                                       off_campus_fac = .$off_campus_fac,
                                       scaling_fac = .$scaling_fac)) %>%
                         mutate(date = as.Date(date, origin="1970-01-01"))  
  
  # actual quarantine use for the next seven days
  to_add_isol_long <- demo_isol %>%
                      filter(iso_entrydate <= report_date) %>%             
                      mutate(iso_exitdate = ifelse(is.na(iso_exitdate) & !is.na(iso_entrydate),
                                                    iso_entrydate + 14,
                                                   iso_exitdate)) %>%
                      mutate(iso_exitdate = as.Date(iso_exitdate, origin="1970-01-01")) %>%
                      rowwise() %>% 
                      do(data.frame(date = .$iso_entrydate:.$iso_exitdate,
                                    uid = .$uid,
                                    location = .$location,
                                    dorm_isol = .$dorm_isol,
                                    quar_isol_bed = .$quar_isol_bed,
                                    campus_fac = .$campus_fac,
                                    off_campus_fac = .$off_campus_fac,
                                    user_status_fac = .$user_status_fac))
  
  to_add_isol  <- to_add_isol_long %>%
                  group_by(date, campus_fac, off_campus_fac) %>%
                  summarize(nbed = sum(quar_isol_bed, na.rm=TRUE)) %>%
                  ungroup() %>%
                  mutate(date = as.Date(date, origin="1970-01-01")) %>%
                  expand_grid(scaling_fac = c(0.5, 2, 4)) 
                
  # adding in total
  to_add_isol <- to_add_isol %>%
                 group_by(campus_fac, date, scaling_fac) %>%
                 summarize(nbed=sum(nbed)) %>%
                 mutate(off_campus_fac = "Total") %>%
                 bind_rows(to_add_isol) %>%
                 mutate(scaling_fac = glue::glue("{scaling_fac}"))
  
  # adding in total with 2x scaling factor, without off-campus
  to_add_isol <- to_add_isol %>%
                 filter(off_campus_fac=="Total" & scaling_fac==4) %>%
                 mutate(scaling_fac = "4 w/o off-campus") %>%
                 bind_rows(to_add_isol) 
  
  # projected quarantine use (adding in projected new entries with actual known use)
  ts_isol_proj <- new_entry_isol_long %>%
                  group_by(date, campus_fac, off_campus_fac, scaling_fac) %>%
                  summarize(nbed = n()) %>%
                  ungroup() %>%
                  mutate(scaling_fac = glue::glue("{scaling_fac}"))
  
  # formatting to add in 'total' + 'total w/o off-campus' categories
  ts_isol_proj <- ts_isol_proj %>%
                  group_by(campus_fac, date, scaling_fac) %>%
                  summarize(nbed=sum(nbed)) %>%
                  mutate(off_campus_fac = "Total") %>%
                  bind_rows(ts_isol_proj)
  
  ts_isol_proj <- ts_isol_proj %>%
                  filter(off_campus_fac=="On Campus" & scaling_fac==4) %>%
                  mutate(off_campus_fac="Total") %>%
                  mutate(scaling_fac = "4 w/o off-campus") %>%
                  bind_rows(ts_isol_proj)
  
  ts_isol_proj_final <- ts_isol_proj %>%
                        bind_rows(to_add_isol) %>%
                        group_by(date, campus_fac, off_campus_fac, scaling_fac) %>%
                        summarize(nbed=sum(nbed)) %>%
                        filter(date >= report_date_minus7)
  
  # formatting to add in 'actual' categories
  # HERE ONLY (not in report): allow for dates up to report_date + 7
  ts_isol_actual <- ts_isol_campus %>%
                    filter(date >= report_date_minus7 & date <= report_date + 8) %>%
                    select(-n) %>%
                    mutate(scaling_fac = "Actual") %>%
                    distinct() 
  
  ts_isol_actual <- ts_isol_actual %>%
                    group_by(date, campus_fac, scaling_fac) %>%
                    summarise(nbed = sum(nbed)) %>%
                    mutate(off_campus_fac="Total") %>%
                    bind_rows(ts_isol_actual) 
  
  ts_isol_proj_final <- ts_isol_actual %>%
                        bind_rows(ts_isol_proj_final %>% filter(date >= report_date))
  
  
  ts_isol_proj_tmp <- ts_isol_proj_final %>%
    filter(campus_fac == "UNH Durham") %>%
    mutate(scaling_fac_lab = factor(scaling_fac,
                                    levels = c("0.5", "2", "4 w/o off-campus", "4", "Actual"))) %>%
    filter(date >= report_date_minus7 & date <= report_date + 8)
  
  ggplot(ts_isol_proj_tmp, aes(x=date, y=nbed, group=scaling_fac_lab, col=scaling_fac_lab)) +
    geom_line(lwd=1.15)+
    facet_wrap(.~off_campus_fac)+
    theme_bw()+
    scale_color_manual(name="", values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black")) +
    scale_y_continuous(name = "Isolation Beds in Use", breaks=breaks_pretty()) +
    scale_x_date(name="", breaks="3 days", limits = c(report_date_minus7, report_date+8)) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
    annotate("rect", alpha=0.1, xmin=report_date, xmax=report_date+8, ymin=-Inf, ymax=Inf) +
    ggtitle(glue("Isolation: {report_date}"))  

  ggsave(glue("figures/isolation_{report_date}.pdf"), width = 8, height = 5.5)
  
#### ------------ OLD ------------ ####
## Function that will output 
## Inputs:
##  - base_isol: vector of how many people will be in isolation for future dates; MUST BE same length as start_date:end_date
##  - new_isol: how many people will be isolated in next week (7 day sum; incorporate multiplier here if using) 
##  - start_date: when isolation projections begin
##  - end_date: when isolation projections end
##  - quar_mult: how many quarantined per person isolated
##  - theta: overdispersion parameter; currently set to 8 based on estimated neg binomial distribution of # new positive cases per day (i.e. minimal overdispersion)

base_isol <- c(27, 24, 23, 19, 10, 6, 5)
start_date <- as.Date("2020-11-02")
end_date <- as.Date("2020-11-08")
quar_mult = 5
theta=8
new_isol = 35
nsim = 100

daily_cases <- expand.grid(nsim = 1:nsim,
                           date = as.Date(names(base_isol)))

make_isol_pred <- function(base_isol, new_isol, end_date, quar_mult=5, theta=8){
  
  
  
xx <- ts_quar_proj_final %>%
      filter(campus_fac == "UNH Durham") %>%
      filter(scaling_fac != "1") %>%
      pivot_wider(names_from = scaling_fac, values_from = nbed)
  
ggplot(xx) +
  geom_line(aes(x=date, y=Actual), col="black") +
  geom_ribbon(aes(x=date, ymin=`0.5`, ymax=`4`), fill="#0044bb", col="#0044bb") +
  geom_line(aes(x=date, y=`4 w/o off-campus`), col="white") +
  geom_ribbon(aes(x=date, ymin=`4`, ymax=`4 w/o off-campus`), fill="#cb4d0b", col="#cb4d0b") +
  facet_wrap(.~off_campus_fac)+
  theme_bw()+
  scale_y_continuous(name = "Quarantine Beds in Use", breaks=breaks_pretty()) +
  scale_x_date(name="", breaks="3 days", limits = c(report_date_minus7, report_date+8)) +
  geom_hline(aes(yintercept=quar_bed_limit), lty=2, col="red") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
  annotate("rect", alpha=0.1, xmin=report_date, xmax=report_date+8, ymin=-Inf, ymax=Inf)
ggsave("figures/quar_proj_new_20200208_updated.pdf", width=7, height=5)

    
}