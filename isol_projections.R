
## Assuming you have gotten to the point of having `demo_quar` object:

report_date <- as.Date("2020-11-09")
report_date_minus7 <- report_date - 7

new_entry <- demo_quar %>%
             filter(!is.na(quar_entrydate)) %>%
             filter(quar_isol_bed==1) %>%
             group_by(quar_entrydate, campus_fac, off_campus_fac) %>%
             summarise(n = n()) %>%
             ungroup() %>%
             filter(quar_entrydate > report_date_minus7 & quar_entrydate <= report_date)

new_entry <- expand_grid(new_entry, 
                         scaling_factor = c(0.5, 1, 2)) %>%
             rename(quar_entrydate_actual = quar_entrydate) %>%
             # shift everything forward 7 days
             mutate(quar_entrydate = quar_entrydate_actual + 7) %>%
             # assign quarantine exit date
             mutate(quar_exitdate = quar_entrydate + 13)

new_entry_long <- new_entry %>%
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
to_add  <- ts_quar_campus %>%
           expand_grid(scaling_fac = c(0.5, 1, 2))

# projected quarantine use (adding in projected new entries)
ts_quar_proj <- new_entry_long %>%
                group_by(date, campus_fac, off_campus_fac, scaling_fac) %>%
                summarize(nbed = n()) %>%
                ungroup() %>%
                # adding in actual use
                bind_rows(to_add) %>%
                group_by(date, campus_fac, off_campus_fac, scaling_fac) %>%
                summarize(nbed = sum(nbed)) %>%
                ungroup() %>%
                mutate(scaling_fac_label = glue::glue("Scaling factor: {scaling_fac}"))

ts_quar_proj_tmp <- ts_quar_proj %>%
                    filter(campus_fac == "UNH Durham") 
ts_quar_proj_label <- ts_quar_proj_tmp %>%
                      filter(date > report_date)
label_height = max(c(ts_quar_proj_tmp$nbed, 180), na.rm=TRUE)


ggplot(ts_quar_proj_tmp, aes(x=date, y=nbed)) +
  geom_col(aes(fill=off_campus_fac))+
  theme_bw()+
  scale_fill_brewer("",palette="Dark2") +
  geom_label(data=ts_quar_proj_label, aes(y=label_height+0.2*label_height/as.numeric(off_campus_fac), label=nbed, fill=off_campus_fac), fontface="bold", show.legend = FALSE) +
  scale_y_continuous(name = "", breaks=breaks_pretty()) +
  scale_x_date(name="", breaks="3 days", limits = c(report_date_minus7, report_date+8)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  geom_hline(aes(yintercept=180), lty=2, col="red") +
  facet_grid(scaling_fac_label~.) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
  annotate("rect", alpha=0.3, xmin=report_date+0.5, xmax=report_date+8, ymin=-Inf, ymax=Inf)

## AS LINES
ts_quar_proj_tmp <- ts_quar_proj %>%
  filter(campus_fac == "UNH Durham") %>%
  group_by(campus_fac, date, scaling_fac_label) %>%
  mutate(nbed = sum(nbed))

ggplot(ts_quar_proj_tmp, aes(x=date, y=nbed, group=scaling_fac_label, color=scaling_fac_label)) +
  geom_line(lwd=1.5)+
  theme_bw()+
  scale_color_brewer("",palette="Dark2") +
  scale_y_continuous(name = "Quarantine Beds in Use", breaks=breaks_pretty()) +
  scale_x_date(name="", breaks="3 days", limits = c(report_date_minus7, report_date+8)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  geom_hline(aes(yintercept=180), lty=2, col="red") +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) +
  annotate("rect", alpha=0.1, xmin=report_date+0.5, xmax=report_date+8, ymin=-Inf, ymax=Inf)

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
  
}