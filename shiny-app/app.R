# U.S. Hospital Readmissions — real CMS/AHRF analytical extract
invisible(try(Sys.setlocale('LC_CTYPE', 'en_US.UTF-8'), silent = TRUE))
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

hospital_data <- read_csv('data/tableau_model_complete.csv.gz',
  col_types = cols(provider_id = col_character(), county_fips = col_character()), show_col_types = FALSE)
stopifnot(!anyDuplicated(hospital_data[c('provider_id', 'measure')]), all(is.finite(hospital_data$err)))
measure_names <- c(AMI = 'Acute myocardial infarction', CABG = 'Coronary artery bypass graft',
 COPD = 'Chronic obstructive pulmonary disease', HF = 'Heart failure', PN = 'Pneumonia', 'HIP-KNEE' = 'Hip/knee arthroplasty')
measures <- sort(unique(hospital_data$measure))
fmt <- function(x) format(x, big.mark = ',', scientific = FALSE, trim = TRUE)
# Semantic palette: sapphire < 1, neutral gray at 1, orange > 1.
blue <- '#245A9C'; amber <- '#E07832'; navy <- '#172B4D'; trend_color <- '#B87A55'
map_limits <- c(0.90, 1.10)
context_choices <- c('County income, 2022 ($)'='median_family_income_2022',
 'County poverty, 2022 (%)'='poverty_rate_2022',
 'County unemployment, 2022 (%)'='unemployment_rate_2022',
 'County uninsured population under 65, 2021 (%)'='uninsured_under65_rate_2021',
 'County population age 65+, 2022 (%)'='pct_age_65plus_2022')
context_label <- function(key) names(context_choices)[match(key,context_choices)]
# Descriptive summaries use only records visible in the corresponding chart.
record_stats <- function(d) {
 y <- d$err[is.finite(d$err)]
 list(n=length(y), median=if(length(y)) median(y) else NA_real_,
      q1=if(length(y)) unname(quantile(y,.25)) else NA_real_,
      q3=if(length(y)) unname(quantile(y,.75)) else NA_real_,
      above=if(length(y)) 100*mean(y>1) else NA_real_)
}
social_analysis <- function(d,key) {
 stopifnot(key %in% unname(context_choices))
 d <- d[is.finite(d[[key]]) & is.finite(d$err),,drop=FALSE]
 available <- nrow(d)>=3 && length(unique(d[[key]]))>1 && length(unique(d$err))>1
 trend <- data.frame(x=numeric(),y=numeric())
 r <- NA_real_; coefficients <- c(intercept=NA_real_,slope=NA_real_)
 if(available) {
   xy <- data.frame(x=d[[key]],y=d$err)
   fit <- lm(y~x,data=xy)
   coefficients <- setNames(unname(coef(fit)),c('intercept','slope'))
   trend <- data.frame(x=seq(min(xy$x),max(xy$x),length.out=100))
   trend$y <- as.numeric(predict(fit,newdata=data.frame(x=trend$x)))
   r <- cor(xy$x,xy$y,method='pearson')
 }
 list(data=d,n=nrow(d),counties=n_distinct(d$county_fips,na.rm=TRUE),r=r,
      available=available,trend=trend,coefficients=coefficients)
}
stat_badge <- function(label,value,tip=NULL) span(class='stat-badge',title=tip,
 span(class='stat-label',paste0(label, ':')),strong(value))
summary_badges <- function(d) {
 st <- record_stats(d)
 if(!st$n) return(div(class='chart-stats','No matching observations'))
 div(class='chart-stats',stat_badge('Median ERR',sprintf('%.3f',st$median)),
 stat_badge('Middle 50%',sprintf('%.3f – %.3f',st$q1,st$q3),'25th–75th percentile of ERR'),
 stat_badge('Observations',fmt(st$n)))
}
social_badges <- function(a) div(class='chart-stats',
 stat_badge('Pearson r',if(a$available)sprintf('%.3f',a$r)else 'Unavailable',
 'Descriptive correlation across filtered hospital–condition/procedure records. County values repeat; this is not a correlation across independent counties or an adjusted model. At least 3 records and variation in both variables are required.'),
 stat_badge('Observations',fmt(a$n),'Finite paired values used for both correlation and trendline'),
 stat_badge('Counties',fmt(a$counties)))
wrap_label <- function(x, width=23) vapply(x, function(v) paste(strwrap(v,width=width),collapse='<br>'),character(1))
# state.center uses display-inset locations for AK/HI; use actual geographic centers here.
state_labels <- data.frame(state=c(state.abb,'DC'), lon=c(state.center$x,-77.04), lat=c(state.center$y,38.90))
state_labels[state_labels$state=='AK',c('lon','lat')] <- c(-152,64)
state_labels[state_labels$state=='HI',c('lon','lat')] <- c(-155.5,20.5)
state_labels$label_lon <- state_labels$lon
state_labels$label_lat <- state_labels$lat
# Callouts give small northeastern states room without covering their neighbors.
callouts <- data.frame(state=c('VT','NH','MA','RI','CT','NJ','DE','MD','DC'),
 label_lon=c(-74.5,-65,-65.5,-65,-66,-67,-68,-69.5,-71.5),
 label_lat=c(46,47,45,43.3,41.8,40,38.6,37.2,35.7))
idx <- match(callouts$state,state_labels$state)
state_labels[idx,c('label_lon','label_lat')] <- callouts[c('label_lon','label_lat')]

th <- theme_minimal(base_size = 15) + theme(panel.grid.minor = element_blank(),
 panel.grid.major = element_line(color = '#edf0f5'), plot.title = element_blank(),
 axis.title = element_text(color = '#53647d',size=14), axis.text = element_text(color = '#465672',size=13),
 legend.text=element_text(size=13),legend.title=element_text(size=14))
card_plot <- function(title, subtitle, id, height = '310px', stats = NULL) div(class = 'chart-card',
 div(class = 'chart-heading', h3(title), p(subtitle)),
 if(!is.null(stats)) uiOutput(stats), plotlyOutput(id, height = height))
insight <- function(id) div(class = 'insight', uiOutput(id))
metric <- function(title, id, hint) div(class='metric', title=hint, div(class='metric-label',title),
 div(class='metric-value',textOutput(id)),div(class='metric-hint',hint))
ui <- page_navbar(
 title = tags$span(class='brand','US Readmissions'), id='page',
 theme = bs_theme(version = 5, primary = blue, base_font = 'Arial'),
 header = tagList(
 tags$head(tags$link(rel='stylesheet',href='style.css')),
 div(class='app-header', div(class='source-label','CMS + AHRF / HOSPITAL ANALYTICS'),
 h1('U.S. Hospital Readmissions'),
 p(class='slogan','Where do readmissions exceed expectations—and what patterns emerge?')),
 div(class='control-deck',div(class='filter-bar',
 selectInput('measure','Condition / procedure',c('All conditions / procedures'='all',setNames(measures,paste(unname(measure_names[measures]),measures,sep=' · ')))),
 selectInput('state','State / district',c('All states / districts'='all',sort(unique(hospital_data$state)))),
 selectInput('ownership','Hospital ownership',c('All ownership groups'='all',sort(unique(hospital_data$ownership_group)))),
 actionButton('reset','Reset all',class='reset-button')),
 div(class='metrics',metric('HOSPITALS','hospitals','Distinct facilities'),metric('OBSERVATIONS','observations','Hospital–condition/procedure records'),
 metric('MEAN ERR','mean_err','Unweighted record mean'),metric('COUNTIES','counties','Distinct counties'))),
 uiOutput('filter_chips')
 ),
 nav_panel('Overview',div(class='overview-page',
 div(class='insights',insight('above_insight'),insight('measure_insight'),insight('spread_insight')),
 div(class='overview-grid',
 card_plot('Geographic variation','Nationwide · Click a state to explore', 'map','100%',stats='map_stats'),
 div(class='chart-card explore-card',
 div(class='chart-heading',h3('Explore readmission patterns'),span(class='info-icon',title='Points, Pearson r and the muted-orange linear trend use the same filtered records. This is descriptive, unadjusted exploration; repeated hospitals/counties are not accounted for. Gray dashed line: ERR = 1.','ⓘ')),
 selectInput('explore_by','Explore by',choices=c('Condition / procedure'='condition','Hospital ownership'='ownership',context_choices)),
 uiOutput('explore_stats'),plotlyOutput('explorer',height='100%'))),
 div(class='two-columns',card_plot('ERR distribution',NULL,'hist','100%',stats='hist_stats'),
 div(class='chart-card editorial findings-card',
 h3('Key findings & context'),
 div(class='finding-main',strong('Community context matters'),
 p('Higher county poverty and unemployment were associated with higher ERR after adjustment for the hospital characteristics and readmission conditions included in the model.')),
 div(class='finding-scope','Original adjusted analysis · Full sample · Not causal'),
 tags$details(tags$summary('Adjusted findings'),
 p('Estimated change in ERR for a one-standard-deviation increase, holding other included predictors constant:'),
 tags$table(class='finding-table',
 tags$thead(tags$tr(tags$th('County characteristic'),tags$th('Change in ERR'),tags$th('95% CI'))),
 tags$tbody(
 tags$tr(tags$td('Poverty'),tags$td('+0.0082'),tags$td('+0.0049 to +0.0116')),
 tags$tr(tags$td('Unemployment'),tags$td('+0.0069'),tags$td('+0.0045 to +0.0094')),
 tags$tr(tags$td('Log population'),tags$td('+0.0060'),tags$td('+0.0034 to +0.0086')))),
 p('The adjusted income and county age-65+ estimates had 95% confidence intervals spanning zero. Evidence for these associations was less clear. These are original model results, not filter-specific estimates.')),
 tags$details(tags$summary('Geographic patterns'),
 p('In the full analytical sample, several Northwestern and Great Plains states have mean ERR below 1. Examples include Idaho (0.913), South Dakota (0.924), Montana (0.949), Oregon (0.952) and Washington (0.973).'),
 p('State averages can hide condition differences: Washington’s overall mean ERR is 0.973, but its COPD mean is 1.025. Sample sizes also vary: North Dakota includes 6 hospitals, compared with 219 in California.'),
 p('These unadjusted geographic summaries are fixed full-sample findings. They are not formal hospital-quality rankings and do not establish that location causes readmissions.')),
 tags$details(tags$summary('Understanding ERR'),
 p('ERR = predicted readmission rate ÷ expected readmission rate for similar patients. Above 1 means more readmissions than expected; below 1 means fewer.'),
 div(class='example-callout',strong('ERR 1.05'),span(' means the predicted rate is 5% above the expected rate, not a 5-percentage-point increase.')),
 p('CMS HRRP indicators concern unplanned readmissions within 30 days after discharge, with some planned readmissions excluded. A hospital may have different ERR values across conditions and procedures.')),
 tags$details(tags$summary('Data & methods'),
 p('The model-complete sample contains 7,944 hospital–condition/procedure records, 2,444 hospitals and 1,197 counties. A record is not a unique patient. Incomplete model records and the Other/Tribal ownership group were excluded.'),
 p('CMS supplies readmission performance and hospital characteristics; HRSA AHRF supplies county socioeconomic context. The original mixed-effects model includes a hospital random intercept to account for repeated condition measurements within hospitals.'),
 p('Choose a condition/procedure, then explore states and community factors. Charts and top highlights respond to filters; the original findings in this card do not. Pearson r and linear trendlines are unadjusted descriptions of the visible records, with repeated hospitals/counties.'),
 p('Source years vary. County income, poverty and unemployment are from 2022; uninsured population under 65 is from 2021. The age-65+ share uses a 2022 numerator and a 2023 population denominator. County context is not patient-level exposure, and observational associations are not causal.'),
 div(class='source-links',tags$a(href='https://data.cms.gov/provider-data/dataset/9n3s-kdb3',target='_blank','CMS HRRP ↗'),
 tags$a(href='https://www.cms.gov/medicare/quality/value-based-programs/hospital-readmissions',target='_blank','ERR definitions ↗'),
 tags$a(href='https://data.hrsa.gov/data/download?data=AHRF',target='_blank','HRSA AHRF ↗'),
 tags$a(href='https://github.com/ada-nguyen-ds/hospital-readmission-analysis',target='_blank','Original analysis ↗'))))))),

 nav_panel('Geography',
 div(class='chart-card',h3('State × condition / procedure'),p(class='chart-subtitle','Does the geographic pattern persist across conditions and procedures? Hover to see mean ERR and sample size.'),
 uiOutput('heatmap_stats'),plotlyOutput('heatmap',height='850px')),
 div(class='method-note','Small groups can have unstable means. All selected states / districts are shown; the heatmap does not establish hospital quality rankings.')),
 nav_panel('Hospitals',
 div(class='two-columns',card_plot('Hospital ownership','Unadjusted distributions • Group sizes shown in labels','box','390px',stats='box_stats'),
 div(class='chart-card',h3('Explore one hospital'),
 selectizeInput('hospital','Hospital',choices=NULL),
 p(class='chart-subtitle','Choose a facility; its records also respect the global filters.'),uiOutput('hospital_stats'),plotlyOutput('hospital_plot',height='290px')))),
 nav_panel('Community',
 div(class='model-callout',strong('From the original adjusted analysis'),
 p('Higher county poverty and unemployment were associated with higher ERR after adjustment. This finding describes the full analytical sample; filters below do not refit the model.')),
 div(class='chart-card',h3('Community characteristics and ERR'),
 selectInput('context','County characteristic',context_choices,selected='poverty_rate_2022'),
 uiOutput('scatter_stats'),plotlyOutput('scatter',height='430px'),div(class='chart-footnote','Muted orange line: unadjusted linear trend. Gray dashed line: ERR = 1.')),
 div(class='method-note','Unadjusted exploration, not causal evidence. Each point is a hospital–condition/procedure record; county values repeat across hospitals and measures. County characteristics are not individual patient attributes.')),
 nav_panel('Data & methods',
 div(class='chart-card',h3('Selected records'),
 div(class='data-controls',textInput('search','Search hospital or facility ID',placeholder='Hospital name or ID'),
 selectInput('sort','Sort by',c('Highest ERR'='high','Lowest ERR'='low','Hospital name'='name')),downloadButton('download','Download CSV')),
 textOutput('table_note'),div(class='table-wrap',tableOutput('records'))),
 div(class='chart-card methods',h3('Sources, sample and interpretation'),
 p('Sources: CMS Hospital Readmissions Reduction Program, CMS hospital characteristics, ZIP-to-county mapping and HRSA Area Health Resources Files.'),
 p('The model-complete sample contains 7,944 hospital–condition/procedure records, 2,444 hospitals and 1,197 counties. It excludes incomplete model records and the Other/Tribal ownership group. It does not represent all U.S. hospitals.'),
 p('One record represents one hospital and one readmission measure. Counts are not unique patients. Means and shares are unweighted across records; ERR > 1 uses a strict greater-than threshold.'),
 p('Sapphire, neutral gray and orange describe ERR relative to 1, not statistical significance. The geographic color scale is fixed at 0.90–1.10; values outside it use endpoint colors and remain exact in tooltips. No uncertainty intervals or formal group comparisons are implied. Geographic and ownership comparisons are unadjusted; measure mix and small samples matter.'),
 p('Scatter plots report descriptive Pearson correlation across finite hospital–condition/procedure records, not independent counties. Trendlines use ordinary least squares on those same filtered records. Repeated hospital and county observations are not adjusted for; no p-values or confidence bands are displayed. Correlation and trend are unavailable with fewer than three records or no variation in either variable.'),
 p('The original project used a hospital random-intercept linear mixed-effects model. The app does not refit models. Associations are observational and ecological; source years vary and residual confounding may remain.'),
 tags$a(href='https://github.com/ada-nguyen-ds/hospital-readmission-analysis',target='_blank','Analysis code and methodology ↗'))),
 footer=div(class='footer','CMS + AHRF • Model-complete analytical sample • Independent analysis by Khanh Nguyen'),
 fillable=FALSE
)

server <- function(input,output,session) {
 hospital_ids <- sort(unique(hospital_data$provider_id))
 updateSelectizeInput(session, 'hospital', choices=setNames(hospital_ids,
   paste(hospital_data$hospital_name[match(hospital_ids,hospital_data$provider_id)],hospital_ids,sep=' · ')),
   selected=hospital_ids[1], server=TRUE)
 filtered_data <- reactive({
  d <- hospital_data
  if(input$measure!='all') d<-filter(d,measure==input$measure)
  if(input$state!='all') d<-filter(d,state==input$state)
  if(input$ownership!='all') d<-filter(d,ownership_group==input$ownership)
  d
 })
 # Map keeps the nationwide comparison visible when a state is selected.
 map_data <- reactive({
  d<-hospital_data
  if(input$measure!='all') d<-filter(d,measure==input$measure)
  if(input$ownership!='all') d<-filter(d,ownership_group==input$ownership)
  d %>% group_by(state) %>% summarise(mean_err=mean(err), hospitals=n_distinct(provider_id),observations=n(),.groups='drop')
 })
 measure_summary <- reactive(filtered_data() %>% group_by(measure) %>% summarise(mean_err=mean(err),n=n(),hospitals=n_distinct(provider_id),.groups='drop'))
 observeEvent(input$reset,{
  updateSelectInput(session,'measure',selected='all');updateSelectInput(session,'state',selected='all')
  updateSelectInput(session,'ownership',selected='all');updateTextInput(session,'search',value='')
  updateSelectInput(session,'explore_by',selected='condition');updateSelectInput(session,'context',selected='poverty_rate_2022')
 })
 output$filter_chips <- renderUI({
 chips <- list()
 if(input$measure!='all') chips <- c(chips,list(actionLink('clear_measure',paste(measure_names[input$measure],'×'),class='filter-chip')))
 if(input$state!='all') chips <- c(chips,list(actionLink('clear_state',paste(input$state,'×'),class='filter-chip')))
 if(input$ownership!='all') chips <- c(chips,list(actionLink('clear_ownership',paste(input$ownership,'×'),class='filter-chip')))
 if(length(chips)) div(class='filter-chips',chips)
 })
 observeEvent(input$clear_measure,updateSelectInput(session,'measure',selected='all'))
 observeEvent(input$clear_state,updateSelectInput(session,'state',selected='all'))
 observeEvent(input$clear_ownership,updateSelectInput(session,'ownership',selected='all'))
 output$hospitals<-renderText(fmt(n_distinct(filtered_data()$provider_id)))
 output$observations<-renderText(fmt(nrow(filtered_data())))
 output$counties<-renderText(fmt(n_distinct(filtered_data()$county_fips)))
 output$mean_err<-renderText(if(nrow(filtered_data()))sprintf('%.3f',mean(filtered_data()$err))else '—')
 output$above_insight<-renderUI({d<-filtered_data();if(!nrow(d))return(tagList(h3('No matching records'),p('Reset or broaden your filters.')))
 tagList(div(class='insight-kicker','ABOVE EXPECTED'),h2(sprintf('%.1f%%',100*mean(d$err>1))),p('of records in this view have ERR > 1.'),NULL)})
 output$measure_insight<-renderUI({s<-measure_summary();if(!nrow(s))return(p('No measure results for this selection.'))
 top<-s %>% arrange(desc(mean_err),measure) %>% slice(1)
 tagList(div(class='insight-kicker','CONDITION / PROCEDURE COMPARISON'),h3(title=unname(measure_names[top$measure]),unname(measure_names[top$measure])),
 p(sprintf('%s: %.3f · %s hospitals.',if(nrow(s)>1)'Highest mean ERR'else 'Mean ERR',top$mean_err,fmt(top$hospitals))),NULL)})
 output$spread_insight<-renderUI({d<-filtered_data();if(!nrow(d))return(p('No distribution available.'))
 q<-quantile(d$err,c(.25,.75));tagList(div(class='insight-kicker','TYPICAL ERR RANGE'),h3(sprintf('%.3f – %.3f',q[1],q[2])),
 p('Middle 50% of ERR observations.'),NULL)})
 interactive_plot<-function(p) ggplotly(p,tooltip='text') %>% layout(font=list(family='Arial',color=navy,size=14),margin=list(l=55,r=20,t=10,b=55)) %>% config(displayModeBar=FALSE,responsive=TRUE)
 check_rows<-function(d) validate(need(nrow(d)>0,'No matching records. Broaden your filters or select Reset all.'))
 output$map<-renderPlotly({s<-map_data();check_rows(s)
 s$tip<-sprintf('%s<br>Mean ERR: %.3f<br>Hospitals: %s<br>Observations: %s',s$state,s$mean_err,fmt(s$hospitals),fmt(s$observations))
 s$map_value <- pmax(map_limits[1],pmin(map_limits[2],s$mean_err))
 p<-plot_ly(s,type='choropleth',locationmode='USA-states',locations=~state,z=~map_value,
 customdata=~state,text=~tip,hoverinfo='text',source='state_map',zmin=map_limits[1],zmax=map_limits[2],zmid=1,
 colorscale=list(c(0,'#245A9C'),c(.25,'#8DB4D5'),c(.5,'#E2E5EA'),c(.75,'#EBA97D'),c(1,'#E07832')),
 marker=list(line=list(color='#ffffff',width=1)),
 colorbar=list(orientation='v',x=1,xanchor='left',y=.5,yanchor='middle',title=list(text='Mean ERR',side='top',font=list(size=13)),thickness=12,len=.85,
 tickvals=c(.90,.95,1,1.05,1.10),ticktext=c('≤ 0.90','0.95','1.00','1.05','≥ 1.10'),tickfont=list(size=12)))
 # Include labels for all states and DC, even where no matching records remain.
 lab <- state_labels
 lab$tip <- s$tip[match(lab$state,s$state)]
 lab$tip[is.na(lab$tip)] <- paste(lab$state[is.na(lab$tip)],'— no matching observations')
 small <- lab[lab$state %in% callouts$state,]
 for(i in seq_len(nrow(small))) {
  p <- add_trace(p,type='scattergeo',mode='lines',lon=c(small$lon[i],small$label_lon[i]),lat=c(small$lat[i],small$label_lat[i]),
   line=list(color='#6b7280',width=.7),hoverinfo='skip',showlegend=FALSE,inherit=FALSE)
 }
 p <- add_trace(p,data=lab,type='scattergeo',mode='text',lon=~label_lon,lat=~label_lat,text=~state,
  customdata=~state,hovertext=~tip,hoverinfo='text',textfont=list(size=ifelse(lab$state %in% callouts$state,13,12),color='#172B4D',family='Arial'),
  showlegend=FALSE,inherit=FALSE) %>%
 layout(dragmode=FALSE,geo=list(domain=list(x=c(0,.96),y=c(0,1)),scope='usa',projection=list(type='albers usa',scale=1.04),showlakes=FALSE,showland=TRUE,landcolor='#D1D5DB',bgcolor='rgba(0,0,0,0)'),
 margin=list(l=0,r=0,t=0,b=0),paper_bgcolor='rgba(0,0,0,0)') %>%
 config(displayModeBar=FALSE,scrollZoom=FALSE,doubleClick=FALSE,topojsonURL='./')
 # Use bundled Plotly topology rather than a third-party map service.
 event_register(p,'plotly_click')
 })
 observeEvent(event_data('plotly_click',source='state_map',priority='event'),{
 e<-event_data('plotly_click',source='state_map',priority='event');st<-e$customdata
 if(is.null(st))st<-e$location
 if(length(st)&&st[1]%in%hospital_data$state)updateSelectInput(session,'state',selected=st[1])
 })
 output$map_stats <- renderUI({s<-map_data()
 if(!nrow(s)) return(NULL)
 div(class='chart-stats',stat_badge('Regions',fmt(nrow(s))),
 stat_badge('State mean ERR',sprintf('%.3f – %.3f',min(s$mean_err),max(s$mean_err)),'Range of unweighted state means; state filter does not restrict this nationwide map'))
 })
 output$hist_stats <- renderUI({d<-filtered_data();st<-record_stats(d)
 if(!st$n)return(NULL)
 div(class='chart-stats',stat_badge('Median ERR',sprintf('%.3f',st$median)),
 stat_badge('Middle 50%',sprintf('%.3f – %.3f',st$q1,st$q3)),stat_badge('ERR > 1',sprintf('%.1f%%',st$above)))
 })
 output$box_stats <- renderUI(summary_badges(filtered_data()))
 output$hospital_stats <- renderUI({d<-filtered_data()%>%filter(provider_id==input$hospital)
 if(!nrow(d))return(NULL)
 div(class='chart-stats',stat_badge('Conditions / procedures',fmt(n_distinct(d$measure))),stat_badge('Mean ERR',sprintf('%.3f',mean(d$err))))})
 output$heatmap_stats <- renderUI({d<-filtered_data()
 if(!nrow(d))return(NULL)
 div(class='chart-stats',stat_badge('Regions',fmt(n_distinct(d$state))),stat_badge('Conditions / procedures',fmt(n_distinct(d$measure))),
 stat_badge('Populated cells',fmt(nrow(distinct(d,state,measure)))))
 })
 output$scatter_stats <- renderUI({req(input$context);social_badges(social_analysis(filtered_data(),input$context))})
 output$explore_stats <- renderUI({d<-filtered_data();req(input$explore_by)
 if(!nrow(d))return(NULL)
 if(input$explore_by=='condition')return(div(class='chart-stats',
   stat_badge('Groups',fmt(n_distinct(d$measure))),stat_badge('Observations',fmt(nrow(d)))))
 if(input$explore_by=='ownership')return(summary_badges(d))
 social_badges(social_analysis(d,input$explore_by))
 })
 output$explore_note <- renderUI({req(input$explore_by)
 if(input$explore_by %in% unname(context_choices))
 div(class='chart-footnote','Muted orange: unadjusted linear trend. Gray dashed: ERR = 1. Correlation and trend describe records with repeated hospitals/counties; they are not adjusted or causal evidence.')
 else div(class='chart-footnote','Descriptive comparison, not adjusted for other factors. Hover to inspect group statistics and sample sizes.')
 })
 output$explore_heading <- renderUI({
 req(input$explore_by)
 title <- switch(input$explore_by,condition='ERR by condition / procedure',ownership='ERR by hospital ownership',paste('ERR and',tolower(context_label(input$explore_by))))
 div(class='explore-view-title',title)
 })
 output$explorer <- renderPlotly({
 d <- filtered_data();check_rows(d);req(input$explore_by)
 if(input$explore_by=='condition') {
  s <- measure_summary()
  short_names <- c(AMI='Heart attack (AMI)',CABG='Coronary bypass (CABG)',COPD='COPD',HF='Heart failure',PN='Pneumonia','HIP-KNEE'='Hip/knee arthroplasty')
  s$label <- unname(short_names[s$measure])
  s$tip <- sprintf('%s (%s)<br>Mean ERR %.3f<br>%s hospitals · %s observations',measure_names[s$measure],s$measure,s$mean_err,fmt(s$hospitals),fmt(s$n))
  return(interactive_plot(ggplot(s,aes(mean_err,reorder(label,mean_err),text=tip))+
   geom_vline(xintercept=1,linetype='longdash',color=navy,linewidth=.9)+geom_point(aes(color=mean_err>1),size=3.8)+
   scale_color_manual(values=c('FALSE'=blue,'TRUE'=amber),guide='none')+labs(x='Mean ERR',y=NULL)+th+theme(axis.text.y=element_text(size=11,face='plain'))) %>% layout(yaxis=list(domain=c(.08,.92))))
 }
 if(input$explore_by=='ownership') return(ownership_chart(d))
 social_chart(d,input$explore_by)
 })
 ownership_chart <- function(d) {
 groups <- sort(unique(d$ownership_group))
 p <- plot_ly()
 for(g in groups) {
  v <- d$err[d$ownership_group==g]; q <- quantile(v,c(.25,.5,.75))
  tip <- sprintf('%s<br>Median ERR %.3f<br>Middle 50%%: %.3f – %.3f<br>IQR width %.3f<br>Observations %s',
   g,q[2],q[1],q[3],q[3]-q[1],fmt(length(v)))
  label <- paste0(g,' (n=',fmt(length(v)),')')
  p <- add_trace(p,type='box',x=v,y=rep(label,length(v)),name=g,orientation='h',quartilemethod='linear',
   line=list(color=blue,width=1.4),fillcolor='rgba(36,90,156,0.15)',marker=list(color=blue,size=3,opacity=.25),
   boxpoints='outliers',showlegend=FALSE,hovertext=tip)
  p <- add_trace(p,type='scatter',mode='markers',x=unname(q[2]),y=label,
   marker=list(size=7,color=navy),text=tip,hoverinfo='text',showlegend=FALSE)
 }
 p %>% layout(font=list(size=13,family='Arial',color=navy),margin=list(l=180,r=20,t=10,b=55),
  xaxis=list(title='ERR'),yaxis=list(title=''),shapes=list(list(type='line',x0=1,x1=1,y0=0,y1=1,xref='x',yref='paper',
  line=list(color='#8994A5',dash='dash',width=1)))) %>% config(displayModeBar=FALSE,responsive=TRUE)
 }
 social_chart <- function(d,key) {
 a <- social_analysis(d,key);d <- a$data;check_rows(d)
 # Short tooltips preserve hospital/condition identity without obscuring the chart.
 d$tip <- sprintf('%s<br>%s · %s<br>%s county<br>ERR %.3f · X %.2f',d$hospital_name,d$state,
   d$measure,d$county_name,d$err,d[[key]])
 p <- plot_ly(d,x=d[[key]],y=~err,type='scatter',mode='markers',
  text=~tip,hoverinfo='text',marker=list(color=blue,size=2.5,opacity=.40),showlegend=FALSE)
 if(a$available) p <- add_trace(p,data=a$trend,x=~x,y=~y,type='scatter',mode='lines',
  line=list(color=trend_color,width=2.3),hoverinfo='skip',showlegend=FALSE,inherit=FALSE)
 p %>% layout(font=list(size=14,family='Arial',color=navy),margin=list(l=45,r=12,t=5,b=60),
  xaxis=list(title=list(text=paste(strwrap(context_label(key),width=40),collapse='<br>'),font=list(size=13)),gridcolor='#edf0f5',zeroline=FALSE),
  yaxis=list(title='ERR',gridcolor='#edf0f5',zeroline=FALSE),
  shapes=list(list(type='line',x0=0,x1=1,y0=1,y1=1,xref='paper',yref='y',layer='below',line=list(color='#929DAE',dash='dash',width=1))),
  hoverlabel=list(font=list(size=12),bgcolor='white',bordercolor='#B9C7D7')) %>% config(displayModeBar=FALSE,responsive=TRUE)
 }
 output$hist<-renderPlotly({d<-filtered_data();check_rows(d)
 bins<-floor(d$err/.025)*.025;h<-data.frame(bin=bins)%>%count(bin)
 h$tip<-sprintf('ERR %.3f – %.3f<br>%s observations',h$bin,h$bin+.025,fmt(h$n))
 interactive_plot(ggplot(h,aes(bin+.0125,n,text=tip))+geom_col(width=.024,fill=blue)+
 geom_vline(xintercept=1,linetype='longdash',color=amber,linewidth=.9)+
 scale_y_continuous(expand=expansion(mult=c(0,.10)))+labs(x='ERR',y='Observations')+th) %>%
 layout(margin=list(l=50,r=12,t=22,b=36),annotations=list(list(x=1,y=1.02,xref='x',yref='paper',
 text='Expected level · ERR = 1',showarrow=FALSE,bgcolor='white',borderpad=4,font=list(size=11,color='#A45120'))))
 })
 output$box<-renderPlotly({d<-filtered_data();check_rows(d);ownership_chart(d)})
 output$heatmap<-renderPlotly({d<-filtered_data();check_rows(d)
 s<-d%>%group_by(state,measure)%>%summarise(err=mean(err),n=n(),hospitals=n_distinct(provider_id),.groups='drop')
 s$tip<-sprintf('%s · %s<br>Mean ERR %.3f<br>%s hospitals · %s observations',s$state,s$measure,s$err,fmt(s$hospitals),fmt(s$n))
 interactive_plot(ggplot(s,aes(measure,state,fill=err,text=tip))+geom_tile(color='white')+
 scale_fill_gradientn(colors=c('#245A9C','#8DB4D5','#E2E5EA','#EBA97D','#E07832'),values=c(0,.25,.5,.75,1),limits=map_limits,oob=scales::squish,name='Mean ERR')+labs(x=NULL,y=NULL)+th+theme(panel.grid=element_blank()))
 })
 output$scatter<-renderPlotly({social_chart(filtered_data(),input$context)})
 output$hospital_plot<-renderPlotly({d<-filtered_data()%>%filter(provider_id==input$hospital);validate(need(nrow(d),'This hospital has no records under the current filters.'))
 d$tip<-sprintf('%s<br>ERR %.3f',d$measure,d$err)
 interactive_plot(ggplot(d,aes(err,measure,text=tip))+geom_vline(xintercept=1,linetype='longdash',color=navy,linewidth=.9)+geom_point(size=4,color=blue)+labs(x='ERR',y=NULL)+th)
 })
 table_data<-reactive({d<-filtered_data();q<-tolower(trimws(input$search))
 if(nzchar(q))d<-d[grepl(q,tolower(d$hospital_name),fixed=TRUE)|grepl(q,d$provider_id,fixed=TRUE),]
 switch(input$sort,high=arrange(d,desc(err)),low=arrange(d,err),name=arrange(d,hospital_name))
 })
 output$table_note<-renderText(paste('Showing',min(100,nrow(table_data())),'of',fmt(nrow(table_data())),'matching records. Download includes all matches. Search affects this table and download only.'))
 output$records<-renderTable({d<-head(table_data()%>%select(provider_id,hospital_name,state,measure,ownership_group,err),100)
 d$measure <- unname(measure_names[d$measure]); names(d)<-c('Facility ID','Hospital','State','Condition / procedure','Ownership','ERR');d},digits=3,striped=TRUE,spacing='s')
 output$download<-downloadHandler(filename=function()paste0('us-readmissions-',Sys.Date(),'.csv'),content=function(file)write_csv(table_data(),file))
}
shinyApp(ui,server)
