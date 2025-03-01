---
title: "Vancouver W1 Health Questionnaire"
date: "Sept. 13th 2019"


output:
  html_document:
    keep_md: true
    code_folding: hide
    fig_height: 7
    fig_width: 12
    highlight: tango
    theme: yeti
    toc: yes
    toc_float:
      collapsed: no
  pdf_document:
    toc: yes
    fig_height: 7
    fig_width: 12
    highlight: tango
  word_document:
    toc: yes

---



The INTErventions, Research, and Action in Cities Team (INTERACT) is a national research collaboration of scientists, urban planners, and engaged citizens uncovering how the design of our cities is shaping the health and wellbeing of Canadians (www.teaminteract.ca). INTERACT is conducting longitudinal, mixed-methods natural experiment studies in four Canadian cities, with the aim of providing evidence on the impacts of urban transformations on people's physical activity, social participation, and wellbeing, and inequalities in these outcomes. 

The Arbutus Greenway in Vancouver is a 9-km former rail corridor, which is being developed into a continuous walking and cycling corridor connecting South Vancouver to False Creek. Participants who lived in one of the 12 Forward Sortation Area (FSA) within 3 km of the Arbutus Greenway were eligible to participate. Exclusion criteria across all sites were being younger than 18 years old, not being able to read or write English (or English or French in Montreal) well enough to answer an online survey and any intention to move out of the region in the next two years. 

Participants were recruited from April 13th to September 21st 2018, through a letter campaign, social media, news media, street and community events outreach, and partner newsletters. 

In vancouver, 334 participants completed the Health Questionnaire. 



## Section 1: Transportation 

### What is your main mode of transportation?



```r
#transp_main_mode

transp_main_mode <- round(prop.table(table(factor(d$transp_main_mode, levels = c("1", "2", "3", "4", "5", "6"))))*100,2)
transp_main_mode <- as.data.frame(transp_main_mode)
transp_main_mode$answer <- substring(row.names(transp_main_mode), 1)
transp_main_mode$answer <- revalue(as.character(transp_main_mode$answer), c("1" = "Walking",  "2" = "Biking", "3"= "Public Transit", "4" = "Car", "5"= "Motorcycle or scooter", "6"= "Other"))

transp_main_mode$plot <- factor(transp_main_mode$answer, transp_main_mode$answer)

transp_main_mode.plot <- ggplot(transp_main_mode, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
  guides(fill = FALSE) +
  scale_fill_manual(values=INTERACTPaletteSet)  +
  ylab("Percent of total")

transp_main_mode.plot + geom_histogram(aes(x = plot), data = transp_main_mode, stat = "identity")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
transp_main_mode.tb <- as.factor(d$transp_main_mode)
transp_main_mode.tb <- summary(transp_main_mode.tb)
transp_main_mode.tb <- as.data.frame(transp_main_mode.tb)
transp_main_mode.tb$Var1 <- substring(row.names(transp_main_mode.tb), 1)
transp_main_mode.tb$answer <- revalue(as.character(transp_main_mode.tb$Var1), c("1" = "Walking",  "2" = "Biking", "3"= "Public Transit", "4" = "Car", "5"= "Motorcycle or scooter", "6"= "Other"))
plot.transp_main_mode <- merge(transp_main_mode, transp_main_mode.tb, by = "answer")
plot.transp_main_mode <- plot.transp_main_mode[-c(2, 4, 6)]
plot.transp_main_mode <- setcolorder(plot.transp_main_mode, c("answer", "transp_main_mode.tb", "Freq"))
plot.transp_main_mode$order <- c(2,4,5,6,3,1)
plot.transp_main_mode <- plot.transp_main_mode %>% arrange(order)
plot.transp_main_mode <- plot.transp_main_mode[-c(4)]
colnames(plot.transp_main_mode) <- c("Response", "N", "Proportion")

kable(plot.transp_main_mode) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Walking </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 23.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biking </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 15.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Public Transit </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 15.27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Car </td>
   <td style="text-align:right;"> 141 </td>
   <td style="text-align:right;"> 42.22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Motorcycle or scooter </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
</tbody>
</table>

### How much do you enjoy using each transportation mode? {.tabset}

#### walking

```r
#preferred_mode_a walking

preferred_mode_a <- round(prop.table(table(factor(d$preferred_mode_a, levels = c("1", "2", "3", "4", "5"))))*100,2)

preferred_mode_a <- as.data.frame(preferred_mode_a)
preferred_mode_a$group <- substring(row.names(preferred_mode_a), 1)
preferred_mode_a$group <- revalue(as.character(preferred_mode_a$group), c("1" = "1 A lot", "4" = "4 Not at all", "5" = "Not applicable"))

preferred_mode_a$plot <- factor(preferred_mode_a$group, preferred_mode_a$group)

preferred_mode_a.plot <- ggplot(preferred_mode_a, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=45, vjust=.6)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
preferred_mode_a.plot + geom_bar(aes(x = plot), data = preferred_mode_a, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
preferred_mode_a.tb <- as.factor(d$preferred_mode_a)
preferred_mode_a.tb <- summary(preferred_mode_a.tb)
preferred_mode_a.tb <- as.data.frame(preferred_mode_a.tb)
preferred_mode_a.tb$Var1 <- substring(row.names(preferred_mode_a.tb), 1)
preferred_mode_a.tb$group <- revalue(as.character(preferred_mode_a.tb$Var1), c("1" = "1 A lot",  "2" = "2", "3" = "3", "4" = "4 Not at all", "5" = "Not applicable"))
plot.preferred_mode_a <- merge(preferred_mode_a, preferred_mode_a.tb, by = "group")
plot.preferred_mode_a <- plot.preferred_mode_a[-c(2, 4, 6)]
plot.preferred_mode_a <- setcolorder(plot.preferred_mode_a, c("group", "preferred_mode_a.tb", "Freq"))

colnames(plot.preferred_mode_a) <- c("Response", "N", "Proportion")

kable(plot.preferred_mode_a) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 A lot </td>
   <td style="text-align:right;"> 235 </td>
   <td style="text-align:right;"> 70.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 22.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 Not at all </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not applicable </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>


#### biking

```r
#preferred_mode_b biking

preferred_mode_b <- round(prop.table(table(factor(d$preferred_mode_b, levels = c("1", "2", "3", "4", "5")), exclude=NULL))*100,2)
preferred_mode_b <- as.data.frame(preferred_mode_b)
preferred_mode_b$group <- substring(row.names(preferred_mode_b), 1)
preferred_mode_b$group <- revalue(as.character(preferred_mode_b$group), c("1" = "1 A lot", "4" = "4 Not at all", "5" = "Not applicable"))

preferred_mode_b$plot <- factor(preferred_mode_b$group, preferred_mode_b$group)

preferred_mode_b.plot <- ggplot(preferred_mode_b, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
preferred_mode_b.plot + geom_bar(aes(x = plot), data = preferred_mode_b, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
preferred_mode_b.tb <- as.factor(d$preferred_mode_b)
preferred_mode_b.tb <- summary(preferred_mode_b.tb)
preferred_mode_b.tb <- as.data.frame(preferred_mode_b.tb)
preferred_mode_b.tb$Var1 <- substring(row.names(preferred_mode_b.tb), 1)
preferred_mode_b.tb$group <- revalue(as.character(preferred_mode_b.tb$Var1), c("1" = "1 A lot",  "2" = "2", "3" = "3", "4" = "4 Not at all", "5" = "Not applicable"))
plot.preferred_mode_b <- merge(preferred_mode_b, preferred_mode_b.tb, by = "group")
plot.preferred_mode_b <- plot.preferred_mode_b[-c(2, 4, 6)]
plot.preferred_mode_b <- setcolorder(plot.preferred_mode_b, c("group", "preferred_mode_b.tb", "Freq"))

colnames(plot.preferred_mode_b) <- c("Response", "N", "Proportion")

kable(plot.preferred_mode_b) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 A lot </td>
   <td style="text-align:right;"> 150 </td>
   <td style="text-align:right;"> 44.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 18.26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 10.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 Not at all </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 8.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not applicable </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 17.66 </td>
  </tr>
</tbody>
</table>

#### public transit

```r
#preferred_mode_c public transit

preferred_mode_c <- round(prop.table(table(factor(d$preferred_mode_c, levels = c("1", "2", "3", "4", "5")), exclude=NULL))*100,2)
preferred_mode_c <- as.data.frame(preferred_mode_c)
preferred_mode_c$group <- substring(row.names(preferred_mode_c), 1)
preferred_mode_c$group <- revalue(as.character(preferred_mode_c$group), c("1" = "1 A lot", "4" = "4 Not at all", "5" = "Not applicable"))

preferred_mode_c$plot <- factor(preferred_mode_c$group, preferred_mode_c$group)

preferred_mode_c.plot <- ggplot(preferred_mode_c, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
preferred_mode_c.plot + geom_bar(aes(x = plot), data = preferred_mode_c, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
preferred_mode_c.tb <- as.factor(d$preferred_mode_c)
preferred_mode_c.tb <- summary(preferred_mode_c.tb)
preferred_mode_c.tb <- as.data.frame(preferred_mode_c.tb)
preferred_mode_c.tb$Var1 <- substring(row.names(preferred_mode_c.tb), 1)
preferred_mode_c.tb$group <- revalue(as.character(preferred_mode_c.tb$Var1), c("1" = "1 A lot",  "2" = "2", "3" = "3", "4" = "4 Not at all", "5" = "Not applicable"))
plot.preferred_mode_c <- merge(preferred_mode_c, preferred_mode_c.tb, by = "group")
plot.preferred_mode_c <- plot.preferred_mode_c[-c(2, 4, 6)]
plot.preferred_mode_c <- setcolorder(plot.preferred_mode_c, c("group", "preferred_mode_c.tb", "Freq"))

colnames(plot.preferred_mode_c) <- c("Response", "N", "Proportion")

kable(plot.preferred_mode_c) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 A lot </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 12.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 35.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 126 </td>
   <td style="text-align:right;"> 37.72 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 Not at all </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 11.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not applicable </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
</tbody>
</table>

#### car

```r
#preferred_mode_d car

preferred_mode_d <- round(prop.table(table(factor(d$preferred_mode_d, levels = c("1", "2", "3", "4", "5")), exclude=NULL))*100,2)
preferred_mode_d <- as.data.frame(preferred_mode_d)
preferred_mode_d$group <- substring(row.names(preferred_mode_d), 1)
preferred_mode_d$group <- revalue(as.character(preferred_mode_d$group), c("1" = "1 A lot", "4" = "4 Not at all", "5" = "Not applicable"))

preferred_mode_d$plot <- factor(preferred_mode_d$group, preferred_mode_d$group)

preferred_mode_d.plot <- ggplot(preferred_mode_d, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
preferred_mode_d.plot + geom_bar(aes(x = plot), data = preferred_mode_d, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
preferred_mode_d.tb <- as.factor(d$preferred_mode_d)
preferred_mode_d.tb <- summary(preferred_mode_d.tb)
preferred_mode_d.tb <- as.data.frame(preferred_mode_d.tb)
preferred_mode_d.tb$Var1 <- substring(row.names(preferred_mode_d.tb), 1)
preferred_mode_d.tb$group <- revalue(as.character(preferred_mode_d.tb$Var1), c("1" = "1 A lot",  "2" = "2", "3" = "3", "4" = "4 Not at all", "5" = "Not applicable"))
plot.preferred_mode_d <- merge(preferred_mode_d, preferred_mode_d.tb, by = "group")
plot.preferred_mode_d <- plot.preferred_mode_d[-c(2, 4, 6)]
plot.preferred_mode_d <- setcolorder(plot.preferred_mode_d, c("group", "preferred_mode_d.tb", "Freq"))

colnames(plot.preferred_mode_d) <- c("Response", "N", "Proportion")

kable(plot.preferred_mode_d) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 A lot </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 23.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 34.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 29.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 Not at all </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 5.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not applicable </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 6.59 </td>
  </tr>
</tbody>
</table>

#### motorcycle or scooter


```r
#preferred_mode_e motorcycle or scooter

preferred_mode_e <- round(prop.table(table(factor(d$preferred_mode_e, levels = c("1", "2", "3", "4", "5")), exclude=NULL))*100,2)
preferred_mode_e <- as.data.frame(preferred_mode_e)
preferred_mode_e$group <- substring(row.names(preferred_mode_e), 1)
preferred_mode_e$group <- revalue(as.character(preferred_mode_e$group), c("1" = "1 A lot", "4" = "4 Not at all", "5" = "Not applicable"))

preferred_mode_e$plot <- factor(preferred_mode_e$group, preferred_mode_e$group)

preferred_mode_e.plot <- ggplot(preferred_mode_e, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
preferred_mode_e.plot + geom_bar(aes(x = plot), data = preferred_mode_e, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
preferred_mode_e.tb <- as.factor(d$preferred_mode_e)
preferred_mode_e.tb <- summary(preferred_mode_e.tb)
preferred_mode_e.tb <- as.data.frame(preferred_mode_e.tb)
preferred_mode_e.tb$Var1 <- substring(row.names(preferred_mode_e.tb), 1)
preferred_mode_e.tb$group <- revalue(as.character(preferred_mode_e.tb$Var1), c("1" = "1 A lot",  "2" = "2", "3" = "3", "4" = "4 Not at all", "5" = "Not applicable"))
plot.preferred_mode_e <- merge(preferred_mode_e, preferred_mode_e.tb, by = "group")
plot.preferred_mode_e <- plot.preferred_mode_e[-c(2, 4, 6)]
plot.preferred_mode_e <- setcolorder(plot.preferred_mode_e, c("group", "preferred_mode_e.tb", "Freq"))

colnames(plot.preferred_mode_e) <- c("Response", "N", "Proportion")

kable(plot.preferred_mode_e) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 A lot </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 Not at all </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 13.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not applicable </td>
   <td style="text-align:right;"> 267 </td>
   <td style="text-align:right;"> 79.94 </td>
  </tr>
</tbody>
</table>




### Do you have access to a car?


```r
#car_access 

car_access <- round(prop.table(table(factor(d$car_access, levels = c("1", "2")), exclude=NULL))*100,2)
car_access <- as.data.frame(car_access)
car_access$answer <- substring(row.names(car_access), 1)
car_access$answer <- revalue(as.character(car_access$answer), c("1" = "Yes",  "2" = "No"))

car_access$plot <- factor(car_access$answer, car_access$answer)

car_access.plot <- ggplot(car_access, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in t5

car_access.plot + geom_bar(aes(x = plot), data = car_access, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
car_access.tb <- as.factor(d$car_access)
car_access.tb <- summary(car_access.tb)
car_access.tb <- as.data.frame(car_access.tb)
car_access.tb$Var1 <- substring(row.names(car_access.tb), 1)
car_access.tb$answer <- revalue(as.character(car_access.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.car_access <- merge(car_access, car_access.tb, by = "answer")
plot.car_access <- plot.car_access[-c(2, 4, 6)]
plot.car_access <- setcolorder(plot.car_access, c("answer", "car_access.tb", "Freq"))
plot.car_access$order <- c(2, 1)
plot.car_access <- plot.car_access %>% arrange(order)
plot.car_access <- plot.car_access[-c(4)]
colnames(plot.car_access) <- c("Response", "N", "Proportion")

kable(plot.car_access) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 294 </td>
   <td style="text-align:right;"> 88.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 40 </td>
   <td style="text-align:right;"> 11.98 </td>
  </tr>
</tbody>
</table>

### Do you have access to a bicycle?


```r
#bike_access 

bike_access <- round(prop.table(table(factor(d$bike_access, levels = c("1", "2")), exclude=NULL))*100,2)
bike_access <- as.data.frame(bike_access)
bike_access$answer <- substring(row.names(bike_access), 1)
bike_access$answer <- revalue(as.character(bike_access$answer), c("1" = "Yes",  "2" = "No"))

bike_access$plot <- factor(bike_access$answer, bike_access$answer)

bike_access.plot <- ggplot(bike_access, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in t5

bike_access.plot + geom_bar(aes(x = plot), data = bike_access, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
bike_access.tb <- as.factor(d$bike_access)
bike_access.tb <- summary(bike_access.tb)
bike_access.tb <- as.data.frame(bike_access.tb)
bike_access.tb$Var1 <- substring(row.names(bike_access.tb), 1)
bike_access.tb$answer <- revalue(as.character(bike_access.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.bike_access <- merge(bike_access, bike_access.tb, by = "answer")
plot.bike_access <- plot.bike_access[-c(2, 4, 6)]
plot.bike_access <- setcolorder(plot.bike_access, c("answer", "bike_access.tb", "Freq"))
plot.bike_access$order <- c(2, 1)
plot.bike_access <- plot.bike_access %>% arrange(order)
plot.bike_access <- plot.bike_access[-c(4)]
colnames(plot.bike_access) <- c("Response", "N", "Proportion")

kable(plot.bike_access) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 267 </td>
   <td style="text-align:right;"> 79.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 20.06 </td>
  </tr>
</tbody>
</table>


### On a scale of 1 to 5, with 1 being 'very safe' and 5 being 'very dangerous', overall, how safe do you think cycling is in your city?



```r
#bike_safety

bike_safety <- round(prop.table(table(factor(d$bike_safety, levels = 1:5)))*100,2)
bike_safety <- as.data.frame(bike_safety)
bike_safety$group <- substring(row.names(bike_safety), 1)
bike_safety$group <- revalue(as.character(bike_safety$group), c("1" = "Very safe", "2" = "Somewhat safe", "3" = "Neither safe nor unsafe", "4" = "Somewhat dangerous", "5" = "Very dangerous"))
bike_safety$plot <- factor(bike_safety$group, bike_safety$group)


p <- ggplot(bike_safety, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in bike_safety


p + geom_bar(aes(x = plot), data = bike_safety, stat = "identity") +
  scale_fill_manual(values=INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("Perception of bicycle risk")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
bike_safety.tb <- as.factor(d$bike_safety)
bike_safety.tb <- summary(bike_safety.tb)
bike_safety.tb <- as.data.frame(bike_safety.tb)
bike_safety.tb$Var1 <- substring(row.names(bike_safety.tb), 1)
bike_safety.tb$group <- revalue(as.character(bike_safety.tb$Var1), c("1" = "Very safe", "2" = "Somewhat safe", "3" = "Neither safe nor unsafe", "4" = "Somewhat dangerous", "5" = "Very dangerous"))
plot.bike_safety <- merge(bike_safety, bike_safety.tb, by = "group")
plot.bike_safety <- plot.bike_safety[-c(2, 4, 6)]
plot.bike_safety <- setcolorder(plot.bike_safety, c("group", "bike_safety.tb", "Freq"))
plot.bike_safety$order <- c(3, 4, 2, 5, 1)
plot.bike_safety <- plot.bike_safety %>% arrange(order)
plot.bike_safety <- plot.bike_safety[-c(4)]
colnames(plot.bike_safety) <- c("Response", "N", "Proportion")

kable(plot.bike_safety) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very safe </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 13.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat safe </td>
   <td style="text-align:right;"> 166 </td>
   <td style="text-align:right;"> 51.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither safe nor unsafe </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 7.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat dangerous </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 25.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very dangerous </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.46 </td>
  </tr>
</tbody>
</table>


### Have you ever heard of the Arbutus Greenway? 



```r
#ag_familiarty 

ag_familiarity <- round(prop.table(table(factor(d$ag_familiarity, levels = c("1", "2"))))*100,2)
ag_familiarity <- as.data.frame(ag_familiarity)
ag_familiarity$answer <- substring(row.names(ag_familiarity), 1)
ag_familiarity$answer <- revalue(as.character(ag_familiarity$answer), c("1" = "Yes",  "2" = "No"))

ag_familiarity$plot <- factor(ag_familiarity$answer, ag_familiarity$answer)

ag_familiarity.plot <- ggplot(ag_familiarity, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in t5

ag_familiarity.plot + geom_bar(aes(x = plot), data = ag_familiarity, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
ag_familiarity.tb <- as.factor(d$ag_familiarity)
ag_familiarity.tb <- summary(ag_familiarity.tb)
ag_familiarity.tb <- as.data.frame(ag_familiarity.tb)
ag_familiarity.tb$Var1 <- substring(row.names(ag_familiarity.tb), 1)

nval.df <- c("0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$ag_familiarity.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("2")
nval.df <- nval.df[-c(1)]

ag_familiarity.tb <- rbind(ag_familiarity.tb, nval.df)

ag_familiarity.tb$answer <- revalue(as.character(ag_familiarity.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.ag_familiarity <- merge(ag_familiarity, ag_familiarity.tb, by = "answer")
plot.ag_familiarity <- plot.ag_familiarity[-c(2, 4, 6)]
plot.ag_familiarity <- setcolorder(plot.ag_familiarity, c("answer", "ag_familiarity.tb", "Freq"))
colnames(plot.ag_familiarity) <- c("Response", "N", "Proportion")

kable(plot.ag_familiarity) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> 334 </td>
   <td style="text-align:right;"> 100 </td>
  </tr>
</tbody>
</table>


### Do you think that the Arbutus Greenway is a good or bad idea for Vancouver? It is a...


```r
ag_idea <- round(prop.table(table(factor(d$ag_idea)))*100,2)
ag_idea <- as.data.frame(ag_idea)
ag_idea$group <- substring(row.names(ag_idea), 1)
ag_idea$group <- revalue(as.character(ag_idea$group), c("1" = "Very good idea", "2" = "Somewhat good idea", "3" = "Somewhat bad idea", "4" = "Very bad idea", "5" = "I don't know"))
ag_idea$plot <- factor(ag_idea$group, ag_idea$group)


p <- ggplot(ag_idea, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in ag_idea


p + geom_bar(aes(x = plot), data = ag_idea, stat = "identity") +
  scale_fill_manual(values=INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("Perception of AG")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ag_idea.tb <- as.factor(d$ag_idea)
ag_idea.tb <- summary(ag_idea.tb)
ag_idea.tb <- as.data.frame(ag_idea.tb)
ag_idea.tb$Var1 <- substring(row.names(ag_idea.tb), 1)
ag_idea.tb$group <- revalue(as.character(ag_idea.tb$Var1), c("1" = "Very good idea", "2" = "Somewhat good idea", "3" = "Somewhat bad idea", "4" = "Very bad idea", "5" = "I don't know"))
plot.ag_idea <- merge(ag_idea, ag_idea.tb, by = "group")
plot.ag_idea <- plot.ag_idea[-c(2, 4, 6)]
plot.ag_idea <- setcolorder(plot.ag_idea, c("group", "ag_idea.tb", "Freq"))
plot.ag_idea$order <- c(3, 2,4,1)
plot.ag_idea <- plot.ag_idea %>% arrange(order)
plot.ag_idea <- plot.ag_idea[-c(4)]
colnames(plot.ag_idea) <- c("Response", "N", "Proportion")

kable(plot.ag_idea) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very good idea </td>
   <td style="text-align:right;"> 307 </td>
   <td style="text-align:right;"> 91.92 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat good idea </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat bad idea </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very bad idea </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>

### Have you ever used the Arbutus Greenway?

```r
#ag_used_ever

ag_used_ever <- round(prop.table(table(factor(d$ag_used_ever, levels = c("1", "2")), exclude=NULL))*100,2)
ag_used_ever <- as.data.frame(ag_used_ever)
ag_used_ever$answer <- substring(row.names(ag_used_ever), 1)
ag_used_ever$answer <- revalue(as.character(ag_used_ever$answer), c("1" = "Yes",  "2" = "No"))

ag_used_ever$plot <- factor(ag_used_ever$answer, ag_used_ever$answer)

ag_used_ever.plot <- ggplot(ag_used_ever, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in t5

ag_used_ever.plot + geom_bar(aes(x = plot), data = ag_used_ever, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ag_used_ever.tb <- as.factor(d$ag_used_ever)
ag_used_ever.tb <- summary(ag_used_ever.tb)
ag_used_ever.tb <- as.data.frame(ag_used_ever.tb)
ag_used_ever.tb$Var1 <- substring(row.names(ag_used_ever.tb), 1)
ag_used_ever.tb$answer <- revalue(as.character(ag_used_ever.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.ag_used_ever <- merge(ag_used_ever, ag_used_ever.tb, by = "answer")
plot.ag_used_ever <- plot.ag_used_ever[-c(2, 4, 6)]
plot.ag_used_ever <- setcolorder(plot.ag_used_ever, c("answer", "ag_used_ever.tb", "Freq"))
plot.ag_used_ever$order <- c(2, 1)
plot.ag_used_ever <- plot.ag_used_ever %>% arrange(order)
plot.ag_used_ever <- plot.ag_used_ever[-c(4)]
colnames(plot.ag_used_ever) <- c("Response", "N", "Proportion")

kable(plot.ag_used_ever) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 310 </td>
   <td style="text-align:right;"> 92.81 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 7.19 </td>
  </tr>
</tbody>
</table>



### How often do you typically travel by foot along the Arbutus Greenway during each season?



```r
# ag_walk_freq_a

d$ag_walk_freq_a[d$ag_walk_freq_a==-7] <- NA
d$ag_walk_freq_b[d$ag_walk_freq_b==-7] <- NA
d$ag_walk_freq_c[d$ag_walk_freq_c==-7] <- NA
d$ag_walk_freq_d[d$ag_walk_freq_d==-7] <- NA

fall <- ggplot(d, aes(x = d$ag_walk_freq_a)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#BF5B04") + xlab("Times in the fall")
winter <- ggplot(d, aes(x = d$ag_walk_freq_b)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#35AAF2") + xlab("Times in the winter")
spring <- ggplot(d, aes(x = d$ag_walk_freq_c)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#76D24A") + xlab("Times in the spring")
summer <- ggplot(d, aes(x = d$ag_walk_freq_d)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#F2B705") + xlab("Times in the summer")



grid.arrange(fall,winter,spring,summer)
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


### How often do you typically travel by bicycle along the Arbutus Greenway during each season?


```r
# ag_bike_freq_a

d$ag_bike_freq_a[d$ag_bike_freq_a==-7] <- NA
d$ag_bike_freq_b[d$ag_bike_freq_b==-7] <- NA
d$ag_bike_freq_c[d$ag_bike_freq_c==-7] <- NA
d$ag_bike_freq_d[d$ag_bike_freq_d==-7] <- NA

fall <- ggplot(d, aes(x = d$ag_bike_freq_a)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#BF5B04") + xlab("Times in the fall")
winter <- ggplot(d, aes(x = d$ag_bike_freq_b)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#35AAF2") + xlab("Times in the winter")
spring <- ggplot(d, aes(x = d$ag_bike_freq_c)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#76D24A") + xlab("Times in the spring")
summer <- ggplot(d, aes(x = d$ag_bike_freq_d)) + geom_histogram (na.rm = TRUE, binwidth = 5, fill="#F2B705") + xlab("Times in the summer")


grid.arrange(fall,winter,spring,summer)
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


### How do you usually get to the Arbutus Greenway?



```r
#intercept_ag_mode

d$intercept_ag_mode[d$intercept_ag_mode==-7] <- NA

intercept_ag_mode <- round(prop.table(table(factor(d$intercept_ag_mode, levels = c("1", "2", "3", "4", "5", "6", "7"))))*100,2)
intercept_ag_mode <- as.data.frame(intercept_ag_mode)
intercept_ag_mode$answer <- substring(row.names(intercept_ag_mode), 1)
intercept_ag_mode$answer <- revalue(as.character(intercept_ag_mode$answer), c("1" = "Walking",  "2" = "Running/Jogging", "3"= "Biking", "4" = "Public Transit", "5"= "Car", "6"= "Motorcycle or scooter", "7" ="Other"))

intercept_ag_mode$plot <- factor(intercept_ag_mode$answer, intercept_ag_mode$answer)

intercept_ag_mode.plot <- ggplot(intercept_ag_mode, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
  guides(fill = FALSE) +
  scale_fill_manual(values = INTERACTPaletteSet) +
  ylab("Percent of total") +
  xlab("")

intercept_ag_mode.plot + geom_histogram(aes(x = plot), data = intercept_ag_mode, stat = "identity")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
intercept_ag_mode.tb <- as.factor(d$intercept_ag_mode)
intercept_ag_mode.tb <- summary(intercept_ag_mode.tb)
intercept_ag_mode.tb <- as.data.frame(intercept_ag_mode.tb)
intercept_ag_mode.tb$Var1 <- substring(row.names(intercept_ag_mode.tb), 1)
intercept_ag_mode.tb$answer <- revalue(as.character(intercept_ag_mode.tb$Var1), c("1" = "Walking",  "2" = "Running/Jogging", "3"= "Biking", "4" = "Public Transit", "5"= "Car", "6"= "Motorcycle or scooter", "7" ="Other"))
plot.intercept_ag_mode <- merge(intercept_ag_mode, intercept_ag_mode.tb, by = "answer")
plot.intercept_ag_mode <- plot.intercept_ag_mode[-c(2, 4, 6)]
plot.intercept_ag_mode <- setcolorder(plot.intercept_ag_mode, c("answer", "intercept_ag_mode.tb", "Freq"))
plot.intercept_ag_mode$order <- c(3,5,6,4,2,1)
plot.intercept_ag_mode <- plot.intercept_ag_mode %>% arrange(order)
plot.intercept_ag_mode <- plot.intercept_ag_mode[-c(4)]
colnames(plot.intercept_ag_mode) <- c("Response", "N", "Proportion")

kable(plot.intercept_ag_mode) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Walking </td>
   <td style="text-align:right;"> 172 </td>
   <td style="text-align:right;"> 55.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Running/Jogging </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biking </td>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 33.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Public Transit </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Car </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.94 </td>
  </tr>
</tbody>
</table>

```r
#kable(table(d$intercept_ag_mode_txt))  %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```


### What is your usual reason for using the Arbutus Greenway?


```r
# intercept_ag_reason


d$intercept_ag_reason[d$intercept_ag_reason==-7] <- NA

intercept_ag_reason <- round(prop.table(table(factor(d$intercept_ag_reason)))*100,2)
intercept_ag_reason <- as.data.frame(intercept_ag_reason)
intercept_ag_reason$group <- substring(row.names(intercept_ag_reason), 1)
intercept_ag_reason$group <- revalue(as.character(intercept_ag_reason$group), c("1" = "For recreation", "2" = "For transportation", "3" = "Both for recreation and transportation"))

intercept_ag_reason$plot <- factor(intercept_ag_reason$group, intercept_ag_reason$group)


p <- ggplot(intercept_ag_reason, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) 


p + geom_bar(aes(x = plot), data = intercept_ag_reason, stat = "identity") + 
  scale_fill_manual(values=INTERACTPaletteSet) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
intercept_ag_reason.tb <- as.factor(d$intercept_ag_reason)
intercept_ag_reason.tb <- summary(intercept_ag_reason.tb)
intercept_ag_reason.tb <- as.data.frame(intercept_ag_reason.tb)
intercept_ag_reason.tb$Var1 <- substring(row.names(intercept_ag_reason.tb), 1)
intercept_ag_reason.tb$group <- revalue(as.character(intercept_ag_reason.tb$Var1), c("1" = "For recreation", "2" = "For transportation", "3" = "Both for recreation and transportation"))
plot.intercept_ag_reason <- merge(intercept_ag_reason, intercept_ag_reason.tb, by = "group")
plot.intercept_ag_reason <- plot.intercept_ag_reason[-c(2, 4, 6)]
plot.intercept_ag_reason <- setcolorder(plot.intercept_ag_reason, c("group", "intercept_ag_reason.tb", "Freq"))
plot.intercept_ag_reason$order <- c(3,1,2)
plot.intercept_ag_reason <- plot.intercept_ag_reason %>% arrange(order)
plot.intercept_ag_reason <- plot.intercept_ag_reason[-c(4)]
colnames(plot.intercept_ag_reason) <- c("Response", "N", "Proportion")

kable(plot.intercept_ag_reason) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> For recreation </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 30.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> For transportation </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 17.42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Both for recreation and transportation </td>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 51.61 </td>
  </tr>
</tbody>
</table>


### In your opinion, the maintenance of the Arbutus Greenway is excellent, good, fair, or poor?


```r
## intercept_ag_maintenance

d$intercept_ag_maintenance[d$intercept_ag_maintenance==-7] <- NA

intercept_ag_maintenance <- round(prop.table(table(factor(d$intercept_ag_maintenance)))*100,2)
intercept_ag_maintenance <- as.data.frame(intercept_ag_maintenance)
intercept_ag_maintenance$group <- substring(row.names(intercept_ag_maintenance), 1)
intercept_ag_maintenance$group <- revalue(as.character(intercept_ag_maintenance$group), c("1" = "Excellent", "2" = "Good", "3" = "Fair", "4" ="Poor", "5" = "I don't know"))

intercept_ag_maintenance$plot <- factor(intercept_ag_maintenance$group, intercept_ag_maintenance$group)


p <- ggplot(intercept_ag_maintenance, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) 


p + geom_bar(aes(x = plot), data = intercept_ag_maintenance, stat = "identity") + 
  scale_fill_manual(values=INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
intercept_ag_maintenance.tb <- as.factor(d$intercept_ag_maintenance)
intercept_ag_maintenance.tb <- summary(intercept_ag_maintenance.tb)
intercept_ag_maintenance.tb <- as.data.frame(intercept_ag_maintenance.tb)
intercept_ag_maintenance.tb$Var1 <- substring(row.names(intercept_ag_maintenance.tb), 1)
intercept_ag_maintenance.tb$group <- revalue(as.character(intercept_ag_maintenance.tb$Var1), c("1" = "Excellent", "2" = "Good", "3" = "Fair", "4" ="Poor", "77" = "I don't know"))
plot.intercept_ag_maintenance <- merge(intercept_ag_maintenance, intercept_ag_maintenance.tb, by = "group")
plot.intercept_ag_maintenance <- plot.intercept_ag_maintenance[-c(2, 4, 6)]
plot.intercept_ag_maintenance <- setcolorder(plot.intercept_ag_maintenance, c("group", "intercept_ag_maintenance.tb", "Freq"))
plot.intercept_ag_maintenance$order <- c(1,3,2,5,4)
plot.intercept_ag_maintenance <- plot.intercept_ag_maintenance %>% arrange(order)
plot.intercept_ag_maintenance <- plot.intercept_ag_maintenance[-c(4)]
colnames(plot.intercept_ag_maintenance) <- c("Response", "N", "Proportion")

kable(plot.intercept_ag_maintenance) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Excellent </td>
   <td style="text-align:right;"> 138 </td>
   <td style="text-align:right;"> 44.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Good </td>
   <td style="text-align:right;"> 140 </td>
   <td style="text-align:right;"> 45.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fair </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 7.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poor </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.29 </td>
  </tr>
</tbody>
</table>


### How safe do you feel travelling along the Arbutus Greenway in terms of{.tabset}

#### safety from traffic?



```r
#intercept_ag_safety_traffic 

d$intercept_ag_safety_traffic[d$intercept_ag_safety_traffic==-7] <- NA

intercept_ag_safety_traffic <- round(prop.table(table(factor(d$intercept_ag_safety_traffic)))*100,2)
intercept_ag_safety_traffic <- as.data.frame(intercept_ag_safety_traffic)
intercept_ag_safety_traffic$group <- substring(row.names(intercept_ag_safety_traffic), 1)
intercept_ag_safety_traffic$group <- revalue(as.character(intercept_ag_safety_traffic$group), c("1" = "Very safe", "2" = "Somewhat safe", "3" = "Neither safe nor unsafe", "4" ="Somewhat unsafe", "5" =  "Very unsafe" , "6" = "I don't know"))

intercept_ag_safety_traffic$plot <- factor(intercept_ag_safety_traffic$group, intercept_ag_safety_traffic$group)


p <- ggplot(intercept_ag_safety_traffic, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) 


p + geom_bar(aes(x = plot), data = intercept_ag_safety_traffic, stat = "identity") + 
  scale_fill_manual(values=INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
intercept_ag_safety_traffic.tb <- as.factor(d$intercept_ag_safety_traffic)
intercept_ag_safety_traffic.tb <- summary(intercept_ag_safety_traffic.tb)
intercept_ag_safety_traffic.tb <- as.data.frame(intercept_ag_safety_traffic.tb)
intercept_ag_safety_traffic.tb$Var1 <- substring(row.names(intercept_ag_safety_traffic.tb), 1)
intercept_ag_safety_traffic.tb$group <- revalue(as.character(intercept_ag_safety_traffic.tb$Var1), c("1" = "Very safe", "2" = "Somewhat safe", "3" = "Neither safe nor unsafe", "4" ="Somewhat unsafe", "5" =  "Very unsafe" , "77" = "I don't know"))
plot.intercept_ag_safety_traffic <- merge(intercept_ag_safety_traffic, intercept_ag_safety_traffic.tb, by = "group")
plot.intercept_ag_safety_traffic <- plot.intercept_ag_safety_traffic[-c(2, 4, 6)]
plot.intercept_ag_safety_traffic <- setcolorder(plot.intercept_ag_safety_traffic, c("group", "intercept_ag_safety_traffic.tb", "Freq"))
plot.intercept_ag_safety_traffic$order <- c(6,3,2,4,1,5)
plot.intercept_ag_safety_traffic <- plot.intercept_ag_safety_traffic %>% arrange(order)
plot.intercept_ag_safety_traffic <- plot.intercept_ag_safety_traffic[-c(4)]
colnames(plot.intercept_ag_safety_traffic) <- c("Response", "N", "Proportion")

kable(plot.intercept_ag_safety_traffic) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very safe </td>
   <td style="text-align:right;"> 225 </td>
   <td style="text-align:right;"> 72.58 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat safe </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 23.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither safe nor unsafe </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat unsafe </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very unsafe </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.32 </td>
  </tr>
</tbody>
</table>
#### personal safety? 


```r
#intercept_ag_safety_personal 


d$intercept_ag_safety_personal[d$intercept_ag_safety_personal==-7] <- NA

intercept_ag_safety_personal <- round(prop.table(table(factor(d$intercept_ag_safety_personal)))*100,2)
intercept_ag_safety_personal <- as.data.frame(intercept_ag_safety_personal)
intercept_ag_safety_personal$group <- substring(row.names(intercept_ag_safety_personal), 1)
intercept_ag_safety_personal$group <- revalue(as.character(intercept_ag_safety_personal$group), c("1" = "Very safe", "2" = "Somewhat safe", "3" = "Neither safe nor unsafe", "4" ="Somewhat unsafe", "5" =  "Very unsafe" , "6" = "I don't know"))

intercept_ag_safety_personal$plot <- factor(intercept_ag_safety_personal$group, intercept_ag_safety_personal$group)


p <- ggplot(intercept_ag_safety_personal, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) 


p + geom_bar(aes(x = plot), data = intercept_ag_safety_personal, stat = "identity") + 
  scale_fill_manual(values=INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
intercept_ag_safety_personal.tb <- as.factor(d$intercept_ag_safety_personal)
intercept_ag_safety_personal.tb <- summary(intercept_ag_safety_personal.tb)
intercept_ag_safety_personal.tb <- as.data.frame(intercept_ag_safety_personal.tb)
intercept_ag_safety_personal.tb$Var1 <- substring(row.names(intercept_ag_safety_personal.tb), 1)
intercept_ag_safety_personal.tb$group <- revalue(as.character(intercept_ag_safety_personal.tb$Var1), c("1" = "Very safe", "2" = "Somewhat safe", "3" = "Neither safe nor unsafe", "4" ="Somewhat unsafe", "5" =  "Very unsafe" , "77" = "I don't know"))
plot.intercept_ag_safety_personal <- merge(intercept_ag_safety_personal, intercept_ag_safety_personal.tb, by = "group")
plot.intercept_ag_safety_personal <- plot.intercept_ag_safety_personal[-c(2, 4, 6)]
plot.intercept_ag_safety_personal <- setcolorder(plot.intercept_ag_safety_personal, c("group", "intercept_ag_safety_personal.tb", "Freq"))
plot.intercept_ag_safety_personal$order <- c(6,3,2,4,1,5)
plot.intercept_ag_safety_personal <- plot.intercept_ag_safety_personal %>% arrange(order)
plot.intercept_ag_safety_personal <- plot.intercept_ag_safety_personal[-c(4)]
colnames(plot.intercept_ag_safety_personal) <- c("Response", "N", "Proportion")

kable(plot.intercept_ag_safety_personal) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very safe </td>
   <td style="text-align:right;"> 216 </td>
   <td style="text-align:right;"> 69.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat safe </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 26.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither safe nor unsafe </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.61 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat unsafe </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very unsafe </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.65 </td>
  </tr>
</tbody>
</table>



### Are you using the Arbutus Greenway (walking, biking, etc.) more, less, or the same since spring 2017?


```r
#intercept_ag_spring

d$intercept_ag_spring[d$intercept_ag_spring==-7] <- NA

intercept_ag_spring <- round(prop.table(table(factor(d$intercept_ag_spring)))*100,2)
intercept_ag_spring <- as.data.frame(intercept_ag_spring)
intercept_ag_spring$group <- substring(row.names(intercept_ag_spring), 1)
intercept_ag_spring$group <- revalue(as.character(intercept_ag_spring$group), c("1" = "More", "2" = "Same", "3" = "Less", "4" = "I don't know"))

intercept_ag_spring$plot <- factor(intercept_ag_spring$group, intercept_ag_spring$group)


p <- ggplot(intercept_ag_spring, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) 


p + geom_bar(aes(x = plot), data = intercept_ag_spring, stat = "identity") + 
  scale_fill_manual(values=INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
intercept_ag_spring.tb <- as.factor(d$intercept_ag_spring)
intercept_ag_spring.tb <- summary(intercept_ag_spring.tb)
intercept_ag_spring.tb <- as.data.frame(intercept_ag_spring.tb)
intercept_ag_spring.tb$Var1 <- substring(row.names(intercept_ag_spring.tb), 1)
intercept_ag_spring.tb$group <- revalue(as.character(intercept_ag_spring.tb$Var1), c("1" = "More", "2" = "Same", "3" = "Less", "77" = "I don't know"))
plot.intercept_ag_spring <- merge(intercept_ag_spring, intercept_ag_spring.tb, by = "group")
plot.intercept_ag_spring <- plot.intercept_ag_spring[-c(2, 4, 6)]
plot.intercept_ag_spring <- setcolorder(plot.intercept_ag_spring, c("group", "intercept_ag_spring.tb", "Freq"))
plot.intercept_ag_spring$order <- c(4,3,1,2)
plot.intercept_ag_spring <- plot.intercept_ag_spring %>% arrange(order)
plot.intercept_ag_spring <- plot.intercept_ag_spring[-c(4)]
colnames(plot.intercept_ag_spring) <- c("Response", "N", "Proportion")

kable(plot.intercept_ag_spring) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> More </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 58.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Same </td>
   <td style="text-align:right;"> 113 </td>
   <td style="text-align:right;"> 36.45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Less </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.26 </td>
  </tr>
</tbody>
</table>


### Do you plan to use the Arbutus Greenway in the future?


```r
# intercept_ag_future


intercept_ag_future <- round(prop.table(table(factor(d$intercept_ag_future)))*100,2)
intercept_ag_future <- as.data.frame(intercept_ag_future)
intercept_ag_future$group <- substring(row.names(intercept_ag_future), 1)
intercept_ag_future$group <- revalue(as.character(intercept_ag_future$group), c("1" = "Yes",  "2" = "No"))

intercept_ag_future$group <- factor(intercept_ag_future$group, intercept_ag_future$group)

intercept_ag_future.plot <- ggplot(intercept_ag_future, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10))

intercept_ag_future.plot + geom_histogram(aes(x = group), data = intercept_ag_future, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
## Table
intercept_ag_future.tb <- data.frame(Response = c("Yes", "No"),
           Frequence = as.numeric(table(d$intercept_ag_future)), 
           Percentage = round(as.numeric(prop.table(table(factor(d$intercept_ag_future)))*100),2))

kable(intercept_ag_future.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 327 </td>
   <td style="text-align:right;"> 97.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.1 </td>
  </tr>
</tbody>
</table>


### Why have you not used the Arbutus Greenway? Check ALL that apply.

_Question asked only to those who reported not using the Arbutus Greenway (n=24). Participants could select more than one option._  



```r
#intercept_ag_not_1

intercept_ag_not_1 <- as.numeric(table(d$intercept_ag_not_1[d$intercept_ag_not_1==1]))
intercept_ag_not_2 <- as.numeric(table(d$intercept_ag_not_2[d$intercept_ag_not_2==1]))
intercept_ag_not_3 <- as.numeric(table(d$intercept_ag_not_3[d$intercept_ag_not_3==1]))
intercept_ag_not_4 <- "0"
intercept_ag_not_5 <- as.numeric(table(d$intercept_ag_not_5[d$intercept_ag_not_5==1]))


intercept_ag_not <- data.frame(Response = c("Health reasons", "Not motivated or interested in walking or cycling", "Greenway doesn't take me where I want to go ", "Greenway design and amenities are not pleasing to me ", "Other "), 
                               Frequence = c(intercept_ag_not_1, intercept_ag_not_2, intercept_ag_not_3, "0", intercept_ag_not_5))

 
kable(intercept_ag_not) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> Frequence </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Health reasons </td>
   <td style="text-align:left;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not motivated or interested in walking or cycling </td>
   <td style="text-align:left;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Greenway doesn't take me where I want to go </td>
   <td style="text-align:left;"> 12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Greenway design and amenities are not pleasing to me </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:left;"> 10 </td>
  </tr>
</tbody>
</table>



## Section 2: Physical Activity

### During the last 7 days, on how many days did you do vigorous physical activities like heavy lifting, digging, heavy construction, or climbing up stairs as part of your work? Think about only those physical activities that you did for at least 10 minutes at a time.


```r
#work_vigpa

ggplot(d, aes(x = d$work_vigpa)) + geom_histogram(na.rm = TRUE, fill = "#1596FF") + xlab("N days vigorous job-related physical activity")
```

![](INTERACT_van_W1_HS_files/figure-html/work_vipa-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$work_vigpa)), Percentage = round(as.numeric(prop.table(table(d$work_vigpa)))*100,2))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 243 </td>
   <td style="text-align:right;"> 72.75 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 5.39 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 5.39 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.59 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 3.89 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days doing vigorous physical activities as part of your work?


```r
#work_vigpa_freq

d$work_vigpa_freq[d$work_vigpa_freq==-7] <- NA

ggplot(d, aes(x = d$work_vigpa_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes vigorous job-related physical activity") 
```

![](INTERACT_van_W1_HS_files/figure-html/work_vigpa_freq-1.png)<!-- -->

```r
summary(d$work_vigpa_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   20.00   60.00   88.57  120.00  480.00     243
```

### Again, think about only those physical activities that you did for at least 10 minutes at a time. During the last 7 days, on how many days did you do moderate physical activities like carrying light loads as part of your work? Please do not include walking.


```r
#work_modpa

ggplot(d, aes(x = d$work_modpa)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("N days moderate job-related physical activity")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$work_modpa)), Percentage = round(as.numeric(prop.table(table(d$work_modpa)))*100,2))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 223 </td>
   <td style="text-align:right;"> 66.77 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 6.59 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 4.79 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 4.79 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 8.38 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 4.79 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days doing moderate physical activities as part of your work?


```r
#work_modpa_freq

d$work_modpa_freq[d$work_modpa_freq==-7] <- NA
ggplot(d, aes(x = d$work_modpa_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes moderate job-related physical activity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
summary(d$work_modpa_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   22.50   60.00   99.05  150.00  480.00     223
```

### During the last 7 days, on how many days did you travel in a motor vehicle like a train, bus, car, or metro?


```r
#travel_motor

ggplot(d, aes(x = d$travel_motor)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("N days")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$travel_motor)), Percentage = round(as.numeric(prop.table(table(d$travel_motor)))*100,2))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 7.49 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 12.57 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 13.17 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 11.38 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 14.97 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 12.28 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:right;"> 26.95 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days travelling in a train, bus, car, metro, or other kind of motor vehicle?

```r
#travel_motor_freq

d$travel_motor_freq[d$travel_motor_freq==-7] <- NA
ggplot(d, aes(x = d$travel_motor_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes travel time") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
summary(d$travel_motor_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   30.00   60.00   62.43   60.00  300.00       4
```

### During the last 7 days, on how many days did you bicycle for at least 10 minutes at a time to go from place to place?


```r
#travel_bike

ggplot(d, aes(x = d$travel_bike)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("N days")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$travel_bike)), Percentage = round(as.numeric(prop.table(table(d$travel_bike)))*100,2))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 176 </td>
   <td style="text-align:right;"> 52.69 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 8.98 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 9.58 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 9.28 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
</tbody>
</table>
### How much time did you usually spend on one of those days to bicycle from place to place?


```r
#travel_bike_freq

d$travel_bike_freq[d$travel_bike_freq==-7] <- NA
ggplot(d, aes(x = d$travel_bike_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes travel time") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```r
summary(d$travel_bike_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   36.25   60.00   62.74   80.00  240.00     176
```

### During the last 7 days, on how many days did you walk for at least 10 minutes at a time to go from place to place?


```r
#travel_walk

ggplot(d, aes(x = d$travel_walk)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("# of days in the last 7 days")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$travel_walk)), Percentage = round(as.numeric(prop.table(table(d$travel_walk)))*100,2))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.59 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 11.68 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 11.08 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 8.38 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 12.57 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 5.69 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> 42.81 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days walking from place to place?


```r
#travel_walk_freq

d$travel_walk_freq[d$travel_walk_freq==-7] <- NA

ggplot(d, aes(x = d$travel_walk_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes travel time") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

```r
summary(d$travel_walk_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   30.00   45.00   56.49   60.00  900.00      12
```

### Not counting any walking for transportation that you have already mentioned, during the last 7 days, on how many days did you walk for at least 10 minutes at a time in your leisure time?


```r
#leisure_walk

ggplot(d, aes(x = d$leisure_walk)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("N days")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$leisure_walk)), Percentage = round(as.numeric(prop.table(table(d$leisure_walk))*100,2)))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days walking in your leisure time?

```r
#leisure_walk_freq

d$leisure_walk_freq[d$leisure_walk_freq==-7] <- NA

ggplot(d, aes(x = d$leisure_walk_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes leisure time") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

```r
summary(d$leisure_walk_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    2.00   30.00   45.00   60.03   60.00  840.00      49
```

### Think about only those physical activities that you did for at least 10 minutes at a time, not counting any activity for transportation or work that you have already mentioned. During the last 7 days, on how many days did you do vigorous physical activities like aerobics, running, fast bicycling, or fast swimming in your leisure time?


```r
#leisure_vigpa

ggplot(d, aes(x = d$leisure_vigpa)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("N days")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$leisure_vigpa)), Percentage = round(as.numeric(prop.table(table(d$leisure_vigpa))*100,2)))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 13 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days doing vigorous physical activities in your leisure time?


```r
#leisure_vigpa_freq

d$leisure_vigpa_freq[d$leisure_vigpa_freq==-7] <- NA

ggplot(d, aes(x = d$leisure_vigpa_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes leisure time") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

```r
summary(d$leisure_vigpa_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   45.00   60.00   68.22   60.00  600.00     118
```

### During the last 7 days, on how many days did you do moderate physical activities like bicycling at a regular pace, swimming at a regular pace, or doubles tennis in your leisure time?


```r
#leisure_modpa

ggplot(d, aes(x = d$leisure_modpa)) + geom_histogram(na.rm = TRUE, fill="#1596FF") + xlab("N days")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

```r
kable(data.frame(Days = 0:7, N = as.numeric(table(d$leisure_modpa)), Percentage = round(as.numeric(prop.table(table(d$leisure_modpa))*100,2)))) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:right;"> Days </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 46 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

### How much time did you usually spend on one of those days doing moderate physical activities in your leisure time?



```r
#leisure_modpa_freq

d$leisure_modpa_freq[d$leisure_modpa_freq==-7] <- NA

ggplot(d, aes(x = d$leisure_modpa_freq)) + geom_histogram(na.rm = TRUE, binwidth = 20, fill= "#35AAC2") + xlab("Minutes leisure time") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

```r
summary(d$leisure_modpa_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00   37.50   60.00   70.34   90.00  360.00     155
```

### During the last 7 days, how much time did you usually spend sitting on a weekday?


```r
#sit_weekday

ggplot(d, aes(x = d$sit_weekday/60)) + geom_histogram(na.rm = TRUE, binwidth = 1, fill= "#35AAC2") + xlab("Hours sitting, weekday") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

```r
summary(d$sit_weekday)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    45.0   195.0   300.0   327.2   420.0   840.0
```

### During the last 7 days, how much time did you usually spend sitting on a weekend day?


```r
#sit_weekend

ggplot(d, aes(x = d$sit_weekend/60)) + geom_histogram(na.rm = TRUE, binwidth = 1, fill= "#35AAC2") + xlab("Hours sitting, weekend") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

```r
summary(d$sit_weekend)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0   180.0   240.0   256.2   345.0   780.0
```


### Do you live with a dog?


```r
#dog 

dog <- (prop.table(table(factor(d$dog)))*100)
dog <- as.data.frame(dog)
dog$group <- substring(row.names(dog), 1)
dog$group <- revalue(as.character(dog$group), c("1" = "Yes",  "2" = "No"))

dog$group <- factor(dog$group, dog$group)

dog.plot <- ggplot(dog, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10))

dog.plot + geom_histogram(aes(x = group), data = dog, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

```r
## Table
dog.tb <- data.frame(Response = c("Yes", "No"),
           Frequence = as.numeric(table(d$dog)), 
           Percentage = round(as.numeric(prop.table(table(factor(d$dog)))*100),2))

kable(dog.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 18.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 272 </td>
   <td style="text-align:right;"> 81.44 </td>
  </tr>
</tbody>
</table>

### Do you walk the dog regularly?


```r
#dog_walk

d$dog_walk[d$dog_walk==-7] <- NA

dog_walk <- prop.table(table(factor(d$dog_walk)))*100
dog_walk <- as.data.frame(dog_walk)
dog_walk$group <- substring(row.names(dog_walk), 1)
dog_walk$group <- revalue(as.character(dog_walk$group), c("1" = "Yes",  "2" = "No"))

dog_walk$group <- factor(dog_walk$group, dog_walk$group)

dog_walk.plot <- ggplot(dog_walk, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) #order responses as in t5

dog_walk.plot + geom_histogram(aes(x = group), data = dog_walk, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

```r
## Table
kable(data.frame(Response = c("Yes", "No"),
           Frequence = as.numeric(table(d$dog_walk)), Percentage = round(as.numeric(prop.table(table(d$dog_walk)))*100,2))) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 88.71 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 11.29 </td>
  </tr>
</tbody>
</table>

### How many hours or minutes a day on average do you walk the dog?


```r
#dog_walk_freq

#d$dog_walk_freq[d$dog_walk_freq==900] <- NA
d$dog_walk_freq[d$dog_walk_freq==-7] <- NA

ggplot(d, aes(x=d$dog_walk_freq)) + geom_histogram (na.rm = TRUE, fill="#1596FF", binwidth = 10) + xlab("Number of minutes per day")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

```r
## Table
summary(d$dog_walk_freq)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   10.00   35.00   60.00   63.91   90.00  180.00     279
```


## Section 3: General Health 

### How tall are you?


```r
#height

#exclude outliers?

ggplot(d, aes(x = d$height)) + geom_histogram(na.rm = TRUE, binwidth = 2, fill="#76D24A") + xlab("Height (cm)") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

```r
summary(d$height)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   147.0   163.0   170.0   169.9   177.0   194.0
```

### How much do you weigh?


```r
#weight

ggplot(d, aes(x = d$weight)) + geom_histogram(na.rm = TRUE, binwidth = 2, fill="#76D24A") + xlab("Weight (kg)") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

```r
summary(d$weight)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   41.00   61.00   68.00   71.01   79.00  115.00
```


### In general, would you say your health is:


```r
#sf1

# Create proportional table
sf1 <- round(prop.table(table(d$sf1))*100,2)
sf1 <- as.data.frame(sf1)
sf1$group <- substring(rownames(sf1), 1)
# or use colnames(sf1)[1] <- "group" : 

# Change category values and transform in factor
## as.character(sf1$group) is because as.data.frame transform character into factor
sf1$group <- revalue(as.character(sf1$group), c("1" = "Excellent", "2" = "Very good", "3" = "Good", "4" = "Fair", "5" = "Poor"))

# Create plot
sf1$plot <- factor(sf1$group, sf1$group) ## Necessary to order x-axis in ggplot

sf1.plot <- ggplot(sf1, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPalette3) +
      ylab("Percent of total") +
      xlab("")
      
sf1.plot + geom_histogram(aes(x = plot), data = sf1, stat = "identity")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
sf1.tb <- as.factor(d$sf1)
sf1.tb <- summary(sf1.tb)
sf1.tb <- as.data.frame(sf1.tb)
sf1.tb$Var1 <- substring(row.names(sf1.tb), 1)
sf1.tb$group <- revalue(as.character(sf1.tb$Var1), c("1" = "Excellent", "2" = "Very good", "3" = "Good", "4" = "Fair", "5" = "Poor"))


## merge with existing prop table data used for plot above 
## order doesn't work
plot.sf1.tb <- merge(sf1, sf1.tb, by = "group")
plot.sf1.tb <- plot.sf1.tb[-c(2, 4, 6)]
plot.sf1.tb <- setcolorder(plot.sf1.tb, c("group", "sf1.tb", "Freq"))

plot.sf1.tb$order <- c(1, 4, 3, 5, 2)
plot.sf1.tb <- plot.sf1.tb %>% arrange(order)
plot.sf1.tb <- plot.sf1.tb[-c(4)]
colnames(plot.sf1.tb) <- c("Response", "N", "Percentage")

kable(plot.sf1.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Excellent </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 21.86 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very good </td>
   <td style="text-align:right;"> 158 </td>
   <td style="text-align:right;"> 47.31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Good </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 24.55 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fair </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 5.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poor </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
</tbody>
</table>


### The following questions are about activities you might do during a typical day. Does your health now limit you in these activities? If so, how much? {.tabset}

#### a. Moderate activities such as moving a table, pushing a vacuum cleaner, bowling, or playing golf


```r
sf2 <- round(prop.table(table(factor(d$sf2, levels = c("1", "2", "3")), exclude = NULL))*100,2)
sf2 <- as.data.frame(sf2)
sf2$group <- substring(row.names(sf2), 1)
sf2$group <- revalue(as.character(sf2$group), c("1" = "Yes, limited a lot",  "2" = "Yes, limited a little", "3" = "No, not at all"))

sf2$plot <- factor(sf2$group, sf2$group)

sf2.plot <- ggplot(sf2, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshorterfade) +
      ylab("Percent of total") +
      xlab("")
sf2.plot + geom_histogram(aes(x = plot), data = sf2, stat = "identity")
```

![](INTERACT_van_W1_HS_files/figure-html/sf2-1.png)<!-- -->

```r
sf2.tb <- as.factor(d$sf2)
sf2.tb <- summary(sf2.tb)
sf2.tb <- as.data.frame(sf2.tb)
sf2.tb$Var1 <- substring(row.names(sf2.tb), 1)
sf2.tb$group <- revalue(as.character(sf2.tb$Var1), c("1" = "Yes, limited a lot",  "2" = "Yes, limited a little", "3" = "No, not at all"))
plot.sf2.tb <- merge(sf2, sf2.tb, by = "group")
plot.sf2.tb <- plot.sf2.tb[-c(2, 4, 6)]
plot.sf2.tb <- setcolorder(plot.sf2.tb, c("group", "sf2.tb", "Freq"))
plot.sf2.tb$order <- c(3, 2, 1)
plot.sf2.tb <- plot.sf2.tb %>% arrange(order)
plot.sf2.tb <- plot.sf2.tb[-c(4)]
colnames(plot.sf2.tb) <- c("Response", "N", "Percentage")

kable(plot.sf2.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes, limited a lot </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes, limited a little </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 12.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No, not at all </td>
   <td style="text-align:right;"> 280 </td>
   <td style="text-align:right;"> 83.83 </td>
  </tr>
</tbody>
</table>

#### b. Climbing several flights of stairs


```r
# sf3 

sf3 <- round(prop.table(table(factor(d$sf3, levels = c("1", "2", "3")), exclude = NULL))*100,2)
sf3 <- as.data.frame(sf3)
sf3$group <- substring(row.names(sf3), 1)
sf3$group <- revalue(as.character(sf3$group), c("1" = "Yes, limited a lot",  "2" = "Yes, limited a little", "3" = "No, not at all"))

sf3$plot <- factor(sf3$group, sf3$group)

sf3.plot <- ggplot(sf3, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshorterfade) +
      ylab("Percent of total") +
      xlab("")
      
sf3.plot + geom_histogram(aes(x = plot), data = sf3, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/sf3-1.png)<!-- -->

```r
# summary table

sf3.tb <- as.factor(d$sf3)
sf3.tb <- summary(sf3.tb)
sf3.tb <- as.data.frame(sf3.tb)
sf3.tb$Var1 <- substring(row.names(sf3.tb), 1)
sf3.tb$group <- revalue(as.character(sf3.tb$Var1), c("1" = "Yes, limited a lot",  "2" = "Yes, limited a little", "3" = "No, not at all"))
plot.sf3.tb <- merge(sf3, sf3.tb, by = "group")
plot.sf3.tb <- plot.sf3.tb[-c(2, 4, 6)]
plot.sf3.tb <- setcolorder(plot.sf3.tb, c("group", "sf3.tb", "Freq"))
plot.sf3.tb$order <- c(3, 2, 1)
plot.sf3.tb <- plot.sf3.tb %>% arrange(order)
plot.sf3.tb <- plot.sf3.tb[-c(4)]
colnames(plot.sf3.tb) <- c("Response", "N", "Percentage")

kable(plot.sf3.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes, limited a lot </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes, limited a little </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 16.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No, not at all </td>
   <td style="text-align:right;"> 265 </td>
   <td style="text-align:right;"> 79.34 </td>
  </tr>
</tbody>
</table>

### During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of your physical health? {.tabset}

#### a. Accomplished less than you would like


```r
#sf4 

sf4<- round(prop.table(table(factor(d$sf4, levels = c("1", "2")), exclude = NULL))*100,2)
sf4 <- as.data.frame(sf4)
sf4$group <- substring(row.names(sf4), 1)
sf4$group <- revalue(as.character(sf4$group), c("1" = "Yes",  "2" = "No"))

sf4$plot <- factor(sf4$group, sf4$group)

sf4.plot <- ggplot(sf4, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

sf4.plot + geom_histogram(aes(x = plot), data = sf4, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

```r
sf4.tb <- as.factor(d$sf4)
sf4.tb <- summary(sf4.tb)
sf4.tb <- as.data.frame(sf4.tb)
sf4.tb$Var1 <- substring(row.names(sf4.tb), 1)
sf4.tb$group <- revalue(as.character(sf4.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.sf4.tb <- merge(sf4, sf4.tb, by = "group")
plot.sf4.tb <- plot.sf4.tb[-c(2, 4, 6)]
plot.sf4.tb <- setcolorder(plot.sf4.tb, c("group", "sf4.tb", "Freq"))
plot.sf4.tb$order <- c(2, 1)
plot.sf4.tb <- plot.sf4.tb %>% arrange(order)
plot.sf4.tb <- plot.sf4.tb[-c(4)]
colnames(plot.sf4.tb) <- c("Response", "N", "Percentage")

kable(plot.sf4.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 26.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 245 </td>
   <td style="text-align:right;"> 73.35 </td>
  </tr>
</tbody>
</table>

#### b. Were limited in the kind of work or other activities


```r
#sf5
sf5<- round(prop.table(table(factor(d$sf5, levels = c("1", "2")), exclude = NULL))*100,2)
sf5 <- as.data.frame(sf5)
sf5$group <- substring(row.names(sf5), 1)
sf5$group <- revalue(as.character(sf5$group), c("1" = "Yes",  "2" = "No"))

sf5$plot <- factor(sf5$group, sf5$group)

sf5.plot <- ggplot(sf5, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

sf5.plot + geom_histogram(aes(x = plot), data = sf5, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

```r
sf5.tb <- as.factor(d$sf5)
sf5.tb <- summary(sf5.tb)
sf5.tb <- as.data.frame(sf5.tb)
sf5.tb$Var1 <- substring(row.names(sf5.tb), 1)
sf5.tb$group <- revalue(as.character(sf5.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.sf5.tb <- merge(sf5, sf5.tb, by = "group")
plot.sf5.tb <- plot.sf5.tb[-c(2, 4, 6)]
plot.sf5.tb <- setcolorder(plot.sf5.tb, c("group", "sf5.tb", "Freq"))
plot.sf5.tb$order <- c(2, 1)
plot.sf5.tb <- plot.sf5.tb %>% arrange(order)
plot.sf5.tb <- plot.sf5.tb[-c(4)]
colnames(plot.sf5.tb) <- c("Response", "N", "Percentage")

kable(plot.sf5.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 20.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 264 </td>
   <td style="text-align:right;"> 79.04 </td>
  </tr>
</tbody>
</table>

### During the past 4 weeks, have you had any of the following problems with your work or other regular daily activities as a result of any emotional problems (such as feeling depressed or anxious)? {.tabset}

#### a. Accomplished less than you would like


```r
#sf6

sf6<- round(prop.table(table(factor(d$sf6, levels = c("1", "2")), exclude = NULL))*100,2)
sf6 <- as.data.frame(sf6)
sf6$group <- substring(row.names(sf6), 1)
sf6$group <- revalue(as.character(sf6$group), c("1" = "Yes",  "2" = "No"))

sf6$plot <- factor(sf6$group, sf6$group)

sf6.plot <- ggplot(sf6, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

sf6.plot + geom_histogram(aes(x = plot), data = sf6, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

```r
# summary table

sf6.tb <- as.factor(d$sf6)
sf6.tb <- summary(sf6.tb)
sf6.tb <- as.data.frame(sf6.tb)
sf6.tb$Var1 <- substring(row.names(sf6.tb), 1)
sf6.tb$group <- revalue(as.character(sf6.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.sf6.tb <- merge(sf6, sf6.tb, by = "group")
plot.sf6.tb <- plot.sf6.tb[-c(2, 4, 6)]
plot.sf6.tb <- setcolorder(plot.sf6.tb, c("group", "sf6.tb", "Freq"))
plot.sf6.tb$order <- c(2, 1)
plot.sf6.tb <- plot.sf6.tb %>% arrange(order)
plot.sf6.tb <- plot.sf6.tb[-c(4)]
colnames(plot.sf6.tb) <- c("Response", "N", "Percentage")

kable(plot.sf6.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 21.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 262 </td>
   <td style="text-align:right;"> 78.44 </td>
  </tr>
</tbody>
</table>

#### b. Did work or activities less carefully than usual


```r
#sf7

sf7<- round(prop.table(table(factor(d$sf7, levels = c("1", "2")), exclude = NULL))*100,2)
sf7 <- as.data.frame(sf7)
sf7$group <- substring(row.names(sf7), 1)
sf7$group <- revalue(as.character(sf7$group), c("1" = "Yes",  "2" = "No"))

sf7$plot <- factor(sf7$group, sf7$group)

sf7.plot <- ggplot(sf7, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

sf7.plot + geom_histogram(aes(x = plot), data = sf7, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

```r
sf7.tb <- as.factor(d$sf7)
sf7.tb <- summary(sf7.tb)
sf7.tb <- as.data.frame(sf7.tb)
sf7.tb$Var1 <- substring(row.names(sf7.tb), 1)
sf7.tb$group <- revalue(as.character(sf7.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.sf7.tb <- merge(sf7, sf7.tb, by = "group")
plot.sf7.tb <- plot.sf7.tb[-c(2, 4, 6)]
plot.sf7.tb <- setcolorder(plot.sf7.tb, c("group", "sf7.tb", "Freq"))
plot.sf7.tb$order <- c(2, 1)
plot.sf7.tb <- plot.sf7.tb %>% arrange(order)
plot.sf7.tb <- plot.sf7.tb[-c(4)]
colnames(plot.sf7.tb) <- c("Response", "N", "Percentage")

kable(plot.sf7.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 14.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 285 </td>
   <td style="text-align:right;"> 85.33 </td>
  </tr>
</tbody>
</table>

### During the past 4 weeks, how much did pain interfere with your normal work (including work outside the home and housework)?


```r
#sf8

sf8 <- round(prop.table(table(factor(d$sf8, levels = c("1", "2", "3", "4", "5")), exclude = NULL))*100,2)
sf8 <- as.data.frame(sf8)
sf8$group <- substring(row.names(sf8), 1)
sf8$group <- revalue(as.character(sf8$group), c("1" = "Not at all",  "2" = "Slightly", "3" = "Moderately", "4" =  "Quite a bit", "5" = "Extremely"))

sf8$plot <- factor(sf8$group, sf8$group)

sf8.plot <- ggplot(sf8, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=rev(INTERACTshortfade)) +
      ylab("Percent of total") +
      xlab("")
      
sf8.plot + geom_histogram(aes(x = plot), data = sf8, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

```r
sf8.tb <- as.factor(d$sf8)
sf8.tb <- summary(sf8.tb)
sf8.tb <- as.data.frame(sf8.tb)
sf8.tb$Var1 <- substring(row.names(sf8.tb), 1)
sf8.tb$group <- revalue(as.character(sf8.tb$Var1), c("1" = "Not at all",  "2" = "Slightly", "3" = "Moderately", "4" =  "Quite a bit", "5" = "Extremely"))
plot.sf8 <- merge(sf8, sf8.tb, by = "group")
plot.sf8 <- plot.sf8[-c(2, 4, 6)]
plot.sf8 <- setcolorder(plot.sf8, c("group", "sf8.tb", "Freq"))
plot.sf8$order <- c(5,3,1,4,2)
plot.sf8 <- plot.sf8 %>% arrange(order)
plot.sf8 <- plot.sf8[-c(4)]
colnames(plot.sf8) <- c("Response", "N", "Percentage")

kable(plot.sf8) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not at all </td>
   <td style="text-align:right;"> 183 </td>
   <td style="text-align:right;"> 54.79 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Slightly </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 30.24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Moderately </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 8.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Quite a bit </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Extremely </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>

### How much of the time during the past 4 weeks. {.tabset}

#### a. Have you felt calm and peaceful?

```r
#sf9 

sf9 <- round(prop.table(table(factor(d$sf9, levels = c("1", "2", "3", "4", "5", "6")), exclude = NULL))*100,2)
sf9 <- as.data.frame(sf9)
sf9$group <- substring(row.names(sf9), 1)
sf9$group <- revalue(as.character(sf9$group), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))

sf9$plot <- factor(sf9$group, sf9$group)

sf9.plot <- ggplot(sf9, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
sf9.plot + geom_histogram(aes(x = plot), data = sf9, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

```r
sf9.tb <- as.factor(d$sf9)
sf9.tb <- summary(sf9.tb)
sf9.tb <- as.data.frame(sf9.tb)
sf9.tb$Var1 <- substring(row.names(sf9.tb), 1)
sf9.tb$group <- revalue(as.character(sf9.tb$Var1), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))
plot.sf9 <- merge(sf9, sf9.tb, by = "group")
plot.sf9 <- plot.sf9[-c(2, 4, 6)]
plot.sf9 <- setcolorder(plot.sf9, c("group", "sf9.tb", "Freq"))
plot.sf9$order <- c(3,5,1,2,6,4)
plot.sf9 <- plot.sf9 %>% arrange(order)
plot.sf9 <- plot.sf9[-c(4)]
colnames(plot.sf9) <- c("Response", "N", "Percentage")

kable(plot.sf9) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> All of the time </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 5.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Most of the time </td>
   <td style="text-align:right;"> 132 </td>
   <td style="text-align:right;"> 39.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A good bit of the time </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 35.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 13.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A little of the time </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> None of the time </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>

#### b. Did you have a lot of energy?


```r
sf10 <- round(prop.table(table(factor(d$sf10, levels = c("1", "2", "3", "4", "5", "6")), exclude = NULL))*100,2)
sf10 <- as.data.frame(sf10)
sf10$group <- substring(row.names(sf10), 1)
sf10$group <- revalue(as.character(sf10$group), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))

sf10$plot <- factor(sf10$group, sf10$group)

sf10.plot <- ggplot(sf10, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
sf10.plot + geom_histogram(aes(x = plot), data = sf10, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-51-1.png)<!-- -->

```r
sf10.tb <- as.factor(d$sf10)
sf10.tb <- summary(sf10.tb)
sf10.tb <- as.data.frame(sf10.tb)
sf10.tb$Var1 <- substring(row.names(sf10.tb), 1)
sf10.tb$group <- revalue(as.character(sf10.tb$Var1), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))
plot.sf10 <- merge(sf10, sf10.tb, by = "group")
plot.sf10 <- plot.sf10[-c(2, 4, 6)]
plot.sf10 <- setcolorder(plot.sf10, c("group", "sf10.tb", "Freq"))
plot.sf10$order <- c(3,5,1,2,6,4)
plot.sf10 <- plot.sf10 %>% arrange(order)
plot.sf10 <- plot.sf10[-c(4)]
colnames(plot.sf10) <- c("Response", "N", "Percentage")

kable(plot.sf10) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> All of the time </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Most of the time </td>
   <td style="text-align:right;"> 116 </td>
   <td style="text-align:right;"> 34.73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A good bit of the time </td>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 31.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 19.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A little of the time </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> None of the time </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
</tbody>
</table>


#### c. Have you felt downhearted and blue?


```r
#check all of the time is 0 

#sf11

sf11 <- round(prop.table(table(factor(d$sf11, levels = c("1", "2", "3", "4", "5", "6")), exclude = NULL))*100,2)
sf11 <- as.data.frame(sf11)
sf11$group <- substring(row.names(sf11), 1)
sf11$group <- revalue(as.character(sf11$group), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))

sf11$plot <- factor(sf11$group, sf11$group)

sf11.plot <- ggplot(sf11, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
sf11.plot + geom_histogram(aes(x = plot), data = sf11, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/sf11-1.png)<!-- -->

```r
sf11.tb <- as.factor(d$sf11)
sf11.tb <- summary(sf11.tb)
sf11.tb <- as.data.frame(sf11.tb)
sf11.tb$Var1 <- substring(row.names(sf11.tb), 1)

nval.df <- c("0") #insert missing values 
nval.df <- as.data.frame(nval.df)
nval.df$sf11.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("1")
nval.df <- nval.df[-c(1)]

sf11.tb <- rbind(sf11.tb, nval.df)

sf11.tb$group <- revalue(as.character(sf11.tb$Var1), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))
plot.sf11 <- merge(sf11, sf11.tb, by = "group")
plot.sf11 <- plot.sf11[-c(2, 4, 6)]
plot.sf11 <- setcolorder(plot.sf11, c("group", "sf11.tb", "Freq"))
plot.sf11$order <- c(3,5,1,2,6,4)
plot.sf11 <- plot.sf11 %>% arrange(order)
plot.sf11 <- plot.sf11[-c(4)]
colnames(plot.sf11) <- c("Response", "N", "Percentage")


kable(plot.sf11) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> All of the time </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Most of the time </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A good bit of the time </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:right;"> 5.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:right;"> 13.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A little of the time </td>
   <td style="text-align:left;"> 142 </td>
   <td style="text-align:right;"> 42.51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> None of the time </td>
   <td style="text-align:left;"> 123 </td>
   <td style="text-align:right;"> 36.83 </td>
  </tr>
</tbody>
</table>

### During the past 4 weeks, how much of the time has your physical health or emotional problems interfered with your social activities (like visiting friends, relatives, etc.)? 


```r
#sf12

sf12 <- round(prop.table(table(factor(d$sf12, levels = c("1", "2", "3", "4", "5", "6")), exclude = NULL))*100,2)
sf12 <- as.data.frame(sf12)
sf12$group <- substring(row.names(sf12), 1)
sf12$group <- revalue(as.character(sf12$group), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))

sf12$plot <- factor(sf12$group, sf12$group)

sf12.plot <- ggplot(sf12, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
sf12.plot + geom_histogram(aes(x = plot), data = sf12, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/sf12-1.png)<!-- -->

```r
sf12.tb <- as.factor(d$sf12)
sf12.tb <- summary(sf12.tb)
sf12.tb <- as.data.frame(sf12.tb)
sf12.tb$Var1 <- substring(row.names(sf12.tb), 1)
sf12.tb$group <- revalue(as.character(sf12.tb$Var1), c("1" = "All of the time",  "2" = "Most of the time", "3" = "A good bit of the time", "4" =  "Some of the time", "5" = "A little of the time", "6" = "None of the time"))
plot.sf12 <- merge(sf12, sf12.tb, by = "group")
plot.sf12 <- plot.sf12[-c(2, 4, 6)]
plot.sf12 <- setcolorder(plot.sf12, c("group", "sf12.tb", "Freq"))
plot.sf12$order <- c(3,5,1,2,6,4)
plot.sf12 <- plot.sf12 %>% arrange(order)
plot.sf12 <- plot.sf12[-c(4)]
colnames(plot.sf12) <- c("Response", "N", "Percentage")

kable(plot.sf12) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> All of the time </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Most of the time </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A good bit of the time </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 4.79 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 11.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A little of the time </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 18.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> None of the time </td>
   <td style="text-align:right;"> 209 </td>
   <td style="text-align:right;"> 62.57 </td>
  </tr>
</tbody>
</table>


## Section 4: Well-being 

### Thinking about your own life and personal circumstances, how satisfied are you.{.tabset}

#### a. With your life as a whole? 


```r
#pwb_a
## check attention n=0 for 10

pwb_a <- round(prop.table(table(factor(d$pwb_a, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")), exclude = NULL))*100,2)


pwb_a <- as.data.frame(pwb_a)
pwb_a$group <- substring(row.names(pwb_a), 1)
pwb_a$group <- revalue(as.character(pwb_a$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_a$plot <- factor(pwb_a$group, pwb_a$group)
pwb_a.plot <- ggplot(pwb_a, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_a.plot + geom_histogram(aes(x = plot), data = pwb_a, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-52-1.png)<!-- -->

```r
pwb_a.tb <- as.factor(d$pwb_a)
pwb_a.tb <- summary(pwb_a.tb)
pwb_a.tb <- as.data.frame(pwb_a.tb)
pwb_a.tb$Var1 <- substring(row.names(pwb_a.tb), 1)

nval.df <- c("0") #insert missing values 
nval.df <- as.data.frame(nval.df)
nval.df$pwb_a.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("10")
nval.df <- nval.df[-c(1)]

pwb_a.tb <- rbind(pwb_a.tb, nval.df)
                           
pwb_a.tb$group <- revalue(as.character(pwb_a.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_a <- merge(pwb_a, pwb_a.tb, by = "group")
plot.pwb_a <- plot.pwb_a[-c(2, 4, 6)]
plot.pwb_a <- setcolorder(plot.pwb_a, c("group", "pwb_a.tb", "Freq"))
plot.pwb_a$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_a <- plot.pwb_a %>% arrange(order)
plot.pwb_a <- plot.pwb_a[-c(4)]
colnames(plot.pwb_a) <- c("Response", "N", "Percentage")

kable(plot.pwb_a) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:right;"> 17.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 120 </td>
   <td style="text-align:right;"> 35.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 62 </td>
   <td style="text-align:right;"> 18.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 11.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:right;"> 6.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:right;"> 4.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>

#### b. With your standard of living?


```r
#pwb_b

pwb_b <- round(prop.table(table(factor(d$pwb_b)))*100,2)

pwb_b <- as.data.frame(pwb_b)
pwb_b$group <- substring(row.names(pwb_b), 1)
pwb_b$group <- revalue(as.character(pwb_b$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_b$plot <- factor(pwb_b$group, pwb_b$group)
pwb_b.plot <- ggplot(pwb_b, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_b.plot + geom_histogram(aes(x = plot), data = pwb_b, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-53-1.png)<!-- -->

```r
pwb_b.tb <- as.factor(d$pwb_b)
pwb_b.tb <- summary(pwb_b.tb)
pwb_b.tb <- as.data.frame(pwb_b.tb)
pwb_b.tb$Var1 <- substring(row.names(pwb_b.tb), 1)
                           
pwb_b.tb$group <- revalue(as.character(pwb_b.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_b <- merge(pwb_b, pwb_b.tb, by = "group")
plot.pwb_b <- plot.pwb_b[-c(2, 4, 6)]
plot.pwb_b <- setcolorder(plot.pwb_b, c("group", "pwb_b.tb", "Freq"))
plot.pwb_b$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_b <- plot.pwb_b %>% arrange(order)
plot.pwb_b <- plot.pwb_b[-c(4)]
colnames(plot.pwb_b) <- c("Response", "N", "Percentage")

kable(plot.pwb_b) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:right;"> 116 </td>
   <td style="text-align:right;"> 34.73 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 24.55 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 16.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 8.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 3.89 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>

#### c. With your health?



```r
#pwb_c

pwb_c <- round(prop.table(table(factor(d$pwb_c)))*100,2)

pwb_c <- as.data.frame(pwb_c)
pwb_c$group <- substring(row.names(pwb_c), 1)
pwb_c$group <- revalue(as.character(pwb_c$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_c$plot <- factor(pwb_c$group, pwb_c$group)
pwb_c.plot <- ggplot(pwb_c, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_c.plot + geom_histogram(aes(x = plot), data = pwb_c, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

```r
pwb_c.tb <- as.factor(d$pwb_c)
pwb_c.tb <- summary(pwb_c.tb)
pwb_c.tb <- as.data.frame(pwb_c.tb)
pwb_c.tb$Var1 <- substring(row.names(pwb_c.tb), 1)
                           
pwb_c.tb$group <- revalue(as.character(pwb_c.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_c <- merge(pwb_c, pwb_c.tb, by = "group")
plot.pwb_c <- plot.pwb_c[-c(2, 4, 6)]
plot.pwb_c <- setcolorder(plot.pwb_c, c("group", "pwb_c.tb", "Freq"))
plot.pwb_c$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_c <- plot.pwb_c %>% arrange(order)
plot.pwb_c <- plot.pwb_c[-c(4)]
colnames(plot.pwb_c) <- c("Response", "N", "Percentage")

kable(plot.pwb_c) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 13.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 107 </td>
   <td style="text-align:right;"> 32.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 20.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 11.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 7.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
</tbody>
</table>

#### d. With what you are achieving in life?


```r
#pwb_d

pwb_d <- round(prop.table(table(factor(d$pwb_d)))*100,2)

pwb_d <- as.data.frame(pwb_d)
pwb_d$group <- substring(row.names(pwb_d), 1)
pwb_d$group <- revalue(as.character(pwb_d$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_d$plot <- factor(pwb_d$group, pwb_d$group)
pwb_d.plot <- ggplot(pwb_d, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_d.plot + geom_histogram(aes(x = plot), data = pwb_d, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-55-1.png)<!-- -->

```r
pwb_d.tb <- as.factor(d$pwb_d)
pwb_d.tb <- summary(pwb_d.tb)
pwb_d.tb <- as.data.frame(pwb_d.tb)
pwb_d.tb$Var1 <- substring(row.names(pwb_d.tb), 1)
                           
pwb_d.tb$group <- revalue(as.character(pwb_d.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_d <- merge(pwb_d, pwb_d.tb, by = "group")
plot.pwb_d <- plot.pwb_d[-c(2, 4, 6)]
plot.pwb_d <- setcolorder(plot.pwb_d, c("group", "pwb_d.tb", "Freq"))
plot.pwb_d$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_d <- plot.pwb_d %>% arrange(order)
plot.pwb_d <- plot.pwb_d[-c(4)]
colnames(plot.pwb_d) <- c("Response", "N", "Percentage")

kable(plot.pwb_d) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 15.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 31.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 21.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 8.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 10.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>

#### e. With your personal relationships?



```r
#pwb_e

pwb_e <- round(prop.table(table(factor(d$pwb_e)))*100,2)

pwb_e <- as.data.frame(pwb_e)
pwb_e$group <- substring(row.names(pwb_e), 1)
pwb_e$group <- revalue(as.character(pwb_e$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_e$plot <- factor(pwb_e$group, pwb_e$group)
pwb_e.plot <- ggplot(pwb_e, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_e.plot + geom_histogram(aes(x = plot), data = pwb_e, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-56-1.png)<!-- -->

```r
pwb_e.tb <- as.factor(d$pwb_e)
pwb_e.tb <- summary(pwb_e.tb)
pwb_e.tb <- as.data.frame(pwb_e.tb)
pwb_e.tb$Var1 <- substring(row.names(pwb_e.tb), 1)
                           
pwb_e.tb$group <- revalue(as.character(pwb_e.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_e <- merge(pwb_e, pwb_e.tb, by = "group")
plot.pwb_e <- plot.pwb_e[-c(2, 4, 6)]
plot.pwb_e <- setcolorder(plot.pwb_e, c("group", "pwb_e.tb", "Freq"))
plot.pwb_e$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_e <- plot.pwb_e %>% arrange(order)
plot.pwb_e <- plot.pwb_e[-c(4)]
colnames(plot.pwb_e) <- c("Response", "N", "Percentage")

kable(plot.pwb_e) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 22.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 27.84 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 23.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 9.88 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
</tbody>
</table>
#### f. With how safe you feel?



```r
#pwb_f

pwb_f <- round(prop.table(table(factor(d$pwb_f, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")), exclude = NULL))*100,2)

pwb_f <- as.data.frame(pwb_f)
pwb_f$group <- substring(row.names(pwb_f), 1)
pwb_f$group <- revalue(as.character(pwb_f$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_f$plot <- factor(pwb_f$group, pwb_f$group)
pwb_f.plot <- ggplot(pwb_f, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_f.plot + geom_histogram(aes(x = plot), data = pwb_f, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-57-1.png)<!-- -->

```r
pwb_f.tb <- as.factor(d$pwb_f)
pwb_f.tb <- summary(pwb_f.tb)
pwb_f.tb <- as.data.frame(pwb_f.tb)
pwb_f.tb$Var1 <- substring(row.names(pwb_f.tb), 1)

nval.df <- c("0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$pwb_f.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("10")
nval.df <- nval.df[-c(1)]

pwb_f.tb <- rbind(pwb_f.tb, nval.df)

                           
pwb_f.tb$group <- revalue(as.character(pwb_f.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_f <- merge(pwb_f, pwb_f.tb, by = "group")
plot.pwb_f <- plot.pwb_f[-c(2, 4, 6)]
plot.pwb_f <- setcolorder(plot.pwb_f, c("group", "pwb_f.tb", "Freq"))
plot.pwb_f$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_f <- plot.pwb_f %>% arrange(order)
plot.pwb_f <- plot.pwb_f[-c(4)]
colnames(plot.pwb_f) <- c("Response", "N", "Percentage")

kable(plot.pwb_f) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:left;"> 143 </td>
   <td style="text-align:right;"> 42.81 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 107 </td>
   <td style="text-align:right;"> 32.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 45 </td>
   <td style="text-align:right;"> 13.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:right;"> 3.89 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>
#### g. With feeling part of your community?


```r
#pwb_g

pwb_g <- round(prop.table(table(factor(d$pwb_g)))*100,2)

pwb_g <- as.data.frame(pwb_g)
pwb_g$group <- substring(row.names(pwb_g), 1)
pwb_g$group <- revalue(as.character(pwb_g$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_g$plot <- factor(pwb_g$group, pwb_g$group)
pwb_g.plot <- ggplot(pwb_g, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_g.plot + geom_histogram(aes(x = plot), data = pwb_g, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-58-1.png)<!-- -->

```r
pwb_g.tb <- as.factor(d$pwb_g)
pwb_g.tb <- summary(pwb_g.tb)
pwb_g.tb <- as.data.frame(pwb_g.tb)
pwb_g.tb$Var1 <- substring(row.names(pwb_g.tb), 1)
                           
pwb_g.tb$group <- revalue(as.character(pwb_g.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_g <- merge(pwb_g, pwb_g.tb, by = "group")
plot.pwb_g <- plot.pwb_g[-c(2, 4, 6)]
plot.pwb_g <- setcolorder(plot.pwb_g, c("group", "pwb_g.tb", "Freq"))
plot.pwb_g$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_g <- plot.pwb_g %>% arrange(order)
plot.pwb_g <- plot.pwb_g[-c(4)]
colnames(plot.pwb_g) <- c("Response", "N", "Percentage")

kable(plot.pwb_g) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 21.86 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 24.55 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 20.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 10.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 12.28 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 4.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
</tbody>
</table>

#### h. With your future security?


```r
#pwb_h

pwb_h <- round(prop.table(table(factor(d$pwb_h, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")), exclude = NULL))*100,2)

pwb_h <- as.data.frame(pwb_h)
pwb_h$group <- substring(row.names(pwb_h), 1)
pwb_h$group <- revalue(as.character(pwb_h$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_h$plot <- factor(pwb_h$group, pwb_h$group)
pwb_h.plot <- ggplot(pwb_h, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_h.plot + geom_histogram(aes(x = plot), data = pwb_h, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-59-1.png)<!-- -->

```r
pwb_h.tb <- as.factor(d$pwb_h)
pwb_h.tb <- summary(pwb_h.tb)
pwb_h.tb <- as.data.frame(pwb_h.tb)
pwb_h.tb$Var1 <- substring(row.names(pwb_h.tb), 1)
                           
pwb_h.tb$group <- revalue(as.character(pwb_h.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_h <- merge(pwb_h, pwb_h.tb, by = "group")
plot.pwb_h <- plot.pwb_h[-c(2, 4, 6)]
plot.pwb_h <- setcolorder(plot.pwb_h, c("group", "pwb_h.tb", "Freq"))
plot.pwb_h$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_h <- plot.pwb_h %>% arrange(order)
plot.pwb_h <- plot.pwb_h[-c(4)]
colnames(plot.pwb_h) <- c("Response", "N", "Percentage")

kable(plot.pwb_h) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 19.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 27.84 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 18.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 8.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 8.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
</tbody>
</table>

#### i. With your spirituality or religion?


```r
#pwb_i

pwb_i <- round(prop.table(table(factor(d$pwb_i, levels= c(1:10))))*100,2)

pwb_i <- as.data.frame(pwb_i)
pwb_i$group <- substring(row.names(pwb_i), 1)
pwb_i$group <- revalue(as.character(pwb_i$group), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

pwb_i$plot <- factor(pwb_i$group, pwb_i$group)
pwb_i.plot <- ggplot(pwb_i, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

      
pwb_i.plot + geom_histogram(aes(x = plot), data = pwb_i, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-60-1.png)<!-- -->

```r
pwb_i.tb <- as.factor(d$pwb_i)
pwb_i.tb <- summary(pwb_i.tb)
pwb_i.tb <- as.data.frame(pwb_i.tb)
pwb_i.tb$Var1 <- substring(row.names(pwb_i.tb), 1)

nval.df <- c("0") #insert missing values 
nval.df <- as.data.frame(nval.df)
nval.df$pwb_i.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("10")
nval.df <- nval.df[-c(1)]

pwb_i.tb <- rbind(pwb_i.tb, nval.df)

pwb_i.tb$group <- revalue(as.character(pwb_i.tb$Var1), c("1" = "1- Completely satisfied", "10" = "10-Completely dissatisfied"))

plot.pwb_i <- merge(pwb_i, pwb_i.tb, by = "group")
plot.pwb_i <- plot.pwb_i[-c(2, 4, 6)]
plot.pwb_i <- setcolorder(plot.pwb_i, c("group", "pwb_i.tb", "Freq"))
plot.pwb_i$order <- c(1, 10, 2, 3, 4, 5, 6, 7, 8, 9)
plot.pwb_i <- plot.pwb_i %>% arrange(order)
plot.pwb_i <- plot.pwb_i[-c(4)]
colnames(plot.pwb_i) <- c("Response", "N", "Percentage")

kable(plot.pwb_i) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Completely satisfied </td>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:right;"> 25.45 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 67 </td>
   <td style="text-align:right;"> 20.06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:right;"> 13.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:right;"> 34.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10-Completely dissatisfied </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>

### In general, I consider myself:


```r
#gwb_a

gwb_a <- round(prop.table(table(factor(d$gwb_a)))*100,2)
gwb_a <- as.data.frame(gwb_a)
gwb_a$group <- substring(row.names(gwb_a), 1)
gwb_a$group <- revalue(as.character(gwb_a$group), c("1" = "1- Not a very happy person", "7" = "7- A very happy person"))
gwb_a$plot <- factor(gwb_a$group, gwb_a$group)

p <- ggplot(gwb_a, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) #order responses as in gwb_a

p + geom_histogram(aes(x = plot), data = gwb_a, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTfade)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-61-1.png)<!-- -->

```r
gwb_a.tb <- as.factor(d$gwb_a)
gwb_a.tb <- summary(gwb_a.tb)
gwb_a.tb <- as.data.frame(gwb_a.tb)
gwb_a.tb$Var1 <- substring(row.names(gwb_a.tb), 1)
gwb_a.tb$group <- revalue(as.character(gwb_a.tb$Var1), c("1" = "1- Not a very happy person", "7" = "7- A very happy person"))

plot.gwb_a <- merge(gwb_a, gwb_a.tb, by = "group")
plot.gwb_a <- plot.gwb_a[-c(2, 4, 6)]
plot.gwb_a <- setcolorder(plot.gwb_a, c("group", "gwb_a.tb", "Freq"))
colnames(plot.gwb_a) <- c("Response", "N", "Percentage")

kable(plot.gwb_a) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Not a very happy person </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 7.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 21.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 43.71 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7- A very happy person </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 18.26 </td>
  </tr>
</tbody>
</table>

### Compared with most of my peers, I consider myself:


```r
#gwb_b

gwb_b <- round(prop.table(table(factor(d$gwb_b)))*100,2)
gwb_b <- as.data.frame(gwb_b)
gwb_b$group <- substring(row.names(gwb_b), 1)
gwb_b$group <- revalue(as.character(gwb_b$group), c("1" = "1- Less happy", "7" = "7- More happy"))
gwb_b$plot <- factor(gwb_b$group, gwb_b$group)

p <- ggplot(gwb_b, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = gwb_b, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTfade)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-62-1.png)<!-- -->

```r
gwb_b.tb <- as.factor(d$gwb_b)
gwb_b.tb <- summary(gwb_b.tb)
gwb_b.tb <- as.data.frame(gwb_b.tb)
gwb_b.tb$Var1 <- substring(row.names(gwb_b.tb), 1)
gwb_b.tb$group <- revalue(as.character(gwb_b.tb$Var1), c("1" = "1- Less happy", "7" = "7- More happy"))

plot.gwb_b <- merge(gwb_b, gwb_b.tb, by = "group")
plot.gwb_b <- plot.gwb_b[-c(2, 4, 6)]
plot.gwb_b <- setcolorder(plot.gwb_b, c("group", "gwb_b.tb", "Freq"))
colnames(plot.gwb_b) <- c("Response", "N", "Percentage")

kable(plot.gwb_b) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Less happy </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 5.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 17.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 28.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 30.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7- More happy </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 14.07 </td>
  </tr>
</tbody>
</table>
### Some people are generally very happy. They enjoy life regardless of what is going on, getting the most out of everything. To what extent does this characterization describe you?


```r
#gwb_c

gwb_c <- round(prop.table(table(factor(d$gwb_c)))*100,2)
gwb_c <- as.data.frame(gwb_c)
gwb_c$group <- substring(row.names(gwb_c), 1)
gwb_c$group <- revalue(as.character(gwb_c$group), c("1" = "1- Not at all", "7" = "7- A great deal"))
gwb_c$plot <- factor(gwb_c$group, gwb_c$group)

p <- ggplot(gwb_c, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = gwb_c, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTfade)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-63-1.png)<!-- -->

```r
gwb_c.tb <- as.factor(d$gwb_c)
gwb_c.tb <- summary(gwb_c.tb)
gwb_c.tb <- as.data.frame(gwb_c.tb)
gwb_c.tb$Var1 <- substring(row.names(gwb_c.tb), 1)
gwb_c.tb$group <- revalue(as.character(gwb_c.tb$Var1), c("1" = "1- Not at all", "7" = "7- A great deal"))

plot.gwb_c <- merge(gwb_c, gwb_c.tb, by = "group")
plot.gwb_c <- plot.gwb_c[-c(2, 4, 6)]
plot.gwb_c <- setcolorder(plot.gwb_c, c("group", "gwb_c.tb", "Freq"))
colnames(plot.gwb_c) <- c("Response", "N", "Percentage")

kable(plot.gwb_c) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Not at all </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 10.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 29.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 29.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7- A great deal </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 16.77 </td>
  </tr>
</tbody>
</table>
### Some people are generally not very happy. Although they are not depressed, they never seem as happy as they might be. To what extent does this characterization describe you?

```r
#gwb_d

gwb_d <- round(prop.table(table(factor(d$gwb_d, levels = 1:7)))*100,2)
gwb_d <- as.data.frame(gwb_d)
gwb_d$group <- substring(row.names(gwb_d), 1)
gwb_d$group <- revalue(as.character(gwb_d$group), c("1" = "1- Not at all", "7" = "7- A great deal"))
gwb_d$plot <- factor(gwb_d$group, gwb_d$group)

p <- ggplot(gwb_d, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = gwb_d, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTfade)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-64-1.png)<!-- -->

```r
gwb_d.tb <- as.factor(d$gwb_d)
gwb_d.tb <- summary(gwb_d.tb)
gwb_d.tb <- as.data.frame(gwb_d.tb)
gwb_d.tb$Var1 <- substring(row.names(gwb_d.tb), 1)
gwb_d.tb$group <- revalue(as.character(gwb_d.tb$Var1), c("1" = "1- Not at all", "7" = "7- A great deal"))

plot.gwb_d <- merge(gwb_d, gwb_d.tb, by = "group")
plot.gwb_d <- plot.gwb_d[-c(2, 4, 6)]
plot.gwb_d <- setcolorder(plot.gwb_d, c("group", "gwb_d.tb", "Freq"))
colnames(plot.gwb_d) <- c("Response", "N", "Percentage")

kable(plot.gwb_d) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1- Not at all </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 38.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 28.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 11.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 8.68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7- A great deal </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.29 </td>
  </tr>
</tbody>
</table>

### The next questions are about how you feel about different aspects of your life. For each one, tell us how often you feel that way. {.tabset}

#### a. How often do you feel that you lack companionship?


```r
#loneliness_a

loneliness_a <- round(prop.table(table(factor(d$loneliness_a, levels = 1:3)))*100,2)
loneliness_a <- as.data.frame(loneliness_a)
loneliness_a$group <- substring(row.names(loneliness_a), 1)
loneliness_a$group <- revalue(as.character(loneliness_a$group), c("1" = "Hardly ever", "2" = "Some of the time", "3" = "Often"))
loneliness_a$plot <- factor(loneliness_a$group, loneliness_a$group)

p <- ggplot(loneliness_a, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = loneliness_a, stat = "identity") +  
  scale_fill_manual(values=rev(INTERACTshorterfade)) +
  guides(fill=FALSE) +
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-65-1.png)<!-- -->

```r
loneliness_a.tb <- as.factor(d$loneliness_a)
loneliness_a.tb <- summary(loneliness_a.tb)
loneliness_a.tb <- as.data.frame(loneliness_a.tb)
loneliness_a.tb$Var1 <- substring(row.names(loneliness_a.tb), 1)
loneliness_a.tb$group <- revalue(as.character(loneliness_a.tb$Var1), c("1" = "Hardly ever", "2" = "Some of the time", "3" = "Often"))

plot.loneliness_a <- merge(loneliness_a, loneliness_a.tb, by = "group")
plot.loneliness_a <- plot.loneliness_a[-c(2, 4, 6)]
plot.loneliness_a <- setcolorder(plot.loneliness_a, c("group", "loneliness_a.tb", "Freq"))
plot.loneliness_a$order <- c(1, 3, 2)
plot.loneliness_a <- plot.loneliness_a %>% arrange(order)
plot.loneliness_a <- plot.loneliness_a[-c(4)]
colnames(plot.loneliness_a) <- c("Response", "N", "Percentage")

kable(plot.loneliness_a) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hardly ever </td>
   <td style="text-align:right;"> 184 </td>
   <td style="text-align:right;"> 55.09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:right;"> 113 </td>
   <td style="text-align:right;"> 33.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Often </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 11.08 </td>
  </tr>
</tbody>
</table>

#### b. How often do you feel left out?


```r
#loneliness_b

loneliness_b <- round(prop.table(table(factor(d$loneliness_b, levels = 1:3)))*100,2)
loneliness_b <- as.data.frame(loneliness_b)
loneliness_b$group <- substring(row.names(loneliness_b), 1)
loneliness_b$group <- revalue(as.character(loneliness_b$group), c("1" = "Hardly ever", "2" = "Some of the time", "3" = "Often"))
loneliness_b$plot <- factor(loneliness_b$group, loneliness_b$group)

p <- ggplot(loneliness_b, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = loneliness_b, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTshorterfade)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-66-1.png)<!-- -->

```r
loneliness_b.tb <- as.factor(d$loneliness_b)
loneliness_b.tb <- summary(loneliness_b.tb)
loneliness_b.tb <- as.data.frame(loneliness_b.tb)
loneliness_b.tb$Var1 <- substring(row.names(loneliness_b.tb), 1)
loneliness_b.tb$group <- revalue(as.character(loneliness_b.tb$Var1), c("1" = "Hardly ever", "2" = "Some of the time", "3" = "Often"))

plot.loneliness_b <- merge(loneliness_b, loneliness_b.tb, by = "group")
plot.loneliness_b <- plot.loneliness_b[-c(2, 4, 6)]
plot.loneliness_b <- setcolorder(plot.loneliness_b, c("group", "loneliness_b.tb", "Freq"))
plot.loneliness_b$order <- c(1, 3, 2)
plot.loneliness_b <- plot.loneliness_b %>% arrange(order)
plot.loneliness_b <- plot.loneliness_b[-c(4)]
colnames(plot.loneliness_b) <- c("Response", "N", "Percentage")

kable(plot.loneliness_b) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hardly ever </td>
   <td style="text-align:right;"> 192 </td>
   <td style="text-align:right;"> 57.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:right;"> 124 </td>
   <td style="text-align:right;"> 37.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Often </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 5.39 </td>
  </tr>
</tbody>
</table>

#### c. How often do you feel isolated from others?


```r
#loneliness_c
loneliness_c <- round(prop.table(table(factor(d$loneliness_c, levels = 1:3)))*100,2)
loneliness_c <- as.data.frame(loneliness_c)
loneliness_c$group <- substring(row.names(loneliness_c), 1)
loneliness_c$group <- revalue(as.character(loneliness_c$group), c("1" = "Hardly ever", "2" = "Some of the time", "3" = "Often"))
loneliness_c$plot <- factor(loneliness_c$group, loneliness_c$group)

p <- ggplot(loneliness_c, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = loneliness_c, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTshorterfade)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-67-1.png)<!-- -->

```r
loneliness_c.tb <- as.factor(d$loneliness_c)
loneliness_c.tb <- summary(loneliness_c.tb)
loneliness_c.tb <- as.data.frame(loneliness_c.tb)
loneliness_c.tb$Var1 <- substring(row.names(loneliness_c.tb), 1)
loneliness_c.tb$group <- revalue(as.character(loneliness_c.tb$Var1), c("1" = "Hardly ever", "2" = "Some of the time", "3" = "Often"))

plot.loneliness_c <- merge(loneliness_c, loneliness_c.tb, by = "group")
plot.loneliness_c <- plot.loneliness_c[-c(2, 4, 6)]
plot.loneliness_c <- setcolorder(plot.loneliness_c, c("group", "loneliness_c.tb", "Freq"))
plot.loneliness_c$order <- c(1, 3, 2)
plot.loneliness_c <- plot.loneliness_c %>% arrange(order)
plot.loneliness_c <- plot.loneliness_c[-c(4)]
colnames(plot.loneliness_c) <- c("Response", "N", "Percentage")

kable(plot.loneliness_c) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hardly ever </td>
   <td style="text-align:right;"> 201 </td>
   <td style="text-align:right;"> 60.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Some of the time </td>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 31.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Often </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 8.38 </td>
  </tr>
</tbody>
</table>

## Section 5: Social Participation

### How would you describe your sense of belonging to your local community? Would you say it is: 


```r
#belonging

## Plot
### Data preparation
belonging <- round(prop.table(table(factor(d$belonging)))*100,2)
belonging <- as.data.frame(belonging)
belonging$group <- substring(row.names(belonging), 1)
belonging$group <- revalue(as.character(belonging$group), c("1" = "Very strong", "2" = "Somewhat strong", "3" = "Somewhat weak", "4" = "Very weak", "5" ="I don't know"))
belonging$group <- factor(belonging$group, belonging$group)

p <- ggplot(belonging, aes(x=group, y=Freq, fill=group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = group), data = belonging, stat = "identity") +
    scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("Sense of belonging")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-68-1.png)<!-- -->

```r
## Table
kable(data.frame(Response = c("Very strong", "Somewhat strong", "Somewhat weak", "Very weak", "I don't know"),
           Frequence = as.numeric(table(d$belonging)), Percentage = round(as.numeric(prop.table(table(d$belonging)))*100,2))) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very strong </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 17.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat strong </td>
   <td style="text-align:right;"> 142 </td>
   <td style="text-align:right;"> 42.51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat weak </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 28.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very weak </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.59 </td>
  </tr>
</tbody>
</table>

### How often do you. {.tabset}

#### a. Say hello to a neighbour?


```r
#spat_a

#per week
ggplot(d, aes(x = d$spat_a/52.1429)) + 
  geom_histogram(binwidth = 1, na.rm = TRUE, fill="#1596FF") + xlab("Times per week")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-69-1.png)<!-- -->

```r
summary(d$spat_a/52.1429)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.992   4.986   4.515   6.981   7.000
```

#### b. Stop and have a chat with a neighbour?


```r
#spat_b

#per week
ggplot(d, aes(x = d$spat_b/52.1429)) + 
  geom_histogram(binwidth = 1, na.rm = TRUE, fill="#1596FF") + xlab("Times per week")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-70-1.png)<!-- -->

```r
summary(d$spat_b/52.1429)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.9973  1.9945  2.6256  3.9890  6.9808
```

#### c. Visit a neighbour, or receive a visit from a neighbour?


```r
#spat_c

#per week
ggplot(d, aes(x = d$spat_c/52.1429)) + 
  geom_histogram(binwidth = 1, na.rm = TRUE, fill="#1596FF") + xlab("Times per week")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-71-1.png)<!-- -->

```r
summary(d$spat_c/52.1429)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.2301  0.8725  0.9973  6.9808
```

#### d. Go somewhere (e.g., to a shop; to a restaurant), together with a neighbour?


```r
#spat_d

#per week
ggplot(d, aes(x = d$spat_d/52.1429)) + 
  geom_histogram(binwidth = 1, na.rm = TRUE, fill="#1596FF") + xlab("Times per week")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-72-1.png)<!-- -->

```r
summary(d$spat_d/52.1429)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.3621  0.2301  6.9808
```

#### e. Ask help/advice from or do you help/give advice to a neighbour yourself?

```r
#spat_e

#per week
ggplot(d, aes(x = d$spat_e/52.1429)) + 
  geom_histogram(binwidth = 1, na.rm = TRUE, fill="#1596FF") + xlab("Times per week")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-73-1.png)<!-- -->

```r
summary(d$spat_e/52.1429)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.2301  0.7177  0.9973  6.9808
```


### Thinking about your neighbourhood, how would you rate the following statements? {.tabset}

#### a. This is a close-knit neighbourhood


```r
# plot spat2_a

spat2_a <- round(prop.table(table(factor(d$spat2_a)))*100,2)
spat2_a <- as.data.frame(spat2_a)
spat2_a$group <- substring(row.names(spat2_a), 1)
spat2_a$group <- revalue(as.character(spat2_a$group), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))
spat2_a$plot <- factor(spat2_a$group, spat2_a$group)

p <- ggplot(spat2_a, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat2_a, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTPalette3)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")+
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-74-1.png)<!-- -->

```r
spat2_a.tb <- as.factor(d$spat2_a)
spat2_a.tb <- summary(spat2_a.tb)
spat2_a.tb <- as.data.frame(spat2_a.tb)
spat2_a.tb$Var1 <- substring(row.names(spat2_a.tb), 1)
spat2_a.tb$group <- revalue(as.character(spat2_a.tb$Var1), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))

plot.spat2_a.tb <- merge(spat2_a, spat2_a.tb, by = "group")
plot.spat2_a.tb <- plot.spat2_a.tb[-c(2, 4, 6)]
plot.spat2_a.tb <- setcolorder(plot.spat2_a.tb, c("group", "spat2_a.tb", "Freq"))

plot.spat2_a.tb$order <- c(3,4,2,5,1)
plot.spat2_a.tb <- plot.spat2_a.tb %>% arrange(order)
plot.spat2_a.tb <- plot.spat2_a.tb[-c(4)]
colnames(plot.spat2_a.tb) <- c("Response", "N", "Percentage")

kable(plot.spat2_a.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly disagree </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 14.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat disagree </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 20.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither agree or disagree </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 35.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat agree </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 21.86 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly agree </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
</tbody>
</table>

#### b. People generally do not get along


```r
# plot spat2_b

spat2_b <- round(prop.table(table(factor(d$spat2_b)))*100,2)
spat2_b <- as.data.frame(spat2_b)
spat2_b$group <- substring(row.names(spat2_b), 1)
spat2_b$group <- revalue(as.character(spat2_b$group), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))
spat2_b$plot <- factor(spat2_b$group, spat2_b$group)

p <- ggplot(spat2_b, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat2_b, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTPalette3)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") +
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-75-1.png)<!-- -->

```r
spat2_b.tb <- as.factor(d$spat2_b)
spat2_b.tb <- summary(spat2_b.tb)
spat2_b.tb <- as.data.frame(spat2_b.tb)
spat2_b.tb$Var1 <- substring(row.names(spat2_b.tb), 1)
spat2_b.tb$group <- revalue(as.character(spat2_b.tb$Var1), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))

plot.spat2_b.tb <- merge(spat2_b, spat2_b.tb, by = "group")
plot.spat2_b.tb <- plot.spat2_b.tb[-c(2, 4, 6)]
plot.spat2_b.tb <- setcolorder(plot.spat2_b.tb, c("group", "spat2_b.tb", "Freq"))

plot.spat2_b.tb$order <- c(3,4,2,5,1)
plot.spat2_b.tb <- plot.spat2_b.tb %>% arrange(order)
plot.spat2_b.tb <- plot.spat2_b.tb[-c(4)]
colnames(plot.spat2_b.tb) <- c("Response", "N", "Percentage")

kable(plot.spat2_b.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly disagree </td>
   <td style="text-align:right;"> 131 </td>
   <td style="text-align:right;"> 39.22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat disagree </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 32.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither agree or disagree </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 20.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat agree </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly agree </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.50 </td>
  </tr>
</tbody>
</table>
#### c. People are willing to help neighbours


```r
# plot spat2_c
spat2_c <- round(prop.table(table(factor(d$spat2_c)))*100,2)
spat2_c <- as.data.frame(spat2_c)
spat2_c$group <- substring(row.names(spat2_c), 1)
spat2_c$group <- revalue(as.character(spat2_c$group), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))
spat2_c$plot <- factor(spat2_c$group, spat2_c$group)

p <- ggplot(spat2_c, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat2_c, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTPalette3)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") +
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-76-1.png)<!-- -->

```r
spat2_c.tb <- as.factor(d$spat2_c)
spat2_c.tb <- summary(spat2_c.tb)
spat2_c.tb <- as.data.frame(spat2_c.tb)
spat2_c.tb$Var1 <- substring(row.names(spat2_c.tb), 1)
spat2_c.tb$group <- revalue(as.character(spat2_c.tb$Var1), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))

plot.spat2_c.tb <- merge(spat2_c, spat2_c.tb, by = "group")
plot.spat2_c.tb <- plot.spat2_c.tb[-c(2, 4, 6)]
plot.spat2_c.tb <- setcolorder(plot.spat2_c.tb, c("group", "spat2_c.tb", "Freq"))

plot.spat2_c.tb$order <- c(3,4,2,5,1)
plot.spat2_c.tb <- plot.spat2_c.tb %>% arrange(order)
plot.spat2_c.tb <- plot.spat2_c.tb[-c(4)]
colnames(plot.spat2_c.tb) <- c("Response", "N", "Percentage")

kable(plot.spat2_c.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly disagree </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 5.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat disagree </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 10.48 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither agree or disagree </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 25.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat agree </td>
   <td style="text-align:right;"> 136 </td>
   <td style="text-align:right;"> 40.72 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly agree </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 17.96 </td>
  </tr>
</tbody>
</table>

#### d. People do not share same values

```r
# plot spat2_d

spat2_d <- round(prop.table(table(factor(d$spat2_d)))*100,2)
spat2_d <- as.data.frame(spat2_d)
spat2_d$group <- substring(row.names(spat2_d), 1)
spat2_d$group <- revalue(as.character(spat2_d$group), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))
spat2_d$plot <- factor(spat2_d$group, spat2_d$group)

p <- ggplot(spat2_d, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat2_d, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTPalette3)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") +
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-77-1.png)<!-- -->

```r
spat2_d.tb <- as.factor(d$spat2_d)
spat2_d.tb <- summary(spat2_d.tb)
spat2_d.tb <- as.data.frame(spat2_d.tb)
spat2_d.tb$Var1 <- substring(row.names(spat2_d.tb), 1)
spat2_d.tb$group <- revalue(as.character(spat2_d.tb$Var1), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))

plot.spat2_d.tb <- merge(spat2_d, spat2_d.tb, by = "group")
plot.spat2_d.tb <- plot.spat2_d.tb[-c(2, 4, 6)]
plot.spat2_d.tb <- setcolorder(plot.spat2_d.tb, c("group", "spat2_d.tb", "Freq"))

plot.spat2_d.tb$order <- c(3,4,2,5,1)
plot.spat2_d.tb <- plot.spat2_d.tb %>% arrange(order)
plot.spat2_d.tb <- plot.spat2_d.tb[-c(4)]
colnames(plot.spat2_d.tb) <- c("Response", "N", "Percentage")

kable(plot.spat2_d.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly disagree </td>
   <td style="text-align:right;"> 47 </td>
   <td style="text-align:right;"> 14.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat disagree </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 30.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither agree or disagree </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 39.82 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat agree </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 12.28 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly agree </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.29 </td>
  </tr>
</tbody>
</table>
#### e. People can be trusted

```r
# plot spat2_e

spat2_e <- round(prop.table(table(factor(d$spat2_e)))*100,2)
spat2_e <- as.data.frame(spat2_e)
spat2_e$group <- substring(row.names(spat2_e), 1)
spat2_e$group <- revalue(as.character(spat2_e$group), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))
spat2_e$plot <- factor(spat2_e$group, spat2_e$group)

p <- ggplot(spat2_e, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat2_e, stat = "identity") +
  scale_fill_manual(values=rev(INTERACTPalette3)) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") +
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-78-1.png)<!-- -->

```r
spat2_e.tb <- as.factor(d$spat2_e)
spat2_e.tb <- summary(spat2_e.tb)
spat2_e.tb <- as.data.frame(spat2_e.tb)
spat2_e.tb$Var1 <- substring(row.names(spat2_e.tb), 1)
spat2_e.tb$group <- revalue(as.character(spat2_e.tb$Var1), c("1"="Strongly disagree", "2"="Somewhat disagree", "3"="Neither agree or disagree", "4"="Somewhat agree", "5"= "Strongly agree"))

plot.spat2_e.tb <- merge(spat2_e, spat2_e.tb, by = "group")
plot.spat2_e.tb <- plot.spat2_e.tb[-c(2, 4, 6)]
plot.spat2_e.tb <- setcolorder(plot.spat2_e.tb, c("group", "spat2_e.tb", "Freq"))

plot.spat2_e.tb$order <- c(3,4,2,5,1)
plot.spat2_e.tb <- plot.spat2_e.tb %>% arrange(order)
plot.spat2_e.tb <- plot.spat2_e.tb[-c(4)]
colnames(plot.spat2_e.tb) <- c("Response", "N", "Percentage")

kable(plot.spat2_e.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly disagree </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat disagree </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither agree or disagree </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 20.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat agree </td>
   <td style="text-align:right;"> 158 </td>
   <td style="text-align:right;"> 47.31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly agree </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 22.46 </td>
  </tr>
</tbody>
</table>


### If you lost a wallet or purse that contained two hundred dollars, how likely is it to be returned with the money in it, if it was found: {.tabset}

#### a. By someone who lives close by? Would it be:


```r
#spat3_a

spat3_a <- round(prop.table(table(factor(d$spat3_a)))*100,2)
spat3_a <- as.data.frame(spat3_a)
spat3_a$group <- substring(row.names(spat3_a), 1)
spat3_a$group <- revalue(as.character(spat3_a$group), c("1"="Very likely", "2"="Somewhat likely", "3"="Not at all likely", "4"="I don't know"))
spat3_a$plot <- factor(spat3_a$group, spat3_a$group)

p <- ggplot(spat3_a, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat3_a, stat = "identity") +
      scale_fill_manual(values = INTERACTPalette3) +
      guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") +
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-79-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
spat3_a.tb <- as.factor(d$spat3_a)
spat3_a.tb <- summary(spat3_a.tb)
spat3_a.tb <- as.data.frame(spat3_a.tb)
spat3_a.tb$Var1 <- substring(row.names(spat3_a.tb), 1)
spat3_a.tb$group <- revalue(as.character(spat3_a.tb$Var1), c("1"="Very likely", "2"="Somewhat likely", "3"="Not at all likely", "77"="I don't know"))


## merge with existing prop table data used for plot above 
plot.spat3_a.tb <- merge(spat3_a, spat3_a.tb, by = "group")
plot.spat3_a.tb <- plot.spat3_a.tb[-c(2, 4, 6)]
plot.spat3_a.tb <- setcolorder(plot.spat3_a.tb, c("group", "spat3_a.tb", "Freq"))

plot.spat3_a.tb$order <- c(4,3,2,1)
plot.spat3_a.tb <- plot.spat3_a.tb %>% arrange(order)
plot.spat3_a.tb <- plot.spat3_a.tb[-c(4)]
colnames(plot.spat3_a.tb) <- c("Response", "N", "Percentage")

kable(plot.spat3_a.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very likely </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 46.41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat likely </td>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 38.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not at all likely </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 6.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 9.28 </td>
  </tr>
</tbody>
</table>
#### b. By a complete stranger? Would it be:


```r
#spat3_b
spat3_b <- round(prop.table(table(factor(d$spat3_b)))*100,2)
spat3_b <- as.data.frame(spat3_b)
spat3_b$group <- substring(row.names(spat3_b), 1)
spat3_b$group <- revalue(as.character(spat3_b$group), c("1"="Very likely", "2"="Somewhat likely", "3"="Not at all likely", "4"="I don't know"))
spat3_b$plot <- factor(spat3_b$group, spat3_b$group)

p <- ggplot(spat3_b, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = spat3_b, stat = "identity") +
      scale_fill_manual(values = INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("") +
      theme(plot.title = element_text(size=16))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-80-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
spat3_b.tb <- as.factor(d$spat3_b)
spat3_b.tb <- summary(spat3_b.tb)
spat3_b.tb <- as.data.frame(spat3_b.tb)
spat3_b.tb$Var1 <- substring(row.names(spat3_b.tb), 1)
spat3_b.tb$group <- revalue(as.character(spat3_b.tb$Var1), c("1"="Very likely", "2"="Somewhat likely", "3"="Not at all likely", "77"="I don't know"))


## merge with existing prop table data used for plot above 
plot.spat3_b.tb <- merge(spat3_b, spat3_b.tb, by = "group")
plot.spat3_b.tb <- plot.spat3_b.tb[-c(2, 4, 6)]
plot.spat3_b.tb <- setcolorder(plot.spat3_b.tb, c("group", "spat3_b.tb", "Freq"))

plot.spat3_b.tb$order <- c(4,3,2,1)
plot.spat3_b.tb <- plot.spat3_b.tb %>% arrange(order)
plot.spat3_b.tb <- plot.spat3_b.tb[-c(4)]
colnames(plot.spat3_b.tb) <- c("Response", "N", "Percentage")

kable(plot.spat3_b.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very likely </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 6.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat likely </td>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 43.71 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not at all likely </td>
   <td style="text-align:right;"> 103 </td>
   <td style="text-align:right;"> 30.84 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 18.86 </td>
  </tr>
</tbody>
</table>

### Here are some questions about your satisfaction with the neighbourhood in which you live. Please indicate your satisfaction with each item.How satisfied are you with... {.tabset}

#### a. your neighbourhood as a good place to live?


```r
#neighb_a

neighb_a <- round(prop.table(table(factor(d$neighb_a)))*100,2)
neighb_a <- as.data.frame(neighb_a)
neighb_a$group <- substring(row.names(neighb_a), 1)
neighb_a$group <- revalue(as.character(neighb_a$group), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))

neighb_a$plot <- factor(neighb_a$group, neighb_a$group)
neighb_a.plot <- ggplot(neighb_a, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPalette3) +
      ylab("Percent of total") +
      xlab("")
      
neighb_a.plot + geom_histogram(aes(x = plot), data = neighb_a, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-81-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neighb_a.tb <- as.factor(d$neighb_a)
neighb_a.tb <- summary(neighb_a.tb)
neighb_a.tb <- as.data.frame(neighb_a.tb)
neighb_a.tb$Var1 <- substring(row.names(neighb_a.tb), 1)
neighb_a.tb$group <- revalue(as.character(neighb_a.tb$Var1), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))


## merge with existing prop table data used for plot above 
plot.neighb_a.tb <- merge(neighb_a, neighb_a.tb, by = "group")
plot.neighb_a.tb <- plot.neighb_a.tb[-c(2, 4, 6)]
plot.neighb_a.tb <- setcolorder(plot.neighb_a.tb, c("group", "neighb_a.tb", "Freq"))

plot.neighb_a.tb$order <- c(4,3,2,5,1)
plot.neighb_a.tb <- plot.neighb_a.tb %>% arrange(order)
plot.neighb_a.tb <- plot.neighb_a.tb[-c(4)]
colnames(plot.neighb_a.tb) <- c("Response", "N", "Percentage")

kable(plot.neighb_a.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly satisfied </td>
   <td style="text-align:right;"> 195 </td>
   <td style="text-align:right;"> 58.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Satisfied </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 35.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither satisfied nor dissatisfied </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dissatisfied </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly dissatisfied </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.60 </td>
  </tr>
</tbody>
</table>
#### b. the number of people you know in your neighbourhood? 


```r
#neighb_b

neighb_b <- round(prop.table(table(factor(d$neighb_b)))*100,2)
neighb_b <- as.data.frame(neighb_b)
neighb_b$group <- substring(row.names(neighb_b), 1)
neighb_b$group <- revalue(as.character(neighb_b$group), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))

neighb_b$plot <- factor(neighb_b$group, neighb_b$group)
neighb_b.plot <- ggplot(neighb_b, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPalette3) +
      ylab("Percent of total") +
      xlab("")
      
neighb_b.plot + geom_histogram(aes(x = plot), data = neighb_b, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-82-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neighb_b.tb <- as.factor(d$neighb_b)
neighb_b.tb <- summary(neighb_b.tb)
neighb_b.tb <- as.data.frame(neighb_b.tb)
neighb_b.tb$Var1 <- substring(row.names(neighb_b.tb), 1)
neighb_b.tb$group <- revalue(as.character(neighb_b.tb$Var1), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))


## merge with existing prop table data used for plot above 
plot.neighb_b.tb <- merge(neighb_b, neighb_b.tb, by = "group")
plot.neighb_b.tb <- plot.neighb_b.tb[-c(2, 4, 6)]
plot.neighb_b.tb <- setcolorder(plot.neighb_b.tb, c("group", "neighb_b.tb", "Freq"))

plot.neighb_b.tb$order <- c(4,3,2,5,1)
plot.neighb_b.tb <- plot.neighb_b.tb %>% arrange(order)
plot.neighb_b.tb <- plot.neighb_b.tb[-c(4)]
colnames(plot.neighb_b.tb) <- c("Response", "N", "Percentage")

kable(plot.neighb_b.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly satisfied </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 17.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Satisfied </td>
   <td style="text-align:right;"> 139 </td>
   <td style="text-align:right;"> 41.62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither satisfied nor dissatisfied </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 22.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dissatisfied </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 15.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly dissatisfied </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
</tbody>
</table>
#### c. the ethnic diversity of your neighbourhood? 

```r
#neighb_c

neighb_c <- round(prop.table(table(factor(d$neighb_c)))*100,2)
neighb_c <- as.data.frame(neighb_c)
neighb_c$group <- substring(row.names(neighb_c), 1)
neighb_c$group <- revalue(as.character(neighb_c$group), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))

neighb_c$plot <- factor(neighb_c$group, neighb_c$group)

neighb_c.plot <- ggplot(neighb_c, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPalette3) +
      ylab("Percent of total") +
      xlab("")
      
neighb_c.plot + geom_histogram(aes(x = plot), data = neighb_c, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-83-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neighb_c.tb <- as.factor(d$neighb_c)
neighb_c.tb <- summary(neighb_c.tb)
neighb_c.tb <- as.data.frame(neighb_c.tb)
neighb_c.tb$Var1 <- substring(row.names(neighb_c.tb), 1)
neighb_c.tb$group <- revalue(as.character(neighb_c.tb$Var1), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))


## merge with existing prop table data used for plot above 
plot.neighb_c.tb <- merge(neighb_c, neighb_c.tb, by = "group")
plot.neighb_c.tb <- plot.neighb_c.tb[-c(2, 4, 6)]
plot.neighb_c.tb <- setcolorder(plot.neighb_c.tb, c("group", "neighb_c.tb", "Freq"))

plot.neighb_c.tb$order <- c(4,3,2,5,1)
plot.neighb_c.tb <- plot.neighb_c.tb %>% arrange(order)
plot.neighb_c.tb <- plot.neighb_c.tb[-c(4)]
colnames(plot.neighb_c.tb) <- c("Response", "N", "Percentage")

kable(plot.neighb_c.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly satisfied </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 17.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Satisfied </td>
   <td style="text-align:right;"> 125 </td>
   <td style="text-align:right;"> 37.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither satisfied nor dissatisfied </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 33.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dissatisfied </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 10.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly dissatisfied </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
</tbody>
</table>

#### d. your neighbourhood as a good place to raise children


```r
#neighb_d

neighb_d <- round(prop.table(table(factor(d$neighb_d)))*100,2)
neighb_d <- as.data.frame(neighb_d)
neighb_d$group <- substring(row.names(neighb_d), 1)
neighb_d$group <- revalue(as.character(neighb_d$group), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))

neighb_d$plot <- factor(neighb_d$group, neighb_d$group)

neighb_d.plot <- ggplot(neighb_d, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPalette3) +
      ylab("Percent of total") +
      xlab("")
      
neighb_d.plot + geom_histogram(aes(x = plot), data = neighb_d, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-84-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neighb_d.tb <- as.factor(d$neighb_d)
neighb_d.tb <- summary(neighb_d.tb)
neighb_d.tb <- as.data.frame(neighb_d.tb)
neighb_d.tb$Var1 <- substring(row.names(neighb_d.tb), 1)
neighb_d.tb$group <- revalue(as.character(neighb_d.tb$Var1), c("1" = "Strongly satisfied", "2" = "Satisfied", "3" = "Neither satisfied nor dissatisfied", "4" = "Dissatisfied", "5" = "Strongly dissatisfied"))


## merge with existing prop table data used for plot above 
plot.neighb_d.tb <- merge(neighb_d, neighb_d.tb, by = "group")
plot.neighb_d.tb <- plot.neighb_d.tb[-c(2, 4, 6)]
plot.neighb_d.tb <- setcolorder(plot.neighb_d.tb, c("group", "neighb_d.tb", "Freq"))

plot.neighb_d.tb$order <- c(4,3,2,5,1)
plot.neighb_d.tb <- plot.neighb_d.tb %>% arrange(order)
plot.neighb_d.tb <- plot.neighb_d.tb[-c(4)]
colnames(plot.neighb_d.tb) <- c("Response", "N", "Percentage")

kable(plot.neighb_d.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Strongly satisfied </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 32.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Satisfied </td>
   <td style="text-align:right;"> 123 </td>
   <td style="text-align:right;"> 36.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neither satisfied nor dissatisfied </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 24.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dissatisfied </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 4.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Strongly dissatisfied </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
</tbody>
</table>


## Section 7: Neighbourhood Selection

### Before moving into your current dwelling, when you were looking for a neighbourhood to live in, to what extent were the following characteristics important? Please report your perspectives, even if the neighbourhood where you currently live does not have these characteristics {.tabset}

#### a. Good access to public transportation


```r
#neigh_pref_a

neigh_pref_a <- round(prop.table(table(factor(d$neigh_pref_a)))*100,2)
neigh_pref_a <- as.data.frame(neigh_pref_a)
neigh_pref_a$group <- substring(row.names(neigh_pref_a), 1)
neigh_pref_a$group <- revalue(as.character(neigh_pref_a$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_a$plot <- factor(neigh_pref_a$group, neigh_pref_a$group)

p <- ggplot(neigh_pref_a, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_a, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-85-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_a.tb <- as.factor(d$neigh_pref_a)
neigh_pref_a.tb <- summary(neigh_pref_a.tb)
neigh_pref_a.tb <- as.data.frame(neigh_pref_a.tb)
neigh_pref_a.tb$Var1 <- substring(row.names(neigh_pref_a.tb), 1)
neigh_pref_a.tb$group <- revalue(as.character(neigh_pref_a.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_a.tb <- merge(neigh_pref_a, neigh_pref_a.tb, by = "group")
plot.neigh_pref_a.tb <- plot.neigh_pref_a.tb[-c(2, 4, 6)]
plot.neigh_pref_a.tb <- setcolorder(plot.neigh_pref_a.tb, c("group", "neigh_pref_a.tb", "Freq"))

plot.neigh_pref_a.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_a.tb <- plot.neigh_pref_a.tb %>% arrange(order)
plot.neigh_pref_a.tb <- plot.neigh_pref_a.tb[-c(4)]
colnames(plot.neigh_pref_a.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_a.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 189 </td>
   <td style="text-align:right;"> 56.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 29.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 10.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>

#### b. Sufficient parks and green spaces


```r
#neigh_pref_b

neigh_pref_b <- round(prop.table(table(factor(d$neigh_pref_b)))*100,2)
neigh_pref_b <- as.data.frame(neigh_pref_b)
neigh_pref_b$group <- substring(row.names(neigh_pref_b), 1)
neigh_pref_b$group <- revalue(as.character(neigh_pref_b$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_b$plot <- factor(neigh_pref_b$group, neigh_pref_b$group)

p <- ggplot(neigh_pref_b, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_b, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade)+
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-86-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_b.tb <- as.factor(d$neigh_pref_b)
neigh_pref_b.tb <- summary(neigh_pref_b.tb)
neigh_pref_b.tb <- as.data.frame(neigh_pref_b.tb)
neigh_pref_b.tb$Var1 <- substring(row.names(neigh_pref_b.tb), 1)
neigh_pref_b.tb$group <- revalue(as.character(neigh_pref_b.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_b.tb <- merge(neigh_pref_b, neigh_pref_b.tb, by = "group")
plot.neigh_pref_b.tb <- plot.neigh_pref_b.tb[-c(2, 4, 6)]
plot.neigh_pref_b.tb <- setcolorder(plot.neigh_pref_b.tb, c("group", "neigh_pref_b.tb", "Freq"))

plot.neigh_pref_b.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_b.tb <- plot.neigh_pref_b.tb %>% arrange(order)
plot.neigh_pref_b.tb <- plot.neigh_pref_b.tb[-c(4)]
colnames(plot.neigh_pref_b.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_b.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 214 </td>
   <td style="text-align:right;"> 64.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 30.24 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 4.19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>
#### c. Sufficient shops and services

```r
#neigh_pref_c

neigh_pref_c <- round(prop.table(table(factor(d$neigh_pref_c)))*100,2)
neigh_pref_c <- as.data.frame(neigh_pref_c)
neigh_pref_c$group <- substring(row.names(neigh_pref_c), 1)
neigh_pref_c$group <- revalue(as.character(neigh_pref_c$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_c$plot <- factor(neigh_pref_c$group, neigh_pref_c$group)

p <- ggplot(neigh_pref_c, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_c, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-87-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_c.tb <- as.factor(d$neigh_pref_c)
neigh_pref_c.tb <- summary(neigh_pref_c.tb)
neigh_pref_c.tb <- as.data.frame(neigh_pref_c.tb)
neigh_pref_c.tb$Var1 <- substring(row.names(neigh_pref_c.tb), 1)
neigh_pref_c.tb$group <- revalue(as.character(neigh_pref_c.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_c.tb <- merge(neigh_pref_c, neigh_pref_c.tb, by = "group")
plot.neigh_pref_c.tb <- plot.neigh_pref_c.tb[-c(2, 4, 6)]
plot.neigh_pref_c.tb <- setcolorder(plot.neigh_pref_c.tb, c("group", "neigh_pref_c.tb", "Freq"))

plot.neigh_pref_c.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_c.tb <- plot.neigh_pref_c.tb %>% arrange(order)
plot.neigh_pref_c.tb <- plot.neigh_pref_c.tb[-c(4)]
colnames(plot.neigh_pref_c.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_c.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 193 </td>
   <td style="text-align:right;"> 57.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 35.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 5.69 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>
#### d. Proximity to doctors, a pharmacy or other health services

```r
#neigh_pref_d

neigh_pref_d <- round(prop.table(table(factor(d$neigh_pref_d)))*100,2)
neigh_pref_d <- as.data.frame(neigh_pref_d)
neigh_pref_d$group <- substring(row.names(neigh_pref_d), 1)
neigh_pref_d$group <- revalue(as.character(neigh_pref_d$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_d$plot <- factor(neigh_pref_d$group, neigh_pref_d$group)

p <- ggplot(neigh_pref_d, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_d, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-88-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_d.tb <- as.factor(d$neigh_pref_d)
neigh_pref_d.tb <- summary(neigh_pref_d.tb)
neigh_pref_d.tb <- as.data.frame(neigh_pref_d.tb)
neigh_pref_d.tb$Var1 <- substring(row.names(neigh_pref_d.tb), 1)
neigh_pref_d.tb$group <- revalue(as.character(neigh_pref_d.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_d.tb <- merge(neigh_pref_d, neigh_pref_d.tb, by = "group")
plot.neigh_pref_d.tb <- plot.neigh_pref_d.tb[-c(2, 4, 6)]
plot.neigh_pref_d.tb <- setcolorder(plot.neigh_pref_d.tb, c("group", "neigh_pref_d.tb", "Freq"))

plot.neigh_pref_d.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_d.tb <- plot.neigh_pref_d.tb %>% arrange(order)
plot.neigh_pref_d.tb <- plot.neigh_pref_d.tb[-c(4)]
colnames(plot.neigh_pref_d.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_d.tb)%>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 33.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 124 </td>
   <td style="text-align:right;"> 37.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 22.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.20 </td>
  </tr>
</tbody>
</table>

#### e. A good knowledge of the neighbourhood

```r
#neigh_pref_e

neigh_pref_e <- round(prop.table(table(factor(d$neigh_pref_e)))*100,2)
neigh_pref_e <- as.data.frame(neigh_pref_e)
neigh_pref_e$group <- substring(row.names(neigh_pref_e), 1)
neigh_pref_e$group <- revalue(as.character(neigh_pref_e$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_e$plot <- factor(neigh_pref_e$group, neigh_pref_e$group)

p <- ggplot(neigh_pref_e, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_e, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-89-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_e.tb <- as.factor(d$neigh_pref_e)
neigh_pref_e.tb <- summary(neigh_pref_e.tb)
neigh_pref_e.tb <- as.data.frame(neigh_pref_e.tb)
neigh_pref_e.tb$Var1 <- substring(row.names(neigh_pref_e.tb), 1)
neigh_pref_e.tb$group <- revalue(as.character(neigh_pref_e.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_e.tb <- merge(neigh_pref_e, neigh_pref_e.tb, by = "group")
plot.neigh_pref_e.tb <- plot.neigh_pref_e.tb[-c(2, 4, 6)]
plot.neigh_pref_e.tb <- setcolorder(plot.neigh_pref_e.tb, c("group", "neigh_pref_e.tb", "Freq"))

plot.neigh_pref_e.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_e.tb <- plot.neigh_pref_e.tb %>% arrange(order)
plot.neigh_pref_e.tb <- plot.neigh_pref_e.tb[-c(4)]
colnames(plot.neigh_pref_e.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_e.tb)%>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 24.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 164 </td>
   <td style="text-align:right;"> 49.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 19.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 5.39 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
</tbody>
</table>

#### f. Presence of relatives, friends or acquaintances

```r
#neigh_pref_f

neigh_pref_f <- round(prop.table(table(factor(d$neigh_pref_f)))*100,2)
neigh_pref_f <- as.data.frame(neigh_pref_f)
neigh_pref_f$group <- substring(row.names(neigh_pref_f), 1)
neigh_pref_f$group <- revalue(as.character(neigh_pref_f$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_f$plot <- factor(neigh_pref_f$group, neigh_pref_f$group)

p <- ggplot(neigh_pref_f, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_f, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-90-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_f.tb <- as.factor(d$neigh_pref_f)
neigh_pref_f.tb <- summary(neigh_pref_f.tb)
neigh_pref_f.tb <- as.data.frame(neigh_pref_f.tb)
neigh_pref_f.tb$Var1 <- substring(row.names(neigh_pref_f.tb), 1)
neigh_pref_f.tb$group <- revalue(as.character(neigh_pref_f.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_f.tb <- merge(neigh_pref_f, neigh_pref_f.tb, by = "group")
plot.neigh_pref_f.tb <- plot.neigh_pref_f.tb[-c(2, 4, 6)]
plot.neigh_pref_f.tb <- setcolorder(plot.neigh_pref_f.tb, c("group", "neigh_pref_f.tb", "Freq"))

plot.neigh_pref_f.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_f.tb <- plot.neigh_pref_f.tb %>% arrange(order)
plot.neigh_pref_f.tb <- plot.neigh_pref_f.tb[-c(4)]
colnames(plot.neigh_pref_f.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_f.tb)%>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 13.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 107 </td>
   <td style="text-align:right;"> 32.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 33.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 18.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
</tbody>
</table>
#### g. A neighbourhood where it is pleasant to walk

```r
#neigh_pref_g

neigh_pref_g <- round(prop.table(table(factor(d$neigh_pref_g)))*100,2)
neigh_pref_g <- as.data.frame(neigh_pref_g)
neigh_pref_g$group <- substring(row.names(neigh_pref_g), 1)
neigh_pref_g$group <- revalue(as.character(neigh_pref_g$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_g$plot <- factor(neigh_pref_g$group, neigh_pref_g$group)

p <- ggplot(neigh_pref_g, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_g, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-91-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_g.tb <- as.factor(d$neigh_pref_g)
neigh_pref_g.tb <- summary(neigh_pref_g.tb)
neigh_pref_g.tb <- as.data.frame(neigh_pref_g.tb)
neigh_pref_g.tb$Var1 <- substring(row.names(neigh_pref_g.tb), 1)
neigh_pref_g.tb$group <- revalue(as.character(neigh_pref_g.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_g.tb <- merge(neigh_pref_g, neigh_pref_g.tb, by = "group")
plot.neigh_pref_g.tb <- plot.neigh_pref_g.tb[-c(2, 4, 6)]
plot.neigh_pref_g.tb <- setcolorder(plot.neigh_pref_g.tb, c("group", "neigh_pref_g.tb", "Freq"))

plot.neigh_pref_g.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_g.tb <- plot.neigh_pref_g.tb %>% arrange(order)
plot.neigh_pref_g.tb <- plot.neigh_pref_g.tb[-c(4)]
colnames(plot.neigh_pref_g.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_g.tb)%>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 233 </td>
   <td style="text-align:right;"> 69.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 25.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 3.29 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>


#### h. A neighbourhood where it is practical to move around by car (ease of parking, low traffic, good access by car)

```r
#neigh_pref_h

neigh_pref_h <- round(prop.table(table(factor(d$neigh_pref_h)))*100,2)
neigh_pref_h <- as.data.frame(neigh_pref_h)
neigh_pref_h$group <- substring(row.names(neigh_pref_h), 1)
neigh_pref_h$group <- revalue(as.character(neigh_pref_h$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_h$plot <- factor(neigh_pref_h$group, neigh_pref_h$group)

p <- ggplot(neigh_pref_h, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_h, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-92-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_h.tb <- as.factor(d$neigh_pref_h)
neigh_pref_h.tb <- summary(neigh_pref_h.tb)
neigh_pref_h.tb <- as.data.frame(neigh_pref_h.tb)
neigh_pref_h.tb$Var1 <- substring(row.names(neigh_pref_h.tb), 1)
neigh_pref_h.tb$group <- revalue(as.character(neigh_pref_h.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_h.tb <- merge(neigh_pref_h, neigh_pref_h.tb, by = "group")
plot.neigh_pref_h.tb <- plot.neigh_pref_h.tb[-c(2, 4, 6)]
plot.neigh_pref_h.tb <- setcolorder(plot.neigh_pref_h.tb, c("group", "neigh_pref_h.tb", "Freq"))

plot.neigh_pref_h.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_h.tb <- plot.neigh_pref_h.tb %>% arrange(order)
plot.neigh_pref_h.tb <- plot.neigh_pref_h.tb[-c(4)]
colnames(plot.neigh_pref_h.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_h.tb)%>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 26.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 35.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 19.76 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 17.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
</tbody>
</table>

#### i. Presence of good schools

```r
#neigh_pref_i

neigh_pref_i <- round(prop.table(table(factor(d$neigh_pref_i)))*100,2)
neigh_pref_i <- as.data.frame(neigh_pref_i)
neigh_pref_i$group <- substring(row.names(neigh_pref_i), 1)
neigh_pref_i$group <- revalue(as.character(neigh_pref_i$group), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "5" = "I don't know"))
neigh_pref_i$plot <- factor(neigh_pref_i$group, neigh_pref_i$group)

p <- ggplot(neigh_pref_i, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = neigh_pref_i, stat = "identity") +
  scale_fill_manual(values=INTERACTshortfade) +
  guides(fill=FALSE)+
  ylab("Percent of total") +
  xlab("") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-93-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
neigh_pref_i.tb <- as.factor(d$neigh_pref_i)
neigh_pref_i.tb <- summary(neigh_pref_i.tb)
neigh_pref_i.tb <- as.data.frame(neigh_pref_i.tb)
neigh_pref_i.tb$Var1 <- substring(row.names(neigh_pref_i.tb), 1)
neigh_pref_i.tb$group <- revalue(as.character(neigh_pref_i.tb$Var1), c("1" = "Very important", "2" = "Somewhat important", "3" = "Not very important", "4" = "Not important at all", "77" = "I don't know"))


## merge with existing prop table data used for plot above 
plot.neigh_pref_i.tb <- merge(neigh_pref_i, neigh_pref_i.tb, by = "group")
plot.neigh_pref_i.tb <- plot.neigh_pref_i.tb[-c(2, 4, 6)]
plot.neigh_pref_i.tb <- setcolorder(plot.neigh_pref_i.tb, c("group", "neigh_pref_i.tb", "Freq"))

plot.neigh_pref_i.tb$order <- c(5,4,3,2,1)
plot.neigh_pref_i.tb <- plot.neigh_pref_i.tb %>% arrange(order)
plot.neigh_pref_i.tb <- plot.neigh_pref_i.tb[-c(4)]
colnames(plot.neigh_pref_i.tb) <- c("Response", "N", "Percentage")

kable(plot.neigh_pref_i.tb)%>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very important </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 23.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somewhat important </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 16.47 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not very important </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 17.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not important at all </td>
   <td style="text-align:right;"> 113 </td>
   <td style="text-align:right;"> 33.83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 8.98 </td>
  </tr>
</tbody>
</table>

## Section 8: Activity Tracking 

These questions were asked in two different questionnaires, depending on the participant's choice of participation option. They were asked on the Ethica app to those who signed up to use the app when registering for the study, and in the Health Survey to those who chose not to use the app. 
The answers below are from the Health Survey only (n= 65), and do not include responses collected via the Ethica app. 

### Do you currently own or use any of the following devices or smartphone apps to monitor your physical activity? {.tabset} 

#### No


```r
#tracking1 and tracking1_use


tracking1_no <- round((sum(d$tracking1_no)/ length(d$tracking1_no))*100,2)

tracking1_no_interest <- round((sum(d$tracking1_no_interest)/ length(d$tracking1_no_interest))*100,2)

no_tracking <- data.frame(Response=c("I do not have one but might be interested in trying one", "I do not have one and I am not interested in trying one"),
                          Percentage= c(tracking1_no, tracking1_no_interest))

kable(no_tracking) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")  
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> I do not have one but might be interested in trying one </td>
   <td style="text-align:right;"> 26.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I do not have one and I am not interested in trying one </td>
   <td style="text-align:right;"> 12.87 </td>
  </tr>
</tbody>
</table>

#### Own



```r
#tracking own 

# Create a vector with variable names
response = paste0("tracking1_own_", 1:4)

# Empty vector to stor output
tracking1_own_prop <- c()

# Calculate univariate proportions
for(i in response){
  tracking1_own_prop[i] <- sum(d[,i]) / nrow(d)
}

# Transform
tracking1_own_prop <- as.data.frame(tracking1_own_prop)
tracking1_own_prop$Own <- c("Wearable devices (Fitbits, Garmins, and Jawbone, etc.)","Smart watches (Apple Watch, Galaxy Gear, Samsung Gear, etc.)","Smartphone app (Apple Health, Samsung Health, Google Fit, Strava, etc.)","Other")

tracking1_own_prop$plot<- factor(tracking1_own_prop$Own, tracking1_own_prop$Own)

ggplot(tracking1_own_prop, aes(x = plot, y = tracking1_own_prop)) + geom_bar(stat = "identity", fill = "#76D24A") + xlab("") + ylab("Percent of participants who selected this answer") + theme(axis.text.x  = element_text(size= 14, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-95-1.png)<!-- -->

```r
tracking1_own_prop <- tracking1_own_prop[-c(3)]
tracking1_own_prop$tracking1_own_prop <- round(tracking1_own_prop$tracking1_own_prop*100,2)

tracking1_own_prop <- setcolorder(tracking1_own_prop, c("Own", "tracking1_own_prop"))

colnames(tracking1_own_prop) <- c("Response", "Percent of participants who selected this answer")

kable(tracking1_own_prop) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")  
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Percent of participants who selected this answer </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> tracking1_own_1 </td>
   <td style="text-align:left;"> Wearable devices (Fitbits, Garmins, and Jawbone, etc.) </td>
   <td style="text-align:right;"> 23.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tracking1_own_2 </td>
   <td style="text-align:left;"> Smart watches (Apple Watch, Galaxy Gear, Samsung Gear, etc.) </td>
   <td style="text-align:right;"> 10.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tracking1_own_3 </td>
   <td style="text-align:left;"> Smartphone app (Apple Health, Samsung Health, Google Fit, Strava, etc.) </td>
   <td style="text-align:right;"> 37.43 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tracking1_own_4 </td>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 3.59 </td>
  </tr>
</tbody>
</table>

#### Use



```r
#tracking use

# Create a vector with variable names
response = paste0("tracking1_use_", 1:4)

# Empty vector to stor output
tracking1_use_prop <- c()

# Calculate univariate proportions
for(i in response){
  tracking1_use_prop[i] <- sum(d[,i]) / nrow(d)
}

# Transform
tracking1_use_prop <- as.data.frame(tracking1_use_prop)
tracking1_use_prop$use <- c("Wearable devices (Fitbits, Garmins, and Jawbone, etc.)","Smart watches (Apple Watch, Galaxy Gear, Samsung Gear, etc.)","Smartphone app (Apple Health, Samsung Health, Google Fit, Strava, etc.)","Other")

tracking1_use_prop$plot<- factor(tracking1_use_prop$use, tracking1_use_prop$use)

ggplot(tracking1_use_prop, aes(x = plot, y = tracking1_use_prop)) + geom_bar(stat = "identity", fill = "#76D24A") + xlab("") + ylab("Percent of participants who selected this answer") + theme(axis.text.x  = element_text(size= 14, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-96-1.png)<!-- -->

```r
tracking1_use_prop <- tracking1_use_prop[-c(3)]
tracking1_use_prop$tracking1_use_prop <- round(tracking1_use_prop$tracking1_use_prop*100,2)

tracking1_use_prop <- setcolorder(tracking1_use_prop, c("use", "tracking1_use_prop"))

colnames(tracking1_use_prop) <- c("Response", "Percent of participants who selected this answer")

kable(tracking1_use_prop) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")  
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Percent of participants who selected this answer </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> tracking1_use_1 </td>
   <td style="text-align:left;"> Wearable devices (Fitbits, Garmins, and Jawbone, etc.) </td>
   <td style="text-align:right;"> 14.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tracking1_use_2 </td>
   <td style="text-align:left;"> Smart watches (Apple Watch, Galaxy Gear, Samsung Gear, etc.) </td>
   <td style="text-align:right;"> 8.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tracking1_use_3 </td>
   <td style="text-align:left;"> Smartphone app (Apple Health, Samsung Health, Google Fit, Strava, etc.) </td>
   <td style="text-align:right;"> 24.55 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tracking1_use_4 </td>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 3.89 </td>
  </tr>
</tbody>
</table>


### Thinking about a typical month, how many days on average do you use your device or smartphone app to monitor your physical activity? If you own several activity trackers, choose the one that you use most often.


```r
#tracking2

d$tracking2[d$tracking2==-7] <- NA

ggplot(d, aes(d$tracking2)) + geom_histogram(na.rm = TRUE, binwidth = 1, fill="#76D24A") + xlab("Days per month") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-97-1.png)<!-- -->

### How concerned are you about the possibility of your location being known by the company which developed the device or app? {.tabset}  

#### When using a device or app to monitor your physical activity



```r
#tracking3a
d$tracking3a[d$tracking3a==-7] <- NA

tracking3a <- round(prop.table(table(factor(d$tracking3a, levels= c(1:6))))*100,2)
tracking3a <- as.data.frame(tracking3a)
tracking3a$group <- substring(row.names(tracking3a), 1)
tracking3a$group <- revalue(as.character(tracking3a$group), c("1" = "Not at all", "2" = "Slightly", "3" = "Moderately", "4" = "Very much", "5" = "Extremely", "6"= "I have no opinion on the subject"))
tracking3a$plot <- factor(tracking3a$group, tracking3a$group)

p <- ggplot(tracking3a, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = tracking3a, stat = "identity") +
  scale_fill_manual(values = INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-98-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
tracking3a.tb <- as.factor(d$tracking3a)
tracking3a.tb <- summary(tracking3a.tb)
tracking3a.tb <- as.data.frame(tracking3a.tb)
tracking3a.tb$Var1 <- substring(row.names(tracking3a.tb), 1)

nval.df <- c("0") #insert missing values 
nval.df <- as.data.frame(nval.df)
nval.df$tracking3a.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("6")
nval.df <- nval.df[-c(1)]

tracking3a.tb <- rbind(tracking3a.tb, nval.df)

tracking3a.tb$group <- revalue(as.character(tracking3a.tb$Var1), c("1" = "Not at all", "2" = "Slightly", "3" = "Moderately", "4" = "Very much", "5" = "Extremely", "6"= "I have no opinion on the subject"))


## merge with existing prop table data used for plot above 
plot.tracking3a.tb <- merge(tracking3a, tracking3a.tb, by = "group")
plot.tracking3a.tb <- plot.tracking3a.tb[-c(2, 4, 6)]
plot.tracking3a.tb <- setcolorder(plot.tracking3a.tb, c("group", "tracking3a.tb", "Freq"))

plot.tracking3a.tb$order <- c(5,6,3,1,2,4)
plot.tracking3a.tb <- plot.tracking3a.tb %>% arrange(order)
plot.tracking3a.tb <- plot.tracking3a.tb[-c(4)]
colnames(plot.tracking3a.tb) <- c("Response", "N", "Percentage")

kable(plot.tracking3a.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not at all </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 25.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Slightly </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 16.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Moderately </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 8.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very much </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 41.67 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Extremely </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 8.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I have no opinion on the subject </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>

#### If you had to use a device or app?



```r
#tracking3b  
d$tracking3b[d$tracking3b==-7] <- NA

tracking3b <- round(prop.table(table(factor(d$tracking3b)))*100,2)
tracking3b <- as.data.frame(tracking3b)
tracking3b$group <- substring(row.names(tracking3b), 1)
tracking3b$group <- revalue(as.character(tracking3b$group), c("1" = "Not at all", "2" = "Slightly", "3" = "Moderately", "4" = "Very much", "5" = "Extremely", "6"= "I have no opinion on the subject"))
tracking3b$plot <- factor(tracking3b$group, tracking3b$group)

p <- ggplot(tracking3b, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = tracking3b, stat = "identity") +
  scale_fill_manual(values = INTERACTPalette3) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-99-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
tracking3b.tb <- as.factor(d$tracking3b)
tracking3b.tb <- summary(tracking3b.tb)
tracking3b.tb <- as.data.frame(tracking3b.tb)
tracking3b.tb$Var1 <- substring(row.names(tracking3b.tb), 1)
tracking3b.tb$group <- revalue(as.character(tracking3b.tb$Var1), c("1" = "Not at all", "2" = "Slightly", "3" = "Moderately", "4" = "Very much", "5" = "Extremely", "6"= "I have no opinion on the subject"))


## merge with existing prop table data used for plot above 
plot.tracking3b.tb <- merge(tracking3b, tracking3b.tb, by = "group")
plot.tracking3b.tb <- plot.tracking3b.tb[-c(2, 4, 6)]
plot.tracking3b.tb <- setcolorder(plot.tracking3b.tb, c("group", "tracking3b.tb", "Freq"))

plot.tracking3b.tb$order <- c(5,6,3,1,2,4)
plot.tracking3b.tb <- plot.tracking3b.tb %>% arrange(order)
plot.tracking3b.tb <- plot.tracking3b.tb[-c(4)]
colnames(plot.tracking3b.tb) <- c("Response", "N", "Percentage")

kable(plot.tracking3b.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not at all </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Slightly </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Moderately </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 13.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very much </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Extremely </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 18.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I have no opinion on the subject </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 11.32 </td>
  </tr>
</tbody>
</table>

### How concerned are you about the possibility of your location being known by your network mobile provider when using a smartphone?
 

```r
#tracking4
 
d$tracking4[d$tracking4==-7] <- NA

tracking4 <- round(prop.table(table(factor(d$tracking4)))*100,2)
tracking4 <- as.data.frame(tracking4)
tracking4$group <- substring(row.names(tracking4), 1)
tracking4$group <- revalue(as.character(tracking4$group), c("1" = "Not at all", "2" = "Slightly", "3" = "Moderately", "4" = "Very much", "5" = "Extremely", "6"= "No opinion", "7" = "I do not use a smartphone"))
tracking4$plot <- factor(tracking4$group, tracking4$group)

p <- ggplot(tracking4, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = tracking4, stat = "identity") +
  scale_fill_manual(values=INTERACTPalettecont) +
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-100-1.png)<!-- -->

```r
# make a clean summary table 
## make a dataframe on count 
tracking4.tb <- as.factor(d$tracking4)
tracking4.tb <- summary(tracking4.tb)
tracking4.tb <- as.data.frame(tracking4.tb)
tracking4.tb$Var1 <- substring(row.names(tracking4.tb), 1)
tracking4.tb$group <- revalue(as.character(tracking4.tb$Var1), c("1" = "Not at all", "2" = "Slightly", "3" = "Moderately", "4" = "Very much", "5" = "Extremely", "6"= "No opinion", "7" = "I do not use a smartphone"))


## merge with existing prop table data used for plot above 
plot.tracking4.tb <- merge(tracking4, tracking4.tb, by = "group")
plot.tracking4.tb <- plot.tracking4.tb[-c(2, 4, 6)]
plot.tracking4.tb <- setcolorder(plot.tracking4.tb, c("group", "tracking4.tb", "Freq"))

plot.tracking4.tb$order <- c(5,6,3,7,1,2,4)
plot.tracking4.tb <- plot.tracking4.tb %>% arrange(order)
plot.tracking4.tb <- plot.tracking4.tb[-c(4)]
colnames(plot.tracking4.tb) <- c("Response", "N", "Percentage")

kable(plot.tracking4.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not at all </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 23.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Slightly </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 15.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Moderately </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 18.46 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Very much </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 21.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Extremely </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 12.31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I do not use a smartphone </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 6.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No opinion </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3.08 </td>
  </tr>
</tbody>
</table>


### Compared with friends of my age, my concern regarding protecting my privacy is.

```r
#tracking5
d$tracking5[d$tracking5==-7] <- NA

tracking5 <- round(prop.table(table(factor(d$tracking5)))*100,2)
tracking5 <- as.data.frame(tracking5)
tracking5$group <- substring(row.names(tracking5), 1)
tracking5$group <- revalue(as.character(tracking5$group), c("1" = "Much lower", "2" = "Lower", "3" = "About the same", "4" = "Higher", "5" = "Much higher"))
tracking5$plot <- factor(tracking5$group, tracking5$group)

p <- ggplot(tracking5, aes(x=group, y=Freq, fill=plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

p + geom_histogram(aes(x = plot), data = tracking5, stat = "identity") +
  scale_fill_manual(values=INTERACTPalette3) + 
  guides(fill=FALSE)+
      ylab("Percent of total") +
      xlab("")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-101-1.png)<!-- -->

```r
tracking5.tb <- as.factor(d$tracking5)
tracking5.tb <- summary(tracking5.tb)
tracking5.tb <- as.data.frame(tracking5.tb)
tracking5.tb$Var1 <- substring(row.names(tracking5.tb), 1)
tracking5.tb$group <- revalue(as.character(tracking5.tb$Var1), c("1" = "Much lower", "2" = "Lower", "3" = "About the same", "4" = "Higher", "5" = "Much higher"))


## merge with existing prop table data used for plot above 
plot.tracking5.tb <- merge(tracking5, tracking5.tb, by = "group")
plot.tracking5.tb <- plot.tracking5.tb[-c(2, 4, 6)]
plot.tracking5.tb <- setcolorder(plot.tracking5.tb, c("group", "tracking5.tb", "Freq"))

plot.tracking5.tb$order <- c(3,4,2,5,1)
plot.tracking5.tb <- plot.tracking5.tb %>% arrange(order)
plot.tracking5.tb <- plot.tracking5.tb[-c(4)]
colnames(plot.tracking5.tb) <- c("Response", "N", "Percentage")

kable(plot.tracking5.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Much lower </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 9.23 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lower </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 6.15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> About the same </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 63.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Higher </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 20.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Much higher </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1.54 </td>
  </tr>
</tbody>
</table>

## Section 9: Demographics 


### What is your current gender identity?


```r
#gender 

gender <- round(prop.table(table(factor(d$gender, levels = c("1", "2", "3", "4", "5", "6"))))*100,2)
gender <- as.data.frame(gender)
gender$response <- substring(row.names(gender), 1)
gender$response <- revalue(as.factor(gender$response), c("1"="Man","2"="Woman","3"="Trans man", "4"="Trans woman", "5"="Genderqueer/Gender non-conforming", "6"="Different identity"))

gender$response <- factor(gender$response, gender$response)

 
p <- ggplot(gender, aes(x = response, y = Freq, fill = response)) + theme(axis.text.x = element_text(size=12, angle=90, vjust = .6, hjust= 1))

p + geom_histogram(aes(x = response), data = gender, stat = "identity") +
  scale_fill_manual(values = INTERACTPaletteSet) +
  guides(fill=FALSE) +
      ylab("Percent of total") +
      xlab("Gender") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-102-1.png)<!-- -->

```r
gender.tb <- as.factor(d$gender) 
gender.tb <- summary(gender.tb)
gender.tb <- as.data.frame(gender.tb)
gender.tb$Var1 <- substring(row.names(gender.tb), 1)


nval.df <- c("0", "0", "0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$gender.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("3", "4", "6")
nval.df <- nval.df[-c(1)]

gender.tb <- rbind(gender.tb, nval.df)

gender.tb$response <- revalue(as.character(gender.tb$Var1), c("1"="Man","2"="Woman","3"="Trans man", "4"="Trans woman", "5"="Genderqueer/Gender non-conforming", "6"="Different identity"))

plot.gender <- merge(gender, gender.tb, by = "response")
plot.gender <- plot.gender[-c(2, 5)]
plot.gender <- setcolorder(plot.gender, c("response", "gender.tb", "Freq"))
plot.gender$order <- c(6,5,1,3,4,2)
plot.gender <- plot.gender %>% arrange(order)
plot.gender <- plot.gender[-c(4)]
colnames(plot.gender) <- c("Response", "N", "Percentage")


kable(plot.gender) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Man </td>
   <td style="text-align:left;"> 110 </td>
   <td style="text-align:right;"> 32.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Woman </td>
   <td style="text-align:left;"> 223 </td>
   <td style="text-align:right;"> 66.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trans man </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trans woman </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Genderqueer/Gender non-conforming </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Different identity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>


### What sex were you assigned at birth?


```r
# Sex

sex <- prop.table(table(factor(Health$sex, levels = 1:3)))*100
sex <- as.data.frame(sex)
sex$response <- substring(row.names(sex), 1)
sex$response <- revalue(as.factor(sex$response), c("1" = "Male", "2" = "Female", "3" = "Other"))

sex$response <- factor(sex$response, sex$response)

p <- ggplot(sex, aes(x = response, y = Freq, fill = response)) + theme(axis.text.x = element_text(angle=90, vjust = .6))

p + geom_histogram(aes(x = response), data = sex, stat = "identity") +
 scale_fill_manual(values = INTERACTPaletteSet) +
  guides(fill=FALSE) +
      ylab("Percent of total") +
      xlab("Sex") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-103-1.png)<!-- -->

```r
## Table- 
kable(data.frame(Response = c("Male","Female"),
           Frequence = as.numeric(table(d$sex)), 
           Percentage = round(as.numeric(prop.table(table(d$sex)))*100,2))) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Male </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 32.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Female </td>
   <td style="text-align:right;"> 224 </td>
   <td style="text-align:right;"> 67.07 </td>
  </tr>
</tbody>
</table>


### What is your marital status? Are you...


```r
#marital_status 

marital <- prop.table(table(factor(d$marital_status, levels = c("1", "2", "3", "4"))))*100
marital <- as.data.frame(marital)
marital$group <- substring(row.names(marital), 1)
marital$group <- revalue(as.character(marital$group), c("1" = "Single", "2" = "Married/commonlaw", "3" = "Separated/divorced", "4" = "Widowed"))

marital$plot <- factor(marital$group, marital$group)

marital.plot <- ggplot(marital, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteSet)  +
      ylab("Percent of total") +
      xlab("")
      
marital.plot + geom_histogram(aes(x = plot), data = marital, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-104-1.png)<!-- -->

```r
## Table- 
kable(data.frame(Response = c("Single", "Married/commonlaw", "Separated/divorced", "Widowed"),
           Frequence = as.numeric(table(d$marital_status)), Percentage = round(as.numeric(prop.table(table(d$marital_status)))*100,2))) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Single </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 21.56 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Married/commonlaw </td>
   <td style="text-align:right;"> 209 </td>
   <td style="text-align:right;"> 62.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Separated/divorced </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 11.38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Widowed </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 4.49 </td>
  </tr>
</tbody>
</table>

### Do you have children?


```r
#children

children <- prop.table(table(factor(d$children, levels = c("1", "2"))))*100
children <- as.data.frame(children)
children$group <- substring(row.names(children), 1)
children$group <- revalue(as.character(children$group), c("1" = "Yes",  "2" = "No"))

children$plot <- factor(children$group, children$group)

children.plot <- ggplot(children, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) #order responses as in t5

children.plot + geom_histogram(aes(x = plot), data = children, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-105-1.png)<!-- -->

```r
## Table- 
kable(data.frame(Response = c("Yes", "No"),
           Frequence = as.numeric(table(d$children)), Percentage = round(as.numeric(prop.table(table(d$children)))*100,2))) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 194 </td>
   <td style="text-align:right;"> 58.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 140 </td>
   <td style="text-align:right;"> 41.92 </td>
  </tr>
</tbody>
</table>


### How many children do you have?


```r
#living_children

d$living_children[d$living_children==-7] <- NA

living_children <- round(prop.table(table(factor(d$living_children)))*100,2)
living_children <- as.data.frame(living_children)
living_children <- as.data.frame(living_children)
living_children$answer <- substring(row.names(living_children), 1)
living_children$answer <- revalue(as.character(living_children$answer))

living_children$plot <- factor(living_children$answer, living_children$answer)

living_children.plot <- ggplot(living_children, aes(x = answer, y = Freq, fill = plot, na.rm = TRUE)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

living_children.plot + geom_histogram(aes(x = plot), data = living_children, stat = "identity") +
guides(fill = FALSE) +
  scale_fill_manual(values = INTERACTPalette3)+
  ylab("Percent of total") +
  xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-106-1.png)<!-- -->

```r
living_children.tb <- data.frame(Response = c("1", "2", "3", "4", "5"),
           Frequence = as.numeric(table(d$living_children)), Percentage = round(as.numeric(prop.table(table(d$living_children)))*100,2)) 


kable(living_children.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")    
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 24.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 50.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 16.49 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 6.70 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.06 </td>
  </tr>
</tbody>
</table>

### What is your current living arrangement? Do you live {.tabset}

#### alone


```r
#d$living_arrange_1[d$living_arrange_1==-7] <- NA

living_arrange_1 <- round(prop.table(table(factor(d$living_arrange_1)))*100,2)
living_arrange_1 <- as.data.frame(living_arrange_1)
living_arrange_1$group <- substring(row.names(living_arrange_1), 1)
living_arrange_1$group <- revalue(as.character(living_arrange_1$group), c("1" = "With other people",  "2" = "Alone"))

living_arrange_1$plot <- factor(living_arrange_1$group, living_arrange_1$group)

living_arrange_1.plot <- ggplot(living_arrange_1, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

living_arrange_1.plot + geom_histogram(aes(x = group), data = living_arrange_1, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-107-1.png)<!-- -->

```r
## Table- 
kable(data.frame(Response = c("With other people", "Alone"),
           Frequence = as.numeric(table(d$living_arrange_1)), Percentage = round(as.numeric(prop.table(table(d$living_arrange_1)))*100,2))) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> With other people </td>
   <td style="text-align:right;"> 238 </td>
   <td style="text-align:right;"> 71.26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alone </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 28.74 </td>
  </tr>
</tbody>
</table>

#### With other people 

_Participants could choose multiple answers_


```r
#living_arrange  

# Create a vector with variable names
response = paste0("living_arrange_", 2:7)

# Empty vector to stor output
living_arrange_prop <- c()

# Calculate univariate proportions
for(i in response){
  living_arrange_prop[i] <- sum(d[,i]) / nrow(d)
}

# Transform
living_arrange_prop <- as.data.frame(living_arrange_prop)
living_arrange_prop$Response <- c("With a spouse (or partner)","With children","With grandchildren","With relatives or siblings?", "With friends", "With other people")

living_arrange_prop$plot<- factor(living_arrange_prop$Response, living_arrange_prop$Response)

ggplot(living_arrange_prop, aes(x = plot, y = living_arrange_prop)) + geom_bar(stat = "identity", fill = "#76D24A") + xlab("") + ylab("Percentage of participants who selected this answer") + theme(axis.text.x  = element_text(size=12, angle=0, vjust=.6)) + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10))
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-108-1.png)<!-- -->

```r
living_arrange_prop$living_arrange_prop <- round(living_arrange_prop$living_arrange_prop*100,2)

living_arrange_prop <- setcolorder(living_arrange_prop, c("Response", "living_arrange_prop"))

colnames(living_arrange_prop) <- c("Response", "Percentage of participants who selected this answer")

living_arrange_prop <- living_arrange_prop[-c(3)]
kable(living_arrange_prop) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")  
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Percentage of participants who selected this answer </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> living_arrange_2 </td>
   <td style="text-align:left;"> With a spouse (or partner) </td>
   <td style="text-align:right;"> 62.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> living_arrange_3 </td>
   <td style="text-align:left;"> With children </td>
   <td style="text-align:right;"> 26.65 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> living_arrange_4 </td>
   <td style="text-align:left;"> With grandchildren </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> living_arrange_5 </td>
   <td style="text-align:left;"> With relatives or siblings? </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> living_arrange_6 </td>
   <td style="text-align:left;"> With friends </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> living_arrange_7 </td>
   <td style="text-align:left;"> With other people </td>
   <td style="text-align:right;"> 1.80 </td>
  </tr>
</tbody>
</table>


### How many children under the age of 16 live in your household?


```r
#children_household

ggplot(d, aes(x = d$children_household)) + geom_bar(na.rm = TRUE,fill="#76D24A", binwidth = 1) + xlab("Number of children under 16 in household")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-109-1.png)<!-- -->

```r
summary(d$children_household)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.2934  0.0000  5.0000
```


### How many adults aged 16 or older live in your household including yourself?


```r
ggplot(d, aes(x = d$adults_household)) + geom_bar(na.rm = TRUE,fill="#76D24A", binwidth = 1) + xlab("Number of adults in household")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-110-1.png)<!-- -->

```r
summary(d$children_household)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.2934  0.0000  5.0000
```


### Thinking about where you live now, are you 


```r
#house_tenure

house_tenure <- round(prop.table(table(factor(d$house_tenure, levels = c(1:6))))*100,2)
house_tenure <- as.data.frame(house_tenure)
house_tenure$group <- substring(row.names(house_tenure), 1)
house_tenure$group <- revalue(as.character(house_tenure$group), c("1" = "An owner", "2" = "A tenant", "3" = "Resident in a relative or friend's home", "4" = "Resident other than in a relative or friend's home", "5" = "Other", "6" = "I don't know"))

house_tenure$plot <- factor(house_tenure$group, house_tenure$group)

house_tenure.plot <- ggplot(house_tenure, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteSet) +
      ylab("Percent of total") +
      xlab("")
      
house_tenure.plot + geom_histogram(aes(x = plot), data = house_tenure, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-111-1.png)<!-- -->

```r
house_tenure.tb <- as.factor(d$house_tenure)
house_tenure.tb <- summary(house_tenure.tb)
house_tenure.tb <- as.data.frame(house_tenure.tb)
house_tenure.tb$Var1 <- substring(row.names(house_tenure.tb), 1)


nval.df <- c("0") #insert missing values 
nval.df <- as.data.frame(nval.df)
nval.df$house_tenure.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("4")
nval.df <- nval.df[-c(1)]

house_tenure.tb <- rbind(house_tenure.tb, nval.df)

house_tenure.tb$group <- revalue(as.character(house_tenure.tb$Var1), c("1" = "An owner", "2" = "A tenant", "3" = "Resident in a relative or friend's home", "4" = "Resident other than in a relative or friend's home", "5" = "Other", "77" = "I don't know"))

## merge with existing prop table data used for plot above 
plot.house_tenure.tb <- merge(house_tenure, house_tenure.tb, by = "group")
plot.house_tenure.tb <- plot.house_tenure.tb[-c(2, 4, 6)]
plot.house_tenure.tb <- setcolorder(plot.house_tenure.tb, c("group", "house_tenure.tb", "Freq"))

plot.house_tenure.tb$order <- c(2,1,6,5,3,4)
plot.house_tenure.tb <- plot.house_tenure.tb %>% arrange(order)
plot.house_tenure.tb <- plot.house_tenure.tb[-c(4)]
colnames(plot.house_tenure.tb) <- c("Response", "N", "Percentage")

kable(plot.house_tenure.tb) %>% kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> An owner </td>
   <td style="text-align:left;"> 207 </td>
   <td style="text-align:right;"> 62.35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A tenant </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:right;"> 30.42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Resident in a relative or friend's home </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 2.41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Resident other than in a relative or friend's home </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:right;"> 4.82 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>

### In what type of dwelling do you currently live? Is it.

```r
#dwelling_type
 
dwelling_type <-round(prop.table(table(factor(d$dwelling_type, levels=c("1","2", "3", "4", "5", "6", "7", "8","9","77"))))*100,2)
dwelling_type <- as.data.frame(dwelling_type)
dwelling_type$answer <- substring(row.names(dwelling_type), 1)
dwelling_type$answer <- revalue(as.character(dwelling_type$answer), c("1" = "Single detached house", "2" = "Semi-detached house", "3" = "Row house",  "4" = "An apartment (or condo) in a duplex or triplex", "5" = "Apartment (or condo) in building with fewer than 5 storeys", "6" = "Apartment (or condo) in building with more than 5 storeys", "7" = "Mobile home/movable dwelling", "8" = "Senior's home", "9" = "Other", "10" = "N/A"))

dwelling_type$plot <- factor(dwelling_type$answer, dwelling_type$answer)

dwelling_type.plot <- ggplot(dwelling_type, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
  guides(fill = FALSE) +
  scale_fill_manual(values=INTERACTPaletteSet) +
  ylab("Percent of total")

dwelling_type.plot + geom_histogram(aes(x = plot), data = dwelling_type, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-112-1.png)<!-- -->

```r
dwelling_type.tb <- as.factor(d$dwelling_type)
dwelling_type.tb <- summary(dwelling_type.tb)
dwelling_type.tb <- as.data.frame(dwelling_type.tb)
dwelling_type.tb$Var1 <- substring(row.names(dwelling_type.tb), 1)

nval.df <- c("0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$dwelling_type.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("7")
nval.df <- nval.df[-c(1)]

dwelling_type.tb <- rbind(dwelling_type.tb, nval.df)


dwelling_type.tb$answer <- revalue(as.character(dwelling_type.tb$Var1), c("1" = "Single detached house", "2" = "Semi-detached house", "3" = "Row house",  "4" = "An apartment (or condo) in a duplex or triplex", "5" = "Apartment (or condo) in building with fewer than 5 storeys", "6" = "Apartment (or condo) in building with more than 5 storeys", "7" = "Mobile home/movable dwelling", "8" = "Senior's home", "9" = "Other", "77" = "N/A"))

## merge with existing prop table data used for plot above 
plot.dwelling_type.tb <- merge(dwelling_type, dwelling_type.tb, by = "answer")
plot.dwelling_type.tb <- plot.dwelling_type.tb[-c(2, 4, 6)]
plot.dwelling_type.tb <- setcolorder(plot.dwelling_type.tb, c("answer", "dwelling_type.tb", "Freq"))

plot.dwelling_type.tb$order <- c(4,5,6,7,10,9,3,2,8,1)
plot.dwelling_type.tb <- plot.dwelling_type.tb %>% arrange(order)
plot.dwelling_type.tb <- plot.dwelling_type.tb[-c(4)]
colnames(plot.dwelling_type.tb) <- c("Response", "N", "Percentage")


kable(plot.dwelling_type.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Single detached house </td>
   <td style="text-align:left;"> 107 </td>
   <td style="text-align:right;"> 32.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Semi-detached house </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:right;"> 2.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Row house </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:right;"> 5.99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> An apartment (or condo) in a duplex or triplex </td>
   <td style="text-align:left;"> 26 </td>
   <td style="text-align:right;"> 7.78 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Apartment (or condo) in building with fewer than 5 storeys </td>
   <td style="text-align:left;"> 108 </td>
   <td style="text-align:right;"> 32.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Apartment (or condo) in building with more than 5 storeys </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:right;"> 16.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mobile home/movable dwelling </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Senior's home </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> N/A </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
</tbody>
</table>


### When did you move to your current residence?


```r
#residence

residence <- as.integer(format(as.Date(d$residence),"%Y"))
time <- 2019 - residence

ggplot(d, aes(x = time)) + geom_histogram(na.rm=TRUE, binwidth = 1, fill="#76D24A") + xlab("Years since moving to current residence") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-113-1.png)<!-- -->

```r
summary(time)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    5.00   13.00   15.04   21.00   69.00
```

### Were you born in Canada?


```r
#born_can
born_can <- prop.table(table(factor(d$born_can, levels = c("1", "2"))))*100
born_can <- as.data.frame(born_can)
born_can$group <- substring(row.names(born_can), 1)
born_can$group <- revalue(as.character(born_can$group), c("1" = "Yes",  "2" = "No"))

born_can$plot <- factor(born_can$group, born_can$group)

born_can.plot <- ggplot(born_can, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) 

born_can.plot + geom_histogram(aes(x = plot), data = born_can, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-114-1.png)<!-- -->

```r
## Table- 

born_can.tb <- data.frame(Response = c("Yes", "No"),
                          Frequence = as.numeric(table(d$born_can)), 
                          Percentage = round(as.numeric(prop.table(table(d$born_can)))*100,2))


kable(born_can.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")   
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 225 </td>
   <td style="text-align:right;"> 67.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 32.63 </td>
  </tr>
</tbody>
</table>


### When did you move to Canada?


```r
#move_can 

d$move_can[d$move_can==-7] <- NA
ggplot(d, aes(x = d$move_can)) + geom_histogram (na.rm=TRUE, binwidth = 1, fill="#76D24A") + xlab("Year of move to Canada")
```

![](INTERACT_van_W1_HS_files/figure-html/move_can-1.png)<!-- -->

```r
summary(d$move_can)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1942    1965    1980    1981    2000    2018     225
```

### To which ethnic or cultural group(s) do you belong? (Check all that apply)


```r
identity <- round(prop.table(table(factor(eth$group, levels = c("Aboriginal", "Asian", "Black", "Caucasian", "Latin American", "Middle Eastern", "I don't know/Prefer not to answer", "Mixed identity"))))*100,2)


identity <- as.data.frame(identity)
identity$group <- substring(row.names(identity), 1)
identity$group <- factor(identity$group, identity$group)

#getPalette = colorRampPalette(brewer.pal(9, "Paired"))

identity.plot <- ggplot(identity, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 10, angle=0.45, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteSet)  +
      ylab("Percent of total") +
      xlab("")
      
identity.plot + geom_histogram(aes(x = Var1), data = identity, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/group_id-1.png)<!-- -->

```r
#table 
identity.tb <- as.factor(eth$group)
identity.tb <- summary(identity.tb)
identity.tb <- as.data.frame(identity.tb)
identity.tb$Var1 <- substring(row.names(identity.tb), 1)

nval.df <- c("0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$identity.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("Aboriginal")
nval.df <- nval.df[-c(1)]

identity.tb <- rbind(identity.tb, nval.df)

## merge with existing prop table data used for plot above 
plot.identity.tb <- merge(identity, identity.tb, by = "Var1")
plot.identity.tb <- plot.identity.tb %>% arrange(group)
plot.identity.tb <- plot.identity.tb[-c(3)]
plot.identity.tb <- setcolorder(plot.identity.tb, c("Var1", "identity.tb", "Freq"))

colnames(plot.identity.tb) <- c("Response", "N", "Percentage")

kable(plot.identity.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Aboriginal </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Asian </td>
   <td style="text-align:left;"> 30 </td>
   <td style="text-align:right;"> 8.98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Caucasian </td>
   <td style="text-align:left;"> 281 </td>
   <td style="text-align:right;"> 84.13 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Latin American </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 1.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Middle Eastern </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know/Prefer not to answer </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mixed identity </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:right;"> 3.89 </td>
  </tr>
</tbody>
</table>


### Which category best describes your annual household income, taking into account all sources of income?


```r
income <- prop.table(table(factor(d$income, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "77"))))*100
income <- as.data.frame(income)
income$group <- substring(row.names(income), 1)
income$group <- revalue(as.character(income$group), c("1" = "No income", "2" = "$1 to $9,999", "3" = "$10,000 to $14,999", "4" = "$15,000 to $19,999", "5" = "$20,000 to $29,999", "6" = "$30,000 to $39,999", "7" = "$40,000 to $49,999", "8" = "$50,000 to $99,999", "9" = "$100,000 to $149,999", "10" = " $150,000 to $199,999", "11" = "$200,000 or more", "12" = "Don't know/prefer no answer"))

income$plot <- factor(income$group, income$group)

income.plot <- ggplot(income, aes(x = group, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values = rev(INTERACTfade)) +
      ylab("Percent of total") +
      xlab("")

  
income.plot + geom_histogram(aes(x = plot), data = income, stat = "identity")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-115-1.png)<!-- -->

```r
income.tb <- as.factor(d$income)
income.tb <- summary(income.tb)
income.tb <- as.data.frame(income.tb)
income.tb$Var1 <- substring(row.names(income.tb), 1)

nval.df <- c("0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$income.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("1")
nval.df <- nval.df[-c(1)]

income.tb <- rbind(income.tb, nval.df)


income.tb$group <- revalue(as.character(income.tb$Var1), c("1" = "No income", "2" = "$1 to $9,999", "3" = "$10,000 to $14,999", "4" = "$15,000 to $19,999", "5" = "$20,000 to $29,999", "6" = "$30,000 to $39,999", "7" = "$40,000 to $49,999", "8" = "$50,000 to $99,999", "9" = "$100,000 to $149,999", "10" = " $150,000 to $199,999", "11" = "$200,000 or more", "77" = "Don't know/prefer no answer"))

## merge with existing prop table data used for plot above 
plot.income.tb <- merge(income, income.tb, by = "group")
plot.income.tb <- plot.income.tb[-c(2, 4, 6)]
plot.income.tb <- setcolorder(plot.income.tb, c("group", "income.tb", "Freq"))

plot.income.tb$order <- c(11,2,3,10,4,5,6,7,8,9,12,1)
plot.income.tb <- plot.income.tb %>% arrange(order)
plot.income.tb <- plot.income.tb[-c(4)]
colnames(plot.income.tb) <- c("Response", "N", "Percentage")


kable(plot.income.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No income </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $1 to $9,999 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 1.1976048 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $10,000 to $14,999 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5988024 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $15,000 to $19,999 </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 2.3952096 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $20,000 to $29,999 </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:right;"> 3.8922156 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $200,000 or more </td>
   <td style="text-align:left;"> 49 </td>
   <td style="text-align:right;"> 14.6706587 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $30,000 to $39,999 </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:right;"> 2.9940120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $40,000 to $49,999 </td>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:right;"> 5.0898204 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $50,000 to $99,999 </td>
   <td style="text-align:left;"> 77 </td>
   <td style="text-align:right;"> 23.0538922 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $100,000 to $149,999 </td>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:right;"> 20.9580838 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $150,000 to $199,999 </td>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:right;"> 8.0838323 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Don't know/prefer no answer </td>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:right;"> 17.0658683 </td>
  </tr>
</tbody>
</table>


### To what extent does this annual household income allow you to satisfy your household's needs?


```r
#income_needs
income_needs <- round(prop.table(table(factor(d$income_needs, levels = c("1", "2", "3", "4", "77"))))*100,2)
income_needs <- as.data.frame(income_needs)
income_needs$group <- substring(row.names(income_needs), 1)
income_needs$group <- revalue(as.character(income_needs$group), c("1" = "Very well", "2" = "Well", "3" = "Not so well", "4" = "Not at all", "5" = "Don't know/prefer no answer"))

income_needs$group <- factor(income_needs$group, income_needs$group)

income_needs.plot <- ggplot(income_needs, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTshortfade) +
      ylab("Percent of total") +
      xlab("")
      
income_needs.plot + geom_histogram(aes(x = group), data = income_needs, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/income_needs-1.png)<!-- -->

```r
## Table- 
income_needs.tb <- data.frame(Response = c("Very well", "Well", "Not so well", "Not at all", "Don't know/prefer no answer"),
           Frequence = as.numeric(table(d$income_needs)), Percentage = round(as.numeric(prop.table(table(d$income_needs)))*100,2))

kable(income_needs.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Very well </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 39.82 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Well </td>
   <td style="text-align:right;"> 141 </td>
   <td style="text-align:right;"> 42.22 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not so well </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 14.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Not at all </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 0.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Don't know/prefer no answer </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 2.69 </td>
  </tr>
</tbody>
</table>

### What is your highest education level?


```r
#education
education <- prop.table(table(factor(d$education, levels = c("1", "2", "3", "4","5", "77"))))*100
education <- as.data.frame(education)
education$group <- substring(row.names(education), 1)
education$group <- revalue(as.character(education$group), c("1" = "Primary/Elementary school", "2" = "Secondary school", "3" = "Trade/Technical school or college diploma", "4" = "University degree", "5" = "Graduate degree", "6" ="I don't know/Prefer not to answer"))

education$group <- factor(education$group, education$group)

education.plot <- ggplot(education, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=rev(INTERACTshortfade))  +
      ylab("Percent of total") +
      xlab("")
      
education.plot + geom_histogram(aes(x = group), data = education, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/education-1.png)<!-- -->

```r
#table 
education.tb <- as.factor(d$education)
education.tb <- summary(education.tb)
education.tb <- as.data.frame(education.tb)
education.tb$Var1 <- substring(row.names(education.tb), 1)

nval.df <- c("0") #insert missing values
nval.df <- as.data.frame(nval.df)
nval.df$education.tb <- as.factor(nval.df$nval.df)
nval.df$Var1 <- c("1")
nval.df <- nval.df[-c(1)]

education.tb <- rbind(education.tb, nval.df)

education.tb$group <- revalue(as.character(education.tb$Var1), c("1" = "Primary/Elementary school", "2" = "Secondary school", "3" = "Trade/Technical school or college diploma", "4" = "University degree", "5" = "Graduate degree", "77" ="I don't know/Prefer not to answer"))

## merge with existing prop table data used for plot above 
plot.education.tb <- merge(education, education.tb, by = "group")
plot.education.tb <- plot.education.tb %>% arrange(Var1.x)
plot.education.tb <- plot.education.tb[-c(2,5)]
plot.education.tb <- setcolorder(plot.education.tb, c("group", "education.tb", "Freq"))
colnames(plot.education.tb) <- c("Response", "N", "Percentage")

kable(plot.education.tb) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:left;"> N </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Primary/Elementary school </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0.000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Secondary school </td>
   <td style="text-align:left;"> 28 </td>
   <td style="text-align:right;"> 8.383233 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Trade/Technical school or college diploma </td>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:right;"> 13.772455 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> University degree </td>
   <td style="text-align:left;"> 117 </td>
   <td style="text-align:right;"> 35.029940 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Graduate degree </td>
   <td style="text-align:left;"> 138 </td>
   <td style="text-align:right;"> 41.317365 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I don't know/Prefer not to answer </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 1.497006 </td>
  </tr>
</tbody>
</table>


### What is your current employment status?


```r
#employment
employment <- prop.table(table(factor(d$employment, levels = c("1", "2", "3", "4","5", "6"))))*100
employment <- as.data.frame(employment)
employment$group <- substring(row.names(employment), 1)
employment$group <- revalue(as.character(employment$group), c("1" = "Retired and not working", "2" = "Employed full-time", "3" = "Employed part-time", "4" = "Unemployed and looking for work", "5" = "Unemployed and not looking for work", "6" ="Other"))

employment$group <- factor(employment$group, employment$group)

employment.plot <- ggplot(employment, aes(x = group, y = Freq, fill = group)) + theme(axis.text.x  = element_text(size= 12, angle=0, vjust=.6))  + scale_x_discrete(labels = function(plot) str_wrap(plot, width = 10)) +
      guides(fill = FALSE) +
      scale_fill_manual(values=INTERACTPaletteSet)  +
      ylab("Percent of total") +
      xlab("")
      
employment.plot + geom_histogram(aes(x = group), data = employment, stat = "identity") 
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-116-1.png)<!-- -->

```r
employment.tb <- data.frame(Response = c(  "Retired and not working",  "Employed full-time",  "Employed part-time", "Unemployed and looking for work", "Unemployed and not looking for work", "Other"),
           Frequence = as.numeric(table(d$employment)), 
           Percentage = round(as.numeric(prop.table(table(d$employment)))*100,2))

kable(employment.tb) %>%  kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> Frequence </th>
   <th style="text-align:right;"> Percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Retired and not working </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 28.74 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Employed full-time </td>
   <td style="text-align:right;"> 134 </td>
   <td style="text-align:right;"> 40.12 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Employed part-time </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 14.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unemployed and looking for work </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 2.40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Unemployed and not looking for work </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.10 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 11.68 </td>
  </tr>
</tbody>
</table>


### Do you use a mobility aid when you walk?


```r
#aid

aid<- prop.table(table(factor(d$aid, levels = c("1", "2"))))*100
aid<- as.data.frame(aid)
aid$answer <- substring(row.names(aid), 1)
aid$answer <- revalue(as.character(aid$answer), c("1" = "Yes",  "2" = "No"))

aid$plot <- factor(aid$answer, aid$answer)

aid.plot <- ggplot(aid, aes(x = answer, y = Freq, fill = plot)) + theme(axis.text.x  = element_text(angle=90, vjust=.6)) #order responses as in t5

aid.plot + geom_bar(aes(x = plot), data = aid, stat = "identity") +
      guides(fill = FALSE) +
      scale_fill_manual(values = INTERACTPaletteYN) +
      ylab("Percent of total") +
      xlab("Response")
```

![](INTERACT_van_W1_HS_files/figure-html/unnamed-chunk-117-1.png)<!-- -->

```r
aid.tb <- as.factor(d$aid)
aid.tb <- summary(aid.tb)
aid.tb <- as.data.frame(aid.tb)
aid.tb$Var1 <- substring(row.names(aid.tb), 1)
aid.tb$answer <- revalue(as.character(aid.tb$Var1), c("1" = "Yes", "2" = "No"))
plot.aid<- merge(aid, aid.tb, by = "answer")
plot.aid<- plot.aid[-c(2, 4, 6)]
plot.aid<- setcolorder(plot.aid, c("answer", "aid.tb", "Freq"))
plot.aid$order <- c(2, 1)
plot.aid<- plot.aid%>% arrange(order)
plot.aid<- plot.aid[-c(4)]
colnames(plot.aid) <- c("Response", "N", "Proportion")

kable(plot.aid) %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Response </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Proportion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2.095808 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 327 </td>
   <td style="text-align:right;"> 97.904192 </td>
  </tr>
</tbody>
</table>

```r
Type <- c("Cane", "Walker", "Guide dog", "Poles")
Count <- c("4", "1", "1", "1")

summaryaid_type <- data.frame(Type, Count)
kable(summaryaid_type)  %>%   kable_styling(bootstrap_options = "striped", full_width = T, position = "left") 
```

<table class="table table-striped" style="">
 <thead>
  <tr>
   <th style="text-align:left;"> Type </th>
   <th style="text-align:left;"> Count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cane </td>
   <td style="text-align:left;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Walker </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Guide dog </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Poles </td>
   <td style="text-align:left;"> 1 </td>
  </tr>
</tbody>
</table>




