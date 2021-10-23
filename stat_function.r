# @Author Tony Hsu                                                             
#Statistical Analysis Template               
# @version: v1.0.3                                                             
# @modified date: 20211023                                                    

#' @Title Create a boxplot 
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH_, output path
#' @param Y_type_, order of the label
#' @param X_cat.len_, number of categorical variable except y
#' @param X_cont.normalized_, logical, normalized or not
#' @param outlier.remove_, logical remove outlier or not
#' @param x.axis.label.rotate_, rotate label of x.axis or not
#' @param flip_, logical, transform x-y axis or not
#' @param PID.omit, logical, 25_hsa-miR-xxx replace to hsa-miR-xxx
#' @param adv.violin, logical, TRUE = use violin + boxplot, FALSE = boxplot only 
#' @param test, logical, TRUE = only produce one graph
#' @param x.axis.label.rotate_, logical, rotate label of x.axis if TRUE
#' @param width_, pdf width
#' @param height_, pdf height
#'
#' @export
gplot.box <- function( DATA_, OUTPUT.PATH_, Y_type_ = "default", X_cat.len_ = 0, X_cont.normalized_ = FALSE, 
                       PID.omit = FALSE, adv.violin = TRUE, test = FALSE, x.axis.label.rotate_ = FALSE, 
                       flip_ = FALSE, width_ = 13.66*0.45, height_ = 7.68*1.5*0.7) {
  
  #Interpret column index of variable
  ID.idx = 1
  Y.idx = 2
  X_cont.idx = (2 + X_cat.len_ + 1) : ncol(DATA_)
  X_cat.idx = (1:ncol(DATA_))[c(-c(1,2), -X_cont.idx)]
  
  #Interpret names of variable
  ID.names = names(DATA_)[1]
  Y.names = names(DATA_)[2]
  X_cat.names = if( length(X_cat.idx) == 0 ) NULL else names(DATA_)[c(-c(1,2),-X_cont.idx)]
  X_cont.names = names(DATA_)[X_cont.idx]
  if(PID.omit == TRUE) names(DATA_) = gsub("^[[:digit:]].*_", "", names(DATA_))
  
  
  #-------------------------------Normalization if required----------------------------------------------------#
  if( X_cont.normalized_ ) DATA_ = X_normalization(DATA_, ID.idx, Y.idx, X_cat.idx, X_cont.idx)
  #------------------------------------------------------------------------------------------------------------#
  
  #Check if X exist categorical variable. l=1 means only plot for y, l>1 means there exists categorical x
  l = if( is.null(X_cat.names)) 1 else 1+length(X_cat.names)
  
  cat.names = c(Y.names, X_cat.names)
  
  pdf(file = paste(OUTPUT.PATH_,"/","Boxplot",".pdf", sep = ""), width = width_, height = height_)
  
  #Usually test height and width for plots, simplify the loop
  if(test == TRUE) X_cont.names = X_cont.names[1]
  
  #loop: j selects y or categorical variables x, and i selects a single continuous variable x each time 
  for( j in 1 : l ){
    
    #----------data processing : gathering x value----------------------------------------------------------------------------#
    data.gather = DATA_ %>% 
      select( ID.names, cat.names[j], X_cont.names ) %>% 
      gather( ., key = "variable", value = "value", -all_of(ID.names), -all_of(cat.names[j]) )
    #-------------------------------------------------------------------------------------------------------------------------#
    
    for( i in 1 : length(X_cont.names) ){
      
      X = X_cont.names[i]
      
      data.gather.filted = data.gather %>% 
        transform(value = as.numeric(value)) %>% 
        filter( variable == X )
      
      if( j == 1 && Y_type_ != "default") data.gather.filted[2] = factor( data.gather.filted[2] %>% unlist, levels = Y_type_ ) 
      
      #compute outlier for each group
      data.cat.group = 
        data.gather.filted %>% 
        group_by_at(2) %>%  # index(2) = category
        mutate(outlier = ifelse(is_outlier(value), value, as.numeric(NA) ), outlier_sample = !!as.symbol(ID.names) )
      
      data.cat.group$outlier_sample[which(is.na(data.cat.group$outlier))] = as.numeric(NA)
      
      ##start to set plots
      
      if(adv.violin == TRUE){
        box.plot = gboxvioplot(data.cat.group_ = data.cat.group, cat.names_ = cat.names[j], X_ = X)
      }else{
        box.plot = gboxplot(data.cat.group_ = data.cat.group, cat.names_ = cat.names[j], X_ = X)
      }
      
      #if rotating label of x.axis is true
      if( x.axis.label.rotate_ ){
        
        box.plot = 
          box.plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        
      }
      
      if( flip_ ){
        
        box.plot = 
          box.plot + coord_flip() + rremove("y.grid") + stat_compare_means( size = 6, label.x.npc = 1, label.y.npc = 0)
        
      }else{
        
        box.plot = 
          box.plot + rremove("x.grid") #+ stat_compare_means( size = 6, label.x.npc = "right", label.y.npc = 0.5)#stat_compare_means( size = 6) 
        
      }
      
      plot(box.plot)
      
      }
  
  }
  dev.off()
  
  print(paste0("----------------- Finish create Boxplot -----------------"))
  
}



#' Title
#'
#' @param data.cat.group_ 
#' @param cat.names_ 
#' @param X_ 
#'
#' @return
#' @export
#'
#' @examples
gboxvioplot <- function( data.cat.group_, cat.names_, X_){
  
  box.plot = 
    ggbetweenstats(data = data.cat.group_, 
                   x = !!as.symbol(cat.names_), 
                   y = value, 
                   messages = FALSE) +
    labs( title = X_ ) +
    theme( 
      plot.title = element_text(size = 30, vjust = 1.2, hjust = 0.5),
      #axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  return(box.plot)
}


gboxplot <- function( data.cat.group_, cat.names_, X_ ) {
  st = data.cat.group_$value %>% boxplot.stats(.) #5 value in boxplot, min, Q1, Q2, Q3, max
  
  box.plot = 
    ggboxplot(data.cat.group_, x = cat.names_, y = "value" , fill = cat.names_ )+
    #geom_text(aes(label = outlier_sample), na.rm=TRUE, nudge_x = 0.1, nudge_y = 0.03, size = 5)+
    labs( title = X_ ) +
    stat_n_text(size = 8)+
    theme_ipsum() +
    #geom_jitter(alpha = 0.3)+
    theme( 
      plot.title = element_text(size = 30, vjust = 1.2, hjust = 0.5),
      #axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "top",
      legend.key.size = unit(1, "cm"),
      legend.direction = "horizontal",
      legend.title = element_blank(),
      #legend.title = element_text(color = "black", size = 16),
      legend.background = element_rect(colour = "white"),
      legend.text = element_text( size = 15 )
      ) +
    stat_compare_means( method = "t.test")
    
  # box.plot = box.plot +
  #   coord_cartesian( ylim = c(st$stats[1],st$stats[5]) )

  return(box.plot)
  
}




X_normalization <- function( DATA_, ID.idx_, Y.idx_, X_cat.idx_, X_cont.idx_){
  
  X.cont = DATA_[,X_cont.idx_] %>% as.matrix
  class(X.cont) = "numeric"
  
  X.cont = X.cont  %>% normalize.quantiles.use.target(., normalize.quantiles.determine.target(.)) %>% 
    as_tibble %>% set_colnames(., colnames(DATA_)[X_cont.idx_])
  
  data = data[,c(ID.idx, Y.idx_, X_cat.idx_)] %>% mutate( X.cont )
  
  return(data)
  
}





is_outlier <- function(x) {
  
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))

} 



#' Generate a curve plot
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH, output path
#' @param comp, when NA occurs, values should plug in
#' @param x.axis.label.rotate_, rotating label of x axis, TRUE = vertical 
#'
#' @export
gplot.curve <- function( DATA_, OUTPUT.PATH_, comp = 0, x.axis.label.rotate_ ) {
  
  #Interpret index of variable
  ID.idx = 1 
  Y.idx = 2 
  X.idx = 3:ncol(DATA_)
  
  #Interpret names of variable
  ID.names = names(DATA_)[ID.idx]
  Y.names = names(DATA_)[Y.idx]
  X.names = names(DATA_)[X.idx]
  
  #List out IDs
  ID.list = DATA_[ID.names] %>% unlist %>% sort %>% unique
  
  for(i in 1 : length(ID.list) ){
    
    #----------data processing: gathering x and values-------------------------#
    gath.data = 
      DATA_ %>% gather(., key = "X", value = "value", -ID.names, -Y.names) %>%
      transform(
        X = factor(X, levels = c(names(DATA_)[-c(1,2)])),
        value = as.numeric(value)
      ) %>%
      filter( !!as.symbol(ID.names) == ID.list[i] ) %>%
        group_by_(ID.names, Y.names, "X") %>% 
          summarise(Mean = mean(value, na.rm = T)) %>%
            mutate(Mean = ifelse(is.na(Mean),comp,Mean))
    #--------------------------------------------------------------------------#
    ##start to set output plots
    curve.plot = 
      ggplot( gath.data, aes_string(x = "X", y = "Mean", group = Y.names, shape = Y.names))+
      geom_line(aes_string( color = Y.names, linetype = Y.names ), size = 1.5)+
      #"blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
      rremove("x.grid")+
      labs( title = ID.list[i] )+
      geom_jitter( size = 4, position = position_jitter(0.01,0.01))+
      theme_ipsum()+
      theme(plot.title = element_text(size = 50, vjust = 1, hjust = 0.5),
            axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
            axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = c(0.5,1),
            legend.direction = "horizontal",
            legend.title = element_text(color = "deepskyblue4", size = 25),
            legend.background = element_rect(fill = "wheat1"),
            legend.text = element_text( size = 25 ),
            legend.key.width = unit(1.5,"cm")
            )
      
    if(x.axis.label.rotate_){
      curve.plot = 
        curve.plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    #open pdf
    pdf(file = paste(OUTPUT.PATH_, "/", "curve_", i, ".pdf", sep = ""), width=13.66, height=7.68*1.5)
    plot(curve.plot)
    dev.off()
    
  }
}



#' Generate a bar plot
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH, output path
#' @param comp, when NA occurs, values should plug in 
#' @param x.axis.label.rotate_, rotate x.axis label or not
#' @param flip_, transform x-y axis
#'
#'
#' @export
gplot.bar <- function( DATA_, OUTPUT.PATH_, comp = 0, x.axis.label.rotate_, flip_ ) { 
  
  #Interpret index of variable
  ID.idx = 1; Y.idx = 2; X.idx = 3 : ncol(DATA_)
  
  #Interpret names of variable
  ID.names = names(DATA_)[ID.idx]
  Y.names = names(DATA_)[Y.idx]
  X.names = names(DATA_)[X.idx]
  
  #List out IDs
  ID.list = DATA_[ID.names] %>% unlist %>% sort %>% unique
  
  for(i in 1 : length(ID.list) ){
    
    #------------data processing: gathering x and values-------------#
    gath.data = 
      DATA_ %>% gather(., key = "X", value = "value", -ID.names, -Y.names) %>%
      transform(
        X = factor(X, levels = c(names(DATA_)[-c(1,2)])),
        value = as.numeric(value)
      ) %>%
      filter( !!as.symbol(ID.names) == ID.list[i] ) %>%
      group_by_(ID.names, Y.names,"X") %>% 
      summarise(Mean = mean(value, na.rm = T)) %>%
      mutate(Mean = ifelse(is.na(Mean),comp,Mean))
    #-------------------------------------------------------------------#
    ##start to set output plots
    bar.plot = 
      ggbarplot( gath.data, x = "X", y = "Mean", fill = Y.names, color = Y.names, position = position_dodge(0.75), width = 0.65 )+
      theme_ipsum()+
      theme(plot.title = element_text(size = 50, vjust = 0.5, hjust = 0.5),
            axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
            axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #legend.position = c(0.5,0.95),
            #legend.direction = "horizontal",
            legend.position = "top",
            legend.title = element_text(color = "deepskyblue4", size = 20),
            legend.background = element_rect(fill = "wheat1"),
            legend.text = element_text( size = 20 )
            )+
      labs( title = ID.list[i] )
      
    if(flip_){
      bar.plot =
        bar.plot + coord_flip() + rremove("y.grid")
    }else{
      bar.plot = 
        bar.plot + rremove("x.grid")
    }
    
    
    if( x.axis.label.rotate_ ){
      bar.plot = 
        bar.plot + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    #open pdf
    pdf(file = paste(OUTPUT.PATH_,"/","bar_",i, ".pdf", sep = ""), width=13.66, height=7.68*1.5)
    print(bar.plot)
    dev.off()
    
  }
}





#' Generate an excel file contains descriptive statistics of all variable x
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH, output path
#'
#' @export
descript_stat.table <- function( DATA_, OUTPUT.PATH_ ) {
  
  #read data table
  
  if(!is.null(DATA_$class)) DATA_ %<>% select(-class)
  
  gath.data = 
    DATA_[,-1] %>% gather(., key = "miRNA", value = "value", -names(DATA_)[2]) %>% 
      transform( value = as.numeric(value)) %>%
        group_by_(names(DATA_)[2], "miRNA") %>% # Group by KEY
          rename(MEAN = value) %>%
            summarise( 
              
              N = sum( !is.na(MEAN) ),
              Min = min( MEAN, na.rm = T ),
              Q1 = quantile( MEAN, 0.25, na.rm = T ),
              Med = median( MEAN,na.rm = T ),
              Mean = mean( MEAN, na.rm = T ),
              Q3 = quantile( MEAN, 0.75, na.rm = T ),
              Max = max( MEAN, na.rm = T ),
              IQR = ( Q3 - Q1 ),
              Range = ( Max - Min ),
              Std = sd( MEAN, na.rm = T ),
              CV = ( Std/Mean * 100 )
              
            ) %>% 
            
    mutate_if(is.numeric, round, digits = 4) %>%
    
    # replace NA as 0
    mutate_all(funs(ifelse(is.na(.), 0, .)))
  
  gath.data$miRNA = gath.data$miRNA #%>% gsub("^[[:digit:]].*_", "", .)
  
  type = unique(gath.data$type)
  
  for(i in 1 : length(type)){
    
    output = gath.data %>% filter(type == type[i])
    write.csv(output, paste0(OUTPUT.PATH_,"/", type[i], "_Descriptive_statistics.csv"), row.names = F)
    
  }
  
  cat(paste0("file ----", "Descriptive_statistics ", "created successfully"))
  
}



#' Generate a test statistics table of x which grouped by y
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH, output path
#'
#'
#' @export
test_stat.table = function( DATA_, OUTPUT.PATH_ ) {
  
  if(!is.null(DATA_$class)) DATA_ %<>% select(-class)
  #write a temporary file
  write.csv(DATA_, file = './temp.csv',row.names = F)
  data = read.csv('./temp.csv', header = T)
  ID.names = names(data)[1]
  Y.names = names(data)[2]
  X.name = names(data)[-c(1,2)]
  
  gath.data = 
    data %>% gather(., key = "KEY", value = "value" , -Y.names, -ID.names)
  
  name = X.name %>% unique 
  
  Tukeytable = 
    lapply(name, function(x) gath.data %>% filter(KEY == x) %>% .[complete.cases(.), ] ) %>% 
    lapply(function(y) aov( unlist(y["value"]) ~ unlist(y[Y.names]) ) ) %>% 
    lapply(function(z) TukeyHSD(z) ) 
  
  ttest.table = lapply(1:length(X.name) , function(x) Tukeytable[[x]][[1]][,4]) %>% data.frame()
  if( length(colnames(ttest.table)) != length(name) ) stop("Invalid statistical testing") 
  colnames(ttest.table) = name
  
  gath.ttest.table = 
    gather(ttest.table %>% rownames_to_column("Comparison"), "KEY","Pvalue(t-test)",-"Comparison")
  
  size.tmp = 
    lapply(X.name, function(x) {gath.data %>% filter(KEY == x) %>% .[complete.cases(.), ]}) %>% 
    lapply(function(x) { x %>% group_by_(Y.names) %>% summarise(n = n())}) 
  
  size.mat = 
    lapply(1:length(name), function(x) size.tmp[[x]] %>% column_to_rownames(var = Y.names)) %>% do.call(cbind, .)
  
  colnames(size.mat) = name
  size.mat = t(size.mat) %>% as.data.frame %>% rownames_to_column(var = "KEY")
  
  #Creat anova test between several types of y
  anova = 
    gath.data %>% transform(value = as.numeric(value)) %>% group_by(KEY) %>% 
    summarise( 
      `P(anova_test)` = summary(aov( value ~ factor(!!as.symbol(Y.names))))[[1]][5] %>% unlist %>% as.numeric %>% .[!is.na(.)] 
    )
  #Creat Kruskal-Wallis test between several types of y
  ks.test = 
    gath.data %>% transform(value = as.numeric(value)) %>% group_by(KEY) %>% 
    summarise(
      `P(kruskal_test)` = kruskal.test( value ~ factor(!!as.symbol(Y.names)))$p.value 
    ) %>% 
    mutate_if(is.numeric, round, digits = 6)
  
  merg.table = size.mat %>% 
    full_join(anova, by = "KEY") %>% 
    full_join(ks.test, by = "KEY")
  
  write_xlsx(list(sample_size = merg.table, one_by_one = gath.ttest.table), path = paste0(OUTPUT.PATH_,"/","test_statistics.xlsx"))
  #remove temporary file
  file.remove('./temp.csv')
  cat(paste0("file ----", "test_statistics ", "created successfully","\n"))
  
}



#' Title
#'
#' @param DATA_ 
#'
#' @return
#' @export
#'
#' @examples
#' 
t_test_table = function(DATA_){
  
  ga = DATA_ %>% 
    #set_colnames(gsub("[[:digit:]].*_", "", colnames(.))) %>% 
    select(-Sample_ID) %>%  
    gather(key = "miRNA", value = "Cq", -as.symbol(names(DATA_)[2])) 
  
  #evaluate mean
  mean_table = ga %>% 
    group_by_(names(ga)[1],names(ga)[2]) %>% 
    summarise(MEAN = round(mean(Cq, na.rm = T), 3)) %>% 
    spread(as.symbol(names(DATA_)[2]), MEAN) %>%
    mutate_(a = names(.)[2], b = names(.)[3]) %>% 
    mutate(`FC(log2 ratio)` = round(a-b, 3)) %>% 
    select(-c(a,b)) %>% 
    rename( !! paste0("Mean_", names(.)[2]) := names(.)[2],
            !! paste0("Mean_", names(.)[3]) := names(.)[3])
  
  #evaluate sample size
  count_table = ga %>% 
    na.omit %>% 
    group_by_(names(ga)[1],names(ga)[2]) %>% 
    summarise(n = n()) %>% 
    spread(as.symbol(names(DATA_)[2]), n) %>% 
    rename( !! paste0("N_", names(.)[2]) := names(.)[2],
            !! paste0("N_", names(.)[3]) := names(.)[3])
  
  #evaluate p-value
  p_table = ga %>% group_by_(names(.)[1], names(.)[2]) %>% 
    summarise(Cq = list(Cq)) %>% 
    spread(as.symbol(names(DATA_)[2]), Cq) %>% #.[1:5,] %>% 
    apply(., 1, function(df) {
      dfname1 = names(df)[2]
      dfname2 = names(df)[3]
      
      tryCatch({
        
        data.frame(
          miRNA = df$miRNA,
          p_value = round(t.test(eval(parse(text = paste0("df$",dfname1))), 
                                 eval(parse(text = paste0("df$",dfname2))))$p.value, 4)
        )
        
      },error = function(e) 
        
        return(
          data.frame( miRNA = df$miRNA, p_value = NA )
        )
      
      ) # end tryCatch
      
    }) %>% # end apply 
    do.call(rbind.data.frame, .)
  
  f.table = p_table %>% 
    left_join(count_table, by = "miRNA") %>% 
    left_join(mean_table, by = "miRNA") %>%
    column_to_rownames("miRNA") %>% 
    set_rownames(gsub("[[:digit:]].*_", "", row.names(.))) %>%
    rownames_to_column("miRNA") %>% 
    relocate(p_value, .after = last_col()) %>% 
    rename(`p-value` = p_value)
  
  return(f.table)
  
}

  # mutate(yy = length(unique(unlist(after))), xx = length(unique(unlist(before)))) %>% 
  # select(-after, -before) %>% get.datatable()

# %>%  mutate(ttest = t.test(unlist(after), unlist(before))$p.value)

#' Generate PCA plot and cluster under PCA plot
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH, output path
#' @param scale_, normalized the value or not
#' @param labelsize_, setting labelsize
#' @param loading.exist_, display loadings on the plot or not
#' @param cluster_, set the number of cluster
#'
#'
#' @export
PCA.cluster.plot <- function( DATA_, OUTPUT.PATH_, scale_, labelsize_, loading.exist_ = TRUE, cluster_ ) {
  
  
  #Interpret X matrix and replace NAs by mean
  x.mat = DATA_[-c(1,2)] %>% as.data.frame() %>% sapply(., as.numeric)
  comp.x.mat = apply(x.mat, 2, mean_replace)
  
  #normalized variable x
  scale.x.mat = apply(comp.x.mat, 2, normalized.scale)
  clus.mat = if( scale_ ) scale.x.mat else comp.x.mat
  
  #Principal component
  pri = prcomp(comp.x.mat, scale. = scale_, loadings = loading.exist_)
  
  PCAplot = 
    autoplot(pri, data = DATA_, colour = names(DATA_)[2], size = 4, loadings = loading.exist_, loadings.label = loading.exist_, 
             loadings.colour = 'navyblue', loadings.label.size = labelsize_ )+
          theme_bw()+
          theme(
            legend.position = "top",
            axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
            axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
            legend.background = element_rect(fill = "wheat1"),
            legend.text = element_text( size = 20 ),
            axis.title.x = element_text( size = 30 ),
            axis.title.y = element_text( size = 30 ),
            legend.title = element_text( color = "deepskyblue4", size = 20)
            )
  
  #open pdf
  pdf(file = paste(OUTPUT.PATH_, "/", "PCA", ".pdf", sep = ""), width=13.66, height=7.68*1.5)
  print(PCAplot)
  dev.off()
  
  
  CLUSTERplot = 
    autoplot(pam(clus.mat, cluster_), label = TRUE, label.size = labelsize_, shape = FALSE, 
             frame = TRUE, frame.type = 'norm')+
    theme_bw()+
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      axis.text.y = element_text(size = 20, vjust = 0.5, hjust = 0.5),
      legend.background = element_rect(fill = "wheat1"),
      legend.text = element_text( size = 20 ),
      axis.title.x = element_text( size = 30 ),
      axis.title.y = element_text( size = 30 ),
      legend.title = element_text( color = "deepskyblue4", size = 20)
      )
  
  #open pdf
  pdf(file = paste(OUTPUT.PATH_, "/", "Cluster", ".pdf", sep = ""), width=13.66, height=7.68*1.5)
  print(CLUSTERplot)
  dev.off()
  
}


PCA2 <- function(data){
  pri = prcomp(data, scale = T)
  
  fviz_pca_ind(pri,axes = c(1,2), label = "none", addEllipses = T, 
               ellipse.level = 0.7, palette = c("red", "#00a800"),
               select.ind = list(name = spl.cand[!spl.cand %in% c()]), 
               habillage = oncodata %>% .$type, title = "", xlab = "PC1", ylab = "PC2", xlim = c(-7.5,4))+
    theme(legend.title = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal" )
}



mean_replace <- function(vec) {
  
  m <- mean(vec, na.rm = TRUE)
  vec[is.na(vec)] <- m
  return(vec)
  
}



normalized.scale <- function(vec) {
  
  m = mean(vec, na.rm = TRUE)
  std = sd(vec, na.rm = TRUE)
  return( sapply( vec, function(x) (x-m)/std)  )
  
}



#' Generate hierarchical heat map
#'
#' @param DATA_, import data
#' @param OUTPUT.PATH, output path
#' 
#' 
#' @export
hier_clustering <- function( DATA_, OUTPUT.PATH_ ) {
  
  x.mat = DATA_ %>% #column_to_rownames("Sample_ID") %>% 
    as.data.frame() %>% 
    set_colnames(gsub("[[:digit:]].*_", "", colnames(.)))%>% 
    mutate_all(funs(ifelse(is.na(.), 40, .)))
  
  heatmaply(
      x.mat, 
      xlab = "",
      ylab = "",
      column_text_angle = 90,
      scale = "none",
      main = "",
      file = paste0(OUTPUT.PATH_,"/heatmap.html")
    )
  
}



stat.requirement <- function() {
  options(warn = -1)
  
  list.packages <- c('tidyverse','magrittr','ggpubr','ggplot','dplyr','readxl', 'ggplot2',
                     'writexl','EnvStats','hrbrthemes','extrafont','arrangements', 'readxl',
                     'ggfortify','cluster','gplots','preprocessCore','heatmaply','ggstatsplot','ggrepel')
  new.packages <- list.packages[!(list.packages %in% installed.packages()[,'Package'])]
  if(length(new.packages)) {
    cat('-- Installing packages,',as.character.Date(Sys.time()),'--\n')
    install.packages(new.packages, quiet = T, repos = "http://cran.us.r-project.org")
  }
  
  invisible(sapply(list.packages, require, character.only = T))
  
}
