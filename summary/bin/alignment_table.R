#####alignment
alignment_table<-function(trim_filename,align_filename,out.path,out.file="alignment.summary_table.tex"){
  Fix.before<-paste(
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{Alignment Summary}",
    "\\label{tab:alignment}",
    "\\medskip",
    "\\tiny",
    "\\begin{tabular}{@{} lS[table-format=8.0]S[table-format=9.0]",
      "S[table-format=1.0]S[table-format=8.0]",
      "S[table-format=9.0]@{}}",
    "\\toprule",
    "& \\multicolumn{3}{c}{\\textsc{\\scriptsize Quality Trimmed Reads}} & \\multicolumn{2}{c}{\\textsc{\\scriptsize Alignment to Reference Genome}} \\\\",
    "\\cmidrule(lr{0.75em}){2-4}",
    "\\cmidrule(lr{0.75em}){5-6}",
    "&{\\scriptsize No. Reads} & \\splitcell{ \\scriptsize Base Pairs \\\\ \\scriptsize(bp)}   & \\splitcell{\\scriptsize Length \\\\ \\scriptsize (bp)} &  \\splitcell{\\scriptsize Alignments\\\\ \\scriptsize ($\\geqslant$1 Location)}  &   \\splitcell{\\scriptsize Unique Alignments \\\\ \\scriptsize (Single Location)}\\\\ ",
    "\\midrule",
    sep="\n"
  )
  cat(Fix.before,file=paste(out.path,out.file,sep="/"),fill=T,append=T)
  
  
  ####main part
  in_trim_table<-read.delim(trim_filename,header = F,comment.char="!")
  sub_trim_table<-in_trim_table[((which(in_trim_table[,1]=="# TOTALS")+1):nrow(in_trim_table)),c(1,5,7,9)]
  #print(sub_trim_table)
  sub_trim_table[,2]<-as.character(sub_trim_table[,2])
  
  in_align_table<-read.delim(align_filename,header = F,comment.char="!")
  total_align<-in_align_table[((which(in_align_table[,1]=="# REDUNDANT ALIGNEMNTS (ALL POSSIBLE)")+2):(which(in_align_table[,1]=="# UNIQUE ALIGNEMNTS (SINGLE LOCI)")-1)),4]
  uniq_align<-in_align_table[((which(in_align_table[,1]=="# UNIQUE ALIGNEMNTS (SINGLE LOCI)")+2):nrow(in_align_table)),4]
  total_align<-as.character(total_align)
  uniq_align<-as.character(uniq_align)
  
  total_align_cal<-gsub("2 x ","",total_align)
  total_align_cal<-gsub(",","",total_align_cal)
  total_align_cal<-as.numeric(total_align_cal)
  #print(total_align_cal)
  
  uniq_align_cal<-gsub("2 x ","",uniq_align)
  uniq_align_cal<-gsub(",","",uniq_align_cal)
  uniq_align_cal<-as.numeric(uniq_align_cal)
  
  no.reads<-gsub("2 x ","",sub_trim_table[,2])
  no.reads<-gsub(",","",no.reads)
  no.reads<-as.numeric(no.reads)
  
  dat<-as.data.frame(cbind(no.reads,total_align_cal,uniq_align_cal))
  dat$total_reads<-paste0(round((dat[,2]/dat[,1])*100,digits =1 ),"%")
  dat$uniq_read<-paste0(round((dat[,3]/dat[,1])*100,digits =1 ),"%")
  #print(dat)
  
  
  out<-as.data.frame(cbind(sub_trim_table,total_align,uniq_align,dat[,4:5]))
  out[,5]<-paste0(out[,5]," (",out[,7],")")
  out[,6]<-paste0(out[,6]," (",out[,8],")")
  out<-out[,1:6]
  out[,5]<-gsub("%","\\\\%",out[,5])
  out[,6]<-gsub("%","\\\\%",out[,6])
  out[,4]<-as.character(out[,4])
  out[,4]<-as.numeric(out[,4])
  for(i in c(1:3,5,6)){
    out[,i]<-paste0("{",out[,i],"}")
  }
  for(j in 1:nrow(out)){
    row.out.one<- paste(unlist(out[j,]), collapse ="&")
    cat("\\rule{0pt}{10pt}",file=paste(out.path,out.file,sep="/"),fill=T,append=T)
    cat(paste(row.out.one,"\\",sep="\\"),file=paste(out.path,out.file,sep="/"),fill=T,append=T)
    
  }
  
  
  ####the end
  Fix.end<-paste(
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    sep="\n"
  )
  cat(Fix.end,file=paste(out.path,out.file,sep="/"),fill=T,append=T)
}


alignment_table("trimming.summary.txt","alignment.txt","./","align_latex.tex")







