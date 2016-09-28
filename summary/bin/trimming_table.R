#####trimming
trimming_table<-function(input_file_name,out.path,out.file="trimming.summary_table.tex"){
  #####header
  Fix.before<-paste(
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{Trimming Summary}",
    "\\label{tab:trimming}",
    "\\medskip",
    "\\tiny",
    "\\begin{tabular}{@{} lS[table-format=8.0]S[table-format=8.0]",
    "S[table-format=1.0]S[table-format=12.0]",
    "S[table-format=12.0]", 
    "S[table-format=2.0]@{}}",
    "\\toprule",
    "& \\multicolumn{3}{c}{\\textsc{\\scriptsize Processed Reads}} & \\multicolumn{3}{c}{\\textsc{\\scriptsize Processed Reads}} \\\\",
    "\\cmidrule(lr{0.75em}){2-4}",
    "\\cmidrule(lr{0.75em}){5-7}",
    "&{\\scriptsize No. Reads} & \\splitcell{ \\scriptsize Base Pairs \\\\ \\scriptsize(bp)} & \\splitcell{\\scriptsize Length \\\\ \\scriptsize (bp)} &   \\scriptsize No. Reads &   \\splitcell{\\scriptsize Base Pairs \\\\ \\scriptsize (bp)} & \\splitcell{\\scriptsize Length \\\\ \\scriptsize (bp)}\\\\ ",
    "\\midrule",
    "\\rule{0pt}{10pt}",
    sep="\n"
  )
  
  cat(Fix.before,file=paste(out.path,out.file,sep="/"),fill=T,append=T)
  
  
  
  #######main part
  in_table<-read.delim(input_file_name,header = F,comment.char="!")
  #print(in_table)
  sub_table<-in_table[((which(in_table[,1]=="# TOTALS")+1):nrow(in_table)),1:9]
  sub_table[,5]<-paste0(sub_table[,5]," (",sub_table[,6],")")
  sub_table[,7]<-paste0(sub_table[,7]," (",sub_table[,8],")")
  final_table<-sub_table[,c(-6,-8)]
  #print(final_table)
  final_table[,5]<-gsub("%","\\\\%",final_table[,5])
  final_table[,6]<-gsub("%","\\\\%",final_table[,6])
  final_table[,4]<-as.character(final_table[,4])
  final_table[,4]<-as.numeric(final_table[,4])
  final_table[,7]<-as.character(final_table[,7])
  final_table[,7]<-as.numeric(final_table[,7])
  for(i in c(1:3,5,6)){
    final_table[,i]<-paste0("{",final_table[,i],"}")
  }
  for(j in 1:nrow(final_table)){
    row.out.one<- paste(unlist(final_table[j,]), collapse ="&")
    cat("\\rule{0pt}{10pt}",file=paste(out.path,out.file,sep="/"),fill=T,append=T)
    cat(paste(row.out.one,"\\",sep="\\"),file=paste(out.path,out.file,sep="/"),fill=T,append=T)
    
  }
  
  ###the end
  Fix.end<-paste(
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    sep="\n"
  )
  cat(Fix.end,file=paste(out.path,out.file,sep="/"),fill=T,append=T)
}




#trimming_table("trimming.summary.txt")
trimming_table("trimming.summary.txt","./","trim_latex.tex")










