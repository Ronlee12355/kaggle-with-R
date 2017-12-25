setwd("E:/kaggle/pagerank")
suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(stringr))

gene_df<-inner_join(fread("hsa_pathway.txt"),fread("hsa.txt"),by="point") %>% sample_frac(0.01)
gene_ls<-rep(0,nrow(gene_df))
for(i in 1:nrow(gene_df)){
  gene_ls[i]<-unlist(str_split(gene_df$gene[i],","))[1]
}
gene_ppi<-fread("String_PPI_400.txt") %>% filter(gene1 %in% gene_ls & gene2 %in% gene_ls) %>% select(gene1:gene2)
W<-matrix(rep(0,length(gene_ls)*length(gene_ls)),nrow = length(gene_ls),dimnames = list(gene_ls,gene_ls))
for(i in 1:2688){
  W[gene_ppi[i,1],gene_ppi[i,2]]<-1
}
D=apply(W,1,sum) %>% diag(length(gene_ls),length(gene_ls))

for(i in 1:length(gene_ls)){
  if(D[i,i] == 0){
    D[i,i]<-1
  }
}
initial_value<-rep(1,length(gene_ls))
ex=rank(initial_value)
r=ex/sum(ex)
n=0
d_value<-0.85
repeat{
  r=(1-d_value)*ex + t(W)%*%solve(D)%*%r*d_value
  n=n+1
  print(r)
  if(n>=40){
    break
  }
}
importance<-data.table("gene"=rownames(r),"rank_value"=r)
importance<- importance %>% arrange(desc(rank_value.V1))
importance