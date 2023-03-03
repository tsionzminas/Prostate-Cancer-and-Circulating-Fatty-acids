require(gplots)
require(RColorBrewer)

rm(list=ls())

PROJECT_DIR = "~/git/ProstateCancerFattyacid" # replace this line with your local path

group = c("Afr_ctrl","Afr_case","AA_ctrl","AA_case","EA_ctrl","EA_case")
n_group = length(group)
race_col = c(rep("#eff3ff",2),rep("#08519c",2),rep("#caff70",2))
caco_col = rep(c("orange","#4d2600"),3)

model_pval = vector("list",n_group)
coef = vector("list",n_group)
pval = vector("list",n_group)
for (i_group in 1:n_group) {
    infile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_Fstat.txt"))
    model_pval[[i_group]] = as.matrix(read.table(infile,header=T,stringsAsFactors=F,sep="\t"))
    feat_class = model_pval[[i_group]][,2]
    model_pval[[i_group]] = model_pval[[i_group]][,c(1,4)] # to use adjusted F-stat p-values
    infile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_coef.txt"))
    coef[[i_group]] = as.matrix(read.table(infile,header=T,stringsAsFactors=F,sep="\t"))
    coef[[i_group]] = coef[[i_group]][,c(1,4,7,10,13,16,19)]
    infile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_pval.txt"))
    pval[[i_group]] = as.matrix(read.table(infile,header=T,stringsAsFactors=F,sep="\t"))
    pval[[i_group]] = pval[[i_group]][,c(1,4,6,8,10,12,14)]
}

feat = model_pval[[1]][,1]
model_pval_all = NULL
pval_all = NULL
for (i_group in 1:n_group) {
    model_pval_all = cbind(model_pval_all,as.numeric(model_pval[[i_group]][,2]))
    pval_all = cbind(pval_all,matrix(as.numeric(pval[[i_group]][,-1]),ncol=ncol(pval[[i_group]])-1))
}
feat_sel = (apply(model_pval_all<0.05,1,sum)>0)&(apply(pval_all<0.05,1,sum)>0)
feat = feat[feat_sel]
feat_class = feat_class[feat_sel]
for (i_group in 1:n_group) {
    model_pval[[i_group]] = model_pval[[i_group]][feat_sel,]
    coef[[i_group]] = coef[[i_group]][feat_sel,]
    pval[[i_group]] = pval[[i_group]][feat_sel,]
}

feat_class_label = unique(feat_class)
n_class = length(feat_class_label)
#class_col = grDevices::rainbow(n_class)
#proc_col[1] = "brown"
#proc_col[5] = "orange"
#class_col = c("#00dae0","#ff9289","#756bb1","#fae100","#cc6600")
class_col = c("#00dae0","#ff9289","#756bb1","#fae100","#009933")

var = c("Age","BMI","Education","Smoking","Diabetes","Aspirin")
n_feat = length(feat)
n_var = length(var)
myCol = rev(redblue(11)[c(1,3,5,6,7,9,11)])
myBreaks = c(-3.9,-2.9,-1.9,-0.9,0.9,1.9,2.9,3.9)

res_merged = matrix(rep(0,n_feat*n_group*n_var),ncol=n_group*n_var)
for (i_group in 1:n_group) {
    res = matrix(rep(0,n_feat*n_var),ncol=n_var)
    signif_mat = matrix(as.numeric(pval[[i_group]][,-1]),ncol=ncol(pval[[i_group]])-1)
    res[which(signif_mat<0.05,arr.ind=T)] = 1
    res[which(signif_mat<0.01,arr.ind=T)] = 2
    res[which(signif_mat<0.001,arr.ind=T)] = 3
    model_mat = matrix(rep(0,n_feat*n_var),ncol=n_var)
    model_mat[as.numeric(model_pval[[i_group]][,-1])<0.05,] = rep(1,n_var)
    coef_mat = matrix(as.numeric(coef[[i_group]][,-1]),ncol=ncol(coef[[i_group]])-1)
    sign_mat = matrix(rep(1,n_feat*n_var),ncol=n_var)
    sign_mat[which(coef_mat<0,arr.ind=T)] = -1
    res = res*sign_mat*model_mat
    for (i_var in 1:n_var) {
        res_merged[,(i_var-1)*n_group+i_group] = res[,i_var]
    }
}
var_labels = rep("",n_group*n_var)
var_labels[seq(2,n_group*n_var,by=3)] = var
rownames(res_merged) = feat
colnames(res_merged) = var_labels

row_ref = rep(NA,n_feat)
for (i_class in 1:n_class) {
    row_ref[feat_class==feat_class_label[i_class]] = class_col[i_class]
}

outfile = file.path(PROJECT_DIR,"RESULTS","multivar_lin_regr_heatmap_by_race.pdf")
pdf(outfile,width=7,height=5)
col_ref = NULL
for (i_var in 1:n_var) {
    col_ref = c(col_ref,race_col)
}
hm <- heatmap.2(res_merged, scale="none", Rowv=F, Colv=F, na.rm=T, na.color="black",
col = myCol, breaks = myBreaks, dendrogram = "none", key=T, density.info="none", trace="none",
cexRow=0.35,ColSideColors=col_ref, RowSideColors=row_ref)
dev.off()

outfile = file.path(PROJECT_DIR,"RESULTS","multivar_lin_regr_heatmap_by_caco.pdf")
pdf(outfile,width=7,height=5)
col_ref = NULL
for (i_var in 1:n_var) {
    col_ref = c(col_ref,caco_col)
}
hm <- heatmap.2(res_merged, scale="none", Rowv=F, Colv=F, na.rm=T, na.color="black",
col = myCol, breaks = myBreaks, dendrogram = "none", key=T, density.info="none", trace="none",
cexRow=0.35,ColSideColors=col_ref, RowSideColors=row_ref)
dev.off()
