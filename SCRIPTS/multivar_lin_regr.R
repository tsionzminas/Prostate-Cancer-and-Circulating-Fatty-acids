
library(readxl)

rm(list=ls())

PROJECT_DIR = "~/git/ProstateCancerFattyacid" # replace this line with your local path

infile = file.path(PROJECT_DIR,"DATA","clinico-demographic_fattyacid.xlsx")
data = as.matrix(read_excel(infile,sheet=1))

feat_index = 11:34
feat_label = colnames(data[,feat_index])
feat_label = matrix(unlist(strsplit(feat_label,"_")),byrow=T,ncol=2)[,2] # remove "fa_" prefixes
#feat_label[feat_label=="Docosapentaenoicn3"] = "Docosapentaenoic-n3"
#feat_label[feat_label=="Docosapentaenoicn6"] = "Docosapentaenoic-n6"
#feat_label[feat_label=="alphaLinolenic"] = "alpha-Linolenic"
#feat_label[feat_label=="gammaLinolenic"] = "gamma-Linolenic"
#feat_label[feat_label=="Dihomoglinolenic"] = "Dihomo-g-Linolenic"
n_feat = length(feat_label)

class_map = as.matrix(read_excel(infile,sheet=3))
#class_map[class_map[,1]=="Docosapentaenoic - n3",1] = "Docosapentaenoic-n3"
#class_map[class_map[,1]=="Docosapentaenoic - n6",1] = "Docosapentaenoic-n6"
#class_map[class_map[,1]=="Dihomo-g-linolenic",1] = "Dihomo-g-Linolenic"
class_map[class_map[,1]=="alpha-Linolenic",1] = "alphaLinolenic"
class_map[class_map[,1]=="gamma-Linolenic",1] = "gammaLinolenic"
class_map[class_map[,1]=="Docosapentaenoic - n3",1] = "Docosapentaenoicn3"
class_map[class_map[,1]=="Docosapentaenoic - n6",1] = "Docosapentaenoicn6"
class_map[class_map[,1]=="Dihomo-g-linolenic",1] = "Dihomoglinolenic"
#o = match(feat_label,class_map[,1])
#class_map = class_map[o,]
o = match(class_map[,1],feat_label)
feat_index = feat_index[o]
feat_label = feat_label[o]

var_index = 5:10
var = colnames(data[,var_index])
var_all = paste0(var,collapse="+")
var_label = c("Age","BMI","Education","Smoking","Diabetes","Aspirin")
n_var = length(var)

group = c("Afr_ctrl","Afr_case","AA_ctrl","AA_case","EA_ctrl","EA_case")
race = c("African","African","African American","African American","European American","European American")
case = c(0,1,0,1,0,1)
n_group = length(group)
for (i_group in 1:n_group) {
    sel = (data[,"race"]==race[i_group])&(data[,"case"]==case[i_group])
    democlin = matrix(as.numeric(data[sel,var_index]),ncol=n_var)
    colnames(democlin) = var
    feat = matrix(log2(as.numeric(data[sel,feat_index])),ncol=n_feat) # log2-transformed
    colnames(feat) = feat_label
    # "drastic/conservative" approach, we don't impute but remove all instances with missing data
    remove = apply(is.na(democlin),1,sum)>0
    democlin = democlin[!remove,]
    feat = feat[!remove,]
    mydata = data.frame(cbind(democlin,feat))
    
    coef = matrix(rep(NA,n_feat*3*n_var),ncol=3*n_var)
    pval = matrix(rep(NA,n_feat*2*n_var),ncol=2*n_var)
    Fstat = rep(NA,n_feat)
    res = matrix(rep(NA,n_feat*(4*n_var+3)),ncol=(4*n_var+3))
    for (i_feat in 1:n_feat) {
        myformula = as.formula(paste0(feat_label[i_feat],"~",var_all))
        fit = lm(myformula,mydata)
        coef[i_feat,] = as.numeric(t(cbind(confint(fit,level=0.95)[-1,1],coef(summary(fit))[-1,1],confint(fit,level=0.95)[-1,2])))
        p = coef(summary(fit))[-1,4]
        padj = p.adjust(p,method="fdr")
        pval[i_feat,] = as.numeric(t(cbind(p,padj)))
        Fstat[i_feat] = 1 - pf(summary(fit)$fstatistic[1], summary(fit)$fstatistic[2], summary(fit)$fstatistic[3])
        res[i_feat,1] = summary(fit)$fstatistic[1]
        res[i_feat,2] = Fstat[i_feat]
        for (i_var in 1:n_var) {
            res[i_feat,4*(i_var-1)+4] = coef(summary(fit))[i_var+1,1]
            res[i_feat,4*(i_var-1)+5] = coef(summary(fit))[i_var+1,2]
            res[i_feat,4*(i_var-1)+6:7] = pval[i_feat,2*(i_var-1)+1:2]
        }
        res[,3] = p.adjust(as.numeric(res[,2]),method="fdr")
        outfile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_res.txt"))
        header = c("Fstat","Fstat.pval","Fstat.pval.adj")
        for (i_var in 1:n_var) {
            header = c(header,paste0(var_label[i_var],".",c("est","se","pval","pval.adj")))
        }
        output = rbind(c("Fatty_Acid","Class",header),cbind(feat_label,class_map[,2],res))
        write(t(output),ncol=ncol(output),file=outfile,sep="\t")
        
        outfile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_Fstat.txt"))
        output = rbind(c("Fatty_Acid","Class","Fstat.pval","Fstat.pval.adj"),cbind(feat_label,class_map[,2],Fstat,p.adjust(Fstat,method="fdr")))
        write(t(output),ncol=ncol(output),file=outfile,sep="\t")
        
        outfile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_coef.txt"))
        header = NULL
        for (i_var in 1:n_var) {
            header = c(header,paste0(var[i_var],"_",c("l","m","u")))
        }
        output = rbind(c("Fatty_Acid","Class",header),cbind(feat_label,class_map[,2],coef))
        write(t(output),ncol=ncol(output),file=outfile,sep="\t")
        
        outfile = file.path(PROJECT_DIR,"RESULTS",paste0(group[i_group],"_pval.txt"))
        header = NULL
        for (i_var in 1:n_var) {
            header = c(header,paste0(var[i_var],"_",c("pval","pval.adj")))
        }
        output = rbind(c("Fatty_Acid","Class",header),cbind(feat_label,class_map[,2],pval))
        write(t(output),ncol=ncol(output),file=outfile,sep="\t")
    }
}

