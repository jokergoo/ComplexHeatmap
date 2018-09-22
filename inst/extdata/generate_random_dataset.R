
set.seed(888)

type = c(rep("Tumor", 10), rep("Control", 10))
gender = sample(c("F", "M"), 20, replace = TRUE)
gender[sample(1:20, 2)] = NA
age = runif(20, min = 30, max = 80)
mutation = data.frame(mut1 = sample(c(TRUE, FALSE), 20, p = c(0.2, 0.8), replace = TRUE),
    mut2 = sample(c(TRUE, FALSE), 20, p = c(0.3, 0.7), replace = TRUE))
anno = data.frame(type = type, gender = gender, age = age, mutation, stringsAsFactors = FALSE) 

anno_col = list(type = c("Tumor" = "red", "Control" = "blue"),
    gender = c("F" = "pink", "M" = "darkgreen"),
    mutation = c("TRUE" = "black", "FALSE" = "#EEEEEE"))

######################################
# generate methylation matrix
rand_meth = function(k, mean) {
    (runif(k) - 0.5)*min(c(1-mean), mean) + mean
}

mean_meth = c(rand_meth(300, 0.3), rand_meth(700, 0.7))
mat_meth = as.data.frame(lapply(mean_meth, function(m) {
    if(m < 0.3) {
        c(rand_meth(10, m), rand_meth(10, m + 0.2))
    } else if(m > 0.7) {
        c(rand_meth(10, m), rand_meth(10, m - 0.2))
    } else {
        c(rand_meth(10, m), rand_meth(10, m + sample(c(1, -1), 1)*0.2))
    }

}))
mat_meth = t(mat_meth)
rownames(mat_meth) = NULL
colnames(mat_meth) = paste0("sample", 1:20)

######################################
# generate directions for methylation
direction = rowMeans(mat_meth[, 1:10]) - rowMeans(mat_meth[, 11:20])
direction = ifelse(direction > 0, "hyper", "hypo")
library(circlize)

#######################################
# generate expression matrix
mat_expr = t(apply(mat_meth, 1, function(x) {
    x = x + rnorm(length(x), sd = abs(runif(1)-0.5)*0.4 + 0.1)
    -scale(x)
}))
dimnames(mat_expr) = dimnames(mat_meth)

#############################################################
# matrix for correlation between methylation and expression
cor_pvalue = sapply(seq_len(nrow(mat_meth)), function(i) {
    cor.test(mat_meth[i, ], mat_expr[i, ])$p.value
})

#####################################################
# matrix for types of genes
gene_type = sample(c("protein_coding", "lincRNA", "microRNA", "psedo-gene", "others"), 
    nrow(mat_meth), replace = TRUE, prob = c(6, 1, 1, 1, 1))

#################################################
# annotation to genes
anno_gene = sapply(mean_meth, function(m) {
    if(m > 0.6) {
        if(runif(1) < 0.8) return("intragenic")
    }
    if(m < 0.4) {
        if(runif(1) < 0.4) return("TSS")
    }
    return("intergenic")
})

anno_gene_col = c("intragenic" = "blue", "TSS" = "red", "intergenic" = "grey")

############################################
# distance to genes
tss_dist = sapply(mean_meth, function(m) {
    if(m < 0.3) {
        if(runif(1) < 0.5) {
            return(round( (runif(1) - 0.5)*1000 + 500))
        } else {
            return(round( (runif(1) - 0.5)*10000 + 500))
        }
    } else if(m < 0.6) {
        if(runif(1) < 0.8) {
            return(round( (runif(1)-0.5)*100000 + 50000 ))
        } else {
            return(round( (runif(1)-0.5)*1000000 + 500000 ))
        }
    }
    return(round( (runif(1) - 0.5)*1000000 + 500000))    
})


#######################################
# annotation to enhancers
rand_tss = function(m) {
    if(m < 0.4) {
        if(runif(1) < 0.25) return(runif(1))
    } else if (runif(1) < 0.1) {
        return(runif(1))
    } 
    return(0)
}
rand_enhancer = function(m) {
    if(m < 0.4) {
        if(runif(1) < 0.6) return(runif(1))
    } else if (runif(1) < 0.1) {
        return(runif(1))
    } 
    return(0)
}
rand_repressive = function(m) {
    if(m > 0.4) {
        if(runif(1) < 0.8) return(runif(1))
    }
    return(0)
}
anno_states = data.frame(
    tss = sapply(mean_meth, rand_tss), 
    enhancer = sapply(mean_meth, rand_enhancer), 
    repressive = sapply(mean_meth, rand_repressive))

save(mat_meth, mat_expr, anno, anno_col, anno_states, cor_pvalue, direction,
    anno_gene, gene_type, tss_dist, file = "random_meth_expr_data.RData")
