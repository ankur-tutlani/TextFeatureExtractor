#Pass input dataset in CSV file with header of 'Text'
#dataset1 <- maml.mapInputPort(1) # class: data.frame
#dataset2 <- maml.mapInputPort(2) # class: data.frame
## below csv,text and zip files are available on the web.

#options(java.parameters = "- Xmx1024m")
#if (class(tryCatch(library('openNLPmodels.en'),error=function(e)e))[1] == "simpleError") {install.packages("openNLPmodels.en",repos = "http://datacube.wu.ac.at/")}

PATH.BING<<-"src/opinion-lexicon-English.RAR"
PATH.NRC<<-"src/NRC-Emotion-Lexicon-v0.92.zip"
PATH.AFINN<<-"src/imm6010.zip"
PATH.BORMUTH<<-"src/dale_chall.txt"
PATH.SPACHE<<-"src/spache1.txt"
N<<-2
tryCatch(if(file.exists(PATH.AFINN)) print(paste('File',PATH.AFINN)),error=function(e){print(paste('File','missing'))})
tryCatch(if(file.exists(PATH.BING)) print(paste('File',PATH.BING)),error=function(e){print(paste('File','missing'))})
tryCatch(if(file.exists(PATH.BORMUTH)) print(paste('File',PATH.BORMUTH)),error=function(e){print(paste('File','missing'))})
tryCatch(if(file.exists(PATH.NRC)) print(paste('File',PATH.NRC)),error=function(e){print(paste('File','missing'))})
tryCatch(if(file.exists(PATH.SPACHE)) print(paste('File',PATH.SPACHE)),error=function(e){print(paste('File','missing'))})

PATH.EMOT.DIC<<-'src/emot_dic.csv'
tryCatch(if(file.exists(PATH.EMOT.DIC)) print(paste('File',PATH.EMOT.DIC)),error=function(e){print(paste('File','missing'))})
EMOT<<-if (exists('EMOT')) EMOT else read.csv(PATH.EMOT.DIC,header=FALSE,stringsAsFactors=FALSE)$V1
PATH.DIC<<-'src/dic.csv'
tryCatch(if(file.exists(PATH.DIC)) print(paste('File',PATH.DIC)),error=function(e){print(paste('File','missing'))})
DIC<<- if (exists('DIC')) DIC else read.csv(PATH.DIC,header=FALSE,stringsAsFactors=FALSE)$V1

#PATH.TREETAGGER<<-"src/TreeTagger/bin/tag-english.bat"
#MD_SENT<<-NULL
#MD_WORD<<-NULL
#MD_POS<<-NULL
#MD_DATE<<-NULL
#MD_LOC<<-NULL
#MD_MONEY<<-NULL
#MD_ORG<<-NULL
#MD_PERSON<<-NULL
#MD_PERCENT<<-NULL
#SPELL.DIC<<-qdapDictionaries::GradyAugmented
#SPELL.METHOD<<-"jw"

install.packages("src/syuzhet_0.2.0.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/sentiment.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/Rstem_0.4-1.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/koRpus_0.05-6.zip",lib=".",repos=NULL,verbose=TRUE)

library(Rstem,lib.loc=".",verbose=TRUE)
library(syuzhet,lib.loc=".",verbose=TRUE)
library(sentiment,lib.loc=".",verbose=TRUE)
library(koRpus,lib.loc=".",verbose=TRUE)

text_to_lexical_diversity_features<-function(text,path.treetagger=PATH.TREETAGGER){
    
    ##path.treetagger=path to treetagger bin folder tag-english batch file (in case of Windows OS)
    
    #library(koRpus)
    library(tm)
    library(SnowballC)
    #set.kRp.env(TT.cmd=path.treetagger, lang="en")
    Encoding(text)<-"UTF-8"
    
    lex_div<-function(text){xz<-rep(0,32);if(nzchar(text)==TRUE){tryCatch({j_token<-tokenize(text, format = "obj", fileEncoding ="UTF-8", split = "[[:space:]]",ign.comp = "-", heuristics = "abbr,suf,pre", heur.fix = list(pre = c("’","'"), suf = c("’", "'")), abbrev = NULL, tag = TRUE, lang = "en",sentc.end = c(".", "!", "?", ";", ":"), detect = c(parag = FALSE, hline = FALSE), clean.raw = NULL, perl = FALSE, stopwords = tm::stopwords("en"),stemmer = SnowballC::wordStem); d3<-lex.div(j_token, segment = 2, factor.size = 0.35, min.tokens = 1,rand.sample = 15, window = 2, case.sens = FALSE, lemmatize = FALSE,detailed = FALSE, measure = c("TTR", "MSTTR", "MATTR", "C", "R", "CTTR", "S", "K", "Maas", "HD-D", "MTLD"), char = c("TTR", "MATTR","C", "R", "CTTR", "S", "K", "Maas", "HD-D", "MTLD"),char.steps = 5, log.base = 10, force.lang =NULL, keep.tokens = FALSE,corp.rm.class = "nonpunct", corp.rm.tag = c(), quiet = TRUE);x <- unlist(sapply(attributes(d3),function(x){y<-unlist(x)})); n <- names(x); x <- as.numeric(x); names(x) <- n; we2<-cbind(x,n);xz<-we2[which(we2[,2]=='tt.num.tokens'|we2[,2]=='tt.num.types'|we2[,2]=='TTR'|we2[,2]=='MSTTR.MSTTR'|we2[,2]=='MSTTR.sd'|we2[,2]=='MATTR.MATTR'|we2[,2]=='MATTR.sd'|we2[,2]=='C.ld'|we2[,2]=='R.ld'|we2[,2]=='CTTR'|we2[,2]=='S.ld'|we2[,2]=='K.ld'|we2[,2]=='Maas'|we2[,2]=='Maas.grw.a'|we2[,2]=='Maas.grw.lgV0'|we2[,2]=='Maas.grw.Vs'|we2[,2]=='HDD.HDD'|we2[,2]=='HDD.ATTR'|we2[,2]=='HDD.summary.Min.'|we2[,2]=='HDD.summary.1st Qu.'|we2[,2]=='HDD.summary.Median'|we2[,2]=='HDD.summary.Mean'|we2[,2]=='HDD.summary.3rd Qu.'|we2[,2]=='HDD.summary.Max.'|we2[,2]=='HDD.sd'|we2[,2]=='MTLD.factors.forw'|we2[,2]=='MTLD.factors.mean'|we2[,2]=='MTLD.factors.back'|we2[,2]=='MTLD.lengths.mean'|we2[,2]=='MTLD.lengths.mean.compl'|we2[,2]=='MTLD.lengths.sd'|we2[,2]=='MTLD.lengths.sd.compl')]},error=function(e){cat("Function error for the following text. Values are coerced to zero :","\n",text,"\n");return(NA)})};return(xz)}
    
    lex<-t(sapply(text,function(text) lex_div(text)))
    
    gt<-matrix(,nrow=length(text),ncol=32)
    gt[,1:32]<-sapply(lex[,1:32],as.numeric)
    gt[!is.finite(gt)]<-0
    
    rownames(gt)<-NULL
    colnames(gt)<-c("R_koRpus_lex.div_tt_num_tokens","R_koRpus_lex.div_tt_num_types","R_koRpus_lex.div_TTR","R_koRpus_lex.div_MSTTR_MSTTR","R_koRpus_lex.div_MSTTR_sd","R_koRpus_lex.div_MATTR_MATTR","R_koRpus_lex.div_MATTR_sd","R_koRpus_lex.div_C_ld","R_koRpus_lex.div_R_ld","R_koRpus_lex.div_CTTR","R_koRpus_lex.div_S_ld","R_koRpus_lex.div_K_ld","R_koRpus_lex.div_Maas","R_koRpus_lex.div_Maas_grw_a","R_koRpus_lex.div_Maas_grw_lgV0","R_koRpus_lex.div_Maas_grw_Vs","R_koRpus_lex.div_HDD_HDD","R_koRpus_lex.div_HDD_ATTR","R_koRpus_lex.div_HDD_summary_Min","R_koRpus_lex.div_HDD_summary_1st_Quartile","R_koRpus_lex.div_HDD_summary_Median","R_koRpus_lex.div_HDD_summary_Mean","R_koRpus_lex.div_HDD_summary_3rd_Quartile","R_koRpus_lex.div_HDD_summary_Max","R_koRpus_lex.div_HDD_sd","R_koRpus_lex.div_MTLD_factors_forw","R_koRpus_lex.div_MTLD_factors_mean","R_koRpus_lex.div_MTLD_factors_back","R_koRpus_lex.div_MTLD_lengths_mean","R_koRpus_lex.div_MTLD_lengths_mean_compl","R_koRpus_lex.div_MTLD_lengths_sd","R_koRpus_lex.div_MTLD_lengths_sd_compl")
    
    return(gt)
   
}


text_to_transformed_text<-function(text,dic=DIC){
    
    ## make 'dic' as character vector containing dictionary corpus (only alpha character words) in global environment
    
    ## compressed text
    compressed_text<-tolower(gsub('[^a-zA-Z0-9]+','',text))
    
    ## chunked text
    chunk<-function(text){if(nchar(text)>=6){x_list<-as.character();sx<-paste(text,'1',sep=""); i<-2; j<-0;repeat{repeat{k1<-substring(sx,1,i);k2<-substring(sx,1,i+1); k3<-substring(sx,1,i+2);k4<-substring(sx,1,i+3);k<-c(k1,k2,k3,k4);z<-sapply(k,function(k) which(k==dic));if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+3;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+3;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+4;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-2;j<-1};if(j==1){break}};x_list<-c(x_list,substring(k1,1,nchar(k1)-1));sx<-substring(sx,nchar(k1),nchar(sx));if(nchar(sx)<=1){break}};x_list<-paste(x_list,collapse=" ")} else x_list<-text;return(x_list)}
    
    chunked_text<-unlist(lapply(compressed_text, function(compressed_text) chunk(compressed_text)))
    
    transformed.text<-cbind(compressed_text,chunked_text)
    
    rownames(transformed.text)<-NULL
    
    colnames(transformed.text)<-c('compressed text','chunked text')
    
    return(transformed.text)
    
}

text_to_chunked_features<-function(text){
    
    cht<-text_to_transformed_text(text)[,2,drop=FALSE]
    
    
    cht_ld<-text_to_lexical_diversity_features(cht)
    colnames(cht_ld)<-sprintf("chunked_%s",colnames(cht_ld))
    
    cht_all<- cht_ld
	
    
    cht_all[!is.finite(cht_all)]<-0
    
    rownames(cht_all)<-NULL
    
    return(cht_all)
    
}

text_to_ngram_features<-function(text,n=N){
    
    ## n represents the number in ngrams
    library(tm)
    
    ngrams <-function(x, n)
	{
	    NN <- length(x)
	    n <- n[(n >= 1L) & (n <= NN)]
	    lapply(unlist(lapply(n,
	                         function(k) {
	                             pos <- seq_len(k)
	                             lapply(seq.int(0, NN - k),
	                                    `+`, pos)
	                         }),
	                  recursive = FALSE),
	           function(e) x[e])
	}
    
    sql1<-matrix(,nrow=length(text),ncol=1)
    sql1[,1]<-rep(0,length(text))
    colnames(sql1)<-"R_NLP_ngrams_NO_NGRAMS_FOUND"
    rownames(sql1)<-NULL
    
    tryCatch({
        ngramTokenizer <- function(x) unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
        tdm.chunked<-TermDocumentMatrix(Corpus(VectorSource(text)), control = list(tokenize = ngramTokenizer))
        tdm.chunked.tm.doc<-cbind(gramid=tdm.chunked$'i',doc=tdm.chunked$'j')
        tdm.chunked.tm.doc<-as.data.frame(tdm.chunked.tm.doc,header=TRUE,stringsAsFactors=FALSE)
        
        tdm.chunked.gram.freq<-table(tdm.chunked$'i')
        tdm.chunked.gram.freq<-as.data.frame(tdm.chunked.gram.freq,stringsAsFactors=FALSE)
        tdm.chunked.gram.freq<-tdm.chunked.gram.freq[with(tdm.chunked.gram.freq,order(-tdm.chunked.gram.freq[,2])),]
        tdm.chunked.gram.freq1<-head(tdm.chunked.gram.freq,1000)
        tdm.chunked.gram.freq1$Var1<-as.numeric(tdm.chunked.gram.freq1$Var1)
        colnames(tdm.chunked.gram.freq1)<-c('gramid','Freq')
        
        tdm.chunked.tm.doc1<-merge(x=tdm.chunked.tm.doc,y=tdm.chunked.gram.freq1,by='gramid',all.x=TRUE)
        tdm.chunked.tm.doc1[is.na(tdm.chunked.tm.doc1)]<-0
        tdm.chunked.tm.doc1$indi<-ifelse(tdm.chunked.tm.doc1$Freq>0,1,0)
        
        if(tdm.chunked$nrow > 1000) {
            sql1<-matrix(,nrow=length(text),ncol=1000)
            var.n<-seq(1:1000)
        }
        else {
            sql1<-matrix(,nrow=length(text),ncol=tdm.chunked$nrow)
            var.n<-seq(1:tdm.chunked$nrow)
        }
        
        for(i in 1:nrow(tdm.chunked.gram.freq1)){
            sql1.1<-tdm.chunked.tm.doc1[which(tdm.chunked.tm.doc1[,1]==tdm.chunked.gram.freq1[,1][i]),]
            sql1[sql1.1[,2],i]<-sql1.1[,4]
        }
        sql1[is.na(sql1)]<-0
        colnames(sql1)<-sprintf("R_NLP_ngrams_ngram_%s",var.n)
        rownames(sql1)<-NULL
    },error=function(e){return(NA)})
    
    sql1[!is.finite(sql1)]<-0
    
    return(sql1)
    
}

text<-dataset1$Text
feature.matrix<-cbind(text_to_lexical_diversity_features(text),text_to_chunked_features(text),text_to_ngram_features(text))
rownames(feature.matrix)<-NULL
    
feature.matrix[!is.finite(feature.matrix)]<-0

feature.matrix1<-cbind(dataset2,feature.matrix)
feature.matrix1<-as.data.frame(feature.matrix1)

#maml.mapOutputPort("feature.matrix1");
