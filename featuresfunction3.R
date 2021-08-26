 Pass input dataset in CSV file with header of 'Text'
dataset1 <- maml.mapInputPort(1) # class: data.frame
dataset2 <- maml.mapInputPort(2) # class: data.frame
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

text_to_misogyny<-function(text){
    
    reg1<-grep(pattern='(\\bsjw|keyboard warrior|feminist|girl gamer|gamer girl|gamergirl\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg2<-grep(pattern='(\\b(ur|you\'re|youre|you are) just a (woman|girl|female|chick)\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg3<-grep(pattern='(\\b(stuckup|stuck up|ugly|unlikeable|pathetic|stupid|dumb|slutty|fat) (woman|girl|female|chick)\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg4<-grep(pattern='(\\b(stuckup|stuck up|ugly|unlikeable|pathetic|stupid|dumb|slutty|fat|fucking) (sjw|keyboard warrior|feminist|girl gamer|gamer girl|gamergirl)\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg5<-grep(pattern='(\\b(ur|you\'re|youre|you are) (just |)a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking )(woman|girl|female|chick)\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg6<-grep(pattern='(\\bno one cares (if |that |)(you\'re|youre|you are|ur|your) (a |)(stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |)(woman|girl|female|chick)\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg7<-grep(pattern='(\\bat least (i\'m|im|i am) not a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |)(woman|girl|female|chick)\\b)',x=text,perl=TRUE, ignore.case=TRUE)
    reg8<-grep(pattern='\\b(men|males) (are|r) (better||greater) (then|than|) (women|female)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg9<-grep(pattern='\\bgo (have|make) (some |sum |)babies\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg10<-grep(pattern='\\bshow (me|) (your|ur) (vag|puss|ass|butt|breast|boob|bewb|tits)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg11<-grep(pattern='\\b(?<![a-z])(?<!pain )(inside|in) (your|ur) (vag|pussy)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg12<-grep(pattern='\\bsu(c|)k my (dick|penis|cock)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg13<-grep(pattern='\\b(nude|naked) (pic|pix)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg14<-grep(pattern='\\b(tits|puss|pussy|vag|vagina|boobs|bewbs|ass|breasts) on (cam|kinect|twitch)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg15<-grep(pattern='\\bu need (some|sum) (of my |)(dick|penis|cock)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg16<-grep(pattern='\\b(pic|pics|pix|pictures|picture) of (your|ur) (tits|puss|vag|boob|bewb|ass|breast)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg17<-grep(pattern='\\b(i\'m|im|i am|iam) horny\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg18<-grep(pattern='\\bmoan (for|4) me\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg19<-grep(pattern='\\bcunt\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg20<-grep(pattern='\\bbitch\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg21<-grep(pattern='\\bslut\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg22<-grep(pattern='\\bslag\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg23<-grep(pattern='\\b(?<![a-z])whore\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg24<-grep(pattern='\\bdyke\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg25<-grep(pattern='\\blesbo\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg26<-grep(pattern='\\b(stuckup|stuck up|ugly|unlikeable|pathetic|stupid|dumb|slutty|fat|fucking|emotional) (cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg27<-grep(pattern='\\bthat(\'s|s| is) why (you\'re|youre|you are|ur) a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg28<-grep(pattern='\\b(don\'t|dont) (b|be) a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg29<-grep(pattern='\\b(stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo) like (your|ur) (mom|nan)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg30<-grep(pattern='\\b(you\'re|you are|youre|ur) just a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg31<-grep(pattern='\\b(i\'m|im|i am) not a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg32<-grep(pattern='\\bsays the (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg33<-grep(pattern='\\bbeing (such |)a (stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)(cunt|bitch|slut|slag|whore|dyke|lesbo)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg34<-grep(pattern='\\b(?<![a-z])(you|u|you\'re|youre|ur|your) (a |)(stuckup |stuck up |ugly |unlikeable |pathetic |stupid |dumb |slutty |fat |fucking |emotional |)whale\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg35<-grep(pattern='\\b(?<![a-z])rape (u|you)(?![a-z])\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg36<-grep(pattern='\\b(?<![a-z])(you|u) get raped\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg37<-grep(pattern='\\bstfu\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg38<-grep(pattern='\\bbutthurt\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg39<-grep(pattern='\\bbullied at school\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg40<-grep(pattern='\\bthink (you|u) can ignore me\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg41<-grep(pattern='\\bu (can\'t|cant|cannot|can not) ignore me\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg42<-grep(pattern='\\bstop crying\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg43<-grep(pattern='\\bfuck (you|u)(?![a-z])\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg44<-grep(pattern='\\b(ur|you\'re|youre|you are) (fucking |)pathetic\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg45<-grep(pattern='\\bi (will|am going (to|2)) find (u|you)(?![a-z])\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg46<-grep(pattern='\\bfind where (you|u) live\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg47<-grep(pattern='\\bkill (your|ur) family\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg48<-grep(pattern='\\bkill (your self|yourself|urself)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg49<-grep(pattern='\\bfuck (your|ur) (bf|boyfriend|husband)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg50<-grep(pattern='\\b(hack|ddos) (you|u)(?![a-z])\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg51<-grep(pattern='\\bkys\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg52<-grep(pattern='\\b(you\'re|you are|youre|ur) insecure\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg53<-grep(pattern='\\bu (just |)(want|crave) (guys |male |mens |)attention\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg54<-grep(pattern='\\b(u|you\'re|youre|ur) (just |)doing it (for|4) (the |)attention\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg55<-grep(pattern='\\bevery (woman|girl|female|chick) has to prove\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg56<-grep(pattern='\\b(prove|show) (that |)(they\'re|theyre|they are) a (woman|girl|female|chick)\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg57<-grep(pattern='\\battention(-| )seeking\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg58<-grep(pattern='\\battention(-| )whore\\b',x=text,perl=TRUE, ignore.case=TRUE)
    reg59<-grep(pattern='\\bu (don\'t|dont|do not) matter\\b',x=text,perl=TRUE, ignore.case=TRUE)
    
    reg.all<-c(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,reg16,reg17,reg18,reg19,reg20,reg21,reg22,reg23,reg24,reg25,reg26,reg27,reg28,reg29,reg30,reg31,reg32,reg33,reg34,reg35,reg36,reg37,reg38,reg39,reg40,reg41,reg42,reg43,reg44,reg45,reg46,reg47,reg48,reg49,reg50,reg51,reg52,reg53,reg54,reg55,reg56,reg57,reg58,reg59)
    
    reg.all<-unique(reg.all)
    reg.all1<-cbind(rep(1,length(reg.all)),reg.all)
    colnames(reg.all1)<-c('v1','v2')
    reg.2<-as.matrix(data.frame(v2=seq(1:length(text)),v3=0),drop=FALSE)
    reg.final<-merge(x=reg.2,y=reg.all1,by='v2',all.x=TRUE)
    reg.final<-as.matrix(reg.final,drop=FALSE)
    reg.final[is.na(reg.final)]<-0
    rownames(reg.final)<-NULL
    colnames(reg.final)<-c('v2','v3','indicator_misogyny')
    
    return(reg.final[,3,drop=FALSE])
    
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
    
    cht_misogyny<-text_to_misogyny(cht)
    colnames(cht_misogyny)<-sprintf("chunked_%s",colnames(cht_misogyny))
    
    cht_ld<-text_to_lexical_diversity_features(cht)
    colnames(cht_ld)<-sprintf("chunked_%s",colnames(cht_ld))
    
    cht_all<-cbind(cht_misogyny,cht_ld)
    
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
feature.matrix<-cbind(text_to_lexical_diversity_features(text),text_to_misogyny(text),text_to_chunked_features(text),text_to_ngram_features(text))
rownames(feature.matrix)<-NULL
    
feature.matrix[!is.finite(feature.matrix)]<-0

feature.matrix1<-cbind(dataset2,feature.matrix)
feature.matrix1<-as.data.frame(feature.matrix1)

maml.mapOutputPort("feature.matrix1");