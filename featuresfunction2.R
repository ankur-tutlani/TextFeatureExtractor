 Pass input dataset in CSV file with header of 'Text'
dataset1 <- maml.mapInputPort(1) # class: data.frame
dataset2 <- maml.mapInputPort(2) # class: data.frame
DIC<<-read.csv('src/dic.csv',header=FALSE,stringsAsFactors=FALSE)$V1
bigrams.d<<-read.table('src/bigram.txt',header = TRUE,stringsAsFactors = FALSE)
PATH.BORMUTH<<-"src/dale_chall.txt"
PATH.SPACHE<<-"src/spache1.txt"
N<<-2
install.packages("src/koRpus_0.05-6.zip",lib=".",repos=NULL,verbose=TRUE)
library(koRpus,lib.loc=".",verbose=TRUE)
library(tm)
library(SnowballC)
text_to_readability_features<-function(text,path.bormuth=PATH.BORMUTH,path.spache=PATH.SPACHE){
    Encoding(text)<-"UTF-8"
    
    read_cols<-c("desc.sentences","desc.words","desc.letters.all","desc.letters.l1","desc.letters.l2","desc.letters.l3","desc.letters.l4","desc.letters.l5","desc.all.chars","desc.syllables.all","desc.lttr.distrib1","desc.lttr.distrib2","desc.lttr.distrib3","desc.lttr.distrib4","desc.lttr.distrib5","desc.lttr.distrib6","desc.lttr.distrib7","desc.lttr.distrib8","desc.lttr.distrib9","desc.lttr.distrib10","desc.lttr.distrib11","desc.lttr.distrib12", "desc.lttr.distrib13","desc.lttr.distrib14","desc.lttr.distrib15","desc.lttr.distrib16","desc.lttr.distrib17","desc.lttr.distrib18","desc.lttr.distrib19","desc.lttr.distrib20","desc.lttr.distrib21","desc.lttr.distrib22","desc.lttr.distrib23","desc.lttr.distrib24","desc.lttr.distrib25","desc.lttr.distrib26","desc.lttr.distrib27","desc.lttr.distrib28","desc.lttr.distrib29","desc.lttr.distrib30","desc.syll.distrib1","desc.syll.distrib2","desc.syll.distrib3","desc.syll.distrib4","desc.syll.distrib5","desc.syll.distrib6","desc.syll.uniq.distrib1","desc.syll.uniq.distrib2","desc.syll.uniq.distrib3","desc.syll.uniq.distrib4","desc.syll.uniq.distrib5","desc.syll.uniq.distrib6","desc.punct","desc.conjunctions","desc.prepositions","desc.pronouns","desc.foreign","desc.TTR","desc.avg.sentc.length","desc.avg.word.length","desc.avg.syll.word","desc.sntc.per.word","desc.sntc.per100","desc.lett.per100","desc.syll.per100","desc.FOG.hard.words","desc.Bormuth.NOL","desc.Dale.Chall.NOL","desc.Spache.NOL","ARI.grade","Bormuth.pct.fam","Bormuth.mean.cloze","Bormuth.grade","Coleman.num.pron","Coleman.num.prep","Coleman.C1","Coleman.C2","Coleman.C3","Coleman.C4","Coleman.Liau.ECP","Coleman.Liau.grade","Coleman.Liau.short","Dale.Chall.pct","Dale.Chall.raw","Dale.Chall.grade.min","Dale.Chall.age.min","Danielson.Bryan.avg.sentc","Danielson.Bryan.DB2.grade.min","Dickes.Steiwer.TTR","Dickes.Steiwer.Dickes.Steiwer","DRP.DRP","ELF.num.exsyls","ELF.ELF","Flesch.RE","Flesch.grade.min","Flesch.Kincaid.grade","Flesch.Kincaid.age","Farr.Jenkins.Paterson.FJP","Farr.Jenkins.Paterson.grade.min","FOG.FOG.hard.words","FOG.FOG","FORCAST.grade","FORCAST.age","Fucks.Fucks","Fucks.grade","Linsear.Write.easy.words","Linsear.Write.hard.words","Linsear.Write.raw","Linsear.Write.grade","LIX.index","LIX.grade.min","RIX.index","RIX.grade","RIX.grade.min","SMOG.grade","SMOG.age","Spache.pct","Spache.grade","Strain.index","Traenkle.Bailer.pct.prep","Traenkle.Bailer.pct.conj","Traenkle.Bailer.TB1","Traenkle.Bailer.TB2","TRI.short","TRI.punct","TRI.foreign","TRI.TRI","Wheeler.Smith.score","Wheeler.Smith.grade.min","Wiener.STF.nWS1","Wiener.STF.nWS2","Wiener.STF.nWS3","Wiener.STF.nWS4","desc.letters.l6","desc.lttr.distrib31","desc.lttr.distrib32","desc.lttr.distrib33","desc.lttr.distrib34","desc.lttr.distrib35","desc.lttr.distrib36")
    
    read_function<-function(text){read.matrix<-matrix(,nrow=2,ncol=140);read.matrix[1,]<-read_cols;if(nzchar(text)==TRUE){tryCatch({j_token<-tokenize(text, format = "obj", fileEncoding ="UTF-8", split = "[[:space:]]",ign.comp = "-", heuristics = "abbr,suf,pre", heur.fix = list(pre = c("’","'"), suf = c("’", "'")), abbrev = NULL, tag = TRUE, lang = "en",sentc.end = c(".", "!", "?", ";", ":"), detect = c(parag = FALSE, hline = FALSE), clean.raw = NULL, perl = FALSE, stopwords = tm::stopwords("en"),stemmer = SnowballC::wordStem); res_read<-readability(j_token, hyphen=NULL,index=c("ARI","Bormuth","Coleman","Coleman.Liau","Dale.Chall","Danielson.Bryan","Dickes.Steiwer","DRP","ELF","Farr.Jenkins.Paterson","Flesch","Flesch.Kincaid","FOG","FORCAST","Fucks","Linsear.Write","LIX","nWS", "RIX", "SMOG", "Spache", "Strain", "Traenkle.Bailer", "TRI","Wheeler.Smith"),parameters=list(),word.lists=list(Bormuth=path.bormuth, Dale.Chall=path.bormuth,Spache=path.spache),fileEncoding="UTF-8", force.lang=NULL,sentc.tag="sentc", nonword.class="nonpunct", nonword.tag=c(),quiet=TRUE);x <- unlist(sapply(attributes(res_read),function(x){if(class(x)=='list'){y<-unlist(x)}else{NULL}})); n <- names(x); x <- as.numeric(x); names(x) <- n;qa1<-cbind(x,n);qa<-na.omit(qa1);for(i in 1:nrow(qa)) {read.matrix[2,which(read.matrix[1,]==qa[,2][i])]<-qa[,1][i]}},error=function(e){cat("Function error for the following text. Values are coerced to zero :","\n",text,"\n");return(NA)})} else {mat_z<-rep(0,140);read.matrix[2,]<-mat_z};read.matrix1<-read.matrix[2,,drop=FALSE];read.matrix2<-matrix(as.numeric(unlist(read.matrix1)),nrow=nrow(read.matrix1));return(read.matrix2)}
    
    read_val<-t(sapply(text,function(text) read_function(text)))
    read_val[!is.finite(read_val)]<-0
    rownames(read_val)<-NULL
    colnames(read_val)<-sprintf("R_koRpus_readability_%s",read_cols)
    return(read_val)
}
text_to_transformed_text<-function(text,dic=DIC){
    compressed_text<-tolower(gsub('[^a-zA-Z0-9]+','',text))
    chunk<-function(text){if(nchar(text)>=6){x_list<-as.character();sx<-paste(text,'1',sep=""); i<-2; j<-0;repeat{repeat{k1<-substring(sx,1,i);k2<-substring(sx,1,i+1); k3<-substring(sx,1,i+2);k4<-substring(sx,1,i+3);k<-c(k1,k2,k3,k4);z<-sapply(k,function(k) which(k==dic));if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+3;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+3;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+4;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-2;j<-1};if(j==1){break}};x_list<-c(x_list,substring(k1,1,nchar(k1)-1));sx<-substring(sx,nchar(k1),nchar(sx));if(nchar(sx)<=1){break}};x_list<-paste(x_list,collapse=" ")} else x_list<-text;return(x_list)}
    
    chunked_text<-unlist(lapply(compressed_text, function(compressed_text) chunk(compressed_text)))
    transformed.text<-cbind(compressed_text,chunked_text)
    rownames(transformed.text)<-NULL
    colnames(transformed.text)<-c('compressed text','chunked text')
    return(transformed.text)
}
text_to_ablation_features<-function(text){
    feature_i_read<-text_to_readability_features(text)
    if(length(text)>1){
    feature_o_read<-text_to_readability_features(paste(text[1:length(text)],collapse=" "))
    feature_o_read<-feature_o_read[rep(seq_len(nrow(feature_o_read)),each=length(text)),]
    feature_oi_read<-feature_o_read-feature_i_read
    colnames(feature_oi_read)<-sprintf("ablation_%s",colnames(feature_i_read))
    read_features<-cbind(feature_i_read,feature_oi_read)
    }
    else {
        feature_oi_read<-feature_i_read-feature_i_read
        colnames(feature_oi_read)<-sprintf("ablation_%s",colnames(feature_i_read))
        read_features<-cbind(feature_i_read,feature_oi_read)
    }
    read_features[!is.finite(read_features)]<-0
    rownames(read_features)<-NULL
    return(read_features)
}
text_to_lexical_diversity_features<-function(text){
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
text_to_chunked_features<-function(text){   
    cht<-text_to_transformed_text(text)[,2,drop=FALSE]
    cht_read<-text_to_readability_features(cht)
    colnames(cht_read)<-sprintf("chunked_%s",colnames(cht_read))
    
    cht_misogyny<-text_to_misogyny(cht)
    colnames(cht_misogyny)<-sprintf("chunked_%s",colnames(cht_misogyny))
    
    cht_ld<-text_to_lexical_diversity_features(cht)
    colnames(cht_ld)<-sprintf("chunked_%s",colnames(cht_ld))
    
    cht_all<-cbind(cht_read,cht_misogyny,cht_ld)
    cht_all[!is.finite(cht_all)]<-0
    rownames(cht_all)<-NULL
    return(cht_all)
}
text_to_ngrams_list<-function(text,n=N){
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
    ngrams_list<-text
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
        
        ngrams_list<-tdm.chunked$dimnames$Terms[tdm.chunked.gram.freq1$Var1]
        
        },error=function(e){return(NA)})
    return(ngrams_list)
}
text_to_ngram_features_scoring<-function(text,n=N){
    mat<-matrix(,nrow=length(text),ncol=1000)
    var.n<-seq(1:1000)
    colnames(mat)<-sprintf("R_NLP_ngrams_ngram_%s",var.n)
    
    for(i in 1: length(text)){
        ngrams.list<-text_to_ngrams_list(text[i])
        ngrams.found<-match(ngrams.list,bigrams.d[,1])
        ngrams.found<-ngrams.found[!is.na(ngrams.found)]
        if(length(ngrams.found)>0){
            for(j in 1:length(ngrams.found)){
                
                mat[i,which(colnames(mat)==bigrams.d[ngrams.found[j],2])]<-1
            }
        }
    }
    
    mat[is.na(mat)]<-0
    rownames(mat)<-NULL
    return(mat)
}
text<-dataset1$Text    
feature.matrix<-cbind(text_to_ablation_features(text),text_to_lexical_diversity_features(text),text_to_misogyny(text),text_to_chunked_features(text),text_to_ngram_features_scoring(text))
rownames(feature.matrix)<-NULL   
feature.matrix[!is.finite(feature.matrix)]<-0
feature.matrix1<-cbind(dataset2,feature.matrix)
feature.matrix1<-as.data.frame(feature.matrix1)   
maml.mapOutputPort("feature.matrix1");