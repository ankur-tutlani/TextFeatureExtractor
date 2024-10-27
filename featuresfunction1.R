#Pass input dataset in CSV file with header of 'Text'
### dataset1 is input dataframe
#dataset1 <- maml.mapInputPort(1) # class: data.frame
# Below objects are required to use some of the functions below in the file.
# below csv and zip files are being available over the web.

EMOT<<-read.csv('src/emot_dic.csv',header=FALSE,stringsAsFactors=FALSE)$V1
DIC<<-read.csv('src/dic.csv',header=FALSE,stringsAsFactors=FALSE)$V1
PATH.BING<<-"src/opinion-lexicon-English.RAR"
PATH.NRC<<-"src/NRC-Emotion-Lexicon-v0.92.zip"
PATH.AFINN<<-"src/imm6010.zip"
install.packages("src/syuzhet_0.2.0.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/sentiment.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/Rstem_0.4-1.zip",lib=".",repos=NULL,verbose=TRUE)
library(Rstem,lib.loc=".",verbose=TRUE)
library(syuzhet,lib.loc=".",verbose=TRUE)
library(sentiment,lib.loc=".",verbose=TRUE)
library(SnowballC)
text_to_character_features<-function(text){
        ch_blanks<-sapply(text,function(text) length(strsplit(text," ")[[1]]))
        ch_blanks<-ch_blanks-1
        ch_blanks<-ifelse(ch_blanks< 0,0,ch_blanks) 
        
        ch_txt_ln<-sapply(text,function(text) nchar(text))
        ch_txt_ln<-ch_txt_ln-ch_blanks
        
        ch_txt_nu<-sapply(text,function(text) length(grep('[0-9]',unlist(strsplit(text,"")))))
     
        ch_txt_al<-sapply(text,function(text) length(grep('[a-zA-Z]',unlist(strsplit(text,"")))))
        
        ch_txt_nalnu<-sapply(text,function(text) length(grep('[a-zA-Z0-9]',unlist(strsplit(text,"")),invert=TRUE)))
        
        character_features<-cbind(ch_blanks,ch_txt_ln,ch_txt_nu,ch_txt_al,ch_txt_nalnu)
        character_features[!is.finite(character_features)]<-0
        colnames(character_features)<-c('character_number_of_whitespace_character_between_tokens','character_length_of_text_excluding_whitespace_character_between_tokens','character_number_of_numeric_characters','character_number_of_alpha_characters','character_number_of_non-alphanumeric_characters')
    rownames(character_features)<-NULL
    return(character_features)
}
text_to_token_features<-function(text,emot=EMOT){
    tokens_count<-sapply(text,function(text) length(strsplit(text,"[.!?*-]| ")[[1]]))
    token_sp<-strsplit(text,"[.!?*-]| ")
 
    token_al<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[a-zA-Z]",token_sp),grep("[0-9]",token_sp,invert=TRUE)),grep("[^a-zA-Z]",token_sp,invert=TRUE)))
    token_al<-sapply(token_al,function(token_al) length(token_al))
 
    token_nu<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[0-9]",token_sp),grep("[a-zA-Z]",token_sp,invert=TRUE)),grep("[^a-zA-Z0-9]",token_sp,invert=TRUE)))
    token_nu<-sapply(token_nu,function(token_nu) length(token_nu))
    
    token_nalnu<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[^a-zA-Z0-9]",token_sp),grep("[0-9]",token_sp)),grep("[a-zA-Z]",token_sp,invert=TRUE)))
    token_nalnu<-sapply(token_nalnu,function(token_nalnu) length(token_nalnu))
    
    token_nalnuj<-lapply(token_sp,function(token_sp) grep("^[^a-zA-Z0-9]*[^a-zA-Z0-9]$",token_sp))
    token_nalnuj<-sapply(token_nalnuj,function(token_nalnuj) length(token_nalnuj))
    
    token_nalal<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[a-zA-Z]",token_sp),grep("[^a-zA-Z]",token_sp)),grep("[0-9]",token_sp,invert=TRUE)))
    token_nalal<-sapply(token_nalal,function(token_nalal) length(token_nalal))
    
    token_alnu<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[a-zA-Z]",token_sp),grep("[0-9]",token_sp)),grep("[^a-zA-Z0-9]",token_sp,invert=TRUE)))
    token_alnu<-sapply(token_alnu,function(token_alnu) length(token_alnu))
    
    token_nalalnu<-lapply(token_sp,function(token_sp) intersect(intersect(intersect(grep("[a-zA-Z]",token_sp),grep("[^a-zA-Z]",token_sp)),grep("[0-9]",token_sp)),grep("[^a-zA-Z0-9]",token_sp)))
    token_nalalnu<-sapply(token_nalalnu,function(token_nalalnu) length(token_nalalnu))
    
    token_le3<-lapply(token_sp,function(token_sp) token_sp[which(nchar(token_sp)<=3)])
    token_le3<-sapply(token_le3,function(token_le3) length(token_le3))
    
    token_le5<-lapply(token_sp,function(token_sp) token_sp[which(nchar(token_sp)<=5)])
    token_le5<-sapply(token_le5,function(token_le5) length(token_le5))
    
    token_gt5<-lapply(token_sp,function(token_sp) token_sp[which(nchar(token_sp)>5)])
    token_gt5<-sapply(token_gt5,function(token_gt5) length(token_gt5))
    
    token_456<-lapply(token_sp,function(token_sp) token_sp[which(nchar(token_sp)==4 | nchar(token_sp)==5 | nchar(token_sp)==6)])
    token_456<-sapply(token_456,function(token_456) length(token_456))
    
    token_789<-lapply(token_sp,function(token_sp) token_sp[which(nchar(token_sp)==7 | nchar(token_sp)==8 | nchar(token_sp)==9)]) 
    token_789<-sapply(token_789,function(token_789) length(token_789))
    
    token_gt10<-lapply(token_sp,function(token_sp) token_sp[which(nchar(token_sp)>=10)])
    token_gt10<-sapply(token_gt10,function(token_gt10) length(token_gt10))
    
    token_nonASCII<-lapply(token_sp,function(token_sp) grep("[^\x20-\x7E]",token_sp))
    token_nonASCII<-sapply(token_nonASCII,function(token_nonASCII) length(token_nonASCII))
    
    token_nalnuj_asc<-lapply(token_sp,function(token_sp) intersect(grep("[^\x20-\x7E]",token_sp,invert=TRUE),grep("^[^a-zA-Z0-9]*[^a-zA-Z0-9]$",token_sp)))
    token_nalnuj_asc<-sapply(token_nalnuj_asc,function(token_nalnuj_asc) length(token_nalnuj_asc))
       
    stemming_f<-function(text){if(nchar(text)>0) {x_list<-c(length(strsplit(text,"[.!?*-]| ")[[1]]),0);tryCatch({txt_s<-strsplit(text,"[.!?*-]| ");txt_s<-unlist(txt_s);txt_s1<-rep("",length(txt_s));y<-rep(0,length(txt_s));for(j in 1 : length(txt_s)) {txt_s1[j]<-wordStem(txt_s[j],language="english");if(txt_s1[j]==txt_s[j]) y[j]==0 else y[j]<-1};y1<-length(which(y==0));y2<-length(which(y==1));x_list<-c(y1,y2)},error=function(e){return(NA)})} else {x_list<-c(0,0)};return(x_list)}
    token_stem<-t(sapply(text,function(text) stemming_f(text)))
    
    token_lal<-lapply(token_sp,function(token_sp) grep("[^a-z]",token_sp,invert=TRUE))
    token_lal<-sapply(token_lal,function(token_lal) length(token_lal))
    
    token_ual<-lapply(token_sp,function(token_sp) grep("[^A-Z]",token_sp,invert=TRUE))
    token_ual<-sapply(token_ual,function(token_ual) length(token_ual))
    
    token_lual<-lapply(token_sp,function(token_sp) intersect(grep("[^A-Z]",token_sp),intersect(grep("[^a-z]",token_sp),grep("[^a-zA-Z]",token_sp,invert=TRUE))))
    token_lual<-sapply(token_lual,function(token_lual) length(token_lual))
    
    token_lnu<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[a-z]",token_sp),grep("[0-9]",token_sp)),grep("[^a-z0-9]",token_sp,invert=TRUE)))
    token_lnu<-sapply(token_lnu,function(token_lnu) length(token_lnu))
    
    token_unu<-lapply(token_sp,function(token_sp) intersect(intersect(grep("[A-Z]",token_sp),grep("[0-9]",token_sp)),grep("[^A-Z0-9]",token_sp,invert=TRUE)))
    token_unu<-sapply(token_unu,function(token_unu) length(token_unu))
    
    token_lunu<-lapply(token_sp,function(token_sp) setdiff(setdiff(intersect(intersect(grep("[a-zA-Z]",token_sp),grep("[0-9]",token_sp)),grep("[^a-zA-Z0-9]",token_sp,invert=TRUE)),intersect(intersect(grep("[a-z]",token_sp),grep("[0-9]",token_sp)),grep("[^a-z0-9]",token_sp,invert=TRUE))),intersect(intersect(grep("[A-Z]",token_sp),grep("[0-9]",token_sp)),grep("[^A-Z0-9]",token_sp,invert=TRUE))))
    token_lunu<-sapply(token_lunu,function(token_lunu)length(token_lunu))
    
    aq<-as.data.frame(table(unlist(sapply(emot,function(emot) grep(emot,text,ignore.case=TRUE)))))
    if(ncol(aq)==1) {aq<-data.frame(var1=1,var3=0)}
    aq3<-cbind(seq(1:length(text)),rep(0,length(text)))
    colnames(aq)<-c("var1","var3")
    colnames(aq3)<-c("var1","var2")
    meg1<-merge(x=aq3, y=aq,by='var1',all.x=TRUE)
    meg1[is.na(meg1)]<-0
    meg1<-meg1[,c('var1','var3')]
    meg1<-as.matrix(meg1,drop=FALSE)
    
    token_features<-cbind(tokens_count,token_al,token_nu,token_nalnu,token_nalnuj,token_nalal,token_alnu,token_nalalnu,token_le3,token_le5,token_gt5,token_456,token_789,token_gt10,token_nonASCII,token_nalnuj_asc,token_stem,token_lal,token_ual,token_lual,token_lnu,token_unu,token_lunu,meg1[,2])
    token_features[!is.finite(token_features)]<-0
    colnames(token_features)<-c('tokens_number_of_tokens','tokens_number_of_tokens_with_just_alpha_characters','tokens_number_of_tokens_with_just_numeric_characters','tokens_number_of_tokens_with_just_non-alpha_and_numeric_characters','tokens_number_of_tokens_with_just_non-alphanumeric_characters','tokens_number_of_tokens_with_just_non-alpha_alpha_characters','tokens_number_of_tokens_with_just_numeric_alpha_characters','tokens_number_of_tokens_with_just_nonalpha_alpha_numeric_characters','tokens_number_of_tokens_less_than_or_equal_to_3_characters','tokens_number_of_tokens_less_than_or_equal_to_5_characters','tokens_number_of_tokens_more_than_5_characters','tokens_number_of_tokens_4_5_6_characters','tokens_number_of_tokens_7_8_9_characters','tokens_number_of_tokens_10_or_more_characters','tokens_number_of_tokens_with_non-ASCII_characters','tokens_number_of_tokens_with_non-alphanumeric_but_ASCII_characters','R_SnowballC_wordStem_tokens_number_of_tokens_not_changed_by_stemming','R_SnowballC_wordStem_tokens_number_of_tokens_changed_by_stemming','tokens_number_of_tokens_with_just_lower-case_characters','tokens_number_of_tokens_with_just_upper-case_characters','tokens_number_of_tokens_with_just_lower-case_and_upper-case_characters','tokens_number_of_tokens_with_just_lower-case_and_numeric_characters','tokens_number_of_tokens_with_just_upper-case_and_numeric_characters','tokens_number_of_tokens_with_just_lower-case_and_upper-case_and_numeric_characters','tokens_number_of_tokens_having_emotion_words')
    rownames(token_features)<-NULL
    return(token_features)
}
text_to_differential_features<-function(text){
    feature_i_c<-text_to_character_features(text)
    feature_i_t<-text_to_token_features(text)
    if(length(text)>1) {
    feature_o_c<-text_to_character_features(paste(text[1:length(text)],collapse=" "))
    feature_o_c<-feature_o_c[rep(seq_len(nrow(feature_o_c)),each=length(text)),]
    feature_oi_c<-feature_o_c-feature_i_c
    colnames(feature_oi_c)<-sprintf("ablation_%s",colnames(feature_i_c))
   
    feature_o_t<-text_to_token_features(paste(text[1:length(text)],collapse=" "))
    feature_o_t<-feature_o_t[rep(seq_len(nrow(feature_o_t)),each=length(text)),]
    feature_oi_t<-feature_o_t-feature_i_t
    colnames(feature_oi_t)<-sprintf("ablation_%s",colnames(feature_i_t))
    
    feature_ablation<-cbind(feature_i_c,feature_oi_c,feature_i_t,feature_oi_t)
   }
   else {
       feature_oi_c<-feature_i_c-feature_i_c
       colnames(feature_oi_c)<-sprintf("ablation_%s",colnames(feature_i_c))
       feature_oi_t<-feature_i_t-feature_i_t
       colnames(feature_oi_t)<-sprintf("ablation_%s",colnames(feature_i_t))
       feature_ablation<-cbind(feature_i_c,feature_oi_c,feature_i_t,feature_oi_t)
   }
    feature_ablation[!is.finite(feature_ablation)]<-0
    rownames(feature_ablation)<-NULL
    return(feature_ablation)
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
text_to_indicator_features<-function(text){
    indi_nu<-lapply(text,function(text)grep('[0-9]',text))
    indi_nu<-as.numeric(indi_nu)
    indi_nu[is.na(indi_nu)]<-0
    
    indi_al<-lapply(text,function(text)grep('[a-zA-Z]',text))
    indi_al<-as.numeric(indi_al)
    indi_al[is.na(indi_al)]<-0
    
    indi_punc<-lapply(text,function(text)grep('[[:punct:]]',text))
    indi_punc<-as.numeric(indi_punc)
    indi_punc[is.na(indi_punc)]<-0
    
    indi_nonASCII<-lapply(text,function(text) grep("[^\x20-\x7E]",text))
    indi_nonASCII<-as.numeric(indi_nonASCII)
    indi_nonASCII[is.na(indi_nonASCII)]<-0
    
    indi_nalnu_asc<-lapply(text,function(text) intersect(grep('[^a-zA-Z0-9]',text),grep('[^\x20-\x7E]',text,invert=TRUE)))
    indi_nalnu_asc<-as.numeric(indi_nalnu_asc)
    indi_nalnu_asc[is.na(indi_nalnu_asc)]<-0
    
    indicator_features<-cbind(indi_nu,indi_al,indi_punc,indi_nonASCII,indi_nalnu_asc)
    colnames(indicator_features)<-c('indicator_at_least_one_numeric_character','indicator_at_least_one_alpha_character','indicator_at_least_one_punctuation','indicator_at_least_one_non-ASCII','indicator_at_least_one_non-alphanumeric_but_ASCII')
    rownames(indicator_features)<-NULL
    return(indicator_features)
}
text_to_sentiment_features<-function(text,path.bing=PATH.BING,path.nrc=PATH.NRC,path.afinn=PATH.AFINN){
    s1_tr<-get_nrc_sentiment(text)
    s1_tr<-as.matrix(s1_tr)

    s2_1<-get_sentiment(text,method="bing",path_to_tagger=path.bing)
    s2_2<-get_sentiment(text,method="nrc",path_to_tagger=path.nrc)
    
    s2_3<-get_sentiment(text,method="afinn",path_to_tagger=path.afinn)
      
    classify_emo<-classify_emotion(text,algorithm="bayes",prior=1.0)
    cl<-classify_emo[,1:6,drop=FALSE]
    cl1<-matrix(as.numeric(unlist(cl)),nrow=nrow(cl))
    
    classify_pol<-classify_polarity(text,algorithm="bayes",prior=1.0)
    dl<-classify_pol[,1:3,drop=FALSE]
    dl1<-matrix(as.numeric(unlist(dl)),nrow=nrow(dl))
     
    sentiment_features<-cbind(s1_tr,s2_1,s2_2,s2_3,cl1,dl1)
    
    sentiment_features[!is.finite(sentiment_features)]<-0
   
    colnames(sentiment_features)<-c('R_syuzhet_get_nrc_sentiment_anger','R_syuzhet_get_nrc_sentiment_anticipation','R_syuzhet_get_nrc_sentiment_disgust','R_syuzhet_get_nrc_sentiment_fear','R_syuzhet_get_nrc_sentiment_joy','R_syuzhet_get_nrc_sentiment_sadness','R_syuzhet_get_nrc_sentiment_surprise','R_syuzhet_get_nrc_sentiment_trust','R_syuzhet_get_nrc_sentiment_negative','R_syuzhet_get_nrc_sentiment_positive','R_syuzhet_get_sentiment_bing','R_syuzhet_get_sentiment_nrc','R_syuzhet_get_sentiment_afinn','R_sentiment_classify_emotion_ANGER','R_sentiment_classify_emotion_DISGUST','R_sentiment_classify_emotion_FEAR','R_sentiment_classify_emotion_JOY','R_sentiment_classify_emotion_SADNESS','R_sentiment_classify_emotion_SURPRISE','R_sentiment_classify_polarity_POS','R_sentiment_classify_polarity_NEG','R_sentiment_classify_polarity_POS/NEG')
    rownames(sentiment_features)<-NULL
    return(sentiment_features) 
}
text_to_chunked_features<-function(text){
    cht<-text_to_transformed_text(text)[,2,drop=FALSE]
    cht_char<-text_to_character_features(cht)
    colnames(cht_char)<-sprintf("chunked_%s",colnames(cht_char))
    
    cht_tok<-text_to_token_features(cht)
    colnames(cht_tok)<-sprintf("chunked_%s",colnames(cht_tok))
    
    cht_sent<-text_to_sentiment_features(cht)
    colnames(cht_sent)<-sprintf("chunked_%s",colnames(cht_sent))
    
    cht_indi<-text_to_indicator_features(cht)
    colnames(cht_indi)<-sprintf("chunked_%s",colnames(cht_indi))
    
    cht_all<-cbind(cht_char,cht_tok,cht_sent,cht_indi)
    cht_all[!is.finite(cht_all)]<-0
    rownames(cht_all)<-NULL
    return(cht_all)
}
text<-dataset1$Text
feature.matrix<-cbind(text_to_differential_features(text),text_to_indicator_features(text),text_to_sentiment_features(text),text_to_chunked_features(text))
rownames(feature.matrix)<-NULL
feature.matrix[!is.finite(feature.matrix)]<-0
feature.matrix<-as.data.frame(feature.matrix)
#maml.mapOutputPort("feature.matrix");
# Output is feature.matrix object
