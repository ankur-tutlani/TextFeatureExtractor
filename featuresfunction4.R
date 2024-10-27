#Pass input dataset in CSV file with header of 'Text'
## dataset1 is input dataframe
## dataset2 is the output from featuresfunction3.R script.
#dataset1 <- maml.mapInputPort(1) # class: data.frame
#dataset2 <- maml.mapInputPort(2) # class: data.frame
### below csv,text and zip files are being available over the web..

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

PATH.TREETAGGER<<-"src/TreeTagger/bin/tag-english.bat"
MD_SENT<<-NULL
MD_WORD<<-NULL
MD_POS<<-NULL
MD_DATE<<-NULL
MD_LOC<<-NULL
MD_MONEY<<-NULL
MD_ORG<<-NULL
MD_PERSON<<-NULL
MD_PERCENT<<-NULL
SPELL.DIC<<-qdapDictionaries::GradyAugmented
SPELL.METHOD<<-"jw"

install.packages("src/syuzhet_0.2.0.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/sentiment.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/Rstem_0.4-1.zip",lib=".",repos=NULL,verbose=TRUE)
install.packages("src/koRpus_0.05-6.zip",lib=".",repos=NULL,verbose=TRUE)

library(Rstem,lib.loc=".",verbose=TRUE)
library(syuzhet,lib.loc=".",verbose=TRUE)
library(sentiment,lib.loc=".",verbose=TRUE)
library(koRpus,lib.loc=".",verbose=TRUE)

text_to_pos_features<-function(text,md_sent=MD_SENT,md_word=MD_WORD,md_pos=MD_POS){
    # POS features - POS tags definition as used in Penn Treebank project (http://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html)
    
    #md_sent,md_word,md_pos: model file used. Deafult value is NULL indicating default model file is used (http://opennlp.sourceforge.net/models-1.5/)
    
    require(NLP)
    require(openNLP)
    sta<-Maxent_Sent_Token_Annotator(model=md_sent) 
    wta<-Maxent_Word_Token_Annotator(model=md_word) 
    pos<-Maxent_POS_Tag_Annotator(model=md_pos) 
    tag_matrix<-matrix(,nrow=length(text),ncol=36)
    tag_matrix1<-matrix(,nrow=1,ncol=36)
    tag_matrix1[1,]<-c("CC","CD","DT","EX","FW","IN","JJ","JJR","JJS","LS","MD","NN","NNS","NNP","NNPS","PDT","POS","PRP","PRP$","RB","RBR","RBS","RP","SYM","TO","UH","VB","VBD","VBG","VBN","VBP","VBZ","WDT","WP","WP$","WRB")
    for (i in 1:length(text)){
       if(nchar(text[i])>0){
           l1<-annotate(text[i],list(sta,wta))
           l2<-annotate(text[i],pos,l1)
           l2w<-subset(l2,type=="word")
           tags<-sapply(l2w$features,`[[`,"POS")
           x_list<-as.data.frame(table(tags),stringsAsFactors=FALSE)
           for(j in 1:length(x_list$tags)) {
               tag_matrix[i,which(tag_matrix1[1,]==x_list$tags[j])]<-x_list$Freq[j]
            }
       } else {
           mat<-rep(NA,36)
           tag_matrix[i,]<-mat
       }
   }
    colnames(tag_matrix)<-sprintf("R_openNLP_Maxent_POS_Tag_Annotator_%s",tag_matrix1[1,])
    
    tag_matrix[!is.finite(tag_matrix)]<-0
    
    rownames(tag_matrix)<-NULL
    
    return(tag_matrix)
}

text_to_entity_features<-function(text,md_sent=MD_SENT,md_word=MD_WORD,md_date=MD_DATE,md_loc=MD_LOC,md_money=MD_MONEY,md_org=MD_ORG,md_person=MD_PERSON,md_percent=MD_PERCENT){
    #Named entity recognition
    #entities considered: date, location, money,organzation,person,percentage
    #md_sent,md_word,md_date,md_loc,md_money,md_org,md_person,md_percent: model files to be used. Default is NULL indicating default model file used for english language (http://opennlp.sourceforge.net/models-1.5/)
    
    require(NLP)
    require(openNLP)
    
    sta<-Maxent_Sent_Token_Annotator(model=md_sent)
    wta<-Maxent_Word_Token_Annotator(model=md_word)
    
    ent_date<-Maxent_Entity_Annotator(kind="date",model=md_date)
    ent_loc<-Maxent_Entity_Annotator(kind="location",model=md_loc)
    ent_mon<-Maxent_Entity_Annotator(kind="money",model=md_money)
    ent_org<-Maxent_Entity_Annotator(kind="organization",model=md_org)
    ent_per<-Maxent_Entity_Annotator(kind="person",model=md_person)
    ent_percent<-Maxent_Entity_Annotator(kind="percent",model=md_percent)
    
    gc(reset=TRUE)
    ent<-c(ent_date,ent_loc,ent_mon,ent_org,ent_per,ent_percent)
    
    entity.matrix<-matrix(,nrow=length(text),ncol=6)
    vec1<-vector("numeric",6)
    
    for(i in 1:length(text)){
        gc(reset=TRUE)
        if(nchar(text[i])>0){
           gc(reset=TRUE)
           l1<-annotate(text[i],list(sta,wta))
            
           for(j in 1:6)
            {
               gc(reset=TRUE)
               l2<-annotate(text[i],ent[j],l1)
               l2w<-subset(l2,type=="entity")
                tags_l2<-sapply(l2w$features,`[[`,"kind")
                vec1[j]<-length(tags_l2)
                
            }
            
            entity.matrix[i,]<-vec1
        } 
        else {
            
           entity.matrix[i,]<-rep(0,6)
        }
   }
    rownames(entity.matrix)<-NULL
    
    entity.matrix[!is.finite(entity.matrix)]<-0
    
    colnames(entity.matrix)<-c('R_openNLP_Maxent_Entity_Annotator_date','R_openNLP_Maxent_Entity_Annotator_location','R_openNLP_Maxent_Entity_Annotator_money','R_openNLP_Maxent_Entity_Annotator_organization','R_openNLP_Maxent_Entity_Annotator_person','R_openNLP_Maxent_Entity_Annotator_percent')
    
    return(entity.matrix) 
}

text_to_differential_features<-function(text){
#ablation on POS features
    
    feature_i_pos<-text_to_pos_features(text)
    feature_o_pos<-text_to_pos_features(paste(text[1:length(text)],collapse=" "))
    feature_o_pos<-feature_o_pos[rep(seq_len(nrow(feature_o_pos)),each=length(text)),]
    feature_oi_pos<-feature_o_pos-feature_i_pos
    colnames(feature_oi_pos)<-sprintf("ablation_%s",colnames(feature_i_pos))
    
    #ablation on entity features
    
    feature_i_entity<-text_to_entity_features(text)
    feature_o_entity<-text_to_entity_features(paste(text[1:length(text)],collapse=" "))
    feature_o_entity<-feature_o_entity[rep(seq_len(nrow(feature_o_entity)),each=length(text)),]
    feature_oi_entity<-feature_o_entity-feature_i_entity
    colnames(feature_oi_entity)<-sprintf("ablation_%s",colnames(feature_i_entity))
    
    feature_ablation<-cbind(feature_oi_pos,feature_oi_entity)
   
    feature_ablation[!is.finite(feature_ablation)]<-0
    
    rownames(feature_ablation)<-NULL
    
    return(feature_ablation)   
}

text_to_transformed_text<-function(text,dic=DIC){
    
    # make 'dic' as character vector containing dictionary corpus (only alpha character words) in global environment
    
    # compressed text
    compressed_text<-tolower(gsub('[^a-zA-Z0-9]+','',text))
    
    # chunked text
    chunk<-function(text){if(nchar(text)>=6){x_list<-as.character();sx<-paste(text,'1',sep=""); i<-2; j<-0;repeat{repeat{k1<-substring(sx,1,i);k2<-substring(sx,1,i+1); k3<-substring(sx,1,i+2);k4<-substring(sx,1,i+3);k<-c(k1,k2,k3,k4);z<-sapply(k,function(k) which(k==dic));if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])>0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+1;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])>0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-i+2;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])==0) {i<-i+3;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])>0 && length(z[[4]])>0) {i<-i+3;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])>0) {i<-i+4;j<-0};if(length(z[[1]])==0 && length(z[[2]])==0 && length(z[[3]])==0 && length(z[[4]])==0) {i<-2;j<-1};if(j==1){break}};x_list<-c(x_list,substring(k1,1,nchar(k1)-1));sx<-substring(sx,nchar(k1),nchar(sx));if(nchar(sx)<=1){break}};x_list<-paste(x_list,collapse=" ")} else x_list<-text;return(x_list)}
    
    chunked_text<-unlist(lapply(compressed_text, function(compressed_text) chunk(compressed_text)))
    
    transformed.text<-cbind(compressed_text,chunked_text)
    
    rownames(transformed.text)<-NULL
    
    colnames(transformed.text)<-c('compressed text','chunked text')
    
    return(transformed.text)
    
}

text_to_chunked_features<-function(text){
    
    cht<-text_to_transformed_text(text)[,2,drop=FALSE]
    
    cht_pos<-text_to_pos_features(cht)
    colnames(cht_pos)<-sprintf("chunked_%s",colnames(cht_pos))
    
    cht_entity<-text_to_entity_features(cht)
    colnames(cht_entity)<-sprintf("chunked_%s",colnames(cht_entity))
    
    cht_all<-cbind(cht_pos,cht_entity)
    
    cht_all[!is.finite(cht_all)]<-0
    
    rownames(cht_all)<-NULL
    
    return(cht_all)
    
}
text<-dataset1$Text
feature.matrix<-cbind(text_to_pos_features(text),text_to_entity_features(text),text_to_differential_features(text),text_to_chunked_features(text))
rownames(feature.matrix)<-NULL
    
feature.matrix[!is.finite(feature.matrix)]<-0
feature.matrix1<-cbind(dataset2,feature.matrix)
feature.matrix1<-as.data.frame(feature.matrix1)
#maml.mapOutputPort("feature.matrix1");
