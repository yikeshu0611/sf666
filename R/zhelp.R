chinese<-function(){
    #judge whether the system is Chinese
    grepl('chinese',tolower(Sys.getlocale()))
}
