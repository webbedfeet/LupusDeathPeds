find_dropbox <- function(){
  require('RCurl')
  if(Sys.info()['sysname'] %in% c('Darwin','Linux')){
    dropbox_info <- file.path('~','.dropbox','host.db')
  }
  if(Sys.info()['sysname'] == "Windows"){
    dropbox_info <- paste0(Sys.getenv('APPDATA'),'\\Dropbox\\host.db')
    if(!file.exists(dropbox_info)){
      dropbox_info <- paste0(Sys.getenv('LOCALAPPDATA'), '\\Dropbox\\host.db')
    }
  }
  base64coded <- readLines(dropbox_info, warn=F)[2]
  base64Decode(base64coded)
}
