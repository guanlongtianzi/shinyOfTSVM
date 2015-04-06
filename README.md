# shinyOfTSVM
基于TSVM的半监督分类算法，shiny应用，使用到了`shiny`、`klaR`、`shinyAce`、`rmarkdown`、`ggplot2`、`gridExtra`、`knitr`、`MASS`等`R`package，使用方法为：
```R
if(!require(shiny)) {
  install.packages(pkgs = 'shiny',quiet = TRUE)
  require(shiny)
}
if(!require(klaR)) {
  install.packages(pkgs = 'klaR',quiet = TRUE)
  require(klaR)
}
if(!require(MASS)) {
  install.packages(pkgs = 'MASS',quiet = TRUE)
  require(MASS)
}
if(!require(shinyAce)) {
  install.packages(pkgs = 'shinyAce',quiet = TRUE)
  require(shinyAce)
}
if(!require(rmarkdown)) {
  install.packages(pkgs = 'rmarkdown',quiet = TRUE)
  require(rmarkdown)
}
if(!require(ggplot2)) {
  install.packages(pkgs = 'ggplot2',quiet = TRUE)
  require(ggplot2)
}
if(!require(gridExtra)) {
  install.packages(pkgs = 'gridExtra',quiet = TRUE)
  require(gridExtra)
}
if(!require(knitr)) {
  install.packages(pkgs = 'knitr',quiet = TRUE)
  require(knitr)
}
runGitHub(username = 'guanlongtianzi',repo = 'shinyOfTSVM') 
```
另外，因为调用了svm-light，github上传不方便，因此给个百度云盘的下载地址：http://pan.baidu.com/s/1tsR06，密码: b5gq。把TSVM这个文件夹放在shiny文件夹内部，即与ui.R、server.R、global.R同一个文件夹下。
