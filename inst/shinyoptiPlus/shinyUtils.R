print("toto")


exaBox <- function(title, value, icon, width, color){

  if(color == "paramCol"){
    cla <- "exaBox"
  }
  if(color == "scoreCol"){
    cla <- "exaBoxdarkblue"
  }


      htmlB <- paste0(
        '<div class="',cla,'"><div><img src="',
        icon,
        '"/><br>',
        title,
        '<br><b>',
        value,
        '</b></div></div>'
      )



   column(HTML(htmlB), width = width)

}


