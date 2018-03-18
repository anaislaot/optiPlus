print("toto")


exaBox <- function(title, value, icon, width){
  htmlB <- paste0(
         '<div class="exaBox"><div><img src="',
         icon,
         '"/><br>',
         title,
         '<br><b>',
         value,
         '</b></div></div>'
         )


   column(HTML(htmlB), width = width)

}


