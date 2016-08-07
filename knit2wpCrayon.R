knit2wpCrayon <- function(input, title="A post from knitr", ...,
                          action=c("newPost", "editPost", "newPage"),
                          postid, encoding=getOption("encoding"),
                          upload=FALSE, publish=FALSE, write=TRUE)
{
  out <- knit(input, encoding=encoding)
  on.exit(unlink(out))
  con <- file(out, encoding=encoding)
  on.exit(close(con), add=TRUE)
  content <- knitr:::native_encode(readLines(con, warn=FALSE))
  content <- paste(content, collapse="\n")
  content <- markdown::markdownToHTML(text=content, fragment.only=TRUE)
  content <- gsub("<pre><code class=\"([[:alpha:]]+)\">(.+?)</code></pre>",
                  "<pre class=\"lang:\\1 decode:true\">\\2</pre>",
                  content)
  content=knitr:::native_encode(content, "UTF-8")
  title=knitr:::native_encode(title, "UTF-8")
  if (write){
    writeLines(text=content,
               con=gsub(x=out, pattern="\\.md$", replacement=".html"))
  }
  if (upload){
    action=match.arg(action)
    WPargs=list(content=list(description=content, title=title, 
                             ...), publish=publish)
    if (action=="editPost") 
      WPargs=c(postid=postid, WPargs)
    do.call("library", list(package="RWordPress", character.only=TRUE))
    print(do.call(action, args=WPargs))
  }
}