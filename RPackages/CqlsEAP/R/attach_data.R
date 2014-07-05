attach.data <- function(...,package="CqlsEAP") {
	attach(system.file("data",...,package=package))
}

detach.data <- function(...,package="CqlsEAP") {
	filename <- system.file("data",...,package=package)
	pos <- which(search() %in% paste("file",filename,sep=":"))
	# eval since detach does not work undirectly with integer and character vector
	if(length(pos)>0) eval(parse(text=paste("detach(",pos[1],")",sep="")))
}