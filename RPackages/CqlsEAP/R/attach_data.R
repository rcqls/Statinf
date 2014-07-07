path.data <- function(...,package="CqlsEAP") {
	system.file("data",...,package=package)
}

attach.data <- function(...,package="CqlsEAP") {
	attach(path.data(...,package=package))
}

detach.data <- function(...,package="CqlsEAP") {
	filename <- path.data(...,package=package)
	pos <- which(search() %in% paste("file",filename,sep=":"))
	# eval since detach does not work undirectly with integer and character vector
	if(length(pos)>0) eval(parse(text=paste("detach(",pos[1],")",sep="")))
}