all: compile
	tools/unix_macro2variable > lib/usocket/consts.sls

compile: tools/unix_macro2variable

tools/unix_macro2variable: tools/unix_macro2variable.c
	gcc tools/unix_macro2variable.c -o tools/unix_macro2variable
