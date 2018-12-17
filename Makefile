CC ?= cc
RM ?= rm -f

all: compile
	tools/unix_macro2variable > lib/usocket/consts.sls

compile: tools/unix_macro2variable

tools/unix_macro2variable: tools/unix_macro2variable.c
	$(CC) tools/unix_macro2variable.c -o tools/unix_macro2variable

clean:
	$(RM) lib/usocket/consts.sls
	$(RM) tools/unix_macro2variable
