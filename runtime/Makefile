LIB=libpulsert.a

DEPS=syscalls.s io.s
OBJ=${DEPS:.s=.o}

%.o: %.s
	as $^ -o $@

$(LIB): $(OBJ)
	ar rcs $@ $(OBJ)

clean:
	$(RM) $(LIB) $(OBJ)
