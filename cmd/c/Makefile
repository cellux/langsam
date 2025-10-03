CFLAGS = -g -O0 -fno-omit-frame-pointer -lm

langsam: langsam.c ../../c/langsam.o ../../c/langsam_l.o ../../c/os.o
	$(CC) $(CFLAGS) -o langsam $^

.PHONY: clean
clean:
	rm -fv langsam
