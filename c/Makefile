CFLAGS = -g -O0 -fno-omit-frame-pointer

langsam_l.c: ../langsam.l
	python3 bin2c.py $< $@ langsam_l

langsam.o: langsam.c langsam.h

os.o: os.c

.PHONY: clean
clean:
	rm -fv *.a *.o langsam_l.c
