langsam.a: langsam.o langsam_l.o

langsam.o: langsam.c langsam.h

langsam_l.c: ../langsam.l
	python3 bin2c.py $< $@ langsam_l

.PHONY: clean
clean:
	rm -fv *.a *.o langsam_l.c
