SRC = spanning-tree.pl
OUT = flp23-log

$(OUT): $(SRC)
	swipl -q -g start -o $(OUT) -c $(SRC)

.PHONY: clean
clean:
	rm -f $(OUT)

.PHONY: zip
zip:
	zip flp-log-xhosad00.zip spanning-tree.pl Makefile README.md in.txt
