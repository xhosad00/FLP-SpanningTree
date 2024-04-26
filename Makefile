SRC = spanning-Tree.pl
OUT = flp23-log

all: $(OUT)

$(OUT): $(SRC)
	swipl -q -g start -o $(OUT) -c $(SRC)

clean:
	rm -f $(OUT)

.PHONY: clean
# TODO UNTESTED