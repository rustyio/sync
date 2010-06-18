compile:
	erl -make

clean:
	rm ebin/*.beam

run:
	erl -pa ebin

